# import libaries
library(tidyverse)
library(shiny)
library( plyr )
library(lubridate)
library(ggplot2)
library(sf)
library(leaflet)
library(dplyr)
library(geosphere)
library(bslib)

## Import the data
#change wd so i can use ldply
setwd('/Users/yevashanperumal/Desktop/Data Science Masters/2021/EDA - STA5092Z/Assignments/EDA_Assignment_3/data')
df <- ldply( .data = list.files(pattern="*.csv"),
                    .fun = read.csv)
# reset wd 
setwd('/Users/yevashanperumal/Desktop/Data Science Masters/2021/EDA - STA5092Z/Assignments/EDA_Assignment_3')

#No nulls?
# sum(is.na(df))

# convert date and time variables from characters
df["date"] <- as_date(df$date)
df["time_new"] <- parse_date_time(df$time,"HMS")

#Storing orignal numeric lng lat for the leaflet map
df["leaf_lng"] <- df$lng
df["leaf_lat"] <- df$lat

# Converting to sfc object so I can do distnace measurments
df <- st_as_sf(df,coords = c("lng","lat"),crs=4326)

# Row Number per group
df<-df%>%group_by(date)%>%
    dplyr::mutate(id = row_number())

#func to calc distance between two succesive rows of points
dist <- function(g1, g2,id) {
    if (any(is.na(c(g1, g2)))) {
        0
    }else if (id==1) {
       0
    }else{
        st_distance(g1,g2)*100000
    }
}
# Apply function to data frame
df <- df %>% group_by(date)%>%
    dplyr::mutate(sf_distance = mapply(dist,geometry,lag(geometry,n=1),id))%>%
    ungroup()


# Cosine Distance
modified_distCosine <- function(Longitude1, Latitude1, Longitude2, Latitude2,id) {
    if (any(is.na(c(Longitude1, Latitude1, Longitude2, Latitude2)))) {
        0
    } else if(id==1) {
        0
    }else{
        distCosine(c(Longitude1, Latitude1), c(Longitude2, Latitude2))
    }
}
df <- df %>% dplyr::group_by(date)%>% 
    dplyr::mutate(cosine_distance = mapply(modified_distCosine,leaf_lng, leaf_lat, lag(leaf_lng), lag(leaf_lat),id))%>%
    dplyr::ungroup()

#Cumulative Distance per date
df <- df %>% dplyr::group_by(date) %>% 
    dplyr::mutate(cum_dist_sf = cumsum(sf_distance),cum_dist_cosine= cumsum(cosine_distance))%>%
    dplyr::mutate(cum_sum_sf_km = round(cum_dist_sf/1000,2),cum_sum_cosine_km = round(cum_dist_cosine/1000,2))%>%
    dplyr::mutate(floor_cumsum_sf_km=floor(cum_sum_sf_km),floor_cumsum_cosine_km=floor(cum_sum_cosine_km))%>%
    dplyr::ungroup()

# Total elevation gain and drop
df<-df%>%group_by(date)%>%
    dplyr::mutate(elev_change = elevation-lag(elevation))%>%
    replace_na(list(elev_change = 0))%>%
    dplyr::mutate(cum_elev_change = cumsum(elev_change))

# Save file to load wrangled version for app
save(df,file = "df.Rdata")

# Single Run Aggregates
head(df)
single_run<- df %>%group_by(date)%>%
                    dplyr::mutate(dist = max(cum_sum_sf_km),
                                dur = round(as.numeric(tail(time_new,1)-time_new[1]),2),
                                pace = round(as.numeric(tail(time_new,1)-time_new[1])/max(cum_sum_sf_km),2),
                                elev = round(max(cum_elev_change)),2)%>%
                                st_set_geometry(NULL)%>%
                                select(date,dist,dur,pace,elev)%>%
                                distinct()
names(single_run) <- c("Date","Distance(Km)","Duration (mins)","Pace per Km","Overall Elevation Change(m)")







test <- df%>% filter(date=='2019-02-27')%>% #pick a selected day 
    arrange(time_new)

head(test)
t<-tail(test)
st_set_geometry(t, NULL)

unique(test2$split)
head(test)
z<-df[365:375,]
sum(test$sf_distance)
test%>% select(sf_distance,cumsum)

test$geometry

ui <- fluidPage(
    theme = bs_theme(version = 4, bootswatch = "solar"),
    titlePanel("Runtastic"),
    tabsetPanel(
        tabPanel("Latest Run Summary",
            fluidRow(
                column(6,selectInput("run_date",label= "Run Date", choices = unique(df$date), selected = max(df$date))),
                column(4,checkboxInput("split","Show Splits Per Km"))
                ),
            fluidRow(
                column(width =  6,leafletOutput("mymap")),
                column(width =  5,offset = 1, tableOutput("mytable"))
                ),
            fluidRow(column(width = 9,offset = 3,dataTableOutput("split_data")))
        ),
        tabPanel("Your Last 5 Runs",
                 fluidRow(plotlyOutput("five_dist")),
                 fluidRow(tableOutput("five_table"))
                 ),
        tabPanel("All Your Runs",dataTableOutput("all_runs")),    
        tabPanel(title = "Be Inspired to Run", icon = icon("youtube"),
                 tags$br(),
                 tags$iframe(
                     width="560", 
                     height="315",
                     src="https://www.youtube.com/embed/G0YwEc50dZg",
                     title="YouTube video player",
                     frameborder="0" ,
                     allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
                     allowfullscreen=TRUE
                 ),
                 tags$p(
                     id = "video-attribution",
                     tags$a(
                         "Video", 
                         href = "https://www.youtube.com/embed/G0YwEc50dZg"
                     ),
                     "by",
                     tags$a(
                         "Beinspiredchannel.com",
                         href = "https://beinspiredchannel.com/",
                         class = "site"
                     )
                 )
        )
        )
)

server <- function(input, output) {
    stats <- reactive({
        current_run <- df%>% filter(date==input$run_date)%>% arrange(time_new) #pick a selected day
        run_duration <-as.numeric(tail(test$time_new,1)-test$time_new[1]) #length of run in minutes
        run_dist <- max(current_run$cum_sum_sf_km)
        avg_pace <- as.numeric(run_duration)/run_dist
        elev_change <- max(current_run$cum_elev_change)
        Stats <- c("Run Duration (min)","Run Distance (km)","Avg Pace (min/km)","Overall Elevation Change(m)")
        # Stats <- c("Run Duration (min)","Run Distance (km)")
        Results <- c(run_duration,run_dist,avg_pace,elev_change)
        tb <-data.frame(Stats,Results)
    })
    
    output$mymap <- renderLeaflet({
        current_run <- df%>% filter(date==input$run_date)%>% arrange(time_new) #pick a selected day
        leaflet()%>% 
            addProviderTiles("Esri.WorldImagery") %>%
            setView(lng = mean(current_run$leaf_lng), lat =mean(current_run$leaf_lat),zoom = 15)%>%
            fitBounds(min(current_run$leaf_lng), min(current_run$leaf_lat), max(current_run$leaf_lng), max(current_run$leaf_lat))%>%
            addCircleMarkers(lng = current_run$leaf_lng[1],lat = current_run$leaf_lat[1],color="Green")%>% #Start point
            addCircleMarkers(lng = tail(current_run$leaf_lng,1),lat = tail(current_run$leaf_lat,1),color = "Red")%>% #Start point
            addMarkers(lng = current_run$leaf_lng[1],lat = current_run$leaf_lat[1],popup = "Start")%>% #Start point
            addMarkers(lng = tail(current_run$leaf_lng,1),lat = tail(current_run$leaf_lat,1),popup = "Finish")%>% #Start point
            addPolylines(lng =current_run$leaf_lng,lat = current_run$leaf_lat)
    })
    output$mytable <- renderTable({
        stats()
    })
    output$split_data <-renderDataTable({
        current_run <- df%>% filter(date==input$run_date)%>% arrange(time_new) #pick a selected day
        if(input$split) {
            s<-current_run%>%select(floor_cumsum_sf_km,time_new)%>%
                st_set_geometry(NULL)%>%
                group_by(floor_cumsum_sf_km)%>%
                dplyr::mutate(split=tail(time_new,1)-time_new[1])%>%
                dplyr::distinct(floor_cumsum_sf_km,split)
            names(s) <- c("Km","Time")
            s$Time <- round(s$Time,2)
            s
        }else{
            NULL
        }
    })
    output$all_runs <- renderDataTable({
        single_run
    })
    
    
}

shinyApp(ui = ui, server = server)

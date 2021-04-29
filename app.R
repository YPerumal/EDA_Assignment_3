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
library(plotly)

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

load("df.Rdata")

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
names(single_run) <- c("Date","Distance(Km)","Duration (mins)","Pace per Km","Net Elevation Change(m)")

five_run <- tail(single_run)

five_run%>%ggplot(aes(x=Date))+
    geom_line(aes(y=`Distance(Km)`))+
    labs(title = "Distance Covered")+
    geom_hline(aes(yintercept = mean(single_run$`Distance(Km)`),color='Lifetime Average'))+
    scale_colour_manual(values = c("red"))+
    scale_color_discrete(name = NULL)+
    theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))
    





test <- df%>% filter(date=='2019-02-27')%>% #pick a selected day 
    arrange(time_new)

head(test)

t<-tail(test)

# Shiny Components
ui <- fluidPage(
    theme = bs_theme(version = 4, bootswatch = "solar"),
    titlePanel("Runtastic"),
    tabsetPanel(
        tabPanel("Latest Run Summary",icon=icon("running"),
            fluidRow(
                column(width = 3, selectInput("run_date",label= "Run Date", choices = unique(df$date), selected = max(df$date))),
                
            ),
            fluidRow(column(width = 3, checkboxInput("split","Show Splits Per Km"))),
            fluidRow(style = "padding-top:20px",
                     column(width = 6,leafletOutput("mymap")),
                     column(width = 6,tableOutput("mytable")))
            ),
            fluidRow(style = "padding-top:20px",
                     column(width = 6,dataTableOutput("split_data")
            )
        ),
        tabPanel("Your Last 10 Runs",icon=icon("chart-line"),
                 fluidRow(column(width = 6, plotOutput("five_g1")),
                          column(width = 6, plotOutput("five_g2"))
                          ),
                 fluidRow(column(offset=2,width = 7,tableOutput("five_table")))
                 ),
        tabPanel("All Your Runs",icon=icon("table"),
                 fluidRow(column(width = 10,offset=1,dataTableOutput("all_runs")))),    
        tabPanel(title = "Be Inspired to Run", icon = icon("youtube"),
                 tags$br(),
                 tags$iframe(
                     src = "https://www.youtube.com/embed/1q57u0Q9DN4" ,
                     width = 560*1.5,
                     height = 315*1.5,
                     allowfullscreen=T
                 )
                     )
    )
)

server <- function(input, output) {
    #Create a table to be used below in a reactive object
    stats <- reactive({
        current_run <- df%>% filter(date==input$run_date)%>% arrange(time_new) #pick a selected day
        run_duration <-as.numeric(tail(current_run$time_new,1)-current_run$time_new[1]) #length of run in minutes
        run_dist <- max(current_run$cum_sum_sf_km)
        avg_pace <- as.numeric(run_duration)/run_dist
        elev_change <- max(current_run$cum_elev_change)
        Stats <- c("Run Duration (min)","Run Distance (km)","Avg Pace (min/km)","Net Elevation Change(m)")
        # Stats <- c("Run Duration (min)","Run Distance (km)")
        Results <- c(run_duration,run_dist,avg_pace,elev_change)
        tb <-data.frame(Stats,Results)
    })
    # Create the Map with the route for the Run
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
    #Summary Stats for the 1 run page
    output$mytable <- renderTable({
        stats()
    })
    # Splits table
    output$split_data <-renderDataTable(options = list(pageLength = 5,dom='tp',caption = 'Table 1: This is a simple caption for the table.'),
                                        {
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
    # DataTable of all 1 run summary
    output$all_runs <- renderDataTable({
        single_run
    })
    #Latest 5 Run Stats
    output$five_g1 <- renderPlot({
        five_run <- tail(single_run,10)
        five_run%>%ggplot(aes(x=Date))+
            geom_line(aes(y=`Distance(Km)`))+
            labs(title = "Distance Covered")+
            geom_hline(aes(yintercept = mean(single_run$`Distance(Km)`),color='Lifetime Average'))+
            scale_colour_manual(values = c("red"))+
            scale_color_discrete(name = NULL)+
            theme(legend.position = "bottom",
                  plot.title = element_text(hjust = 0.5),
                  plot.background = element_rect(fill = "#C8CBCE", color = "pink"))
    })
    output$five_g2 <- renderPlot({
        five_run <- tail(single_run,10)
        five_run%>%ggplot(aes(x=Date))+
            geom_line(aes(y=`Duration (mins)`))+
            labs(title = "Run Duration")+
            geom_hline(aes(yintercept = mean(single_run$`Duration (mins)`),color='Lifetime Average'))+
            scale_colour_manual(values = c("blue"))+
            scale_color_discrete(name = NULL)+
            theme(legend.position = "bottom",
                  plot.title = element_text(hjust = 0.5),
                  plot.background = element_rect(fill = "#C8CBCE", color = "pink"))
    })
    output$five_table<- renderTable({
        five_run <- tail(single_run,10)
        five_run$Date <- as.character(five_run$Date)
        five_run
    })
}
shinyApp(ui = ui, server = server)

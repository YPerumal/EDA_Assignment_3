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


test <- df%>% filter(date=='2019-02-27')%>% #pick a selected day 
    arrange(time_new)%>%
    mutate(cumsum = cumsum(sf_distance))#Make sure sorted by timestamp
t<-tail(test)

head(test)
z<-df[365:375,]
sum(test$sf_distance)
test%>% select(sf_distance,cumsum)

test$geometry
#add start and end markers


# ui <- fluidPage(
#     theme = bs_theme(version = 4, bootswatch = "solar"),
#     titlePanel("Runatastic"),
#     sidebarLayout(
#         sidebarPanel(
#             selectInput("run_date",label= "Run Date", choices = unique(df$date), selected = "2019-02-27")
#         ),
#         mainPanel(
#             leafletOutput("mymap"),
#             dataTableOutput("mytable")
#         )
#     )
# )


ui <- fluidPage(
    theme = bs_theme(version = 4, bootswatch = "solar"),
    titlePanel("Runtastic"),
    tabsetPanel(
        tabPanel("1 Run Summary",
            fluidRow(
                column(6,selectInput("run_date",label= "Run Date", choices = unique(df$date), selected = "2019-02-27"))
                ),
            fluidRow(
                column(6,leafletOutput("mymap")),
                column(6,tableOutput("mytable"))
                )
        ),
        tabPanel("Your Last 5 Runs"),
        tabPanel("Some Other Stuff"),    
        tabPanel(title = "Video", icon = icon("youtube"),
                 tags$br(),
                 tags$iframe(
                     src = "https://www.youtube.com/embed/QsBT5EQt348",
                     width = 560,
                     height = 315
                 ),
                 tags$p(
                     id = "video-attribution",
                     tags$a(
                         "Video", 
                         href = "https://www.youtube.com/embed/QsBT5EQt348"
                     ),
                     "by",
                     tags$a(
                         "Kurzgesagt",
                         href = "https://kurzgesagt.org/",
                         class = "site"
                     )
                 )
        )
        )
)

server <- function(input, output) {
    some_data <- reactive({
        current_run <- df%>% filter(date==input$run_date)%>% arrange(time_new) #pick a selected day
        run_duration <-as.numeric(tail(test$time_new,1)-test$time_new[1]) #length of run in minutes
        run_dist <- max(current_run$cum_sum_sf_km)
        avg_pace <- as.numeric(run_duration)/run_dist
        Stats <- c("Run Duration (min)","Run Distance (km)","Avg Pace (min/km)")
        # Stats <- c("Run Duration (min)","Run Distance (km)")
        Results <- c(run_duration,run_dist,avg_pace)
        tb <-data.frame(Stats,Results)
    })
    
    output$mymap <- renderLeaflet({
        current_run <- df%>% filter(date==input$run_date)%>% arrange(time_new) #pick a selected day
        leaflet()%>% 
            addProviderTiles("Esri.WorldImagery") %>%
            setView(lng = mean(current_run$leaf_lng), lat =mean(current_run$leaf_lat),zoom = 15)%>%
            fitBounds(min(current_run$leaf_lng), min(current_run$leaf_lat), max(current_run$leaf_lng), max(current_run$leaf_lat))%>%
            addMarkers(lng = current_run$leaf_lng[1],lat = current_run$leaf_lat[1])%>% #Start point
            addMarkers(lng = tail(current_run$leaf_lng,1),lat = tail(current_run$leaf_lat,1))%>% #Start point
            addPolylines(lng =current_run$leaf_lng,lat = current_run$leaf_lat)
    })
    output$mytable <- renderTable({
        some_data()
    })
}

shinyApp(ui = ui, server = server)

# import libaries
library(tidyverse)
library(shiny)
library( plyr )
library(lubridate)
library(ggplot2)
library(sf)
library(leaflet)
library(dplyr)

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

test <- df%>% filter(date=='2019-02-27')%>% #pick a selected day 
                arrange(time_new) #Make sure sorted by timestamp

head(test)
m <- leaflet()%>% 
    addProviderTiles("Esri.WorldImagery") %>%
    setView(lng = test$lng[1], lat =test$lat[1],zoom = 12.3)%>%
    addMarkers(lng = test$lng[1],lat = test$lat[1])%>% #Start point
    addMarkers(lng = tail(test$lng,1),lat = tail(test$lat,1))%>% #Start point
    addPolylines(lng =test$lng,lat = test$lat)

#add start and end markers

ui <- fluidPage(
    titlePanel("Population Explosion"),
    sidebarLayout(
        sidebarPanel(
            selectInput("run_date",label= "Run Date", choices = unique(df$date), selected = "2019-02-27")
        ),
        mainPanel(
            leafletOutput("mymap")
        )
    )
)

server <- function(input, output) {
    output$mymap <- renderLeaflet({
        current_run <- df%>% filter(date==input$run_date)%>% arrange(time_new) #pick a selected day
        leaflet()%>% 
            addProviderTiles("Esri.WorldImagery") %>%
            setView(lng = current_run$leaf_lng[1], lat =current_run$leaf_lat[1],zoom = 12.3)%>%
            addMarkers(lng = current_run$leaf_lng[1],lat = current_run$leaf_lat[1])%>% #Start point
            addMarkers(lng = tail(current_run$leaf_lng,1),lat = tail(current_run$leaf_lat,1))%>% #Start point
            addPolylines(lng =current_run$leaf_lng,lat = current_run$leaf_lat)
    })
}

shinyApp(ui = ui, server = server)

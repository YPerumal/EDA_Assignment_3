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
head(df)
tail(df)
dim(df)
# reset wd 
setwd('/Users/yevashanperumal/Desktop/Data Science Masters/2021/EDA - STA5092Z/Assignments/EDA_Assignment_3')

#No nulls?
sum(is.na(df))

# convert date and time variables from characters
str(df)
df["date"] <- as_date(df$date)
df["time_new"] <- parse_date_time(df$time,"HMS")

head(df)
# Converting to sfc object
# df <- st_as_sf(df,coords = c("lng","lat"),crs=4326)

test <- df%>% filter(date=='2019-02-27') #pick a selected day
test <- test%>% arrange(time_new) #Make sure sorted by timestamp
dim(test)

head(test)
# lng = 18.4241, lat = -33.9249, zoom = 10
m <- leaflet()%>% 
    addProviderTiles("Esri.WorldImagery") %>%
    setView(lng = test$lng[1], lat =test$lat[1],zoom = 12.3)%>%
    addMarkers(lng = test$lng[1],lat = test$lat[1])%>% #Start point
    addMarkers(lng = tail(test$lng,1),lat = tail(test$lat,1))%>% #Start point
    addPolylines(lng =test$lng,lat = test$lat)

#add start and end markers

test
 
ui <- fluidPage(
    titlePanel("Population Explosion"),
    sidebarLayout(
        sidebarPanel(
            selectInput("run_date",label= "Run_date", choices = unique(df$date), selected = "2019-02-27")
            # sliderInput("size", "Point size", min = 0, max = 5, value = 1),
            # checkboxInput("line", "Plot line"),
            # checkboxInput("show_table", "Show table", value = TRUE)
        ),
        mainPanel(
            leafletOutput("mymap")
        )
    )
)

server <- function(input, output) {
    # output$plot <- renderPlotly({
    #     data <- populations %>% filter(code %in% input$country)
    #     p <- ggplot(data, aes(x = year,
    #                           y = population / 1000000)) +
    #         scale_y_log10("Population (million)") +
    #         theme(legend.title = element_blank())
    #     
    #     if (input$line) p <- p + geom_line(aes(group = code))
    #     
    #     p + geom_point(aes(color = code), size = input$size)
    # })
    # output$table <- renderDataTable({
    #     if (input$show_table) {
    #         populations %>%
    #             filter(code %in% input$country) %>%
    #             group_by(code) %>%
    #             arrange(code, desc(year)) %>%
    #             slice(1) %>%
    #             ungroup() %>%
    #             arrange(desc(population))
    #     } else {
    #         NULL
    #     }
    # })
    output$mymap <- renderLeaflet({
        test <- df%>% filter(date==input$run_date)%>% arrange(time_new) #pick a selected day
        leaflet()%>% 
            addProviderTiles("Esri.WorldImagery") %>%
            setView(lng = test$lng[1], lat =test$lat[1],zoom = 12.3)%>%
            addMarkers(lng = test$lng[1],lat = test$lat[1])%>% #Start point
            addMarkers(lng = tail(test$lng,1),lat = tail(test$lat,1))%>% #Start point
            addPolylines(lng =test$lng,lat = test$lat)
    })
}

shinyApp(ui = ui, server = server)

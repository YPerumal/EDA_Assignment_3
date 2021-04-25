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

test <- df%>% filter(date=='2019-02-27')
dim(test)

head(test)
# lng = 18.4241, lat = -33.9249, zoom = 10
leaflet()%>% 
    addProviderTiles("Esri.WorldStreetMap") %>%
    setView(lng = test$lng[1], lat =test$lat[1],zoom = 12.3)%>%
    addPolylines(lng =test$lng,lat = test$lat)

#add start and end markers

test
 
ui <- fluidPage(
    titlePanel("Population Explosion"),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "country",
                label = "Country",
                choices = c(
                    "Lesotho",
                    "South Africa",
                    "Swaziland"
                )
            )
        ),
        mainPanel(
            plotOutput("plot")
        )
    )
)

server <- function(input, output) {
    output$plot <- renderPlot({
        data <- populations %>% filter(name == "South Africa")
        ggplot(data, aes(x = year, y = population)) + geom_point()
    })
}

shinyApp(ui = ui, server = server)
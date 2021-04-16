######################################
# BLG610 - Data Science for Biology
# Data VisualizationFinal Project
# Author: Junaira Serrame 500711485
# Published: April 12, 2021
######################################

#install.packages("leaflet")
#install.packages("ggmap")
#install.packages("leafpop")
#install.packages("RColorBrewer")
#install.packages("shinydashboard")
library(tidyverse)
library(readr)
library(leaflet)
library(raster)
library(htmltools)
library(shiny)
library(leafpop)
library(RColorBrewer)
library(sf)
library(dplyr)

############## PREPARING THE DATA #####################

#load water quality and coordinates data  
#data source: https://www.donneesquebec.ca/recherche/dataset/vmtl-rsma-qualite-
#de-l-eau-en-rive-qualo/resource/f0bed0d5-ccaa-494e-9ab7-1b59358e98be
#I'm using read.csv2 instead of read_csv because the data set is in French and 
#the commas are not a delimiter but rather a decimal character
data_waterqual <- read.csv2("../data/donnees_qualo_2019.csv", sep = ",", dec = ",")
station <- read.csv2("../data/stations_qualo.csv", sep = ",", dec = ",")

view(data_waterqual)

##tidying the data
###removing rows with empty cells
summary(data_waterqual)
sum(is.na(data_waterqual$fecal_coliform))
data_waterqual <- data_waterqual %>%
    na_if("") %>%
    na.omit

###separating the date and time                    
data_waterqual <- data_waterqual %>%    #separate the date and time
    separate(Date, into = c("Date", "Time"), sep = " ", remove = FALSE) %>%
    mutate(Date = lubridate::as_date(Date, format = "%Y-%m-%d"),
           Time = hms::as_hms(str_c(Time, ":00")))

###removing irrelevant variables
data_waterqual <- dplyr::select(data_waterqual, -c(Sign, Time, Temperature)) #certain functions are masked by other packages so we need to use package::function
general_waterqual <- dplyr::select(data_waterqual, -c(Date))

##checking for the class of each column
sapply(data_waterqual, class) 

summary(station)
sum(is.na(station))

###selecting relevant variables
station <- dplyr::select(station, Site, Name, Longitude, Latitude) 
sapply(station, class)

#Finding the average conductivity, pH and coliform levels 
site_group <- group_by(general_waterqual, Site)
average_waterqual <- summarise(site_group,
                               count = n(),
                               avg_conductivity = mean(Conductivity, na.rm = FALSE),
                               avg_pH = mean(pH, na.rm = FALSE),
                               avg_coliform = mean(fecal_coliform, na.rm = FALSE))

join_site_group <- average_waterqual %>% 
    right_join(station, by = "Site")  #using right join because we'd like to see river banks that were not surveyed

join_site_group$avg_coliform[is.na(join_site_group$avg_coliform)] <- 0

#Setting boundaries for coliform levels according to COURDO Parameters
#http://ville.montreal.qc.ca/portal/page?_pageid=7237,75321646&_dad=portal&_schema=PORTAL
##Excellent (1-100), Good (101-200), Satisfactory (201-1000),
##Polluted (1001-2000), Extremely Polluted (>2000)

max(join_site_group$avg_coliform)   #max = 12615.35
join_site_group$parameters <- cut(join_site_group$avg_coliform,
                                  breaks = c(0, 1, 100, 200, 1000, 2000, Inf), 
                                  right = FALSE, labels = c("No Data", "Excellent", "Good", 
                                                            "Satisfactory", "Polluted", "Extremely Polluted"))


#Define colour palette according to coliform parameters
pal = colorFactor(palette = c("ivory4", "darkgreen", "green1", "yellow1", "darkorange1", "red2"),
                  domain = join_site_group$parameters)

#Map labels
join_site_group$label <- paste("<p>", "River Bank:", join_site_group$Name, "<p>",
                               "<p>", "Average Conductivity = ", join_site_group$avg_conductivity, "<p>",
                               "<p>", "Average pH = ", join_site_group$avg_pH, "<p>",
                               "<p>", "Average Coliform Level (colonies/100 mL) = ", 
                               join_site_group$avg_coliform, "<p>",
                               "<p>", "Water Quality = ", join_site_group$parameters, "<p>")

###################### SHINY ###############################

ui <- bootstrapPage(
    titlePanel("Water Quality of Montreal River Banks"),
    h1 = "Caution: Do NOT drink water specially from yellow, orange or red zones 
        where coliform levels are above 200 colonies/100mL. Water in these areas 
        are contaminated and your risk of contracting water-born illness is very 
        high.",
    tags$style(type = "text/css", "html, body {width:100%;height:95%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 100, right = 50,
                  selectInput("water", "Water Quality 2019",
                              choices = c("All", "Excellent", "Good", 
                                          "Satisfactory", "Polluted",
                                          "Extremely Polluted", "No Data"),
                              selected = 1), 
                  hr(),
                  fluidRow(column(7, verbatimTextOutput("montreal_map")))
    )
)
server <- function(input, output, session) {
    
    #filter the coliform data based on the water quality selected by user
    filteredMaps <- reactive({
        if(input$water == "All"){
            join_site_group
        } else filter(join_site_group, parameters == input$water)
    }) 
    
    
    output$map <- renderLeaflet({
        leaflet(filteredMaps()) %>%
            addProviderTiles(providers$OpenStreetMap.HOT) %>%
            setView(lng = -73.5673, lat = 45.5017, zoom = 10) %>% #Montréal coordinates
            addCircleMarkers(lng = join_site_group$Longitude,
                             lat = join_site_group$Latitude,
                             color = pal(join_site_group$parameters),
                             radius = 5, weight = 5, opacity = 25,
                             label = (lapply(join_site_group$label, HTML))) %>% 
            addLegend(position =c("bottomright"), pal = pal, values = ~join_site_group$parameters, 
                      title = "Water Quality of Montreal Riverbanks 2019", 
                      opacity = 25)
        })
    
    
    observe(leafletProxy("map", data = filteredMaps ()) %>%
                clearShapes() %>% 
                addMarkers()
            )
    
}  

shinyApp(ui, server)


#Leaflet manual: http://rstudio.github.io/leaflet/shiny.html
#Project inspiration: http://ville.montreal.qc.ca/portal/page?_pageid=7237,75397570&_dad=portal&_schema=PORTAL


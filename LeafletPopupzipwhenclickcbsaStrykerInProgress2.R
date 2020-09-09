rm(list=ls())
setwd("C:/Users/blake/OneDrive/Stryker Project/Country Leaflet Information Dashboard")
#load("StrykerIntelligenceWaltkthroughWorkspace.rdata")
#load("CensusZIPbigdataworkspace2.rdata") # less intensive workspace
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
# Load variables
x <- c("lubridate","data.table","zoo","riem","dplyr","tidyr","countrycode","RCurl","stringr","tidyverse","shiny","miniUI","taskscheduleR","gtrendsR","tidycensus","sf","leaflet","mapview","viridis","tidyquant","tigris","tmap","sf","maps","tidycensus","scales","tmaptools","purrr","plotly","hrbrthemes","DT")
lapply(x, require, character.only = TRUE)
library(rmapshaper)
library(rgdal)
library(sp)
library(RColorBrewer)
library(htmltools)
library(leaflet.extras)

display.brewer.all()
# MUST RUN
options(tigris_use_cache = TRUE)
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
# CBSA SHape file
cbsa <- readOGR(dsn=".", layer = "cb_2018_us_cbsa_500k")
zcta <- readOGR(dsn = ".",layer = "cb_2018_us_zcta510_500k")
zip2cbsa <- read.csv("zip2cbsa.csv");colnames(zip2cbsa)[1] <- "ZCTA5CE10";zip2cbsa <- zip2cbsa[c(1,2)]

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
# Zip code level information
totalpop2011 <-  "B01003_001" # population variable

# grabbing census information
ZIPpopulation <- get_acs(geography = "zcta", variables = totalpop2011, year = 2011, survey = "acs5", output = "wide", geometry = TRUE, keep_geo_vars=TRUE)
ZIPpopulation <- ZIPpopulation[c(1,2,11,12,13)]
ZIPpopulation2 <- merge(ZIPpopulation,zip2cbsa,by = c("ZCTA5CE10"),all.x= TRUE)
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################



ui <- fluidPage(
  leafletOutput("map"),
  # p(),
  # leafletOutput("map2")
)

server <- function(input, output, session) {
  
  
  # This allows you to grab id or GEOID FROM MAP FOR FILTERING AND PLOTTING
  observeEvent(input$map_shape_click, { # update the location selectInput on map clicks
    p <- input$map_shape_click
    z <- input$map_shape_click$id
    print(p)
  })
  # observeEvent(input$map2_shape_click, { # update the location selectInput on map clicks
  #   p <- input$map2_shape_click
  #   z <- input$map2_shape_click$id
  #   print(p)
  # })
  
  output$mymap <- renderLeaflet({
    # Zoom in further
    leaflet() %>%
      addProviderTiles('CartoDB')%>%   
      addPolygons(data = cbsa,layerId = ~GEOID, fillColor = 'transparent',color = "transparent", group ="revenue",fillOpacity = 0.6,weight = 1, smoothFactor = 0.2,highlightOptions = highlightOptions(stroke = 4, weight = 2)) %>%
      addPolylines(data = cbsa, color = "black", opacity = 1, weight = .4,group = "lines")%>%
      setView(lng = -98.583, lat = 39.833, zoom = 4)
  })
  
  # output$map2 <- renderLeaflet({
  #   # Zoom in further
  #   leaflet() %>%
  #     addProviderTiles('CartoDB')%>%   
  #     addPolygons(data = cbsa,layerId = ~GEOID, fillColor = 'transparent',color = "transparent", group ="revenue",fillOpacity = 0.6,weight = 1, smoothFactor = 0.2,highlightOptions = highlightOptions(stroke = 4, weight = 2)) %>%
  #     addPolylines(data = cbsa, color = "black", opacity = 1, weight = .4,group = "lines")%>%
  #     setView(lng = -98.583, lat = 39.833, zoom = 4)
  # })
}

shinyApp(ui, server)
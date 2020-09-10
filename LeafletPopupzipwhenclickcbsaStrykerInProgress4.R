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

# merge zip population and zcta
zcta@data <- left_join(zcta@data,ZIPpopulation2,by =c("ZCTA5CE10"),all.x=TRUE)
zcta@data <- na.omit(zcta@data)


#######################################################################################################################
#######################################################################################################################
#######################################################################################################################



ui <- fluidPage(
  leafletOutput("map"),
  p(),
  leafletOutput("map2")
)

server <- function(input, output, session) {

  output$map <- renderLeaflet({
    # Zoom in further
    leaflet() %>%
      addProviderTiles('CartoDB')%>%   
      addPolygons(data = cbsa,layerId = ~GEOID, fillColor = 'transparent',color = "transparent", group ="revenue",fillOpacity = 0.6,weight = 1, smoothFactor = 0.2,highlightOptions = highlightOptions(stroke = 4, weight = 2)) %>%
      addPolylines(data = cbsa, color = "black", opacity = 1, weight = .4,group = "lines")%>%
      setView(lng = -98.583, lat = 39.833, zoom = 4)
  })
  
  
  # This allows you to grab id or GEOID FROM MAP FOR FILTERING AND PLOTTING
  observeEvent(input$map_shape_click, { # update the location selectInput on map clicks
    p <- input$map_shape_click
    z <- input$map_shape_click$id
    print(p)
  })
  
  cbsa_click <- eventReactive(input$map_shape_click, {
    x <- input$map_shape_click
    y <- x$id
    lat <- x$lat
    lng <- x$lng
    return(y)
    print(y)
  })

#pal <- colorNumeric(palette = "Blues",domain = zcta@data$CBSA)
  
  observeEvent(input$map_shape_click, {
    x <- input$map_shape_click
    y <- x$id
    lat <- x$lat
    lng <- x$lng
    
    #define click object
    click <- input$map_shape_click
    
    #subset counties shapefile so that only counties from the clicked state are mapped
    selected <- zcta[zcta$data$CBSA == click$id,]
    #selected <- subset(zcta, CBSA == click$id)
    
   # zcta <- zcta[zcta$CBSA > 30000,]
    
        #define color palette for counties 
  #  pal <- colorNumeric(palette = "Reds", domain=selected@data$B01003_001E)
    
    #if click id isn't null (i.e. if ANY polygon is clicked on), draw map of counties
    if(!is.null(click$id)){
      output$map2 <- renderLeaflet({
        pal <- colorNumeric(palette = "Reds", domain=selected@data$B01003_001E)
        
        leaflet() %>% 
          addTiles() %>% 
          addPolygons(data = selected,fillColor = "Reds",fillOpacity = 1, color = ~ pal(selected$B01003_001E), weight = 1)%>%
          setView(lng = lng, lat = lat, zoom = 7)
        
      }) #END RENDERLEAFLET
    } #END CONDITIONAL
  })
}



shinyApp(ui, server)
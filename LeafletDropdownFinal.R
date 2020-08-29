# Upload packages
library(rgdal)
library(sp)
library(leaflet)
#library(geojsonio)
library(shinythemes)
library(shiny)
#install.packages("rmapshaper", lib = "C:/R/R-4.0.2/library")
library(rmapshaper)

#setwd("C:/Users/blake/OneDrive/Stryker Project/Country Leaflet Information Dashboard/Building Leaflet Final Interactive Map/Leaflet Map App")
#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################
# Load workspace
#load("C:/Users/blake/OneDrive/Stryker Project/Country Leaflet Information Dashboard/Building Leaflet Final Interactive Map/Leaflet Map App/FinalCombinationWorkspace.R")

#Load the dataframe from the folder
#county <- readOGR(dsn=".", layer = "cb_2018_us_county_500k")
# convert the GEOID to a character (BECAUSE TIDY CENSUS IS CHARACTER FOR SF FILE)
#county@data$GEOID <-as.character(county@data$GEOID)

# final shape file 
# df.polygon <- county
# 
# # County shape dataframe subset this!!!
#df.polygon2  <- df.polygon# moving variable

# Load dataframe
#census <- read.csv("census.csv");census <- census[-c(1)];census <- census[c(1,2,3,7,10,14)]
#
#df.polygon2@data <- census
# censuscovid$data <- as.Date(censuscovid$date);censuscovid$GEOID <- as.character(censuscovid$GEOID)

#df.polygon5 <- rmapshaper::ms_simplify(df.polygon2, keep = 0.05, keep_shapes = TRUE)
#######################################################################################################################

# UI
ui <- shinyUI(fluidPage(theme = shinytheme("united"),
                        titlePanel(HTML("<h1><center><font size=14> County Level Demographic Variables </font>
</center></h1>")), 
                        sidebarLayout(
                          sidebarPanel(
                            
                            selectInput("VariableInput", label = h3("Variable"),
                                        choices = c("VulIndex","z_Over64","PopulationToGrowthRateRatio"))),
                          mainPanel(leafletOutput(outputId = 'map', height = 
                                                    800) 
                          ))
))


# SERVER
server <- shinyServer(function(input, output, session) {
  

  # selected Var
  selectedVar <- reactive({switch(input$VariableInput, 
                                  "VulIndex"=df.polygon2$VulIndex, 
                                  "z_Over64"=df.polygon2$z_Over64, 
                                  "PopulationToGrowthRateRatio"=df.polygon2$PopulationToGrowthRateRatio)
  })
  pal2 <- colorNumeric(palette = "Reds", domain=NULL)
  
  output$map <- renderLeaflet({
    
    
    leaflet(df.polygon2) %>% 
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      setView(lng = -98.583, lat = 39.833, zoom = 4) %>%
      addPolygons(data = df.polygon2 ,fillColor = ~pal2(selectedVar()),
                  popup = paste0("<strong>County: </strong>",df.polygon2$county,"<br>","<strong>Vulindex: </strong>",round(df.polygon2$z_Over64,digits=2),"<br>","<strong>Vulindex: </strong>",round(df.polygon2$PopulationToGrowthRateRatio,digits = 2)),
                  color = "#BDBDC3",
                  fillOpacity = 0.8,
                  weight = 1)%>%
      addLegend(position = "topright", pal = pal2, values = df.polygon2[[input$VariableInput]] ,
                title =  ~paste(input$VariableInput))
    
  })
  
  
})

# Run app! 
shinyApp(ui = ui, server = server)
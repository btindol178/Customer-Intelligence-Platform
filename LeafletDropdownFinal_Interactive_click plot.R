# Upload packages
library(rgdal)
library(sp)
library(leaflet)
#library(geojsonio)
library(shinythemes)
library(dplyr)
library(shiny)
#install.packages("rmapshaper", lib = "C:/R/R-4.0.2/library")
library(rmapshaper)

#  setwd("C:/Users/blake/OneDrive/Stryker Project/Country Leaflet Information Dashboard/Building Leaflet Final Interactive Map/Leaflet Map App")
# # #################################################################################################################################################
# # #################################################################################################################################################
# # #################################################################################################################################################
# # # Load workspace
# # load("C:/Users/blake/OneDrive/Stryker Project/Country Leaflet Information Dashboard/Building Leaflet Final Interactive Map/Leaflet Map App/FinalCombinationWorkspace.R.RData")
# 
# #Load the dataframe from the folder
# county <- readOGR(dsn=".", layer = "cb_2018_us_county_500k")
# # convert the GEOID to a character (BECAUSE TIDY CENSUS IS CHARACTER FOR SF FILE)
# county@data$GEOID <-as.character(county@data$GEOID)
# # 
# # # # final shape file 
#    df.polygon <- county
# # # # 
# # # # # County shape dataframe subset this!!!
#   df.polygon2  <- df.polygon# moving variable
# # # # 
# # # # # Load dataframe
#    census <- read.csv("census.csv");census <- census[-c(1)];census <- census[c(1,2,3,7,10,14)];census$GEOID <- as.character(census$GEOID)
# # # 
#        census$GEOID <-ifelse(census$state == " Alabama",paste("0",census$GEOID,sep =""),census$GEOID)
#        census$GEOID <-ifelse(census$state == " Arkansas",paste("0",census$GEOID,sep =""),census$GEOID)
#        census$GEOID <-ifelse(census$state == " California",paste("0",census$GEOID,sep =""),census$GEOID)
#        census$GEOID <-ifelse(census$state == " Arizona",paste("0",census$GEOID,sep =""),census$GEOID)
#        census$GEOID <-ifelse(census$state == " Colorado",paste("0",census$GEOID,sep =""),census$GEOID)
#        census$GEOID <-ifelse(census$state == " Connecticut",paste("0",census$GEOID,sep =""),census$GEOID)
#        census$GEOID <-ifelse(census$state == " Alaska",paste("0",census$GEOID,sep =""),census$GEOID)
# # #  # # # 
# # # # 
# # # 
#   df.polygon5 <- rmapshaper::ms_simplify(df.polygon2, keep = 0.05, keep_shapes = TRUE)
# # # 
#   df.polygon5@data <- left_join(df.polygon5@data, census,by = c("GEOID"),all.x = TRUE)

#######################################################################################################################

# UI
ui <- shinyUI(fluidPage(theme = shinytheme("united"),
                        titlePanel(HTML("<h1><center><font size=14> County Level Demographic Variables </font>
</center></h1>")), 
                        sidebarLayout(
                          sidebarPanel(
                            
                            selectInput("VariableInput", label = h3("Variable"),
                                        choices = c("VulIndex","z_Over64","PopulationToGrowthRateRatio"))),
                          mainPanel(leafletOutput(outputId = 'map', height = 800), 
                                    plotOutput('Hist')
                          

                          ))
))


# SERVER
server <- shinyServer(function(input, output, session) {
  

  # This allows you to grab id or GEOID FROM MAP FOR FILTERING AND PLOTTING
   observeEvent(input$map_shape_click, { # update the location selectInput on map clicks
     p <- input$map_shape_click
     z <- input$map_shape_click$id
     print(p)
   })
  
  county_click <- eventReactive(input$map_shape_click, {
    x <- input$map_shape_click
    y <- x$id
    return(y)
    print(y)
  })
  
  output$Hist <- renderPlot({
    z <- filter(census,GEOID == county_click())
    hist(as.numeric(z))
  })
  
  # selected Var
  selectedVar <- reactive({switch(input$VariableInput, 
                                  "VulIndex"=df.polygon5$VulIndex, 
                                  "z_Over64"=df.polygon5$z_Over64, 
                                  "PopulationToGrowthRateRatio"=df.polygon5$PopulationToGrowthRateRatio)
  })
  pal2 <- colorNumeric(palette = "Reds", domain=NULL)
  
  output$map <- renderLeaflet({
    
    
    leaflet(df.polygon5) %>% 
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      setView(lng = -98.583, lat = 39.833, zoom = 4) %>%
      addPolygons(data = df.polygon5 ,
                  layerId = ~GEOID, # NEED GEOID TO BE IN THE LAYER IF YOU WANT TO CLICK AND HAVE REACTIVE PLOT
                  fillColor = ~pal2(selectedVar()),
                  popup = paste0("<strong>County: </strong>",df.polygon5$county,"<br>","<strong>Vulindex: </strong>",round(df.polygon5$z_Over64,digits=2),"<br>","<strong>Vulindex: </strong>",round(df.polygon5$PopulationToGrowthRateRatio,digits = 2)),
                  color = "#BDBDC3",
                  fillOpacity = 0.8,
                  weight = 1)%>%
      addLegend(position = "topright", pal = pal2, values = df.polygon5[[input$VariableInput]] ,
                title =  ~paste(input$VariableInput))
    
  })
  
  
})

# Run app! 
shinyApp(ui = ui, server = server)
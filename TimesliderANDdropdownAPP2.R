# Upload packages
library(rgdal)
library(sp)
library(leaflet)
library(dplyr)
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
# #county@data$GEOID <-as.character(county@data$GEOID)
# # 
# # # # final shape file 
#   df.polygon <- county
# # # # # # 
# # # #  County shape dataframe subset this!!!
#   df.polygon2  <- df.polygon# moving variable
# # # # # 
# # # # # # Make map for slider
#   df.polygon4 <- rmapshaper::ms_simplify(df.polygon2, keep = 0.05, keep_shapes = TRUE)
# # # # # 
# # # # for dropdown rendering
#   df.polygon5 <- rmapshaper::ms_simplify(df.polygon2, keep = 0.05, keep_shapes = TRUE)
# # # 
# # # # # # census covid for time slider
#   censuscovid <- read.csv("censuscovid.csv");censuscovid <- censuscovid[-c(1)]; censuscovid$GEOID <- as.character(censuscovid$GEOID)
# # # 
# # # # # still need
#    censuscovid$GEOID <-ifelse(censuscovid$state == " Alabama",paste("0",censuscovid$GEOID,sep =""),censuscovid$GEOID)
#    censuscovid$GEOID <-ifelse(censuscovid$state == " Arkansas",paste("0",censuscovid$GEOID,sep =""),censuscovid$GEOID)
#    censuscovid$GEOID <-ifelse(censuscovid$state == " California",paste("0",censuscovid$GEOID,sep =""),censuscovid$GEOID)
#    censuscovid$GEOID <-ifelse(censuscovid$state == " Arizona",paste("0",censuscovid$GEOID,sep =""),censuscovid$GEOID)
#    censuscovid$GEOID <-ifelse(censuscovid$state == " Colorado",paste("0",censuscovid$GEOID,sep =""),censuscovid$GEOID)
#    censuscovid$GEOID <-ifelse(censuscovid$state == " Connecticut",paste("0",censuscovid$GEOID,sep =""),censuscovid$GEOID)
#    censuscovid$GEOID <-ifelse(censuscovid$state == " Alaska",paste("0",censuscovid$GEOID,sep =""),censuscovid$GEOID)
# # # 
# # 
# # 
# # # # # # give timeslider data
#    #df.polygon4@data <- censuscovid
# # #  
#   df.polygon4@data <- left_join(df.polygon4@data, censuscovid,by = c("GEOID"),all.x = TRUE)
# # 
# # # # # # Load dataframe
#  census <- read.csv("census.csv");census <- census[-c(1)];census <- census[c(1,2,3,7,10,14)]; census$GEOID <- as.character(census$GEOID)
# # #
# # # # # still need
#    census$GEOID <-ifelse(census$state == " Alabama",paste("0",census$GEOID,sep =""),census$GEOID)
#    census$GEOID <-ifelse(census$state == " Arkansas",paste("0",census$GEOID,sep =""),census$GEOID)
#    census$GEOID <-ifelse(census$state == " California",paste("0",census$GEOID,sep =""),census$GEOID)
#    census$GEOID <-ifelse(census$state == " Arizona",paste("0",census$GEOID,sep =""),census$GEOID)
#    census$GEOID <-ifelse(census$state == " Colorado",paste("0",census$GEOID,sep =""),census$GEOID)
#    census$GEOID <-ifelse(census$state == " Connecticut",paste("0",census$GEOID,sep =""),census$GEOID)
#    census$GEOID <-ifelse(census$state == " Alaska",paste("0",census$GEOID,sep =""),census$GEOID)
# # # 
# # # # # #
# # #  df.polygon5@data <- census
# # # # 
#    df.polygon5@data <- left_join(df.polygon5@data, census,by = c("GEOID"),all.x = TRUE)
# #   

#######################################################################################################################

# UI
ui <- fluidPage(navbarPage("Blake's Covid App",
                           tabPanel("Census Information",
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("VariableInput", label = h4("Variable"),
                                                    choices = c("VulIndex","z_Over64","PopulationToGrowthRateRatio"))),
                                      mainPanel(leafletOutput(outputId = 'map', height = 800),
                                                DT::dataTableOutput("table")
                                                
                                      ))
                           ),
                           tabPanel("Covid Time Slider",
                                    sidebarLayout(
                                      sidebarPanel(sliderInput("Date", "Date",
                                                               min =as.Date("2020-01-22","%Y-%m-%d"), max = as.Date("2020-08-24","%Y-%m-%d"),
                                                               value =as.Date("2020-01-22","%Y-%m-%d"),
                                                               timeFormat="%Y-%m-%d",),
                                                    #sliderInput("DateRange", "Date",
                                                     #           min =as.Date("2020-01-22","%Y-%m-%d"), max = as.Date("2020-08-24","%Y-%m-%d"),
                                                       #         value =c(as.Date("2020-01-22","%Y-%m-%d"),as.Date("2020-08-24","%Y-%m-%d")),
                                                        #        timeFormat="%Y-%m-%d")
                                                   # animate = animationOptions(interval = 500, loop = TRUE), 
                                                    #sep = '')
                                      ),
                                      mainPanel(
                                        
                                        leafletOutput("map2"),
                                       # DT::dataTableOutput("table2")
                                        
                                      )
                                    ) 
                           )
)
)

# SERVER
server <- shinyServer(function(input, output, session) {
  # 
   #   output$table2 <- renderTable({
   #     data2 <- censuscovid
   #     data2 <- subset(data2,date >= input$DateRange[1] & date <= input$DateRange[2])
   #     data2
   # })
  
  # Replace the renderTable() with DT's version
  output$table <- DT::renderDataTable({
    data <- census
    data
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
      addPolygons(data = df.polygon5 ,fillColor = ~pal2(selectedVar()),
                  popup = paste0("<strong>County: </strong>",df.polygon5$county,"<br>","<strong>Vulindex: </strong>",round(df.polygon5$z_Over64,digits=2),"<br>","<strong>Vulindex: </strong>",round(df.polygon5$PopulationToGrowthRateRatio,digits = 2)),
                  color = "#BDBDC3",
                  fillOpacity = 0.8,
                  weight = 1)%>%
      addLegend(position = "topright", pal = pal2, values = df.polygon5[[input$VariableInput]] ,
                title =  ~paste(input$VariableInput))
    
  })
  
  selected <- reactive({
    subset(censuscovid,date == input$Date)
  })
  
  output$map2 <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles("CartoDB.DarkMatter") %>%
      setView(lat = 39.8283, lng = -98.5795, zoom = 4) 
    
  })
  
  observe({
    df.polygon4@data<- left_join(df.polygon@data, selected(),by = c("GEOID"),all.x = TRUE)
    
    # cannot use color numeric because the heavy skew
    qpal <-  risk.bins <-c(0, 100, 200, 500, 1000,1500, 2500,3500, 5000,7000,17000, 152227) 
    qpal <-  colorBin("plasma", bins=risk.bins, na.color = "#aaff56")
    
    popup <- paste0("<strong>ID: </strong>",df.polygon4$NAME,"<br>","<strong>Confirmed: </strong>",df.polygon4$confirmed)
    
    leafletProxy("map2", data = df.polygon4) %>%
      addProviderTiles("CartoDB.Positron") %>% 
      clearShapes() %>% 
      clearControls() %>% 
      addPolygons(data = df.polygon4, fillColor = ~qpal(confirmed), fillOpacity = 0.7, 
                  color = "white", weight = 2, popup = popup) %>%
      addLegend(pal = qpal, values = ~confirmed, opacity = 0.7,
                position = 'bottomright', 
                title = paste0(input$datez, "<br>"))
  })
  
})

# Run app! 
shinyApp(ui = ui, server = server)
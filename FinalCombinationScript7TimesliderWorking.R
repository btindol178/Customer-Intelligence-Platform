library(rgdal)
library(sp)
library(dplyr)
library(leaflet)
library(shinythemes)
library(shiny)
#install.packages("rmapshaper", lib = "C:/R/R-4.0.2/library")
library(rmapshaper)
# try to make rendering faster
#https://stackoverflow.com/questions/44356224/leaflet-shiny-integration-slow


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
#df.polygon <- county

# County shape dataframe subset this!!!
# df.polygon2  <- df.polygon# moving variable

# Load dataframe
#censuscovid <- read.csv("censuscovid.csv"); censuscovid <- censuscovid[-c(1)]
# df.polygon@data <- censuscovid
# censuscovid$data <- as.Date(censuscovid$date);censuscovid$GEOID <- as.character(censuscovid$GEOID)

# not needed anymore
# censuscovid$GEOID <-ifelse(censuscovid$state == " Alabama",paste("0",censuscovid$GEOID,sep =""),censuscovid$GEOID)
# censuscovid$GEOID <-ifelse(censuscovid$state == " Arkansas",paste("0",censuscovid$GEOID,sep =""),censuscovid$GEOID)
# censuscovid$GEOID <-ifelse(censuscovid$state == " California",paste("0",censuscovid$GEOID,sep =""),censuscovid$GEOID)
# censuscovid$GEOID <-ifelse(censuscovid$state == " Arizona",paste("0",censuscovid$GEOID,sep =""),censuscovid$GEOID)
# censuscovid$GEOID <-ifelse(censuscovid$state == " Colorado",paste("0",censuscovid$GEOID,sep =""),censuscovid$GEOID)
# censuscovid$GEOID <-ifelse(censuscovid$state == " Connecticut",paste("0",censuscovid$GEOID,sep =""),censuscovid$GEOID)
# censuscovid$GEOID <-ifelse(censuscovid$state == " Alaska",paste("0",censuscovid$GEOID,sep =""),censuscovid$GEOID)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# This makes the polygon render faster!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#df.polygon4 <- rmapshaper::ms_simplify(df.polygon2, keep = 0.05, keep_shapes = TRUE)
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#################################################################################################################################################
#################################################################################################################################################



ui <- navbarPage("Cell Connectivity",
                 tabPanel("Country based Cellular Connections",
                          sidebarLayout(
                            sidebarPanel(sliderInput("Date", "Date",
                                                     min =as.Date("2020-01-22","%Y-%m-%d"), max = as.Date("2020-08-24","%Y-%m-%d"),
                                                     value =as.Date("2020-01-22","%Y-%m-%d"),
                                                     timeFormat="%Y-%m-%d",)
                                                     # animate = animationOptions(interval = 500, loop = TRUE), 
                                                     # sep = '')
                            ),
                            mainPanel(
                              
                              leafletOutput("map")
                            )
                          ) 
                 )
)

server <- function(input, output) {
  
  selected <- reactive({
    subset(censuscovid,date == input$Date)
  })
  
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles("CartoDB.DarkMatter") %>%
    setView(lat = 39.8283, lng = -98.5795, zoom = 4) 
      
  })
  
  # output$map <- renderLeaflet({
  #   
  #  # df.polygon2
  #   # popup <- paste0("County: ", df.polygon2$NAME, "<br>", "State: ",df.polygon2$state, "<br>","VulIndex: ",round(df.polygon2$VulIndex,digits = 2),"<br>","Poverty: ",round(df.polygon2$z_Poverty,digits= 2),"<br>","NonWhite: ",round(df.polygon2$z_NonWhite,digits=2),"<br>","Under5: ",round(df.polygon2$z_Under5,digits=2),"<br>","Over64: ",round(df.polygon2$z_Over64,digits=2),"<br>","CoveredinVulnerableAgeGroup: ",round(df.polygon2$z_CoveredVulnerableAge,digits=2),"<br>","totalpopulation: ",round(df.polygon2$totalpopulation,digits=2),"<br>","PercentIncreaseinPopulation: ",round(df.polygon2$PercentIncreaseInPopulation,digits=2),"<br>","TotalIncreaseInPopulation: ",round(df.polygon2$TotalIncreaseInPopulation,digits=2),"<br>","PopulationGrowthRateRatio: ",round(df.polygon2$PopulationToGrowthRateRatio,digits=2))
  #   
  #   risk.bins <-c(0, 100, 200, 500, 1000,1500, 2500,3500, 5000,7000,17000, 152227) 
  #   pal <- colorBin("plasma", bins=risk.bins, na.color = "#aaff56")
  #   
  #   leaflet(df.polygon2) %>%
  #     addProviderTiles("CartoDB.DarkMatter") #%>%
  #   # addPolygons(data = df.polygon2, 
  #   #             fillColor = ~pal(confirmed), 
  #   #             color = "#b2aeae", # you need to use hex colors
  #   #             fillOpacity = 0.7, 
  #   #             weight = 1, 
  #   #             smoothFactor = 0.2,
  #   #             popup = popup) %>%
  #   # addLegend(pal = pal, 
  #   #           values = df.polygon2$confirmed, 
  #   #           position = "bottomright", 
  #   #           title = "Covid Cases Index")
  # })
  
  observe({
    df.polygon4@data<- left_join(df.polygon@data, selected(),by = c("GEOID"),all.x = TRUE)
    
   # cannot use color numeric because the heavy skew
    qpal <-  risk.bins <-c(0, 100, 200, 500, 1000,1500, 2500,3500, 5000,7000,17000, 152227) 
    qpal <-  colorBin("plasma", bins=risk.bins, na.color = "#aaff56")
    
    popup <- paste0("<strong>ID: </strong>",df.polygon4$NAME,"<br>","<strong>Confirmed: </strong>",df.polygon4$confirmed)
    
    leafletProxy("map", data = df.polygon4) %>%
      addProviderTiles("CartoDB.Positron") %>% 
      clearShapes() %>% 
      clearControls() %>% 
      addPolygons(data = df.polygon4, fillColor = ~qpal(confirmed), fillOpacity = 0.7, 
                  color = "white", weight = 2, popup = popup) %>%
      addLegend(pal = qpal, values = ~confirmed, opacity = 0.7,
                position = 'bottomright', 
                title = paste0(input$datez, "<br>"))
  })
  
  
}

shinyApp(ui = ui, server = server)

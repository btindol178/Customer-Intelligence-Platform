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
library(purrr)
library(tidycensus)
library(tidyr)
library(mapview)
library(tidyverse)
library(plotly)

options(tigris_use_cache = TRUE)
# 
 setwd("C:/Users/blake/OneDrive/Stryker Project/Country Leaflet Information Dashboard/Building Leaflet Final Interactive Map/Leaflet Map App")
# # # # # #################################################################################################################################################
# # # # # #################################################################################################################################################
# # # # # #################################################################################################################################################
# # # # # # Load workspace
# # # #load("C:/Users/blake/OneDrive/Stryker Project/Country Leaflet Information Dashboard/Building Leaflet Final Interactive Map/Leaflet Map App/FinalCombinationWorkspace.R.RData")
 # # # 
# # # # #Load the dataframe from the folder
  county <- readOGR(dsn=".", layer = "cb_2018_us_county_500k")
# # # # convert the GEOID to a character (BECAUSE TIDY CENSUS IS CHARACTER FOR SF FILE)
   county@data$GEOID <-as.character(county@data$GEOID)
# # # # # 
# # # # # # # final shape file 
       df.polygon <- county
# # # # # # # 
# # # # # # # # County shape dataframe subset this!!!
      df.polygon2  <- df.polygon# moving variable
# # # # # # # 
# # # # # # # # Load dataframe
       census <- read.csv("census.csv");census <- census[-c(1)];census <- census[c(1,2,3,7,10,14)];census$GEOID <- as.character(census$GEOID)
# #  #  # 
           census$GEOID <-ifelse(census$state == " Alabama",paste("0",census$GEOID,sep =""),census$GEOID)
           census$GEOID <-ifelse(census$state == " Arkansas",paste("0",census$GEOID,sep =""),census$GEOID)
           census$GEOID <-ifelse(census$state == " California",paste("0",census$GEOID,sep =""),census$GEOID)
           census$GEOID <-ifelse(census$state == " Arizona",paste("0",census$GEOID,sep =""),census$GEOID)
           census$GEOID <-ifelse(census$state == " Colorado",paste("0",census$GEOID,sep =""),census$GEOID)
           census$GEOID <-ifelse(census$state == " Connecticut",paste("0",census$GEOID,sep =""),census$GEOID)
           census$GEOID <-ifelse(census$state == " Alaska",paste("0",census$GEOID,sep =""),census$GEOID)
# # # # # #  # # # 
# # # # # # # 
# # # # # # 
      df.polygon5 <- rmapshaper::ms_simplify(df.polygon2, keep = 0.05, keep_shapes = TRUE)
# # #  # # 
     df.polygon5@data <- left_join(df.polygon5@data, census,by = c("GEOID"),all.x = TRUE)
# # # 
# #######################################################################################################################
# #######################################################################################################################
# # #######################################################################################################################

# # # get state information
#   stateid <- read.csv("statesid.csv");us <- stateid$state_id
# # # 
# # # # Get information for every county and then go back and fill in the rest
#   totcountchar <- NULL;
#   for(i in 1:length(unique(us))){
#     totalcountypop <-NULL;
#     tryCatch({ 
#   totalcountypop <- get_estimates(geography = "county",
#                                   product = "characteristics",
#                                   breakdown = c("SEX", "AGEGROUP", "HISP","RACE"),
#                             breakdown_labels = TRUE,
#                                   state = us[i])
#     }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#   totcountchar <- rbind(totcountchar,totalcountypop)
#   }
# 
# # # Saving county characteristics
# compare <- filter(totcountchar, str_detect(AGEGROUP, "^Age"),
#                        HISP != "Both Hispanic Origins",
#                        SEX != "Both sexes") %>%
#        mutate(value = ifelse(SEX == "Male", -value, value)) 
# 
# # make a population dataset for plot and race dataset for plot
# agepopulation <- compare[c(1,2,3,4,5)]
# racepopulation <- compare[c(1,2,3,7)]
# 
# # Fix dataframe to match geoid
# agepopulation$GEOID <-ifelse(grepl("Alabama", agepopulation$NAME, fixed = TRUE),paste("0",agepopulation$GEOID,sep =""),agepopulation$GEOID)
# agepopulation$GEOID <-ifelse(grepl("Arkansas", agepopulation$NAME, fixed = TRUE),paste("0",agepopulation$GEOID,sep =""),agepopulation$GEOID)
# agepopulation$GEOID <-ifelse(grepl("California", agepopulation$NAME, fixed = TRUE),paste("0",agepopulation$GEOID,sep =""),agepopulation$GEOID)
# agepopulation$GEOID <-ifelse(grepl("Arizona", agepopulation$NAME, fixed = TRUE),paste("0",agepopulation$GEOID,sep =""),agepopulation$GEOID)
# agepopulation$GEOID <-ifelse(grepl("Colorado", agepopulation$NAME, fixed = TRUE),paste("0",agepopulation$GEOID,sep =""),agepopulation$GEOID)
# agepopulation$GEOID <-ifelse(grepl("Connecticut", agepopulation$NAME, fixed = TRUE),paste("0",agepopulation$GEOID,sep =""),agepopulation$GEOID)
# agepopulation$GEOID <-ifelse(grepl("Alaska", agepopulation$NAME, fixed = TRUE),paste("0",agepopulation$GEOID,sep =""),agepopulation$GEOID)
# 
# racepopulation$GEOID <-ifelse(grepl("Alabama", racepopulation$NAME, fixed = TRUE),paste("0",racepopulation$GEOID,sep =""),racepopulation$GEOID)
# racepopulation$GEOID <-ifelse(grepl("Arkansas", racepopulation$NAME, fixed = TRUE),paste("0",racepopulation$GEOID,sep =""),racepopulation$GEOID)
# racepopulation$GEOID <-ifelse(grepl("California", racepopulation$NAME, fixed = TRUE),paste("0",racepopulation$GEOID,sep =""),racepopulation$GEOID)
# racepopulation$GEOID <-ifelse(grepl("Arizona", racepopulation$NAME, fixed = TRUE),paste("0",racepopulation$GEOID,sep =""),racepopulation$GEOID)
# racepopulation$GEOID <-ifelse(grepl("Colorado", racepopulation$NAME, fixed = TRUE),paste("0",racepopulation$GEOID,sep =""),racepopulation$GEOID)
# racepopulation$GEOID <-ifelse(grepl("Connecticut", racepopulation$NAME, fixed = TRUE),paste("0",racepopulation$GEOID,sep =""),racepopulation$GEOID)
# racepopulation$GEOID <-ifelse(grepl("Alaska", racepopulation$NAME, fixed = TRUE),paste("0",racepopulation$GEOID,sep =""),racepopulation$GEOID)
# 
# write.csv(agepopulation,file="agepopulation.csv")
# write.csv(racepopulation,file="racepopulation.csv")
# 

# load dataset
agepopulation <- read.csv("agepopulation.csv");agepopulation <- agepopulation[-c(1)]
#racepopulation <- read.csv("racepopulation.csv");racepopulation <- racepopulation[-c(1)]

#######################################################################################################################
# 
# # How to pull estimates for all counties and plot population plot reactivly 
# Hidalgo_estimates <- get_estimates(geography = "county",
#                                    product = "characteristics",
#                                    breakdown = c("SEX", "AGEGROUP", "HISP","RACE"),
#                                    breakdown_labels = TRUE,
#                                    state = "MI",
#                                    county = "Kalamazoo")


#############################################################################################################################
# UI
ui <- shinyUI(fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("VariableInput", label = h3("Variable"),
                                        choices = c("VulIndex","z_Over64","PopulationToGrowthRateRatio"))),
                          mainPanel(leafletOutput(outputId = 'map', height = 800), 
                                    plotlyOutput("plot2") # THis is not working ye
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
  
  county_data <- eventReactive(input$map_shape_click, {
    x <- input$map_shape_click
    z <- agepopulation[agepopulation$GEOID == x$id,]
    return(z)
  })
  
   output$plot2 <- renderPlotly({

     # PYRIMID PLOT POPULATION FOR EACH AGE GROUP
     population_by_age <- plot_ly(county_data(),x = county_data()$value, y = county_data()$AGEGROUP, color  = county_data()$SEX, type = 'bar', orientation = 'h',
                                  hoverinfo = 'y+text+name', text = county_data()$value) %>%
       layout(title = paste("Population"),bargap = 0.1, barmode = 'overlay',
              xaxis = list(tickmode = 'array', tickvals = c(min(county_data()$value), min(county_data()$value)*.75 ,min(county_data()$value)*.50,min(county_data()$value)*.25, 0,max(county_data()$value)*.25,max(county_data()$value)*.50,max(county_data()$value)*.75, max(county_data()$value)), ticktext = c(min(county_data()$value), min(county_data()$value)*.75 ,min(county_data()$value)*.50,min(county_data()$value)*.25, 0,max(county_data()$value)*.25,max(county_data()$value)*.50,max(county_data()$value)*.75, max(county_data()$value))))
     
     
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

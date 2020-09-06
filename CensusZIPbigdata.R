# Zip code level understanding of Census information make layering 
rm(list=ls())
setwd("C:/Users/blake/OneDrive/Stryker Project/Country Leaflet Information Dashboard")
#load("StrykerIntelligenceWaltkthroughWorkspace.rdata")
load("CensusZIPbigdataworkspace2.rdata") # less intensive workspace
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
# Get state information to loop over
# census key code
#census_api_key("e3a3dbad3edfa4d96cb59f65931694b311565c63",install = TRUE,overwrite = TRUE)

# Census variable list
all_vars_acs5 <- load_variables(year = 2018, dataset = "acs5") # read all the variable list
all_vars_acs5

options(tigris_use_cache = TRUE)

# CBSA SHape file
cbsa <- readOGR(dsn=".", layer = "cb_2018_us_cbsa_500k")
#csa <- readOGR(dsn=".", layer = "cb_2018_us_csa_500k")
zcta <- readOGR(dsn = ".",layer = "cb_2018_us_zcta510_500k")
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
# EducationVariables <- c(EduTotal = "B16010_001", Edu1 = "B16010_002", Edu2 = "B16010_015", Edu3 = "B16010_028", Edu4 = "B16010_041")
totalpop2011 <-  "B01003_001"
ZIPpopulation <- get_acs(geography = "zcta",
                                variables = totalpop2011,
                                year = 2011, survey = "acs5",
                                output = "wide", geometry = TRUE,
                                keep_geo_vars=TRUE)

head(ZIPpopulation)

# Make new column to merge states 
#ZIPpopulation$GEOID2 <- ZIPpopulation$GEOID

zcta@data <- left_join(zcta@data,ZIPpopulation,by =c("ZCTA5CE10"),all.x=TRUE)

# # UNDERSTAND OR FIND OUT WHICH ZIP CODES ARE IN WHICH STATES
# zipcodes <- read.csv("zip_code_database.csv");zipcodes <- zipcodes[c(1,4,7,8,13,14)]; colnames(zipcodes) <- c("ZCTA5CE10","city","state","county","zip_lat","zip_lon")
# 
# # MERGE ZIP AND STATE FOR EAZY FILTERING
# ZIPpopulation <- merge(ZIPpopulation,zipcodes, by = c("ZCTA5CE10"),all.x = TRUE)
# ZIPpopulation <- na.omit(ZIPpopulation)
# ZipMichigan <- ZIPpopulation[ZIPpopulation$state == "MI",]
# ZipMichigan <- ZIPpopulation[ZIPpopulation$ZCTA5CE10 == "49009",]
# 
# mapview(ZipMichigan, zcol="B01003_001E",legend= TRUE)
#########################################################################################################################################
#########################################################################################################################################3
#########################################################################################################################################3
# Core based statistical area
# use these for when they click on the map it will filter for the cbsa and then plot zip in another map
zip_cbsa <- read.csv("ZIP_CBSA_062020.csv");colnames(zip_cbsa)[1]<- "ZIP" # use these for when they click on the map it will filter for 
cbsa_zip <- read.csv("CBSA_ZIP_062020.csv"); colnames(cbsa_zip)[1]<-"GEOID"

tranfer2 <- read.delim("zip07_cbsa06.txt",header = TRUE, sep = ",");tranfer2 <- tranfer2[c(8,1,2,3,4,5,6,7,9,10,11,12,13,14,15,16)];colnames(tranfer2)[1] <- "GEOID"; tranfer2$GEOID <- as.character(tranfer2$GEOID)
tranfer2 <- tranfer2[c(1,2,5,6,7,8,9)];tranfer2 <- na.omit(tranfer2)
tranfer2 <- tranfer2[tranfer2$STATE != "PR",]

# Split first number of collumn based on 
tranfer3 <- tranfer2
tranfer3$zip5z <- tranfer3$ZIP5
tranfer4 <- tranfer3 %>%  separate(ZIP5, into = c("Area","Remaining"), sep= 1)

# Create stryker territories
tranfer4$new <- ifelse(tranfer4$Area == "1","Very High",
       ifelse(tranfer4$Area == "2","High",
              ifelse(tranfer4$Area == "3","Medium",
                     ifelse(tranfer4$Area == "4","Low","Lowest"))))

tranfer4 <- tranfer4[c(1,9,3,4,5,6,7,8,10)];colnames(tranfer4)[1] <- "GEOID"; colnames(tranfer4)[9] <- "revenue"

#us_cbsa_components <- get_estimates(geography = "metropolitan statistical area/micropolitan statistical area", product = "components")
us_cbsa_population <- get_estimates(geography = "metropolitan statistical area/micropolitan statistical area", product = "population")

us_cbsa_population2 <- merge(us_cbsa_population,tranfer4, by = c("GEOID"),all.x =TRUE)
us_cbsa_population3 <- distinct(us_cbsa_population2)
us_cbsa_population3 <- us_cbsa_population3[us_cbsa_population3$variable == "POP",]

df.cbsa <- rmapshaper::ms_simplify(cbsa, keep = 0.05, keep_shapes = TRUE)
#df.zcta <- rmapshaper::ms_simplify(zcta, keep = 0.05, keep_shapes = TRUE)

df.cbsa@data <- left_join(df.cbsa@data, us_cbsa_population3,by = c("GEOID"),all.x = TRUE)

# df.cbsa@data <- left_join(df.cbsa@data, z,by = c("GEOID"),all.x = TRUE)

pal <- colorFactor(palette = c("red","blue","green","yellow","orange"),df.cbsa$revenue)
#pal <- colorFactor(palette = "YlGnBu",df.cbsa$revenue)

 # df.cbsa %>%
 #   leaflet() %>%
 #   addTiles()%>%
 #   addPolygons()

pal2 <-  colorNumeric(palette = "YlGnBu",domain = zcta@data$B01003_001E)
# zcta%>%
#   addTiles(group = "OSM") %>% 
#   addProviderTiles("CartoDB", group = "Carto") %>% 
#      addPolygons(stroke=FALSE,smoothFactor = 0.2,fillOpacity = 1,color = ~pal2(zcta$B01003_001E))
#   
# pal2 <-  colorNumeric(palette = "YlGnBu",domain = df.cbsa@data$RES_RATIO)
#  df.cbsa %>%
#    leaflet() %>%
#    addTiles()%>%
#    addPolygons(stroke=FALSE,smoothFactor = 0.2,fillOpacity = 1,color = ~pal2(df.cbsa$RES_RATIO))



####################################
# Make random hospital points for longitude and lattitude  
# Try to make icons https://stackoverflow.com/questions/54978367/custom-markers-with-different-shapes-and-colors-in-leaflet-r
 # For hover point use "C:\Users\blake\OneDrive\Stryker Project\Learning Leaflet\myFinalMap.html" also use longitude and lattitude from this dataset would be easier

 leaflet() %>%
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addProviderTiles("Esri", group = "Esri") %>% 
  addProviderTiles("CartoDB.DarkMatter",group = "CartoDB.DarkMatter")%>%
  addPolygons(data = df.cbsa, fillColor = ~pal(revenue),color = "#b2aeae", group ="revenue",fillOpacity = 0.7,weight = 1, smoothFactor = 0.2) %>%
  addPolygons(data = zcta, fillColor = ~pal2(zcta$B01003_001E),color = "YlGnBu", group ="B01003_001E",fillOpacity = 0.7,weight = 1, smoothFactor = 0.2) %>%
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri","CartoDB.DarkMatter"), overlayGroups =c("revenue","B01003_001E")) %>% 
  hideGroup("B01003_001E")%>%
  setView(lat = 39.8282, lng = -98.5795, zoom = 4) 


ipeds <- read.csv("IPEDS.csv"); colnames(ipeds)[1] <- "name"
ipeds2 <-ipeds[sample(nrow(ipeds), 30), ] # make dataframe smaller take sample
# Create stryker territories
ipeds2$sector_label <- ifelse(ipeds2$sector_label == "Private","High",
                       ifelse(ipeds2$sector_label == "Public","Medium",
                              ifelse(ipeds2$sector_label == "For-Profit","Low","Lowest")))

# Make many drop down options
High <- ipeds2[ipeds2$sector_label == "High",]
Medium <- ipeds2[ipeds2$sector_label == "Medium",]
Low <- ipeds2[ipeds2$sector_label == "Low",]

popup_text <-   paste0('<strong>', ipeds2$name, '</strong>', '<br/>', '<strong>', ipeds2$sector_label, '</strong>') %>%   lapply(htmltools::HTML)
popup_text2 <-   paste0('<strong>', df.cbsa$CBSA.TITLE, '</strong>', '<br/>', '<strong>', df.cbsa$revenue, '</strong>') %>%   lapply(htmltools::HTML)


addCircleMarkers(data = High, radius = 2, label = ~popup_text, color = ~pal(sector_label), group = "High")

leaflet() %>%
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addProviderTiles("Esri", group = "Esri") %>% 
  addProviderTiles("CartoDB.DarkMatter",group = "CartoDB.DarkMatter")%>%
  addPolygons(data = df.cbsa, fillColor = ~pal(revenue),color = "#b2aeae", group ="revenue",fillOpacity = 0.7,weight = 1, smoothFactor = 0.2) %>%
  addCircleMarkers(data = High, radius = 2, label = ~popup_text, color = ~pal(sector_label), group = "High")%>%
  addPolygons(data = zcta, fillColor = ~pal2(zcta$B01003_001E),color = "YlGnBu", group ="B01003_001E",fillOpacity = 0.7,weight = 1, smoothFactor = 0.2) %>%
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri","CartoDB.DarkMatter"), overlayGroups =c("revenue","B01003_001E","High")) %>% 
  hideGroup("B01003_001E")%>%
  setView(lat = 39.8282, lng = -98.5795, zoom = 4)



leaflet() %>%
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addProviderTiles("Esri", group = "Esri") %>% 
  addProviderTiles("CartoDB.DarkMatter",group = "CartoDB.DarkMatter")%>%
  addPolygons(data = df.cbsa, fillColor = ~pal(revenue),color = "#b2aeae", group ="revenue",fillOpacity = 0.6,weight = 1, smoothFactor = 0.2,label = ~popup_text2) %>%
  addCircleMarkers(data = High, radius = 1, label = ~popup_text, color = "black", group = "High")%>%
 # addPolygons(data = zcta, fillColor = ~pal2(zcta$B01003_001E),color = "YlGnBu", group ="B01003_001E",fillOpacity = 0.7,weight = 1, smoothFactor = 0.2) %>%
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri","CartoDB.DarkMatter"), overlayGroups =c("revenue","High")) %>% 
  setView(lat = 39.8282, lng = -98.5795, zoom = 4)

ipeds2$color <- rep(c("red", "green", "gray"), 5)

ipeds3 <- ipeds2 %>%
  mutate(Group = case_when(
    sector_label == "Medium" ~ "triangle",
    sector_label == "High" ~ "circle",
    sector_label == "Low" ~ "square"),
    group_color = as.factor(paste(Group, color, sep = "_")))

my_icons2 <- iconList(triangle <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/triangle-png-28.png",iconWidth = 18, iconHeight = 18))

my_icons2 <- iconList(triangle <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/hospital-icon-1.png",iconWidth = 18, iconHeight = 18))

https://www.freeiconspng.com/uploads/blue-hospital-sign-icon-10.png
square2 <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/black-square-frame-23.png", iconWidth = 1, iconHeight = 1)


leaflet() %>%
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addProviderTiles("Esri", group = "Esri") %>% 
  addProviderTiles("CartoDB.DarkMatter",group = "CartoDB.DarkMatter")%>%
  #addPolygons(data = df.cbsa, fillColor = ~pal(revenue),color = "#b2aeae", group ="revenue",fillOpacity = 0.6,weight = 1, smoothFactor = 0.2,label = ~popup_text2) %>%
  addMarkers(data = ipeds2,lng = ~lng, lat = ~ lat, icon = ~ my_icons2, label = ~popup_text)%>% 
  # addPolygons(data = zcta, fillColor = ~pal2(zcta$B01003_001E),color = "YlGnBu", group ="B01003_001E",fillOpacity = 0.7,weight = 1, smoothFactor = 0.2) %>%
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri","CartoDB.DarkMatter")) %>% 
  setView(lat = 39.8282, lng = -98.5795, zoom = 4)


leaflet() %>%
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addProviderTiles("Esri", group = "Esri") %>% 
  addProviderTiles("CartoDB.DarkMatter",group = "CartoDB.DarkMatter")%>%
  addPolygons(data = df.cbsa, fillColor = ~pal(revenue),color = "#b2aeae", group ="revenue",fillOpacity = 0.6,weight = 1, smoothFactor = 0.2,label = ~popup_text2) %>%
  addMarkers(data = ipeds2,lng = ~lng, lat = ~ lat, icon = ~ my_icons2, label = ~popup_text,group = "hospitals")%>% 
  # addPolygons(data = zcta, fillColor = ~pal2(zcta$B01003_001E),color = "YlGnBu", group ="B01003_001E",fillOpacity = 0.7,weight = 1, smoothFactor = 0.2) %>%
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri","CartoDB.DarkMatter"),overlayGroups =c("revenue","hospitals")) %>% 
  setView(lat = 39.8282, lng = -98.5795, zoom = 4)


leaflet() %>%
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addProviderTiles("Esri", group = "Esri") %>% 
  addProviderTiles("CartoDB.DarkMatter",group = "CartoDB.DarkMatter")%>%
  addPolygons(data = df.cbsa, fillColor = ~pal(revenue),color = "#b2aeae", group ="revenue",fillOpacity = 0.6,weight = 1, smoothFactor = 0.2,label = ~popup_text2) %>%
  addMarkers(data = ipeds2,lng = ~lng, lat = ~ lat, icon = ~ my_icons2, label = ~popup_text,group = "hospitals")%>% 
  addPolygons(data = zcta, fillColor = ~pal2(zcta$B01003_001E),color = "YlGnBu", group ="B01003_001E",fillOpacity = 0.7,weight = 1, smoothFactor = 0.2) %>%
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri","CartoDB.DarkMatter"),overlayGroups =c("revenue","hospitals","B01003_001E")) %>% 
  hideGroup("B01003_001E")%>%
  setView(lat = 39.8282, lng = -98.5795, zoom = 4)


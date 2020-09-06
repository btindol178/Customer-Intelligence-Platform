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
zcta <- readOGR(dsn = ".",layer = "cb_2018_us_zcta510_500k")
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
# Zip code level information
totalpop2011 <-  "B01003_001" # population variable

# grabbing census information
ZIPpopulation <- get_acs(geography = "zcta",
                         variables = totalpop2011,
                         year = 2011, survey = "acs5",
                         output = "wide", geometry = TRUE,
                         keep_geo_vars=TRUE)

head(ZIPpopulation)
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#UPload zipcode to cbsa dataset
tranfer2 <- read.delim("zip07_cbsa06.txt",header = TRUE, sep = ",");tranfer2 <- tranfer2[c(8,1,2,3,4,5,6,7,9,10,11,12,13,14,15,16)];colnames(tranfer2)[1] <- "GEOID"; tranfer2$GEOID <- as.character(tranfer2$GEOID)
tranfer2 <- tranfer2[c(1,2,5,6,7,8,9)];tranfer2 <- na.omit(tranfer2) # na values removed for simplicity for now
tranfer2 <- tranfer2[tranfer2$STATE != "PR",] # remove puetorico 

# Split first number of collumn based on 
tranfer3 <- tranfer2 # hold on to this
tranfer3$zip5z <- tranfer3$ZIP5 # make new column to hold old zip
tranfer4 <- tranfer3 %>%  separate(ZIP5, into = c("Area","Remaining"), sep= 1) # try to split areas based on first number of string

# Create stryker territories based on the split column if area is 1 then high sales 2 low sales etc
tranfer4$new <- ifelse(tranfer4$Area == "1","Very High",
                       ifelse(tranfer4$Area == "2","High",
                              ifelse(tranfer4$Area == "3","Medium",
                                     ifelse(tranfer4$Area == "4","Low","Lowest"))))

# rename that new column revenue and reorganize columns
tranfer4 <- tranfer4[c(1,9,3,4,5,6,7,8,10)];colnames(tranfer4)[1] <- "GEOID"; colnames(tranfer4)[9] <- "revenue"

# Get cbsa population information
us_cbsa_population <- get_estimates(geography = "metropolitan statistical area/micropolitan statistical area", product = "population")
us_cbsa_population2 <- merge(us_cbsa_population,tranfer4, by = c("GEOID"),all.x =TRUE) # merge cbsa with zipcode information
us_cbsa_population3 <- distinct(us_cbsa_population2) # keep unique values
us_cbsa_population3 <- us_cbsa_population3[us_cbsa_population3$variable == "POP",] # dont need density for now

# simplify cbsa shape file 
df.cbsa <- rmapshaper::ms_simplify(cbsa, keep = 0.05, keep_shapes = TRUE)
df.cbsa@data <- left_join(df.cbsa@data, us_cbsa_population3,by = c("GEOID"),all.x = TRUE) # join the us_cbsa to it
zcta@data <- left_join(zcta@data,ZIPpopulation,by =c("ZCTA5CE10"),all.x=TRUE) # simplify the zipcode level information

#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################
# Read in simulated hospital data
ipeds <- read.csv("IPEDS.csv"); colnames(ipeds)[1] <- "name"
ipeds2 <-ipeds[sample(nrow(ipeds), 30), ] # make dataframe smaller take sample

# Create stryker territories
ipeds2$sector_label <- ifelse(ipeds2$sector_label == "Private","High",
                              ifelse(ipeds2$sector_label == "Public","Medium",
                                     ifelse(ipeds2$sector_label == "For-Profit","Low","Lowest")))

# make color for map
pal <- colorFactor(palette = c("red","blue","green","yellow","orange"),df.cbsa$revenue) # cbsa level
pal2 <-  colorNumeric(palette = "YlGnBu",domain = zcta@data$B01003_001E) # zipcode level color

# make popup text for the hosptials and the cbsa level data could add the zip later
popup_text <-   paste0('<strong>', ipeds2$name, '</strong>', '<br/>', '<strong>', ipeds2$sector_label, '</strong>') %>%   lapply(htmltools::HTML)
popup_text2 <-   paste0('<strong>', df.cbsa$CBSA.TITLE, '</strong>', '<br/>', '<strong>', df.cbsa$revenue, '</strong>') %>%   lapply(htmltools::HTML)

# Grabbing hosptial icon from this url
my_icons2 <- iconList(triangle <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/hospital-icon-1.png",iconWidth = 18, iconHeight = 18))

#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################
# using only hospital layer
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

#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################
# only hospital and csba layer
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

#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################
# All layers longer loading!!!!!!!!!!!!! instead of all zips maybe just when hover over cbsa or stryker region
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
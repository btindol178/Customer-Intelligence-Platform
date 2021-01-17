##################################################################################################################################################################################################################################################
# Top dataframes (df.polygon2, mobilitypolygon, covid.polygon)

load("C:/Users/blake/OneDrive/Stryker Project/Country Leaflet Information Dashboard/Building Leaflet Final Interactive Map/FinalCombinationWorkspace.R")
library(rgdal)
library(dplyr)
library(leaflet)
library(tidyr)
library(RColorBrewer)
display.brewer.all()

# Run this if fips_codes isnt working but only if
x <- c("lubridate","data.table","zoo","riem","dplyr","tidyr","countrycode","RCurl","stringr","tidyverse","shiny","miniUI","taskscheduleR","gtrendsR","tidycensus","sf","leaflet","mapview","viridis","tidyquant","tigris","tmap","sf","maps","tidycensus","ggsflabel","scales","tmaptools","purrr","plotly","hrbrthemes","DT")
lapply(x, require, character.only = TRUE)

##################################################################################################################################################################################################################################################
##################################################################################################################################################################################################################################################
#tract <- readOGR(dsn=".", layer = "cb_2014_36_tract_500k")
# Set working directory to the county shape file (SpatialPolygonsDataFrame)
setwd("C:/Users/blake/OneDrive/Stryker Project/Country Leaflet Information Dashboard/cb_2018_us_county_500k")

library(rgdal)
#Load the dataframe from the folder
county <- readOGR(dsn=".", layer = "cb_2018_us_county_500k")
# convert the GEOID to a character (BECAUSE TIDY CENSUS IS CHARACTER FOR SF FILE)
county@data$GEOID <-as.character(county@data$GEOID)

setwd("C:/Users/blake/OneDrive/Stryker Project/Country Leaflet Information Dashboard/Building Leaflet Final Interactive Map/Leaflet Map App")


# Read in census data
census <- read.csv("census.csv");census <- census[-c(1)]

# load mobility data
# Refer to strykerintelligencewalkthroughscript for this part
# mobility <- read.csv("mobility.csv"); mobility <- mobility[-c(1)]
# data(fips_codes); fips_codes <- data.frame(fips_codes); fips_codes <- fips_codes[c(3,5,1,2,4)];colnames(fips_codes)[1]<-"state"; colnames(fips_codes)[3] <- "state_abbr"
# mobility2 <- merge(fips_codes,mobility,by = c("state","county"),all.x = TRUE) # merge by county name and state name
# mobility2$GEOID <- paste(mobility2$state_code,mobility2$county_code,sep="")
# mobility2 <- mobility2[c(13,1,2,3,4,5,6,7,8,9,10,11,12)]

mobility2 <- read.csv("mobility2.csv");mobility2 <- mobility2[-c(1)];mobility2$GEOID <- as.character(mobility2$GEOID)

# load covid data
final_covid <- read.csv("final_covid.csv");final_covid <- final_covid[-c(1)]; final_covid$GEOID <- as.character(final_covid$GEOID)


census$GEOID <- as.character(census$GEOID)
# Can remove white space in front of name if want (for some reason missing zero so dataframe cant merge properly)
census$GEOID <-ifelse(census$state == " Alabama",paste("0",census$GEOID,sep =""),census$GEOID)
census$GEOID <-ifelse(census$state == " Arkansas",paste("0",census$GEOID,sep =""),census$GEOID)
census$GEOID <-ifelse(census$state == " California",paste("0",census$GEOID,sep =""),census$GEOID)
census$GEOID <-ifelse(census$state == " Arizona",paste("0",census$GEOID,sep =""),census$GEOID)
census$GEOID <-ifelse(census$state == " Colorado",paste("0",census$GEOID,sep =""),census$GEOID)
census$GEOID <-ifelse(census$state == " Connecticut",paste("0",census$GEOID,sep =""),census$GEOID)
census$GEOID <-ifelse(census$state == " Alaska",paste("0",census$GEOID,sep =""),census$GEOID)


final_covid$GEOID <-ifelse(final_covid$state == "Alabama",paste("0",final_covid$GEOID,sep =""),final_covid$GEOID)
final_covid$GEOID <-ifelse(final_covid$state == "Arkansas",paste("0",final_covid$GEOID,sep =""),final_covid$GEOID)
final_covid$GEOID <-ifelse(final_covid$state == "California",paste("0",final_covid$GEOID,sep =""),final_covid$GEOID)
final_covid$GEOID <-ifelse(final_covid$state == "Arizona",paste("0",final_covid$GEOID,sep =""),final_covid$GEOID)
final_covid$GEOID <-ifelse(final_covid$state == "Colorado",paste("0",final_covid$GEOID,sep =""),final_covid$GEOID)
final_covid$GEOID <-ifelse(final_covid$state == "Connecticut",paste("0",final_covid$GEOID,sep =""),final_covid$GEOID)
final_covid$GEOID <-ifelse(final_covid$state == "Alaska",paste("0",final_covid$GEOID,sep =""),final_covid$GEOID)

mobility2$GEOID <-ifelse(mobility2$state == "Alabama",paste("0",mobility2$GEOID,sep =""),mobility2$GEOID)
mobility2$GEOID <-ifelse(mobility2$state == "Arkansas",paste("0",mobility2$GEOID,sep =""),mobility2$GEOID)
mobility2$GEOID <-ifelse(mobility2$state == "California",paste("0",mobility2$GEOID,sep =""),mobility2$GEOID)
mobility2$GEOID <-ifelse(mobility2$state == "Arizona",paste("0",mobility2$GEOID,sep =""),mobility2$GEOID)
mobility2$GEOID <-ifelse(mobility2$state == "Colorado",paste("0",mobility2$GEOID,sep =""),mobility2$GEOID)
mobility2$GEOID <-ifelse(mobility2$state == "Connecticut",paste("0",mobility2$GEOID,sep =""),mobility2$GEOID)
mobility2$GEOID <-ifelse(mobility2$state == "Alaska",paste("0",mobility2$GEOID,sep =""),mobility2$GEOID)
# make it mergable
final_covid <- final_covid %>% separate(GEOID, into = c('STATEFP', 'COUNTYFP'), sep = 2)
final_covid$GEOID <- paste(final_covid$STATEFP,final_covid$COUNTYFP,sep=""); final_covid <- final_covid[c(1,2,10,3,4,5,6,7,8,9)]
final_covid <- final_covid[final_covid$state != "Alaska" & final_covid$state != "Hawaii" & final_covid$state != "Puerto Rico" & final_covid$state != "American Samoa" & final_covid$state != "Guam" & final_covid$state != "Northern Mariana Islands" & final_covid$state != "Virgin Islands" & final_covid$state != "Diamond Princess" & final_covid$state != "Grand Princess",]

# make it mergable
mobility2 <- mobility2 %>% separate(GEOID, into = c('STATEFP', 'COUNTYFP'), sep = 2)
mobility2$GEOID <- paste(mobility2$STATEFP,mobility2$COUNTYFP,sep="");mobility2 <- mobility2[c(1,2,15,3,4,5,6,7,8,9,10,11,12,13,14)]

census2 <- merge(census,final_covidz, by = c("GEOID"),all.x = TRUE)
colnames(census2)[30] <- "confirmed";colnames(census2)[31] <- "deaths";census2 <- census2[-c(21,22,23)] 
# check class
class(county)

# Create a new version
df.polygon2 <- county

# create covid polygon
covid.polygon <- county

# create mobility polygon
mobility.polygon <- county

# create a rec-field to make sure that we have the order correct
# this probably is unnecessary but it helps to be careful
df.polygon2@data$rec<-1:nrow(df.polygon2@data)# census
covid.polygon@data$rec<-1:nrow(covid.polygon@data) # covid
mobility.polygon@data$rec<-1:nrow(mobility.polygon@data) # mobility


library(dplyr)
# Left join the data
tmp <- left_join(df.polygon2@data, census2, by=c("GEOID")) %>% 
  arrange(rec)

final_covidz <- final_covid[final_covid$date =="2020-04-22",]
colnames(final_covidz)[1]<- "county"
final_covidz <- final_covidz[c(4,1,2,3,4,5,6,7,8,9,10)]
tmp2 <- left_join(covid.polygon@data, final_covidz, by=c("NAME")) %>% 
  arrange(rec)

tmp2 <- na.omit(tmp2)
tmp2 <- tmp2[c(5,6,13,27,28)]

tmp3 <- left_join(mobility.polygon@data, mobility2, by=c("GEOID")) %>% 
  arrange(rec)
tmp3 <- na.omit(tmp3)
tmp3z <- tmp3[tmp3$date =="2020-05-22",]

# replace the original data with the new merged data
df.polygon2@data<-tmp
covid.polygon@data<-tmp2
mobility.polygon@data<-tmp3z



#Make pop up value 
popup <- paste0("County: ", df.polygon2$NAME, "<br>", "State: ",df.polygon2$state, "<br>","VulIndex: ",round(df.polygon2$VulIndex,digits = 2),"<br>","Poverty: ",round(df.polygon2$z_Poverty,digits= 2),"<br>","NonWhite: ",round(df.polygon2$z_NonWhite,digits=2),"<br>","Under5: ",round(df.polygon2$z_Under5,digits=2),"<br>","Over64: ",round(df.polygon2$z_Over64,digits=2),"<br>","CoveredinVulnerableAgeGroup: ",round(df.polygon2$z_CoveredVulnerableAge,digits=2),"<br>","totalpopulation: ",round(df.polygon2$totalpopulation,digits=2),"<br>","PercentIncreaseinPopulation: ",round(df.polygon2$PercentIncreaseInPopulation,digits=2),"<br>","TotalIncreaseInPopulation: ",round(df.polygon2$TotalIncreaseInPopulation,digits=2),"<br>","PopulationGrowthRateRatio: ",round(df.polygon2$PopulationToGrowthRateRatio,digits=2))

#pal <- colorNumeric(palette = "YlGnBu",  domain = df.polygon2$confirmed)
risk.bins <-c(0, 100, 200, 500, 1000,1500, 2500,3500, 5000,7000,17000, 152227) 
pal <- colorBin("plasma", bins=risk.bins, na.color = "#aaff56")

library(leaflet)

map1<-leaflet() %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolygons(data = df.polygon2, 
              fillColor = ~pal(confirmed), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = df.polygon2$confirmed, 
            position = "bottomright", 
            title = "Covid Cases Index")
# , labFormat = labelFormat(suffix = "%")) 
map1


###########################################################################################################
popup <- paste0("County: ", covid.polygon$county,"<br>","Confirmed: ",covid.polygon$confirmed)

#palz<- colorNumeric(palette = "YlGnBu",domain = covid.polygon$confirmed)
risk.bins <-c(0, 100, 200, 500, 1000,1500, 2500,3500, 5000,7000,17000) 
palz <- colorBin("plasma", bins=risk.bins, na.color = "#aaff56")

leaflet() %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolygons(data = covid.polygon, fillColor = ~palz(confirmed),color = "#b2aeae", fillOpacity = 0.7,weight = 1, smoothFactor = 0.2,popup = popup) %>%
  addLegend(pal =palz, values = covid.polygon$confirmed, position = "bottomleft",title = "Covid Cases")

##########################################################################################################
# ADDING ADDITIONAL FEATURES NOW
map2 <-leaflet() %>%
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addProviderTiles("Esri", group = "Esri") %>% 
  addProviderTiles("CartoDB.DarkMatter",group = "CartoDB.DarkMatter")%>%
  addPolygons(data = df.polygon2, 
              fillColor = ~pal(VulIndex), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = df.polygon2$VulIndex, 
            position = "bottomright", 
            title = "Social Vulderability Index")%>%
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri","CartoDB.DarkMatter")) %>% 
  setView(lat = 39.8282, lng = -98.5795, zoom = 4) 
map2

##########################################################################################################
##########################################################################################################
##########################################################################################################
# Make Pal for each different layer/ variable in the dataframe..
z_Poverty_pal <- colorNumeric(palette = "YlGnBu",domain = df.polygon2$z_Poverty)
z_NonWhite_pal <- colorNumeric(palette = "YlGnBu",domain = df.polygon2$z_NonWhite)
z_Under5_pal <- colorNumeric(palette = "YlGnBu",domain = df.polygon2$z_Under5)
z_Over64_pal <- colorNumeric(palette = "YlGnBu",domain = df.polygon2$z_Over64)
z_CoveredVulnerableAge_pal <- colorNumeric(palette = "YlGnBu",domain = df.polygon2$z_CoveredVulnerableAge)
totalpopulation_pal <- colorNumeric(palette = "YlGnBu",domain = df.polygon2$totalpopulation)
vulIndex_pal <- colorNumeric(palette = "YlGnBu",domain = df.polygon2$VulIndex)
PercentIncreaseInPopulation_pal <- colorNumeric(palette = "YlGnBu",domain = df.polygon2$PercentIncreaseInPopulation)
PercentcoveredinsuranceInvulnerableAge_pal <- colorNumeric(palette = "YlGnBu",domain = df.polygon2$PercentcoveredinsuranceInvulnerableAge)
TotalIncreaseInPopulation_pal <- colorNumeric(palette = "YlGnBu",domain = df.polygon2$TotalIncreaseInPopulation)
PopulationToGrowthRateRatio_pal <- colorNumeric(palette = "YlGnBu",domain = df.polygon2$PopulationToGrowthRateRatio)
retail.and.recreation_pal <- colorNumeric(palette = "YlGnBu",domain = df.polygon2$retail.and.recreation)
grocery.and.pharmacy_pal <- colorNumeric(palette = "YlGnBu",domain = df.polygon2$grocery.and.pharmacy)
parks_pal <- colorNumeric(palette = "YlGnBu",domain = df.polygon2$parks)
transit.stations_pal <- colorNumeric(palette = "YlGnBu",domain = df.polygon2$transit.stations)
workplaces_pal <- colorNumeric(palette = "YlGnBu",domain = df.polygon2$workplaces)
residential_pal <- colorNumeric(palette = "YlGnBu",domain = df.polygon2$residential)
confirmed_pal <- colorNumeric(palette = "YlGnBu",domain = df.polygon2$confirmed)
deaths_pal <- colorNumeric(palette = "YlGnBu",domain = df.polygon2$deaths)

# # reassign a dataframe for specific column
# df.polygon3 <- df.polygon2
# df.polygon3@data <- df.polygon3@data[c(1:13)]

# ADDING ADDITIONAL FEATURES NOW
map2 <-leaflet() %>%
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addProviderTiles("Esri", group = "Esri") %>% 
  addProviderTiles("CartoDB.DarkMatter",group = "CartoDB.DarkMatter")%>%
  addPolygons(data = df.polygon2, fillColor = ~pal(VulIndex),color = "#b2aeae", group ="VulIndex",fillOpacity = 0.7,weight = 1, smoothFactor = 0.2,popup = popup) %>%
  addPolygons(data = df.polygon2, fillColor = ~z_Poverty_pal(z_Poverty),color = "#b2aeae", group ="z_Poverty",fillOpacity = 0.7,weight = 1, smoothFactor = 0.2,popup = popup) %>%
  #addLegend(pal = pal, values = df.polygon2$VulIndex, position = "bottomright", title = "Social Vulderability Index")%>%
  # can put names(df[c(1:13)]) for overlay groups
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri","CartoDB.DarkMatter"), overlayGroups =c("VulIndex","z_Poverty")) %>% 
  setView(lat = 39.8282, lng = -98.5795, zoom = 4) 
map2  

###############################################
# ADDING DYNAMIC LEGEND FEATURE


map3 <-leaflet() %>%
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addProviderTiles("Esri", group = "Esri") %>% 
  addProviderTiles("CartoDB.DarkMatter",group = "CartoDB.DarkMatter")%>%
  addPolygons(data = df.polygon2, fillColor = ~pal(VulIndex),color = "#b2aeae", group ="VulIndex",fillOpacity = 0.7,weight = 1, smoothFactor = 0.2,popup = popup) %>%
  addPolygons(data = df.polygon2, fillColor = ~z_Poverty_pal(z_Poverty),color = "#b2aeae", group ="z_Poverty",fillOpacity = 0.7,weight = 1, smoothFactor = 0.2,popup = popup) %>%
  addLegend(pal =vulIndex_pal, values = df.polygon2$VulIndex, group = "VulIndex", position = "bottomleft",title = "Vulnerability Index")%>%
  addLegend(pal =z_Poverty_pal,values = df.polygon2$z_Poverty, group = "z_Poverty", position = "bottomleft",title = "Poverty Score") %>% 
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri","CartoDB.DarkMatter"), overlayGroups =c("VulIndex","z_Poverty")) %>% 
  hideGroup("z_Poverty")%>%
  setView(lat = 39.8282, lng = -98.5795, zoom = 4) 
map3



map4 <-leaflet() %>%
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addProviderTiles("Esri", group = "Esri") %>% 
  addProviderTiles("CartoDB.DarkMatter",group = "CartoDB.DarkMatter")%>%
  addPolygons(data = df.polygon2, fillColor = ~vulIndex_pal(VulIndex),color = "#b2aeae", group ="VulIndex",fillOpacity = 0.7,weight = 1, smoothFactor = 0.2,popup = popup) %>%
  addPolygons(data = df.polygon2, fillColor = ~z_Poverty_pal(z_Poverty),color = "#b2aeae", group ="z_Poverty",fillOpacity = 0.7,weight = 1, smoothFactor = 0.2,popup = popup) %>%
  addPolygons(data = df.polygon2, fillColor = ~z_NonWhite_pal(z_NonWhite),color = "#b2aeae", group ="z_NonWhite",fillOpacity = 0.7,weight = 1, smoothFactor = 0.2,popup = popup) %>%
  addPolygons(data = df.polygon2, fillColor = ~z_Under5_pal(z_Under5),color = "#b2aeae", group ="z_Under5",fillOpacity = 0.7,weight = 1, smoothFactor = 0.2,popup = popup) %>%
  addPolygons(data = df.polygon2, fillColor = ~z_Over64_pal(z_Over64),color = "#b2aeae", group ="z_Over64",fillOpacity = 0.7,weight = 1, smoothFactor = 0.2,popup = popup) %>%
  addPolygons(data = df.polygon2, fillColor = ~z_CoveredVulnerableAge_pal(z_CoveredVulnerableAge),color = "#b2aeae", group ="z_CoveredVulnerableAge",fillOpacity = 0.7,weight = 1, smoothFactor = 0.2,popup = popup) %>%
  addPolygons(data = df.polygon2, fillColor = ~totalpopulation_pal(totalpopulation),color = "#b2aeae", group ="totalpopulation",fillOpacity = 0.7,weight = 1, smoothFactor = 0.2,popup = popup) %>%
  addPolygons(data = df.polygon2, fillColor = ~PercentIncreaseInPopulation_pal(PercentIncreaseInPopulation),color = "#b2aeae", group ="PercentIncreaseInPopulation",fillOpacity = 0.7,weight = 1, smoothFactor = 0.2,popup = popup) %>%
  addPolygons(data = df.polygon2, fillColor = ~TotalIncreaseInPopulation_pal(TotalIncreaseInPopulation),color = "#b2aeae", group ="TotalIncreaseInPopulation",fillOpacity = 0.7,weight = 1, smoothFactor = 0.2,popup = popup) %>%
  addPolygons(data = df.polygon2, fillColor = ~PopulationToGrowthRateRatio_pal(PopulationToGrowthRateRatio),color = "#b2aeae", group ="PopulationToGrowthRateRatio",fillOpacity = 0.7,weight = 1, smoothFactor = 0.2,popup = popup) %>%
  addLegend(pal =vulIndex_pal, values = df.polygon2$VulIndex, group = "VulIndex", position = "bottomleft",title = "Vulnerability Index")%>%
  addLegend(pal =z_Poverty_pal,values = df.polygon2$z_Poverty, group = "z_Poverty", position = "bottomleft",title = "z_Poverty") %>% 
  addLegend(pal =z_NonWhite_pal,values = df.polygon2$z_NonWhite, group = "z_NonWhite", position = "bottomleft",title = "z_NonWhite") %>% 
  addLegend(pal =z_Under5_pal,values = df.polygon2$z_Poverty, group = "z_Under5", position = "bottomleft",title = "z_Under5") %>% 
  addLegend(pal =z_Over64_pal,values = df.polygon2$z_Over64, group = "z_Over64", position = "bottomleft",title = "z_Over64") %>% 
  addLegend(pal =z_CoveredVulnerableAge_pal,values = df.polygon2$z_CoveredVulnerableAge, group = "z_CoveredVulnerableAge", position = "bottomleft",title = "z_CoveredVulnerableAge") %>% 
  addLegend(pal =totalpopulation_pal,values = df.polygon2$totalpopulation, group = "totalpopulation", position = "bottomleft",title = "totalpopulation") %>% 
  addLegend(pal =PercentIncreaseInPopulation_pal,values = df.polygon2$PercentIncreaseInPopulation, group = "PercentIncreaseInPopulation", position = "bottomleft",title = "PercentIncreaseInPopulation") %>% 
  addLegend(pal =TotalIncreaseInPopulation_pal,values = df.polygon2$TotalIncreaseInPopulation, group = "TotalIncreaseInPopulation", position = "bottomleft",title = "TotalIncreaseInPopulation") %>% 
  addLegend(pal =PopulationToGrowthRateRatio_pal,values = df.polygon2$PopulationToGrowthRateRatio, group = "PopulationToGrowthRateRatio", position = "bottomleft",title = "PopulationToGrowthRateRatio") %>% 
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri","CartoDB.DarkMatter"), overlayGroups =c("VulIndex","z_Poverty","z_NonWhite","z_Under5","z_Over64","z_CoveredVulnerableAge","totalpopulation","PercentIncreaseInPopulation","TotalIncreaseInPopulation","PopulationToGrowthRateRatio"), options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup("z_Poverty")%>%
  hideGroup("z_NonWhite")%>%
  hideGroup("z_Under5")%>%
  hideGroup("z_Over64")%>%
  hideGroup("z_CoveredVulnerableAge")%>%
  hideGroup("totalpopulation")%>%
  hideGroup("PercentIncreaseInPopulation")%>%
  hideGroup("TotalIncreaseInPopulation")%>%
  hideGroup("PopulationToGrowthRateRatio")%>%
  setView(lat = 39.8282, lng = -98.5795, zoom = 4) 
map4 


###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################
# Function to search last error
search_last_err_on_so <- function() {
  last_err_msg <- geterrmessage()
  if(grepl("rlang::last_error",last_err_msg)) {
    last_err_msg <- rlang::last_error()$message
  }
  browseURL(paste0(
    "https://stackoverflow.com/search?q=",
    URLencode(last_err_msg)
  ))
}
options(error = search_last_err_on_so)

search_last_err_on_so() # use this to search last error

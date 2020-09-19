library(gtrendsR)
library(leaflet)
library(dplyr)
setwd("C:/Users/blake/OneDrive/Stryker Project/Country Leaflet Information Dashboard/Building Leaflet Final Interactive Map/Leaflet Map App")

uscities <- read.csv("uscities.csv")
MI <- gtrends("Dominos Pizza" ,geo = "US-MI",time = "all" )$interest_by_city
uscitiesMI <- uscities[uscities$state_id =="MI",]
MI <- data.frame(MI); colnames(MI)[1]<- "city"


uscitiesMI <- uscitiesMI[uscitiesMI$city =="Traverse City",]
MI2 <- MI[MI$location == "Traverse City",]

merge1 <-merge(uscitiesMI,MI,by = c("city"),all.x = TRUE)
merge2 <- merge1[! is.na(merge1$hits),]
merge2$color <- ifelse(merge2$city)

merge2$color <- merge2 %>%
  mutate(color = case_when(hits <= 20 ~ 'Low',
                                hits >= 20  & hits < 50 ~ "med",
                                hits >= 50  & hits < 80 ~ "med high",
                                hits >= 80 ~ "High"))


merge2
pal <- colorFactor(palette = c("red","yellow","blue","green"),
                   levels = c("Low","med","med high","High"))

leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(data = merge2,
                   lng = ~lng,
                   lat = ~lat,
                   popup =  ~paste0("<b>","<Strong>","City: ",city, "<b>", "<br/>","<Strong>","Population Density: ",density,"<br/>","<b>","<Strong>","Google Searches: ",hits,"<b>"), # making bold and add break
                   color = ~pal(merge2$color$color),
                   radius = ~hits/10)

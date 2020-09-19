library(gtrendsR)
setwd("C:/Users/blake/OneDrive/Stryker Project/Country Leaflet Information Dashboard/Building Leaflet Final Interactive Map/Leaflet Map App")

uscities <- read.csv("uscities.csv")
MI <- gtrends("Dominos Pizza" ,geo = "US-MI",time = "all" )$interest_by_city
uscitiesMI <- uscities[uscities$state_id =="MI",]
MI <- data.frame(MI); colnames(MI)[1]<- "city"

uscitiesMI <- uscitiesMI[uscitiesMI$city =="Traverse City",]
MI2 <- MI[MI$location == "Traverse City",]

merge1 <-merge(uscitiesMI,MI,by = c("city"),all.x = TRUE)

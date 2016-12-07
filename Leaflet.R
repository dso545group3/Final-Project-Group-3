library(lubridate)
library(ggplot2)
library(dplyr)

## read data
request<- read.csv("MyLA311_Service_Request_Data_2016.csv")
population <- read.csv("CDpopu&area.csv")
# clean data date
request$CreatedDate <- mdy_hms(request$CreatedDate)
request$UpdatedDate <- mdy_hms(request$UpdatedDate)
request$ServiceDate <- mdy_hms (request$ServiceDate)


# filter date
int <- interval(ymd("2016-08-22"), ymd("2016-11-21"))
request<- filter(request, request$CreatedDate %within% int) ## exclude data out of range
solved <- filter(request, Status == "Closed" & !is.na(ServiceDate)) ## remain closed incidents & incidents with service data
solved <- filter(solved, !is.na(Longitude) & !is.na(Latitude))
solved1 = head(solved,100)
levels(solved$DC)
# calculate solve time
solved$duration <- as.numeric(solved$UpdatedDate-solved$CreatedDate,units="mins")
solved <- filter(solved, duration >20)

solved_new <- merge(solved,population,by.x = "CD", by.y = "District",all.x = T)

###############################################leaflet########################
library(leaflet)
library(raster)
library(RColorBrewer)
library(dplyr)
library(htmltools)
library(rgdal)
# add CD polygon shapefile
cd <- readOGR("l.a. city council district (2012).shp")
lev = levels(cd$name)
lev[c(1,8,9,10,11,12,13,14,15,2,3,4,5,6,7)]
cd$name = factor(cd$name, levels = lev)


# 
#for (i in 0:14){
#  cd@polygons[[1]]@plotOrder=3
# }
# merge cd & request count 
cdcount <- solved%>%
  filter(!is.na(CD))%>%
  group_by(CD)%>%
  summarise(count = n())

cd@data <- merge(cd@data, cdcount,by.x ="name" , by.y="CD",all.x=T,all.y=F)
cd@data$name<- factor(cd@data$name,levels=c(1:15))
cd@data <- cd@data[order(cd@data$name),]
rownames(cd@data)<-c(0:14)

# pop-up
content <- NULL
for (i in c(10:15, 1:9)) {
  content <- append(content, paste(sep = "<br/>",
                                   paste("<b><a>", as.numeric(cd@polygons[[i]]@ID)+1, "</a ></b>"),
                                   paste("<b><a>", as.numeric(cd@polygons[[i]]@ID)+2, "</a ></b>")))
}
# leaflet
leaflet() %>%
  addProviderTiles(provider="CartoDB.Positron")%>%
  #  addTiles(urlTemplate= "http://{s}.latimes.com/quiet-la-0.4.0/{z}/{x}/{y}.png",
  #           attribution="© OpenStreetMap")%>%
  setView(lng=-118.4, lat=34.09, zoom=9)%>%
  addPolygons(data=cd, opacity=0.3,fillOpacity = 0.6,
              stroke=T, weight=1,popup= content,
              color = ~colorNumeric("OrRd",count)(count))
########################################################################


library(leaflet)
library(raster)
library(RColorBrewer)
library(dplyr)
library(htmltools)
library(rgdal)
library(lubridate)

## read data
request<- read.csv("MyLA311_Service_Request_Data_2016.csv")
save(request, file = "FinalData.rda")
load("FinalData.rda")

# clean data date
request$CreatedDate <- mdy_hms(request$CreatedDate)
request$UpdatedDate <- mdy_hms(request$UpdatedDate)
request$ServiceDate <- mdy_hms(request$ServiceDate)

# filter date
int <- interval(ymd("2016-08-22"), ymd("2016-11-21"))
request<- filter(request, request$CreatedDate %within% int) ## exclude data out of range
solved <- filter(request, Status == "Closed" & !is.na(ServiceDate)) ## remain closed incidents & incidents with service data
solved <- filter(solved, !is.na(Longitude) & !is.na(Latitude))

# calculate solve time
solved$duration <- as.numeric(solved$UpdatedDate-solved$CreatedDate, units = "mins") ## duration of service
solved <- filter(solved, duration >20) ## filter duration > 20

# add CD polygon shapefile
cd <- readOGR("cdinfo/l.a. city council district (2012).shp")

# 
#for (i in 0:14){
#  cd@polygons[[1]]@plotOrder=3
# }
# merge cd & request count 
cdcount <- solved %>%
  filter(!is.na(CD)) %>%
  group_by(CD) %>%
  summarise(count = n())

cd@data <- merge(cd@data, cdcount, by.x = "name", by.y = "CD", all.x = T, all.y = F)
cd@data$name <- factor(cd@data$name, levels = c(1:15))
cd@data <- cd@data[order(cd@data$name), ]
rownames(cd@data) <- c(0:14)

# pop-up
content <- NULL
for (i in c(10:15, 1:9)) {
  content <- append(content, paste(sep = "<br/>",
                                   paste("<b><a><font color = 'Grey'>", "CD Number: ", "</font>", as.numeric(cd@polygons[[i]]@ID)+1, "</a></b>")
                                  ))
}


# threshold now: data max volumn -- 65536
## Scale the solved1 dataset if necessary
ThresholdVery = dim(solved)[1]
if (ThresholdVery <= 65536) {
  solved1 = solved
} else {
  solved1 = solved[1:65536, ]
}

solved1$duration = round(solved1$duration)
solved1$durationStr = seconds_to_period(60*solved1$duration)
solved1 = select(solved1, RequestType, Longitude, Latitude, CreatedDate, CD, Address, durationStr)
solved1$CreatedDate = as.factor(solved1$CreatedDate)
solved1$CD = as.factor(solved1$CD)
solved1$Address = as.factor(solved1$Address)
solved1$durationStr = as.character(solved1$durationStr)

# Make a list of icons. We'll index into it based on name.
typeicon <- iconList(
  Graffiti = makeIcon("icon/giraffiti.png"),
  Bulky = makeIcon("icon/bulky.png"),
  Dump = makeIcon("icon/dump.png"),
  Appliance = makeIcon("icon/appliance.png"),
  Ewaste = makeIcon("icon/ewaste.png"),
  Rat = makeIcon("icon/rat.png"),
  Singlelight = makeIcon("icon/singlelight.png"),
  Multilight = makeIcon("icon/multilight.png"),
  Homeless = makeIcon("icon/homeless.png"))

# Some fake data
solved2 <- sp::SpatialPointsDataFrame(
  cbind(
    solved1[,"Longitude"],  # lng
    solved1[,"Latitude"],  # lat
    solved1[,"RequestType"],  # RequestType
    solved1[,"CD"]  # CD
#    solved1[,"Address"],  # Address
#    solved1[,"durationStr"],  # durationStr
#    solved1[,"CreatedDate"]  #CreatedDate
  ),
  data.frame(type = factor(
    ifelse(solved1$RequestType == "Graffiti Removal", "Graffiti", 
           ifelse(solved1$RequestType == "Bulky Items", "Bulky", 
                  ifelse(solved1$RequestType == "Illegal Dumping Pickup", "Dump", 
                         ifelse(solved1$RequestType == "Metal/Household Appliances", "Appliance", 
                                ifelse(solved1$RequestType == "Electronic Waste", "Ewaste", 
                                       ifelse(solved1$RequestType == "Dead Animal Removal", "Rat", 
                                              ifelse(solved1$RequestType == "Single Streetlight Issue", "Singlelight", 
                                                     ifelse(solved1$RequestType == "Multiple Streetlight Issue", "Multilight", "Homeless")))))))),
    c("Graffiti", "Bulky", "Dump", "Appliance", "Ewaste", "Rat", "Singlelight", "Multilight", "Homeless")
  ))
)
# add more features to the solved2 spatial dataset
solved2@data$CD = solved1$CD
solved2@data$Address = solved1$Address
solved2@data$durationStr = solved1$durationStr
solved2@data$CreatedDate = solved1$CreatedDate

## build the html popup for solved2
contentSol <- paste(sep = "<br/>",
                    paste("<b><a><font color = 'Grey'>", "Request Type: ", "</font>", as.character(solved2@data[, 1]), "</a></b>"),
                          paste("<b><a><font color = 'Grey'>", "CD Number: ", "</font>", as.character(solved2@data[, 2]), "</a></b>"),
                                paste("<b><a><font color = 'Grey'>", "Address: ", "</font>", as.character(solved2@data[, 3]), "</a></b>"),
                                      paste("<b><a><font color = 'Grey'>", "Created Date: ", "</font>", as.character(solved2@data[, 5]), "</a></b>"),
                                            paste("<b><a><font color = 'Grey'>", "Processing Time: ", "</font>", as.character(solved2@data[, 4]), "</a></b>"))

# leaflet: draw everything
leaflet(solved2) %>% addTiles() %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  setView(lng = -118.4, lat = 34.09, zoom = 9) %>%
  addMarkers(lng = solved2@coords[, 1], lat = solved2@coords[, 2], icon = ~typeicon[type], 
             clusterOptions = markerClusterOptions(),
             popup = paste(contentSol)) %>%
  addPolygons(data = cd, opacity = 0.3, fillOpacity = 0.6,
              stroke = T, weight = 1, popup = content,
              color =~ colorNumeric("OrRd", count)(count))


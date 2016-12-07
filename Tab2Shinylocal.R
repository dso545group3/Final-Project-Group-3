library(ggplot2)
library(dplyr)
library(lubridate)
library(devtools)
library(grid)
library(curl)
library(rCharts)
install.packages(c('XML', 'reshape2', 'devtools', 'plyr'))
install_github('ramnathv/rCharts@dev')
date1 <- "2016-05-18"
date2 <- "2016-09-13"
districtnum1 <- 1
districtnum2 <- 2
url = NULL
reqdistall <- NULL

for (i in seq(0, 1500000, 50000)){
  options("scipen" = 20)
  url <- append(url,paste("https://data.lacity.org/resource/ndkd-k878.csv?$select=createddate,updateddate,servicedate,closeddate,requesttype,address,cd,longitude,latitude&$order=createddate%20DESC&$where=createddate%3E%20%27",date1,"%27%20AND%20createddate%3C%20%27",date2,"%27%20AND%20cd=",districtnum1,"%20OR%20cd=",districtnum2,"&$limit=50000&$offset=",i,sep = ""))
  a<- length(url)
  requestdist <- read.csv(curl_download(url[a], "requestdis.csv", 
                                        quiet = TRUE, mode = "wb",
                                        handle = new_handle()))
  if (nrow(requestdist)>1){
    reqdistall<- rbind(reqdistall,
                       requestdist)
  }else{
    break
  }
}

population <- read.csv("CDpopu&area.csv")
reqdistall_new <- merge(reqdistall, population, by.x = "CD", by.y = "CD", all.x = T)
reqdistall_new$CreatedDate <- mdy_hms(reqdistall_new$CreatedDate)
reqdistall_new$UpdatedDate <- mdy_hms(reqdistall_new$UpdatedDate)
reqdistall_new$ServiceDate <- mdy_hms (reqdistall_new$ServiceDate)
reqdistall_new$duration <- as.numeric(reqdistall_new$UpdatedDate - reqdistall_new$CreatedDate, units="mins")



data1 = reqdistall_new %>%
  group_by(CD,RequestType) %>%
  summarise(count = n())

data1$CD <- as.factor(data1$CD)
lev = levels(data1$CD)
lev = lev[c(2,1)]
data1$CD = factor(data1$CD, levels = lev)

ggplot(data1, aes(x= RequestType, y = count,label = count, fill = factor(CD))) +
  geom_bar(stat = "identity", position ="dodge") +
  geom_text(position = position_dodge(0.9),size=2,hjust = -0.1) +
  xlab("") +
  ylab("") +
  ggtitle("") +
  scale_fill_manual(values=c("#bdd7e7", "#6baed6"), 
                    name="Council District") +
  guides(fill = guide_legend(reverse = T)) +
  theme_classic() +
  theme(legend.position = "top",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank()
  ) +
  coord_flip()



data2 = reqdistall_new %>%
  group_by(CD,RequestType) %>%
  summarise(avg_duration = sum(duration)/n())
data2$avg_duration = as.integer(data2$avg_duration)

data2$CD <- as.factor(data2$CD)
lev = levels(data2$CD)
lev = lev[c(2,1)]
data2$CD = factor(data2$CD, levels = lev)



ggplot(data2, aes(x= RequestType, y = avg_duration, fill = factor(CD),label = avg_duration 
)) +
  geom_bar(stat = "identity",position = "dodge") +
  geom_text(position = position_dodge(0.9),size=2,hjust = -0.1) +
  xlab("") +
  ylab("") +
  ggtitle("") +
  scale_fill_manual(values=c("#bdd7e7", "#6baed6"), 
                    name="Council District") +
  guides(fill = guide_legend(reverse = T)) +
  theme_classic() +
  theme(legend.position = "top",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank()
  ) +
  coord_flip()



data3 = data1 %>%
  filter(CD == 1)
data4 = data1 %>%
  filter(CD == 2)

library(plotrix)
xx <- data3$count
yy <- data4$count
xylabels <- data3$RequestType
pic1 = pyramid.plot(xx,yy,labels=xylabels,top.labels=c("CD1","Request Type","CD2"),
                     gap=90000,show.values=F)









####################################rCharts################
library(XML)
library(reshape2)
library(rCharts)
library(plyr)
dPyramid <- function(country, year, colors=NULL) {
  dat <- getAgeTable(country, year)
  dat$Male <- -1 * dat$Male
  
  keep <- c("Year", "Age", "Male", "Female", "ord")
  
  dat.sub <- dat[,keep]
  
  dat.melt <- melt(dat.sub, 
                   value.name='Population', 
                   variable.name = 'Gender', 
                   id.vars=c('Age', 'ord', 'Year') )
  
  dat.melt$gencode <- ifelse(dat.melt$Gender == 'Male', 1, 2)
  
  d1 <- dPlot(
    x = "Population", 
    y = "Age", 
    groups = "Gender", 
    data = dat.melt, 
    type = 'bar')
  
  
  d1$yAxis(type = "addCategoryAxis", orderRule = "ord")
  d1$xAxis(type = "addMeasureAxis")
  d1$legend( x = 60, y = 10, width = 700, height = 20, horizontalAlign = "right")
  
  if (!is.null(colors)){
    d1$colorAxis(
      type = "addColorAxis", 
      colorSeries = "gencode", 
      palette = colors
    )
  }
  if (length(year) > 1) {
    d1$set(storyboard = "Year")
    max_x <- round_any(max(dat.melt$Population), 10000, f = ceiling)
    min_x <- round_any(min(dat.melt$Population), 10000, f = floor)
    d1$xAxis(overrideMax = max_x, overrideMin = min_x)
  }
  
  if (max(dat.melt$Population >= 1000000)) {
    d1$setTemplate( afterScript = 
                      "
                    <script>
                    x._getFormat = function () {
                    return function(d) {
                    return d3.format(',.1f')(Math.abs(d) / 1000000) + 'm';
                    };
                    };
                    myChart.draw()
                    </script>
                    ")
  } else {
    d1$setTemplate( afterScript = 
                      "
                    <script>
                    x._getFormat = function () {
                    return function(d) {
                    return d3.format(',.0f')(Math.abs(d) / 1000) + 'k';
                    };
                    };
                    myChart.draw()
                    </script>
                    ")
  }
  
  d1
}






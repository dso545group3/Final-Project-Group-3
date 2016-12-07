library(leaflet)
library(raster)
library(RColorBrewer)
library(dplyr)
library(htmltools)
library(lubridate)
library(stringr)
## read data
request<- read.csv("MyLA311_Service_Request_Data_2016.csv")
save(request, file = "FinalData.rda")
load("FinalData.rda")

population <- read.csv("CDpopu&area.csv")
data <- merge(request, population, by.x = "CD", by.y = "CD")
data$CreatedDate <- mdy_hms(data$CreatedDate)
data$UpdatedDate <- mdy_hms(data$UpdatedDate)
data$ServiceDate <- mdy_hms (data$ServiceDate)
data <- filter(data,CreatedDate >= ymd_hms("2016-01-01 00:00:00"))
data$week=week(data$CreatedDate)
data$duration <- as.numeric(data$UpdatedDate - data$CreatedDate, units="mins")
data$duration = as.integer(data$duration)
data$Density = as.integer(data$Density)
data$CD = !is.na(data$CD)




library(ggplot2)
library(dplyr)

a=data%>%
  filter(MobileOS %in% c("Android","iOS"))%>%
  group_by(MobileOS,as.Date(CreatedDate))%>%
  summarise(count=n())
week(a$CreatedDate)
datebreaks <- seq(ymd("2016-01-01"), ymd("2016-12-03"), by = "1 months")

colnames(a)=c("MobileOS","CreatedDate","count")
ggplot(a,aes(x=CreatedDate,y=count,color=MobileOS))+
  geom_line()+
  scale_x_date(breaks = as.Date(datebreaks))+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  guides(color=guide_legend(reverse=T))+
  theme(axis.title = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0,400),
                        breaks=seq(0,400,50),
                          labels=seq(0,400,50))+
  ggtitle("Mobile Device Comparision")

unique(data$RequestSource)

b=data%>%
  group_by(RequestSource)%>%
  summarise(count=n())%>%
  arrange(-count)%>%
  head(n=5)
ggplot(b,aes(x=reorder(RequestSource,-count),y=count,fill=RequestSource))+
  geom_bar(stat="identity")+
  theme(axis.title = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle("TOP 5 Request Source")

c=data%>%
  group_by(weekdays(CreatedDate),hour(CreatedDate))%>%
  summarise(count=n())
levels(c$`weekdays(CreatedDate)`)
colnames(c)=c("weekday","hour","count")
c$weekday=as.factor(c$weekday)
c$weekday=factor(c$weekday,levels=levels(c$weekday)[c(2,6,7,5,1,3,4)])
ggplot(c,aes(x=weekday,y=factor(hour),fill=count))+
  geom_tile()+
  scale_fill_gradient(low="#f7fbff",high = "#08519c",guide = F)+
  theme(axis.title = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle("Heatmap Of Requests Breakdown By Weekday And Hour")
  
unique(data$RequestType)
datatop3=filter(data,data$RequestType %in% c("Graffiti Removal","Bulky Items","Metal/Household Appliances"))

#bulky
bulky=datatop3%>%filter(RequestType=="Bulky Items")%>%
  group_by(CD,week)%>%
  summarise(count=n())
bulkyav=bulky%>%
  group_by(week)%>%
  summarise(ave=mean(count))
  
ggplot(bulky,aes(x=week,y=count))+
  geom_line()+
  facet_wrap(~CD,nrow=3)+
  geom_line(data=bulkyav,aes(x=week,y=ave),color="red")+
  xlim(1,48)+
  theme(axis.title = element_blank(),
                  panel.background = element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.ticks.y=element_blank())+
  ggtitle("2016 Trend of Request (Bulky Items)")

#gra
gra=datatop3%>%filter(RequestType=="Graffiti Removal")%>%
  group_by(CD,week)%>%
  summarise(count=n())
graver=gra%>%
  group_by(week)%>%
  summarise(ave=mean(count))

ggplot(gra,aes(x=week,y=count))+
  geom_line()+
  facet_wrap(~CD,nrow=3)+
  geom_line(data=graver,aes(x=week,y=ave),color="red")+
  xlim(1,48)+
  theme(axis.title = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle("2016 Trend of Request (Graffiti Removal)")
  
  
#metal
metal=datatop3%>%filter(RequestType=="Metal/Household Appliances")%>%
  group_by(CD,week)%>%
  summarise(count=n())
metalave=metal%>%
  group_by(week)%>%
  summarise(ave=mean(count))

ggplot(metal,aes(x=week,y=count))+
  geom_line()+
  facet_wrap(~CD,nrow=3)+
  geom_line(data=metalave,aes(x=week,y=ave),color="red")+
  xlim(1,48)+
  theme(axis.title = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle("2016 Trend of Request (Metal/Household Appliances)")




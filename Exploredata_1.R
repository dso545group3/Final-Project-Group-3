library(leaflet)
library(raster)
library(RColorBrewer)
library(dplyr)
library(htmltools)
library(rgdal)
library(lubridate)
library(ggplot2)
library(stringr)

load("FinalData.rda")
request$CreatedDate <- mdy_hms(request$CreatedDate)
request$UpdatedDate <- mdy_hms(request$UpdatedDate)
request$CreatedDate = paste(year(request$CreatedDate), 
                                      month(request$CreatedDate), 
                                      day(request$CreatedDate), sep = "-")
request$CreatedDate = ymd(request$CreatedDate)

## Q1: Distribution of requests
## Distribution of Request Type - data processing
request_dist = request %>%
  filter(!is.na(RequestType)) %>%
  group_by(RequestType) %>%
  summarise(count_type = n())

request_dist$colortype = ifelse(request_dist$RequestType %in% c("Graffiti Removal", "Bulky Items"),
                                1, 2)

## Distribution of Request Type - plot
ggplot(data = request_dist, aes(x = reorder(RequestType, count_type), y = count_type, fill = as.factor(colortype))) + 
  geom_bar(stat = "identity") + 
  xlab("Request Type") + 
  ylab("Total Number of Each Request") + 
  ggtitle("Distribution of Request Type") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  guides(fill=FALSE)

## Q2: distribution departments referred to 
unique(request$AssignTo)  ## 86 levels
table(request$AssignTo) ## 方差巨大，1/4值显著
## Distribution of Assigned Department - data processing
request_assign = request %>%
  filter(!is.na(AssignTo)) %>%
  group_by(AssignTo) %>%
  summarise(count_assign = n()) %>%
  filter(count_assign > 5000)

request_assign$colortype = ifelse(request_assign$AssignTo %in% c("SLA", "EV", "NC"),
                                1, 2)

## Distribution of Assigned Department - plot
ggplot(data = request_assign, aes(x = reorder(AssignTo, count_assign), y = count_assign, fill = factor(colortype))) + 
  geom_bar(stat = "identity") + 
  xlab("Department Assigned") + 
  ylab("Total Number of Each assignment") + 
  ggtitle("Distribution of Assigned Department (Over 5000 records)") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  guides(fill=FALSE)
## Comment: Department 有点太多，图不好；

## Q3: App vs Phone call referrals
unique(request$RequestSource)   ## 17 levels
table(request$RequestSource) ## 方差巨大，5个值显著
## Distribution of RequestSource - data processing
request_source = request %>%
  filter(!is.na(RequestSource)) %>%
  group_by(RequestSource) %>%
  summarise(count_source = n())

## Distribution of RequestSource - plot
ggplot(data = request_source, aes(x = reorder(RequestSource, count_source), y = count_source)) + 
  geom_bar(stat = "identity") + 
  xlab("Request Source") + 
  ylab("Total Number of Each Source") + 
  ggtitle("Distribution of Source") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))  

## choose the five significant resource
## service type top 3 questions for each input resource
request_source_sign = request %>%
  filter(RequestSource %in% c("Email", "Self Service", "Mobile App", "Driver Self Report", "Call")) %>%
  group_by(RequestSource, RequestType) %>%
  summarise(count_source_type = n())

## "Call"
request_source_sign1 = request_source_sign %>%
  filter(RequestSource == "Call") %>%
  arrange(-count_source_type) 
request_source_sign1 = request_source_sign1[1:3,]
## "Driver Self Report"
request_source_sign2 = request_source_sign %>%
  filter(RequestSource == "Driver Self Report") %>%
  arrange(-count_source_type) 
request_source_sign2 = request_source_sign2[1:3, ]
## "Mobile App"
request_source_sign3 = request_source_sign %>%
  filter(RequestSource == "Mobile App") %>%
  arrange(-count_source_type) 
request_source_sign3 = request_source_sign3[1:3, ]
## "Self Service"
request_source_sign4 = request_source_sign %>%
  filter(RequestSource == "Self Service") %>%
  arrange(-count_source_type) 
request_source_sign4 = request_source_sign4[1:3, ]
## "Email"
request_source_sign5 = request_source_sign %>%
  filter(RequestSource == "Email") %>%
  arrange(-count_source_type) 
request_source_sign5 = request_source_sign5[1:3, ]
## rbind
request_source_sign_T = rbind(request_source_sign1,
                              request_source_sign2, request_source_sign3,
                              request_source_sign4, request_source_sign5)
## Top 3 Main Request Types of Five Siginicant Sources
ggplot(request_source_sign_T, aes(x = reorder(RequestSource, -count_source_type), 
                                  y = count_source_type, fill = RequestType)) +
  geom_bar(stat = "identity") + 
  xlab("Significant Request Source") + 
  ylab("Total Number of Each Source") + 
  ggtitle("Top 3 Request Types of Significant Sources") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))  


## Q4: Overall trends in calls / apps sums and counts 
## ps: no idea why call and app only
request_call_app = request %>%
  filter(RequestSource %in% c("Call","Mobile App"))
head(request_call_app)

# per week - count, duration
## start date
date1 = min(request_call_app$CreatedDate)
## end date
date2 = max(request_call_app$CreatedDate)
new1 = 8 - wday(date1)
datestart = date1 + days(new1)
new2 = 8 - wday(date2)
dateend = date2 + days(new2) - 1
## weeks that in our calculation
weekcal = (dateend - datestart + 1) / 7
weekcal = as.numeric(weekcal)
weekcal ## for this example, weekcal returns with numerical value 16 

## classification based on week 
request_call_app$weeknum = ceiling(as.numeric((request_call_app$CreatedDate - datestart + 1)/7))

## filter data start from weeknum-1 to weeknum-weekcal
request_call_app = filter(request_call_app, weeknum >= 1, weeknum <= weekcal)
request_call_app$weeknum = factor(request_call_app$weeknum)

## Groupped dataset
request_call_app_group = request_call_app %>%
  group_by(weeknum, RequestSource) %>%
  summarise(count_num = n())

## set datebreak
datebreaks = seq(datestart, dateend, by = "1 week")
datebreaks = as.character(datebreaks)
for(i in 1:weekcal){
  if(ceiling((i-1)/5) == floor((i-1)/5)){
    datebreaks[i] = datebreaks[i]
  }else{
    datebreaks[i] = ""
  }
}

## plot count by weeknum, color by RequestType -- app / calls
ggplot(request_call_app_group, aes(x = weeknum, y = count_num, color = RequestSource, group = RequestSource)) + 
  geom_point(size = 1) + 
  geom_line(size = 1) + 
  scale_x_discrete(labels = datebreaks) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid = element_blank()) + 
  xlab("Week (shown by the first day)") + 
  ylab("Number of Each Request Source") + 
  ggtitle("Overall Weekly Trends in Calls / Apps") 




  
  
  
  
  
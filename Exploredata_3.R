library(ggplot2)
library(lubridate)
library(dplyr)
library(grid)
request = read.csv("MyLA311_Service_Request_Data_2016.csv")
population <- read.csv("CDpopu&area.csv")
solved <- merge(request, population, by.x = "CD", by.y = "CD",all.x = T)
data1$CreatedDate <- mdy_hms(data1$CreatedDate)
data1$UpdatedDate <- mdy_hms(data1$UpdatedDate)
data1$ServiceDate <- mdy_hms (data1$ServiceDate)
data1$duration <- as.numeric(data1$UpdatedDate - data1$CreatedDate, units="mins")
data1$duration = as.integer(data1$duration)
data1$Density = as.integer(data1$Density)
data1$CD = !is.na(data1$CD)
data2 = data1 %>%
  filter(!is.na(CD)) %>%
  group_by(CD) %>%
  summarise(sum = sum(duration))

ggplot(data2, aes(x = reorder(as.factor(CD),sum), y = sum)) +
  geom_bar(stat = "identity")

data3 = data1 %>%
  group_by(RequestType) %>%
  summarise(duration = sum(as.numeric(duration)))

ggplot(data3, aes(x = reorder(RequestType,duration), y = duration)) +
  geom_bar(stat = "identity") +
  coord_flip()

data4 = data1 %>%
  filter(RequestType %in% c("Bulky Items","Graffiti Removal","Metal/Household Appliance")) %>%
  group_by(RequestType) %>%
  summarise(count = n())

ggplot(data1, aes(x = CD, y = RequestType)) +
  geom_bar(stat = "identity")































































































#data1 = reqdistall_new %>%
#  filter(CD == 1 | CD == 2) %>%
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




data2 = data %>%
  filter(CD == 1 | CD == 2) %>%
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











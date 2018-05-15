fordgobike<-read.csv(file = "C:/Users/Tiange/Desktop/fordgobike.csv", header = T,sep=',')

library(grid)
library(ggplot2)
library(sqldf)
library(dplyr)
library(ggmap)

head(fordgobike)
str(fordgobike)
#change seconds to minutes and select duration under or equal to 30 minutes
fordgobike$duration_min<-round(fordgobike$duration_sec/60,2)
fordgobike$duration_sec<-NULL
fordgobike_dur_under30<-sqldf('select*from fordgobike where duration_min <= 30')
boxplot(fordgobike_dur_under30$duration_min)
summary(fordgobike_dur_under30$duration_min)


#abstract week, month and start_hour for later use
fordgobike_dur_under30$week <- weekdays(as.Date(fordgobike_dur_under30$start_time),abbreviate=F)
fordgobike_dur_under30$month<-months(as.Date(fordgobike_dur_under30$start_time),abbreviate=F)
fordgobike_dur_under30$start_hour<-as.integer(substr(fordgobike_dur_under30$start_time,12,13))

#we can divide day into morning, noon etc
fordgobike_dur_under30$period<-cut(fordgobike_dur_under30$start_hour,c(00,06,10,15,19,23)
                                   ,labels=c("else","morning","noon","afternoon","evening"))
ggplot(fordgobike_dur_under30, aes(x=period))+geom_bar()
table(fordgobike_dur_under30$period)

#seperate data into weekdays and weekends
fordgobike_dur_under30_weekdays<-sqldf('select * from fordgobike_dur_under30 
                                       where week not in ("saturday","Sunday")')
fordgobike_dur_under30_weekends<-sqldf('select * from fordgobike_dur_under30 
                                       where week  in ("saturday","Sunday")')

#use "fill= "function to compare the subscriber and customer
ggplot(fordgobike_dur_under30_weekdays,aes(x=period))+
  geom_bar(aes(fill=user_type))+xlab("Different Period Each Day")+ylab("")+
  ggtitle("Weekdays Bike Usage Based On Different Period")+labs(fill="user type")

ggplot(fordgobike_dur_under30_weekends,aes(x=period))+
  geom_bar(aes(fill=user_type))+xlab("Different Period Each Day")+
  ggtitle("Weekends Bike Usage Based On Different Period")+labs(fill="user type")

ggplot(fordgobike_dur_under30_weekdays,aes(x=start_hour))+
  geom_bar(aes(fill=user_type))+xlab("Weekday StartHour")+
  ggtitle("Weekdays Start Hour") 

ggplot(fordgobike_dur_under30_weekends,aes(x=start_hour))+
  geom_bar(aes(fill=user_type))+xlab("Weekday StartHour")+
  ggtitle("Weekends Start Hour") 

#alpha is for adjusting the transparency level
#position="identity" mean overlap the data. we have certain position can be used.
ggplot(fordgobike_dur_under30_weekdays)+
  geom_bar(aes(x=start_hour,fill=user_type,col=user_type),colour = "lightblue",alpha=0.5,position = "identity")+
  scale_fill_manual(values = c("black", "pink"))+xlab("Weekday StartHour")+
  ggtitle("Weekdays Start Hour")


ggplot(fordgobike_dur_under30_weekends)+
  geom_bar(aes(x=start_hour,fill=user_type,col=user_type),colour = "lightblue",alpha=0.5,position = "identity")+scale_fill_manual(values = c("black", "pink"))+xlab("Weekends StartHour")+ylab("")+
  ggtitle("Weekends Start Hour")

#to label percentage on geom_bar
ggplot(fordgobike_dur_under30_weekends %>% count(start_hour, user_type) %>% mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),
       aes(start_hour, n, fill=user_type))+
       geom_bar(stat="identity")+geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")),
                                           position=position_stack(),size=4,vjust=1)

ggplot(fordgobike_dur_under30_weekdays %>% count(start_hour, user_type) %>% mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),
       aes(start_hour, n, fill=user_type))+
       geom_bar(stat="identity")+geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")),
                                           position=position_stack(),size=4,vjust=1)


ggplot(fordgobike_dur_under30_weekends)+
  geom_bar(aes(x=start_hour,fill=user_type,col=user_type),colour = "lightblue",alpha=0.5,position = "identity")+
  scale_fill_manual(values = c("black", "pink"))+xlab("Weekdends StartHour")+ylab("")+
  ggtitle("Weekends Start Hour")



fordgobike_dur_under30$duration_interval<-cut( fordgobike_dur_under30$duration_min, breaks = seq(0,30,5))
ggplot(fordgobike_dur_under30_weekdays,aes(x=start_hour))+
  geom_bar(aes(fill=duration_interval))+xlab("Weekdays_Starthour")+ylab("")+
  ggtitle("Weekdays Start Hour and interval")

ggplot(fordgobike_dur_under30_weekends,aes(x=start_hour))+
  geom_bar(aes(fill=duration_interval))+xlab("Weekends_Starthour")+ylab("")+
  ggtitle("Weekends Start Hour and interval")

ggplot(fordgobike_dur_under30_weekdays)+
  geom_bar(aes(x=duration_interval,fill=user_type,col=user_type),colour = "lightblue",alpha=0.5,position = "identity")+
  scale_fill_manual(values = c("black", "pink"))+xlab("weekdays duration")+
  ggtitle("Weekdays Bike Usage Duration")


ggplot(fordgobike_dur_under30_weekends)+
  geom_bar(aes(x=duration_interval,fill=user_type,col=user_type),colour = "lightblue",alpha=0.5,position = "identity")+
  scale_fill_manual(values = c("black", "pink"))+xlab("weekends duration")+
  ggtitle("Weekends Bike Usage Duration")

#make plot in weekdays order
fordgobike_dur_under30$week <- ordered(fordgobike_dur_under30$week , levels=c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday"))
ggplot(fordgobike_dur_under30,aes(x=week))+geom_bar(aes(fill=duration_interval)) 

#this code is learned from Stackoverflow (https://stackoverflow.com/questions/37817809/r-ggplot-stacked-bar-chart-with-counts-on-y-axis-but-percentage-as-label)
fordgobike_dur_under30$duration_interval<-cut( fordgobike_dur_under30$duration_min, breaks = seq(0,30,5))
ggplot(fordgobike_dur_under30 %>% count(week, duration_interval) %>% mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),aes(week, n, fill=duration_interval))+
  geom_bar(stat="identity")+geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")),
                                      position=position_stack(),size=4,vjust=1)

station_name_paired <-sqldf('select *, count(*) as count_t from fordgobike_dur_under30 where start_station_name != end_station_name 
                            GROUP BY start_station_name, end_station_name order by count_t desc ')

ggplot(station_name_paired, aes(x = start_hour, y = count_t))+
  geom_bar(aes(fill = user_type), stat = "identity",position = "dodge")+
  facet_wrap(~month,scales = "free")


ggplot(station_name_paired, aes(x = start_hour, y = count_t))+
  geom_bar(aes(fill = user_type), stat = "identity",position = position_dodge(0.9))+
  facet_wrap(~week,scales = "free")+xlab("Start Hour")+ggtitle("Start Hour in different weekdays")

#we found that most subscribers riding between 5 to 10 minutes during weekdays.
#The peak hour for weekdays are 8-9 in the morning and 5-6 in the afternoon. There are not so many customers using bicycle during weekdays. 
#Subscribers might be people working in the city and using bicycle for commute purpose. 
#During weekends, both subscribers and customers liek to use bicycles between 11am-4pm. 
#The riding duration during weekends is longer than that during weekdays. Many customers riding around 10-20 minutes.  
#Compared to the usage betwee June to August, the usage between September and December is low.

#a little abit ggmap
pop_startlocation<-sqldf('select start_station_latitude, start_station_longitude from station_name_paired')
SanFran<-get_map(location = "San Francisco",source="google",maptype = "terrain",zoom = 11)

ggmap(SanFran)+geom_point(data=pop_startlocation,aes(x=start_station_longitude,y=start_station_latitude))



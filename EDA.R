#Histogram of duration 
trainData %>% 
  ggplot(aes(x=trip_duration)) + 
  geom_histogram(bins=1000, fill="blue")+
  theme_bw()+theme(axis.title = element_text(size=12),axis.text = element_text(size=12))+
  ylab("Density")+coord_cartesian(x=c(0,9000))

summary(trainData$trip_duration)
skewness(trainData$trip_duration)

summary(trainData[trainData$trip_duration<30,"cdistance"])
a=trainData[trainData$trip_duration<30,"cdistance"]
length(a[a>1000])


#Histogram of distance
trainData %>% 
  ggplot(aes(x=cdistance)) + 
  geom_histogram(bins=100, fill="blue")+
  ylab("Density")

summary(trainData$cdistance)
summary(trainData[trainData$cdistance==0,11])

#Barplot fomr vendor 
ggplot(trainData, aes(x=factor(trainData$vendor_id)))+
  geom_bar(width=0.7, fill="blue")+
  theme_minimal()


#Barplot fomr stre_and_fwd_flag 
ggplot(trainData, aes(x=factor(trainData$store_and_fwd_flag)))+
  geom_bar(width=0.7, fill="blue")+
  theme_minimal()+
  labs(x = 'store_and_fwd_fag', y = 'count', title = 'Store and fwd flag')

#Barplot for passenger count 
ggplot(trainData, aes(x=factor(trainData$passenger_count)))+
  geom_bar(width=0.7, fill="blue")+
  theme_minimal()+
  labs(x = 'Passenger_count', y = 'count', title = 'Passenger Count')


#No of Steps analysis
summary(trainData$number_of_steps)

#Barplot for day of week 
ggplot(trainData, aes(x=factor(trainData$day)))+
  geom_bar(width=0.7, fill="blue")+
  theme_minimal()+
  labs(x = 'Day Of Week', y = 'count', title = 'Count')


#Barplot for day of month 
ggplot(trainData, aes(x=factor(trainData$p_day)))+
  geom_bar(width=0.7, fill="blue")+
  theme_minimal()+
  labs(x = 'Day of month', y = 'count', title = 'Count')



#Median Trip duration by day 

ggplot(aes(x =day,y = trip_duration), data = trainData,) +
  stat_summary(fun.y = "median", geom = "bar")+
  labs(x = 'Day', y = 'Median Trip Duration', title = 'Median Trip duration by day of Week')


#Median Trip duration by hour of the day

ggplot(aes(x =p_hour,y = trip_duration), data = trainData,) +
  stat_summary(fun.y = "median", geom = "bar")+
  labs(x = 'Hour', y = 'Median Trip Duration', title = 'Median Trip duration by hour of day')


#Median Trip duration by hour of the day

ggplot(aes(x =hourCat,y = trip_duration), data = trainData,) +
  stat_summary(fun.y = "median", geom = "bar")+
  labs(x = 'Time of Day', y = 'Median Trip Duration', title = 'Median Trip duration by time of Day')


#Median distance by hour of the day

ggplot(aes(x =hourCat,y = cdistance), data = trainData,) +
  stat_summary(fun.y = "median", geom = "bar")+
  labs(x = 'Time of Day', y = 'Median Trip Distance', title = 'Median Trip distance by time of Day')


#Median duration by vendor

ggplot(aes(x =vendor_id,y = trip_duration), data = trainData,) +
  stat_summary(fun.y = "median", geom = "bar")+
  labs(x = 'Vendor Type', y = 'Median Trip Duration', title = 'Median Trip duration by vendor type')


#Median duration by weekend

ggplot(aes(x =weekend,y = trip_duration), data = trainData,) +
  stat_summary(fun.y = "median", geom = "bar")+
  labs(x = 'Weekend', y = 'Median Trip Duration', title = 'Median Trip duration by weekend')


#Median duration by day of montnh 

ggplot(aes(x =p_day,y = trip_duration), data = trainData,) +
  stat_summary(fun.y = "median", geom = "bar")+
  labs(x = 'day of Month', y = 'Median Trip Duration', title = 'Median Trip duration by day of month')


#Median duration by season 

ggplot(aes(x =season,y = trip_duration), data = trainData,) +
  stat_summary(fun.y = "median", geom = "bar")+
  labs(x = 'season of Year', y = 'Median Trip Duration', title = 'Median Trip duration by season of year')


#Median duration by passenger count 

ggplot(aes(x =passenger_count,y = trip_duration), data = trainData,) +
  stat_summary(fun.y = "median", geom = "bar")+
  labs(x = 'passenger count', y = 'Median Trip Duration', title = 'Median Trip duration by passenger count')



#Median duration by store_and_fwd_flag

ggplot(aes(x =trainData$store_and_fwd_flag,y = trip_duration), data = trainData,) +
  stat_summary(fun.y = "median", geom = "bar")+
  labs(x = 'Store_and_fwd_flag', y = 'Median Trip Duration', title = 'Median Trip duration by store_and_fwd_flag')



# Scatterplot distance and duration 
ggplot(trainData, aes(x =trip_duration , y = number_of_steps)) +
  geom_point(size = 5, alpha = 0.6) +
  labs(title = "distance vs duration") +
  ylab("Number of Streets") + xlab("duration in Seconds")

cor(trainData$trip_duration,trainData$number_of_steps)

# Scatterplot no of streets and duration 
ggplot(trainData, aes(x =trip_duration , y = number_of_steps)) +
  geom_point(size = 5, alpha = 0.6) +
  labs(title = "Number of streets vs duration") +
  ylab("Number of Streets") + xlab("duration in Seconds")

cor(trainData$trip_duration,trainData$cdistance)


# Scatterplot no of streets and duration 
ggplot(trainData, aes(x =cdistance , y = number_of_steps)) +
  geom_point(size = 5, alpha = 0.6) +
  labs(title = "Distance vs Number of Streets") +
  ylab("Number of Streets") + xlab("Distance")

cor(trainData$cdistance,trainData$number_of_steps)


#Median duration by time of hour
trainData %>%
  ggplot(aes(x = trainData$hourCat, y = median(trainData$trip_duration))) +
  geom_bar(stat = 'identity', fill = 'blue') +
  labs(x = 'Time of Day', y = 'Median Trip Duration', title = 'Median Trip duration by time of Day')


#Median distance by time of hour
trainData %>%
  ggplot(aes(x = trainData$hourCat, y = median(trainData$cdistance))) +
  geom_bar(stat = 'identity', fill = 'blue') +
  labs(x = 'Time of Day', y = 'Median Distance', title = 'Median Trip distance by time of Day')

#Median duration by seasons
trainData %>%
  ggplot(aes(x = trainData$season, y = median(trainData$trip_duration))) +
  geom_bar(stat = 'identity', fill = 'blue') +
  labs(x = 'Season', y = 'Median Duration', title = 'Median Trip duration season')


#Median duration by month 
trainData %>%
  ggplot(aes(x = trainData$p_month, y = median(trainData$trip_duration))) +
  geom_bar(stat = 'identity', fill = 'blue') +
  labs(x = 'Season', y = 'Median Duration', title = 'Median Trip duration season')



#Median duration by passanger count
trainData %>%
  ggplot(aes(x = trainData$passenger_count, y = median(trainData$trip_duration))) +
  geom_bar(stat = 'identity', fill = 'blue') +
  labs(x = 'passenger count', y = 'Median Trip Duration', title = 'Median Trip duration by passenger count')







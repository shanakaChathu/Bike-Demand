tod=function()
{
  len=length(train$p_hour)
  for(i in 1:len)
  {
    if(train$p_hour[i]>=1 && train$p_hour[i]<=6)
    {
      train$hour[i]="Morning"
    }
    else if (train$p_hour[i]>=7 && train$p_hour[i]<=16)
    {
      train$hour[i]="Office"
    }
    else 
    {
      train$hour[i]="Night"
    }
    
  }
}

tod()








library(geosphere)

# create distance matrix
mat=distm(list1[,c(train$pickup_longitude,train$pickup_latitude)], list2[,c(train$dropoff_longitude,train$dropoff_latitude)], fun=distVincentyEllipsoid)

# assign the name to the point in list1 based on shortest distance in the matrix
list1$locality= list2$locality[max.col(-mat)]

head(train)



distance=distHaversine(trainOri[,c(6,7)], trainOri[,c(8,9)])
distance
trainOri$cdistance=distance 


str(trainOri)


?distHaversine
head(train)


ggplot(data = train, mapping = aes(x=train$pickup_longitude, y=train$pickup_latitude))+
  geom_jitter(size=0.6) +
  labs(x = "longitudes", y = "latitudes")



----
  
  #Observing  new york coordinates and placing a limit 
min_lat <- 40.6
max_lat <- 40.9
min_long <- -74.05
max_long <- -73.7

ggplot(train, aes(x=train$pickup_longitude, y=train$pickup_latitude)) +
  geom_jitter(size=0.6) +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat))
#Now it shows all the pickups points with their latitudes and longitudes
m_dist <- mahalanobis(df2, colMeans(df2), cov(df2))
df$MD <- round(m_dist, 1)


ggplot(training, aes(x = training$trip_duration, y = training$cdistance)) +
  geom_point(size = 5, alpha = 0.6) +
  labs(title = "Weight vs Height",
       subtitle = "Outlier Detection in ") 

 



m_dist = mahalanobis(training[,c(11,12)], colMeans(training[,c(11,12)]), cov(training[,c(11,12)]))
a= round(m_dist, 1)

summary(a)
hist(a)






ggplot(data=trainData, aes(x=season, y=median(trip_duration), group=as.factor(vendor_id))) +
  geom_line(linetype="dashed", color="blue", size=1.2)+
  geom_point(color="red", size=3)





library(dplyr)
td = trainData %>% group_by(passenger_count) %>% summarise(me = mean(trip_duration))
library(ggplot2)
ggplot(aes(x = passanger_cout, y = me), data = td) + geom_bar(stat = "identity")


ggplot(aes(x = passenger_count, y = trip_duration), data = trainData,) +
  stat_summary(fun.y = "median", geom = "bar")+
  labs(x = 'passenger count', y = 'Median Trip Duration', title = 'Median Trip duration by passenger count')




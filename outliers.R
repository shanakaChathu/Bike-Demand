
# Calculate Mahalanobis Distance with distance and trip_duration distributions
m_dist = mahalanobis(training[,c(11,12)], colMeans(training[,c(11,12)]), cov(training[,c(11,12)]))
out= round(m_dist, 2)
dist=out
summary(out)
training$dist=dist

# Mahalanobis Outliers - Threshold set to 2
training$out = "No"
training$out[training$dist > 1]= "Yes"

#Hist of mahalanobis distance 
training %>% 
  ggplot(aes(x=dist)) + 
  geom_histogram(bins=1000, fill="blue")+
  theme_bw()+theme(axis.title = element_text(size=12),axis.text = element_text(size=12))+
  ylab("Density")+coord_cartesian(x=c(0,10000))

skewness(training$dist)

# Scatterplot with Maha Outliers
ggplot(training, aes(x =trip_duration , y = cdistance)) +
  geom_point(size = 5, alpha = 0.6) +
  labs(title = "distance vs duration") +
  ylab("distance in meters") + xlab("duration in Seconds")

ggplot(trainData, aes(trip_duration, cdistance))+
  geom_point(size = 2)+
 geom_point(aes(colour = factor(out)), size = 2)
 
#Removing outliers from the dataset 
trainData=training[training$out=='No',]
save(trainData,file="trainData.RData")

# Scatterplot with Maha Outliers
ggplot(trainData, aes(x =trip_duration , y = cdistance)) +
  labs(title = "distance vs duration") +
  ylab("distance in meters") + xlab("duration in Seconds")



load("trainData.RData")


#1.Vendor with trip_duration
v1=trainData[trainData$vendor_id==1,'trip_duration']
v2=trainData[trainData$vendor_id==2,'trip_duration']

#Variance test 
var.test(v1,v2)

#2-Sample T test 
t.test(v1,v2, var.equal=TRUE, paired=FALSE)

#Visualize

library("ggpubr")
ggboxplot(trainData, x =as.factor(trainData$vendor_id)  , y = trainData$trip_duration, 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "Weight", xlab = "Groups")

--------------------------------------


#2.Seasons with trip_duration
  
s1=trainData[trainData$season=='Winter','trip_duration']
s2=trainData[trainData$season=='Spring','trip_duration']

#Variance test 
var.test(s1,s2)

#2-Sample T test 
t.test(s1,s2, var.equal=FALSE, paired=FALSE)

#Visualize

library("ggpubr")
ggboxplot(trainData, x =as.factor(trainData$vendor_id)  , y = trainData$trip_duration, 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "Weight", xlab = "Groups")

---------------------------
  
#3.Weekday/end with trip_duration
w1=trainData[(trainData$dow==1 | trainData$dow==7),'trip_duration']
w2=trainData[(trainData$dow>=2 & trainData$dow<=6),'trip_duration']


#Variance test 
var.test(w1,w2)

#2-Sample T test 
t.test(w1,w2, var.equal=FALSE, paired=FALSE)

#Visualize

library("ggpubr")
ggboxplot(trainData, x =trainData$vendor_id,y = trainData$trip_duration, 
          color = trainData$vendor_id,
          ylab = "trip_duration", xlab = "vendor Type")

ggboxplot(trainData, x ="vendor_id" , y = "trip_duration", outline=FALSE,
          color = "vendor_id",
          ylab = "Trip_Duration", xlab = "Vendor Type")


--------------------------------
  
#4.Time of Day with trip_duration
  
res.aov=aov(trip_duration ~ hourCat, data = trainData)
# Summary of the analysis
summary(res.aov)


-----------------------------
#5. Passenger count with trip_duration

res.aov=aov(trip_duration ~ passenger_count, data = trainData)
# Summary of the analysis
summary(res.aov)

------------------------
#6. Distance with trip_duration 
 
cor.test(trainData$trip_duration, trainData$cdistance, method = "pearson")

------------------------------
  
#6.  number of streets and trip_duration 
  
cor.test(trainData$trip_duration, trainData$number_of_steps, method = "pearson")

#7. Day of week 

res.aov=aov(trip_duration ~ day, data = trainData)
# Summary of the analysis
summary(res.aov)


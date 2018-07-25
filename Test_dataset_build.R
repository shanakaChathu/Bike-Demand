load("data.RData")
head(model1)

#Calculate distane from pickup and dropoff
head(test)
distance=distHaversine(test[,c(5,6)], test[,c(7,8)])
distance
test$cdistance=distance

#Preprocessing of the variables 
date=substr(test$pickup_datetime,1,10)
time=substr(test$pickup_datetime,12,19)
test$date=date
test$time=time
year=substr(date,1,4)
month=substr(date,6,7)
day=substr(date,9,10)
hour=substr(time,1,2)
minutes=substr(time,4,5)
seconds=substr(time,7,8)

test$p_year=year
test$p_month=month
test$p_day=day
test$p_hour=hour
test$p_minutes=minutes
test$p_seconds=seconds

test$date=as.Date(test$date)
test$p_hour=as.numeric(test$p_hour)
test$passenger_count=as.character(test$passenger_count)

#Identify the day of week using date 
library(lubridate)
dow=wday(ymd_hms(test$pickup_datetime))
test$dow=dow
test$day=weekdays(test$date)


#Weekend
test$weekend=ifelse(test$dow==1 | test$dow==7,"Week-end","not-Weekend")


#Hour of Day
test$hourCat=ifelse(test$p_hour>=1 & test$p_hour<=6 ,"Morning","Evening")
test[test$hourCat=='Evening',]$hourCat=
  ifelse(test[test$hourCat=='Evening',]$p_hour>=7 &
           test[test$hourCat=='Evening',]$p_hour<=17 ,"Office","Night")


#Seasons 
test$p_month=as.numeric(test$p_month)
test$season=ifelse(test$p_month>=1 & test$p_hour<=3 ,"Winter","Spring")


save(test,file="test.RData")
load("modelT.RData")

modelT=data.frame(test$cdistance,test$number_of_steps,test$p_hour,test$weekend,test$hourCat)
colnames(modelT)=c("cdistance" ,"number_of_steps","p_hour","weekend","hourCat")
modelT$weekend=as.character(modelT$weekend)
modelT$hourCat=as.character(modelT$hourCat)
str(modelT)


------------------------------------------
  
#Redying for xgboost 
setDT(modelT)
table(is.na(modelT))
new_ts1 = model.matrix(~.+0,data = modelT[,with=F])
dtest = xgb.DMatrix(data = new_ts1)

p_test = predict (xgb1,dtest)
p_test=ifelse(p_test<0,0,p_test)

save(modelT,file="modelT.Rdata")

#Write prediction into csv file
testPrediction=data.frame(test$id,p_test)
write.csv(testPrediction,file="testPrediction.csv")



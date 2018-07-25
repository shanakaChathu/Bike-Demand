#Loading R objects 
load("data.RData")
load("train.RData")
load("training.RData")
load("trainData.RData")
#Importing library
library(data.table)
library(pacman)
library(geosphere)
library(lubridate)
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(caret)
library(corrplot)
library(leaflet)
library(stringr)
library(rgdal)
library(plyr)
library(car)
library(e1071) 
install.packages("devtools")


#Loading Data 
trainOri=read.csv("train.csv")
testOri=read.csv("test.csv")
fastR1=read.csv("fastest_routes_train_part_1.csv")
fastR2=read.csv("fastest_routes_train_part_2.csv")
#fastR3=read.csv("second_fastest_routes_train.csv")
fastT=read.csv("fastest_routes_test.csv")

#External data 
fastR=rbind(fastR1,fastR2)

#Merging 2 datasets
train=left_join(trainOri,fastR,by="id")
test=left_join(tesstOri,fastT,by="id")

#Saving all
save(trainOri,testOri,fastR1,fastR2,fastR3,fastR,fastT,train,test, file='data.RData')

#Checking missing values 
sum(is.na(train)) #11
sum(is.na(test))  #0

#Remove missing values 
train=train[complete.cases(train), ]

#Calculate distane from pickup and dropoff
head(train)
distance=distHaversine(train[,c(6,7)], train[,c(8,9)])
distance
train$cdistance=distance

save(train,file="train.RData")

#Extracting required variables 
trainNew=train[,c(1,2,3,4,5,6,7,8,9,10,11,23,12,13,16)]

#Preprocessing of the variables 
date=substr(trainNew$pickup_datetime,1,10)
time=substr(trainNew$pickup_datetime,12,19)
trainNew$date=date
trainNew$time=time
year=substr(date,1,4)
month=substr(date,6,7)
day=substr(date,9,10)
hour=substr(time,1,2)
minutes=substr(time,4,5)
seconds=substr(time,7,8)

trainNew$p_year=year
trainNew$p_month=month
trainNew$p_day=day
trainNew$p_hour=hour
trainNew$p_minutes=minutes
trainNew$p_seconds=seconds

head(train)
training=trainNew[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21)]

#Convert variable types 
training[,16]=as.Date(training[,16])
training$p_hour=as.numeric(training$p_hour)
training$passenger_count=as.character(training$passenger_count)


#Identify the day of week using date 
library(lubridate)
dow=wday(ymd_hms(training$pickup_datetime))
training$dow=dow
training$day=weekdays(training$date)

#Weekend
training$weekend=ifelse(training$dow==1 | training$dow==7,"Week-end","not-Weekend")
trainData$weekend=ifelse(trainData$dow==1 | trainData$dow==7,"Week-end","not-Weekend")


#Save train R object 
save(training,file="training.RData")
save(trainData,file="trainData.RData")

#Analyze individual avariables 


str(trainData)
#Hourofday

trainData$hourCat=ifelse(trainData$p_hour>=1 & trainData$p_hour<=6 ,"Morning","Evening")
trainData[trainData$hourCat=='Evening',]$hourCat=
ifelse(trainData[trainData$hourCat=='Evening',]$p_hour>=7 &
       trainData[trainData$hourCat=='Evening',]$p_hour<=17 ,"Office","Night")

head(trainData)
save(trainData,file='trainData.RData')


#Seasons 
trainData$p_month=as.numeric(trainData$p_month)
trainData$season=ifelse(trainData$p_month>=1 & trainData$p_hour<=3 ,"Winter","Spring")


#Calculate distince 

distance=distHaversine(train[,c(5,6)], train[,c(7,8)])
train$distance=distance
save(train,file="trainv2.RData")







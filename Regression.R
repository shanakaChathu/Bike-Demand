load("trainData.RData")
load("model1.RData")

head(trainData)
#Extracting required variables 
model1=trainData[,c(2,5,10,12,15,20,22,23,26,27,11)]
head(model1)

#Save
save(model1,file="model1.RData")
load("model1.RData")
#Checking for missing values 
sum(is.na(model1)) 

#Outlier Detection 
str(model1)

#Divide dataset into train and test set 
ind=sample(2,nrow(model1),replace=T,prob=c(0.7,0.3))
model1.train=model1[ind==1,]
model1.test=model1[ind==2,]


#Fit liner regression basic momdel 
fit = lm(trip_duration~ cdistance+number_of_steps+season+weekend+vendor_id+hourCat+p_hour
         +day+as.numeric(passenger_count), data=model1.train)
p=predict(fit,model1.test)
p=ifelse(p<0,0,p)
a=model1.test$trip_duration
rmse=sqrt(sum((log(p+1)-log(a+1))^2)/length(p))
rmse


#Fit decision tree model

library(caret)
library(rpart)
fit = rpart(trip_duration~ ., data=model1.train
            ,control=rpart.control(minsplit=10))
p=predict(fit,model1.test)
p=ifelse(p<0,0,p)
print(fit$cptable)

a=model1.test$trip_duration
rmse=sqrt(sum((log(p+1)-log(a+1))^2)/length(p))
rmse


#Fit random forest model 
library(randomForest)
fit = randomForest(trip_duration~ cdistance+number_of_steps+season+weekend+vendor_id, data=model1.train
            ,ntree=100)
p=predict(fit,model1.test)

print(fit$cptable)

a=model1.test$trip_duration
rmse=sqrt(sum((log(p+1)-log(a+1))^2)/length(p))
rmse

head(model1)


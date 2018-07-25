#load libraries
library(data.table)
library(mlr)
library(xgboost)

#Loading Data 
load("trainData.RData")
load("model1.RData")

model1=model1[,c(4,5,6,8,9,11)]


ind=sample(2,nrow(model1),replace=T,prob=c(0.8,0.2))
model1.train=model1[ind==1,]
model1.test=model1[ind==2,]

#convert data frame to data table
setDT(model1.train) 
setDT(model1.test)

#check missing values 
table(is.na(model1.train))
table(is.na(model1.test))


#using one hot encoding 
tr_label=model1.train$trip_duration
ts_label=model1.test$trip_duration

new_tr = model.matrix(~.+0,data = model1.train[,-c("trip_duration"),with=F]) 
new_ts = model.matrix(~.+0,data = model1.test[,-c("trip_duration"),with=F])


#preparing matrix 
dtrain = xgb.DMatrix(data = new_tr,label = tr_label) 
dtest = xgb.DMatrix(data = new_ts,label=ts_label)


#default parameters
params = list(objective = "reg:linear",
              eval_metric = "rmse", 
              booster = "gbtree",
              eta = 0.1, #learning rate
              gamma = 1, #minimum loss reduction to split
              colsample_bytree = 0.7, #variables per tree 
              subsample = 0.8, #data subset per tree 
              min_child_weight=2,
              max_depth = 6, #tree levels
              seed = 171221)

   
cv.res = xgb.cv(params=params, data=dtrain, nfold=5, early_stopping_rounds=5, nrounds=200,
                showsd=T,stratified = T,maximize = F)
cv.res

library(dplyr)
cv.res$evaluation_log %>%
  select(iter, train_rmse_mean, test_rmse_mean) %>%
  melt(id.vars="iter", measure.vars=c("train_rmse_mean", "test_rmse_mean")) %>%
  ggplot() +
  geom_line(aes(x=iter, y=value, group=variable, colour=variable)) + 
  scale_y_log10() + 
  labs(title="rmse_mean of training & test set")


# model training
xgb1= xgb.train(params = params, data = dtrain, nrounds = 100,
                watchlist = list(val=dtest,train=dtrain),maximize = F)

save(xgb1,file="xgb1.RData")
#model prediction
p = predict (xgb1,dtest)
p=ifelse(p<0,0,p)


#Evaluating using test set
a=model1.test$trip_duration
rmse=sqrt(sum((log(p+1)-log(a+1))^2)/length(p))
rmse

#view variable importance plot
mat=xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20]) 


#----------------------------------------
  
#Parameter tuning using caret 

ControlParamteres =trainControl(method = "cv",
                                  number = 5,
                                  savePredictions = TRUE,
                                  classProbs = FALSE
)


parametersGrid =  expand.grid(eta = c(0.1,0.5,1), 
                              colsample_bytree=c(0.5,0.7),
                              max_depth=c(3,6),
                              nrounds=100,
                              gamma=c(0.1,0.5,1,1.5),
                              min_child_weight=2,
                              subsample=0.8
                              )

tunedf=data.frame(new_tr,tr_label)
library(caret)
modelxgboost = train(tr_label~., 
                      data = tunedf,
                      method = "xgbTree",
                      trControl = ControlParamteres,
                      tuneGrid=parametersGrid)







## start loading train and test data
Add_Count_Feature<-function(train_df,group_by,aggregate_col,New_Feature_Name,aggregate_type,filter_clause)
{
  sqlstmt<-paste("select",group_by,",",aggregate_type,"(",aggregate_col,")",New_Feature_Name,"from",train_df,filter_clause,"group by",group_by,sep = " ")  
  tempdf<-sqldf(sqlstmt)
  eval(parse(text=paste(train_df,'$',New_Feature_Name,'<-NULL',sep="")))
  bigmartdata<-merge(bigmartdata,tempdf,by=group_by,all.x = T)
  eval(parse(text="bigmartdata[,c(New_Feature_Name)]<-as.numeric(bigmartdata[,c(New_Feature_Name)])"))
  return(bigmartdata)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

swfun <- function(x) {
  switch(x,
         'LF'='Low Fat'
         ,'low fat'='Low Fat'
         ,'reg'='Regular'
         ,x
  )
}
bigmartdatatrain<-read.csv('/home/mithilesh/Documents/MK/Data Science/Analytics Vidhya/Big Mart Sales/Train_UWu5bXk.csv',header=TRUE,sep = ',')
bigmartdatatest<-read.csv('/home/mithilesh/Documents/MK/Data Science/Analytics Vidhya/Big Mart Sales/Test_u94Q5KV.csv',header=TRUE,sep = ',')
### end loading train and test data
bigmartdatatest$Item_Outlet_Sales<--99999
bigmartdata<-rbind(bigmartdatatrain,bigmartdatatest)

library(corrplot)
#library(missForest)

summary(bigmartdata)

# start data prep
#install.packages("mice")
library(mice)
#start Item_Weight imputation
library(sqldf)

x<-sqldf("select Item_Identifier,avg(Item_Weight) Item_Weight_Mean from bigmartdata group by Item_Identifier")
bigmartdata <- merge(bigmartdata, x, by = "Item_Identifier", all.x = TRUE)
bigmartdata<-transform(bigmartdata, Item_Weight = ifelse(is.na(bigmartdata$Item_Weight), 
                                                         bigmartdata$Item_Weight_Mean,bigmartdata$Item_Weight))
bigmartdata$Item_Weight_Mean<-NULL
#sql1<-("update bigmartdata set Item_Weight = (select Item_Weight from x where Item_Identifier=bigmartdata.Item_Identifier )
#where Item_Weight is null")
#end Item_Weight imputation
summary(bigmartdata$Outlet_Size)
#start Outlet_Size imputation
bigmartdata$Outlet_Size[bigmartdata$Outlet_Size==""]<-NA
x<-aggregate(bigmartdata$Outlet_Size~bigmartdata$Outlet_Type,bigmartdata[!is.na(bigmartdata$Outlet_Size),],Mode)
colnames(x)<-c("Outlet_Type","Outlet_Size_Mode")
#x<-sqldf("select Outlet_Type,Outlet_Size,count(Outlet_Size) Outlet_Size_Mode from bigmartdata group by Outlet_Type,Outlet_Size")
bigmartdata <- merge(bigmartdata, x, by = "Outlet_Type", all.x = TRUE)
bigmartdata<-transform(bigmartdata, Outlet_Size = (ifelse(is.na(bigmartdata$Outlet_Size), 
                                                          as.factor(bigmartdata$Outlet_Size_Mode),bigmartdata$Outlet_Size)))
bigmartdata$Outlet_Size<-as.factor(bigmartdata$Outlet_Size)
bigmartdata$Outlet_Size_Mode<-NULL
#end Outlet_Size imputation

#start exploring Outlet_Type to group Type2 & Type3
summary(bigmartdata$Item_Outlet_Sales)
aggregate(Item_Outlet_Sales~Outlet_Type,bigmartdata[bigmartdata$Item_Outlet_Sales!=-99999.00,],mean)
#since both types have different sales mean hence cannot be grouped as difference is significant
#end exploring Outlet_Type to group Type2 & Type3

#start Item_Visibility imputation

  # x<-sqldf("select Item_Identifier,Outlet_Type,avg(Item_Visibility) Item_Visibility_Mean from bigmartdata group by Item_Identifier,Outlet_Type")
  # bigmartdata <- merge(bigmartdata, x, by = c("Item_Identifier","Outlet_Type"), all.x = TRUE)
  # bigmartdata<-transform(bigmartdata, Item_Visibility = ifelse(bigmartdata$Item_Visibility==0, 
  #                                                              bigmartdata$Item_Visibility_Mean,bigmartdata$Item_Visibility))
  # bigmartdata$Item_Visibility_Mean<-NULL
  # summary(bigmartdata$Item_Visibility)
#start for those items having no corresponding Outlet_Type for same item in another store
x<-sqldf("select Item_Identifier,avg(Item_Visibility) Item_Visibility_Mean from bigmartdata group by Item_Identifier")
bigmartdata <- merge(bigmartdata, x, by = c("Item_Identifier"), all.x = TRUE)
bigmartdata<-transform(bigmartdata, Item_Visibility = ifelse(bigmartdata$Item_Visibility==0, 
                                                             bigmartdata$Item_Visibility_Mean,bigmartdata$Item_Visibility))
bigmartdata$Item_Visibility_Mean<-NULL

#end

#sql1<-("update bigmartdata set Item_Weight = (select Item_Weight from x where Item_Identifier=bigmartdata.Item_Identifier )
#where Item_Weight is null")
#end Item_Visibility imputation

#start Outlet years of operation
bigmartdata$Outlet_Years<-(2013-bigmartdata$Outlet_Establishment_Year)
#end Outlet years of operation

#start Broad category of item
x<-as.factor(substr(bigmartdata$Item_Identifier,1,2))
bigmartdata$Item_Category<-x
summary(bigmartdata$Item_Category)
#end Broad category of item

#start Modify Fat Content
x<-sapply(as.character(bigmartdata$Item_Fat_Content),swfun)
bigmartdata$Item_Fat_Content<-as.factor(x)
summary(bigmartdata$Item_Fat_Content)
bigmartdata$Item_Fat_Content<- as.factor(ifelse(bigmartdata$Item_Category=='NC','Non Edible',bigmartdata$Item_Fat_Content))
#end Modify Fat Content

levels(bigmartdata$Outlet_Size)<-c(levels(bigmartdata$Outlet_Size),'A','B')
bigmartdata$Outlet_Size[bigmartdata$Outlet_Location_Type=='Tier 1']<-'A'
bigmartdata$Outlet_Size[bigmartdata$Outlet_Location_Type=='Tier 2']<-'A'
bigmartdata$Outlet_Size[bigmartdata$Outlet_Location_Type=='Tier 3']<-'B'
summary(bigmartdatatrain$Outlet_Size)

# start feature engineering
names(bigmartdata)

table(bigmartdata$Item_Type)
#plot(bigmartdata$Item_Type,bigmartdata$Item_Weight)
bigmartdata$Vis_fat<-as.factor(ifelse(bigmartdata$Item_Weight>12 & bigmartdata$Item_Visibility<0.12
                    & bigmartdata$Outlet_Location_Type %in% c('Tier 2','Tier 3')
                    ,1,0))

  # bigmartdata$Outlet_ItemCat_Years<-as.factor(ifelse(bigmartdata$Outlet_Years>15
  #                                 & bigmartdata$Item_Category %in% c('FD')
  #                                 ,1,0))
#bigmartdata$Vis_fat<-(ifelse(bigmartdata$Item_Weight>12 & bigmartdata$Item_Visibility<0.12
#                                      & bigmartdata$Outlet_Location_Type %in% c('Tier 2','Tier 3'),(bigmartdata$Item_Visibility**0.0005)*(bigmartdata$Item_Weight**0.0005),0))
str(bigmartdata)
# table(bigmartdata$Item_Cat)
# 
# sum(bigmartdata$Vis_fat!=0)
# sum(bigmartdata$Outlet_ItemCat_Years!=0)
# plot(bigmartdatatrain$Outlet_ItemCat_Years,bigmartdatatrain$Item_Outlet_Sales)
#bigmartdata$breakfast_sub<-as.factor(ifelse(bigmartdata$Item_Type %in% c('Dairy','Breads','Breakfast','Snack Foods'),1,0))
#table(bigmartdata$breakfast_sub)

#start Add count features
bigmartdata<-Add_Count_Feature('bigmartdata','Outlet_Identifier','Outlet_Identifier','Out_Identifier_Count','count','')
#bigmartdata<-Add_Count_Feature('bigmartdata','Outlet_Type','Outlet_Type','Out_Type_Count','count','')
bigmartdata<-Add_Count_Feature('bigmartdata','Outlet_Identifier','Item_Outlet_Sales','Out_Identifier_Sales_Mean','avg','where Item_Outlet_Sales <>-99999')
bigmartdata<-Add_Count_Feature('bigmartdata','Outlet_Identifier','Item_Outlet_Sales','Out_Identifier_Sales_Min','min','where Item_Outlet_Sales <>-99999')
bigmartdata<-Add_Count_Feature('bigmartdata','Outlet_Identifier','Item_Outlet_Sales','Out_Identifier_Sales_Max','max','where Item_Outlet_Sales <>-99999')
bigmartdata$Out_Identifier_Sales_Range<-(bigmartdata$Out_Identifier_Sales_Mean)/(bigmartdata$Out_Identifier_Sales_Max-bigmartdata$Out_Identifier_Sales_Min)
bigmartdata$Out_Identifier_Sales_Min<-NULL
bigmartdata$Out_Identifier_Sales_Max<-NULL
bigmartdata$Item_vis_cat<-ifelse(bigmartdata$Item_Visibility>=0.07,1,0)

  # bigmartdata$flag_high<-ifelse(bigmartdata$Item_Outlet_Sales>bigmartdata$Out_Identifier_Sales_Mean,1,0)
  # item_high_prop1<-sqldf("select Item_Identifier,avg(flag_high) item_high_prop from bigmartdata group by Item_Identifier")
  # bigmartdata<-merge(bigmartdata,item_high_prop1,by=c("Item_Identifier"),all.x = T)  
  # bigmartdata$flag_high<-NULL

#bigmartdata$item_high_prop.y<-NULL

#bigmartdatatrain$Item_Sold<-round(bigmartdatatrain$Item_Outlet_Sales/bigmartdatatrain$Item_MRP,digits = 0)
summary(bigmartdata$Item_Visibility)
str(bigmartdata)
#end Add count features

#end feature engineering

#start split back train & test data
bigmartdatatrain<-bigmartdata[bigmartdata$Item_Outlet_Sales!=-99999,]
bigmartdatatest<-bigmartdata[bigmartdata$Item_Outlet_Sales==-99999,]
bigmartdatatest$Item_Outlet_Sales<- NULL
bigmartdatatest$Out_Identifier_Count[is.na(bigmartdatatest$Out_Identifier_Count)]<-0

#end split back train & test data
#plot(bigmartdatatrain$Item_MRP,bigmartdatatrain$Item_Outlet_Sales)
# end data prep

# start to check correlation among independent variables 
library(corrplot)
library(car)
cor1<-cor(bigmartdata[sapply(bigmartdatatrain, is.numeric)])
corrplot(cor1)
# start model training
library(caret)
library(e1071)
#install.packages("Metrics")
library(Metrics)
#start linear regression
bmart_model<-lm(Item_Outlet_Sales~Item_MRP+Outlet_Type+Outlet_Identifier#+Item_Fat_Content
                +Vis_fat+Out_Identifier_Count+Out_Identifier_Sales_Mean
                #+Out_Identifier_Sales_Min+Out_Identifier_Sales_Max
                +Out_Identifier_Sales_Range
                ,data=bigmartdatatrain)
#plot(bmart_model)
summary(bmart_model)
#rmse(bigmartdatatrain$Item_Outlet_Sales, exp(bmart_model$fitted.values))
#end linear regression

traincontrol <- trainControl(method = "repeatedcv", number = 5, repeats = 5)
gbm_model <- train(Item_Outlet_Sales~Item_MRP+Outlet_Type+Outlet_Identifier+Vis_fat
              , data = bigmartdatatrain[,], method = "gbm", trControl = traincontrol,verbose = TRUE,metric='RMSE')

rf_model <- train(Item_Outlet_Sales~Item_MRP+Outlet_Type+Outlet_Identifier+Vis_fat+Out_Identifier_Count+Out_Type_Count
              , data = bigmartdatatrain[,], method = "rf", trControl = traincontrol,verbose = TRUE,metric='RMSE')

svmtune <- tune.svm(Item_Outlet_Sales ~Item_MRP+Outlet_Type+Outlet_Identifier#+Item_Fat_Content
                    +Vis_fat+Out_Identifier_Count+Out_Identifier_Sales_Mean
                    +Out_Identifier_Sales_Min+Out_Identifier_Sales_Max+Out_Identifier_Sales_Range,
                    data = bigmartdatatrain,gamma = 2^c(-3,-2,-1,1)
                    , cost = 8 ,epsilon = seq(0.1,0.4,0.1)
                    )

svm_model<-svm(formula = Item_Outlet_Sales ~ Item_MRP+Outlet_Type+Outlet_Identifier#+Item_Fat_Content
               +Vis_fat+Out_Identifier_Count+Out_Identifier_Sales_Mean
               #+Out_Identifier_Sales_Min+Out_Identifier_Sales_Max
               +Out_Identifier_Sales_Range
               #+Outlet_ItemCat_Years
               ,data = bigmartdatatrain,metric='RMSE',kernel='radial'
               , cost = 8,epsilon = 0.20
               ,gamma=0.50)
#c=0.5,gam=0.125,eps=0.2
summary(gbm_model)
summary(svm_model)
plot(svm_model)
svm_model$residuals
summary(svm_model1)
#bigmartdatatest$log.Item_MRP<-log(bigmartdatatest$Item_MRP)
svm_model1<-svmtune$best.model
predsvm <- predict(svm_model, bigmartdatatest[,c(1,3,8,14,15,16,17,18,19)])
pred <- predict(svm_model, bigmartdatatrain[,c(1,3,8,15,16,17,18)])

predgbm <- predict(gbm_model, bigmartdatatest[,c(7,8,2,14,15)])
predrf<-predict(rf_model, bigmartdatatest[,c(7,8,2,10,15)])
predlm <- predict(bmart_model, bigmartdatatest[,c(7,8,2,4)])
bigmartdatatest$Item_Outlet_Sales<- predsvm
rmse(pred,bigmartdatatrain$Item_Outlet_Sales)
#start deep learning
#install.packages("h2o",dependencies = TRUE)
#install.packages("openssl")
library("h2o")
#start initialize h2o server
h2o.shutdown()
h2o.init(nthreads=-1, max_mem_size="2G")
#end initialize h2o server
predictors<-c("Item_MRP","Outlet_Type","Outlet_Identifier","Vis_fat","Outlet_ItemCat_Years","Item_Visibility","Item_Fat_Content","Out_Identifier_Count","Out_Identifier_Sales_Mean","Out_Identifier_Sales_Min","Out_Identifier_Sales_Max","item_high_prop")
response<-"Item_Outlet_Sales"
#bigmartdatatrain$Item_Outlet_Sales<-log(bigmartdatatrain$Item_Outlet_Sales)

deepdata<-as.h2o(bigmartdatatrain)
 splits <- h2o.splitFrame(deepdata, c(0.6,0.2), seed=1234)
 train  <- h2o.assign(splits[[1]], "train.hex") # 60%
 valid  <- h2o.assign(splits[[2]], "valid.hex") # 20%
 test   <- h2o.assign(splits[[3]], "test.hex")  # 20%
deepdatatest<-as.h2o(bigmartdatatest)
nrow(deepdata)
m2 <- h2o.deeplearning(
  model_id="deep_model_first", 
  training_frame=deepdata, 
  nfolds=6,
  #validation_frame=valid,   ## validation dataset: used for scoring and early stopping
  x=predictors,
  y=response,
  #activation="Rectifier",  ## default
  #hidden=c(200,200),       ## default: 2 hidden layers with 200 neurons each
  hidden=c(200,200),                  ## small network, runs faster
  epochs=100,                      ## hopefully converges earlier...
  #score_validation_samples=10000,      ## sample the validation dataset (faster)
  #stopping_rounds=2,
  stopping_metric="MSE",
  rate=0.005,
  rate_annealing=2e-6,            
  momentum_start=0.2,             ## manually tuned momentum
  momentum_stable=0.4, 
  momentum_ramp=1e7, 
  adaptive_rate = F,
  #l1=1,
  activation="RectifierWithDropout",
  variable_importances=T,    ## not enabled by default
  seed=1122
  #,input_dropout_ratio = 0.2 # % of inputs dropout
  ,hidden_dropout_ratios = c(0.2,0.2) # % for nodes dropout
  ,max_w2 = 10
  ,l1=1e-4
  ,rho = 0.95
  ,epsilon = 1e-6
  #,variable_importances = T
)

#start h2o.gbm
h2ogbm <- h2o.gbm(
  ## standard model parameters
  x = predictors, 
  y = response, 
  training_frame = deepdata, 
  #validation_frame = valid,
  nfolds=10,
  ## smaller learning rate is better (this is a good value for most datasets, but see below for annealing)
  learn_rate=0.005,                                                         
  ntrees = 1500,
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MSE", 
  ## sample 80% of rows per tree
  sample_rate = 0.75,                                                       
  ## sample 80% of columns per split
  col_sample_rate = 0.70,                                                   
  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                                 
)
#end h2o.gbm

h2o.performance(m2)
h2o.performance(m2,newdata = valid)
summary(m2)
h2o.performance(h2ogbm)
summary(h2ogbm)
plot(h2ogbm)
sqrt(1061192)
h2o.saveModel(object = m1)
deepdatatest$Item_Outlet_Sales<-NULL
bigmartdatatest$Item_Outlet_Sales<-NULL
preddeep<-h2o.predict(m2,test)
preddeep<-h2o.predict(m2,deepdatatest)
preddeep<-h2o.predict(h2ogbm,deepdatatest)
deepdatatest$Item_Outlet_Sales<-preddeep
deepdatatest1<-as.data.frame(deepdatatest)
deepdatatest1$Item_Outlet_Sales<-(2*deepdatatest1$Item_Outlet_Sales+predsvm)/3
#end deep learning
rmse(preddeep,test$Item_Outlet_Sales)

# start predicting for test data
write.csv(deepdatatest1[,c(1,2,21)],file = "/home/mithilesh/Documents/MK/Data Science/Analytics Vidhya/Big Mart Sales/sample_submission.csv")
write.csv(bigmartdatatest[,c(1,2,20)],file = "/home/mithilesh/Documents/MK/Data Science/Analytics Vidhya/Big Mart Sales/sample_submission.csv")
# end predicting for test data

#xgb(nround=600,maxdepth=5,eta=0.005)-1165
#svm(cost=2,gamma=0.25,eps=0.2)-1147.95
#svm(cost=2,gamma=0.25,eps=0.2,log(Item_MRP))-1149
#svm(cost=2,gamma=0.25,eps=0.2,,log(Item_Outlet_Sales),log(Item_MRP))-1153

#start xgb
library(xgboost)

print(cv)
cv<-xgb.cv(data=data.matrix(bigmartdatatrain[,c(1,3,8,15,16,17,18,19,20)])
           ,label = data.matrix(bigmartdatatrain$Item_Outlet_Sales)
           ,nrounds = 500,eta=0.005,nfold = 8,objective="reg:linear"
           ,metrics = 'rmse')

#score 1169.
bst <- xgboost(data = data.matrix(bigmartdatatrain[,c(1,3,8,15,16,17,18,19,20)])
               ,label = data.matrix(bigmartdatatrain$Item_Outlet_Sales)
               , max.depth = 8, eta = 0.005
               ,nround =500 , objective = "reg:linear")
xgb.importance(setdiff(colnames(bigmartdatatrain),c("Item_Outlet_Sales")),model = bst)

predxgb <- predict(bst, data.matrix(bigmartdatatest[,c(1,3,8,14,15,16,17,18,19)]))
bigmartdatatest$Item_Outlet_Sales<- predxgb

#end xgb
h2o.grid()
# svm(c=8,gamma=0.4,eps=0.2,Vis_fat) test score-1146.89
# svm(c=8,gamma=0.5,eps=0.2,Vis_fat) test score-1146.00
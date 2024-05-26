library(caret)
library(tidyverse)

#Classification And Regression Tree => caret

#Simple ML pipeline 
# 1. split data => train test split
# 2. train model 
# 3. score model aka . prediction
# 4. evaluate model

full_df <- mtcars %>%
  select(mpg,hp,wt,am)
#check NA
full_df  %>%
  complete.cases() %>%
  mean()
# drop roll missing
clean_df<-full_df %>%
  drop_na()
# split data 80% train, 20% test
split_data<-function(df) {
  set.seed(43)
  n<- nrow(df)
  train_id<- sample(1:n,size =0.8*n)
  train_df<- df[train_id, ]
  test_df<-df[-train_id, ]
  return(list(training = train_df,
              testing = test_df))
}




set.seed(43)
n<- nrow(clean_df)
train_id<- sample(1:n,size =0.8*n)
train_df<- clean_df[train_id, ]
test_df<-clean_df[-train_id, ]


prep_data<-split_data(clean_df)
train_df<- prep_data[[1]]
test_df<- prep_data[[2]]
#2.train model
set.seed(43)
lm_model<- train(mpg ~ . ,
                 data=train_df,
                 method="lm")

#3.score model
p<-predict(lm_model, newdata = test_df) 

#4. evaluate model
mae<-mean(abs(p-test_df$mpg))

rmae<-sqrt(mean((p-test_df$mpg)**2))



library(tidyverse)
library(caret)
library(mlbench)

data("BostonHousing")

view(BostonHousing)
fd<-BostonHousing
mean(complete.cases(fd))

#1. train test split 
split_data<-function(fd,train_size=0.8){
  set.seed(52)
  n<-nrow(fd)
  id<-sample(1:n,size=n*train_size)
  train_df<-fd[id, ]
  test_df<-fd[-id, ]
  list(train=train_df,test=test_df)
}
prep_data2<-split_data(fd)
train_data<-prep_data2[[1]]
test_data<-prep_data2[[2]]


#2. train model
set.seed(1)
model<-train(medv ~ rm + b+ crim,
             data = train_data,
             method="lm",
             trControl=ctrl)
#3.score/predict 
p<-predict(model,newdata = test_data)

#4.evaluate 
cal_mae<-function(actual_pred){
  error<-actual-pred
  mean(abs(error))
}
cal_mse<-function(actual_pred){
  error<-actual-pred
  mean(error**2)
}
cal_rmse<-function(actual_pred){
  error<-actual-pred
  sqrt(mean(error**2))
}
##save_model.RDS
saveRDS(model,"lm_model.RDS")
mae<-mean(abs(p-test_data$medv))
rmae<-sqrt(mean((p-test_data$medv)**2))


# train control
#change resampling technique
set.seed(43)
ctrl<-trainControl(
  method="cv",##สามารถเปลี่ยนตรงนี้เป็น boot LOOCV ได้
  number= 5,
  verboseIter = TRUE
)

##variable importance
varImp(model)
##add preProcessdata
## hyperparameter tuning in train process
model <- train(medv ~ rm + b +crim+lstat+age,
               data= train_data,
               method = "knn",
               preProcess = c("range","zv","nzv"),
               tuneLength = 5,
               trControl = ctrl)




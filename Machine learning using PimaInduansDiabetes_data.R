library(mlbench)
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)

data("PimaIndiansDiabetes")
df<-PimaIndiansDiabetes
## spilt data
n<- nrow(df)
id<-sample(n,size=0.8*n)
train_df<-df[id, ]
test_df<-df[-id, ]
## train
set.seed(42)
ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE)
my_grid<-expand.grid(alpha = 0:1,
                     lambda = seq(0.0005,0.05,length = 20))
glmet_model <- train(diabetes ~ .,
                   data = train_df,
                   method = "glmnet",
                   tuneGrid= my_grid,
                   metric = "Accuracy",
                   trControl = ctrl)
##score
P<-predict(rf_model,newdata = test_df)
##evaluate model
confusionMatrix(P,test_df$diabetes,
                positive = "pos",
                mode = "prec_recall")
install.packages("rpart")

##decistion tree 
tree_model<-train(diabetes ~ .,
                  data = train_df,
                  method = "rpart",
                  #complexity parameter
                  #high cp => good generalization
                  tuneGrid = expand.grid(cp = c(0.02,0.1,0.25)),
                  trControl = ctrl)

rpart.plot(tree_model$finalModel)
#ramdon forest model
#mtry hyperparameter
rf_model <- train(diabetes ~ .,
                  data = train_df,
                  method = "rf",
                  metric = "Accuracy",
                  tuneLength = 10,
                  trControl = ctrl)
##resample() => compare model performance
##predict diabetes
model1<-train(diabetes ~ .,
              data = train_df,
              method = "glm",
              trControl = trainControl(
                method = "cv",number = 5)
                  )
model2<-train(diabetes ~ .,
              data = train_df,
              method = "rf",
              trControl = trainControl(
                method = "cv",number = 5)
)
model3<-train(diabetes ~ .,
               data = train_df,
               method = "rpart",
               trControl = trainControl(
                 method = "cv",number = 5)
)
model4<-train(diabetes ~ .,
               data = train_df,
               method = "glmnet",
               trControl = trainControl(
                 method = "cv",number = 5)
)

## resample
list_model = list(
  logistic = model1,
  tree =model3,
  ramdonForest = model2,
  glmnet = model4,
  nnet = model5
)
Result <-resamples(list_model)

summary(Result)

model5<-train(diabetes ~ .,
              data = train_df,
              method = "nnet",
              trControl = trainControl(
                method = "cv",number = 5)
)





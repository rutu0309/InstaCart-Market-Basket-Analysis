install.packages("ROCR")
library(xgboost)
library(dplyr)
library(caret)
library(pROC)
library(ROCR)
load("/Users/rutu/R/Instacart/train2.RData")
load("/Users/rutu/R/Instacart/test2.RData")

train2 <- train
test2 <- test

train2[is.na(train2$reordered),]$reordered <- 0
test2[is.na(test2$reordered),]$reordered <- 0

train_shuffled2 <- train2[sample(nrow(train2)),]

#train_shuffled$X = NULL
train_shuffled2$sumdays.y <- NULL
test2$sumdays.y <- NULL


train_x2 = train_shuffled2[,-c(1,2,3,4,8,9,10)]
test_x2 <- test2[,-c(1,2,3,4,8,9,10)]

train_x2 = as.matrix(train_x2)
test_x2 <- as.matrix(test_x2)
train_y2 = as.vector(train_shuffled2$reordered)
test_y2 = as.vector(test2$reordered)

class(test_x2)
class(train_x2)
class(train_y2)
class(test_y2)


# transform to sparse matrix
train_x2 = xgb.DMatrix(data = train_x2, label = train_y2)
test_x2 <- xgb.DMatrix(data = test_x2, label = test_y2)

# same parameters setting as before (tunned)
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=0.85, 
               colsample_bytree=1)

xgbcv <- xgb.cv( params = params, 
                 data = train_x2, 
                 nrounds = 250, nfold = 5, showsd = T, 
                 stratified = T, print_every_n = 5, 
                 early_stop_round = 20, maximize = F,
                 metrics ='auc', verbose = T, prediction = T)


xg_model_new = xgb.train(params = params,
                         data = train_x2,
                         nrounds = 250,
                         verbose = 1,
                         print_every_n = 5,
                         save_name = "xgboost_new.model")



#Train fitting
fitted_values_new <- predict(xg_model_new, train_x2)

#Predicted Values for training DF
#Taking thhreshold at 0.35
predicted_values_new = ifelse(fitted_values_new >= 0.35, 1, 0)

#Threshold = 0.5
predicted_values_new1 = ifelse(fitted_values_new >= 0.5, 1, 0)

#Threshold = 0.18
predicted_values_new2 = ifelse(fitted_values_new >= 0.18, 1, 0)

#creating auc curve for fitting 
plot(pROC::roc(response = train_y2,
               predictor = predicted_values_new,
               levels=c(0, 1)),lwd=1.5) 


#Training ROC 
xgb.ROC <- roc(predictor=predicted_values_new,
                response=train_y2,
                levels=levels(factor(train_y2)))

auc_35 <- xgb.ROC$auc

plot(xgb.ROC, main= "XGBOOST AUC- Threshhold= 0.35")

xgb.ROC1 <- roc(predictor=predicted_values_new1,
               response=train_y2,
               levels=levels(factor(train_y2)))

auc_05 <- xgb.ROC1$auc
plot(xgb.ROC1, main = "XGBOOST AUC- Threshold =0.5")

xgb.ROC2 <- roc(predictor=predicted_values_new2,
                response=train_y2,
                levels=levels(factor(train_y2)))

auc_18 <- xgb.ROC2$auc
plot(xgb.ROC2, main = "XGBOOST AUC- Threshold =0.18")

coords(xgb.ROC,"best")


#confusion Matrix
confusionMatrix(factor(predicted_values_new),factor(train_y2))


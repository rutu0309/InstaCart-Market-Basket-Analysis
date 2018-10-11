library(xgboost)
library(dplyr)
# install.packages("DiagrammeR")
#install.packages("caret")
#install.packages("pROC")
library(DiagrammeR)
library(caret)

#Importing data 
train_df <- read.csv('train_df.csv')
test_df <- read.csv('test_df.csv')
train_test <- read.csv('train_test.csv')


# Removing index from imported data
train_df$X1 <- NULL
test_df$X1 <- NULL
train_test$X1 <- NULL


# shuffle the data
train_df[is.na(train_df$reordered),]$reordered <- 0
test_df[is.na(test_df$reordered),]$reordered <- 0

train_shuffled <- train_df[sample(nrow(train_df)),]

train_x <- train_shuffled

# initialize data
## exclude label column, eval_test, user_id, order_id
train_x = train_shuffled[,-c(2,4,5,6)]
test_x <- test_df[,-c(2,4,5,6)]
train_x = data.matrix(train_x)
train_y = as.vector(train_shuffled$reordered)
test_x <- data.matrix(test_x)
test_y <- as.vector(test_df$reordered)

test_y
# transform to sparse matrix
train_x = xgb.DMatrix(data = train_x, label = train_y)

test_x <- xgb.DMatrix(data = test_x, label= test_y)
test_y <- as.numeric(test_y)

class(test_x)
class(train_x)
class(train_y)
class(test_y)

dim(train_x)
dim(test_x)

# modeling trial with xgb.train (advanced training)
params_lst <- list(booster = "gbtree", objective = "binary:logistic", 
                   eta=0.1, gamma=10, max_depth=15)

watchlist <- list(train=train_x, test=test_x )

xg_model1 = xgb.train(params = params_lst,
                      data = train_x,
                      watchlist = watchlist,
                      nrounds = 100,
                      save_name = "xgboost.model1")

#Chhecking fitting
fitted_values <- predict(xg_model1, train_x)
fitted_values
#Predicted Values for training DF
#Taking thhreshold at 0.35
predicted_values = ifelse(fitted_values >= 0.35, 1, 0)

#Accuracy: Train
sum(predicted_values == train_y)
train_accuracy <- sum(predicted_values == train_y) / length(predicted_values)

#confusion Matrix
confusionMatrix(factor(predicted_values),factor(train_y))


# #Perform the prediction on test data 
# pred <- predict(xg_model1, test_x)
# pred <- ifelse(pred >= 0.35, 1, 0)
# 
# 
# #Accuracy: Test
# sum(pred == test_y) / length(pred)



#Tunning xgboost parameters
# cross validation to find the best nround
#Tunning 1st: Taking deault parameters
params1 <- list(booster = "gbtree", objective = "binary:logistic", 
                eta=0.1, gamma=0, max_depth=6, 
                min_child_weight=1, subsample=1, 
                colsample_bytree=1)

xgbcv1 <- xgb.cv( params = params1, 
                  data = train_x, 
                  nrounds = 250, nfold = 5, showsd = T, 
                  stratified = T, print_every_n = 5, 
                  early_stop_round = 20, maximize = F)

best_nround <- which.min(xgbcv1$evaluation_log$test_error_mean)
best_nrounf_val <- min(xgbcv1$evaluation_log$test_error_mean)

# train a default model
xg_model_d = xgb.train(params = params,
                       data = train_x,
                       nrounds = 250,
                       verbose = 1,
                       save_name = "xgboost_default.model")

#Tunning other parameters manually 
##Tunning min-child-weight

## min-child-weight = 1
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=1, subsample=1, 
               colsample_bytree=1)

xgbcv_1 <- xgb.cv( params = params, 
                   data = train_x, 
                   nrounds = 200, nfold = 5, showsd = T, 
                   stratified = T, print_every_n = 5, 
                   early_stop_round = 20, maximize = F,
                   metrics = 'auc', verbose = T)
# #Plotting te auc 
# library(pROC)
# plot(pROC::roc(response = train_y ,
#                predictor = xgbcv_1$pred,
#                levels=c(0, 1)),
#      lwd=1.5) 

## min-child-weight = 2 skipping it (no major change)

## min-child-weight = 3
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=1, 
               colsample_bytree=1)

xgbcv_3 <- xgb.cv( params = params, 
                   data = train_x, 
                   nrounds = 250, nfold = 5, showsd = T, 
                   stratified = T, print_every_n = 5, 
                   early_stop_round = 20, maximize = F,
                   metrics = 'auc', verbose = T)

xg_model_3 = xgb.train(params = params,
                       data = train_x,
                       nrounds = 250,
                       verbose = 2,
                       save_name = "xgboost_3.model")

fitted_values_3 <- predict(xg_model_3, train_x)

#Predicted Values for training DF
#Taking thhreshold at 0.35
predicted_values_3 = ifelse(fitted_values_3 >= 0.35, 1, 0)


#creating auc curve for fitting 
plot(pROC::roc(response = train_y,
               predictor = predicted_values_3,
               levels=c(0, 1)),lwd=1.5) 

#confusion Matrix
confusionMatrix(factor(predicted_values_3),factor(train_y))



# decision: min_child_weight = 3, nrounds = 250
# decision: colsample = 1


importance_table = xgb.importance(feature_names = colnames(train_shuffled[,-c(2,4,5,6)]), model = xg_model_3)
xgb.plot.importance(importance_table)


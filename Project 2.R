install.packages("readr","glmnet","neuralnet","caret","randomForest")
library(readr)
library(glmnet)
library(neuralnet)
library(caret)
library(randomForest)

set.seed(123)

#import dataset
test_df<- read_csv("Data2020testX.csv")
train_df <- read_csv("Data2020.csv")

alpha_grid <- 10^seq(10, -2, length = 100)

#MODELS
model_linear <- train(Y ~ ., data = train_df, method = "lm")
model_ridge1 <- train(Y ~ ., data = train_df, method = "glmnet",
                      trControl = trainControl(method = "cv"),
                      tuneGrid = expand.grid(alpha = alpha_grid, lambda = 0))
model_ridge2 <- train(Y ~ ., data = train_df, method = "glmnet",
                      trControl = trainControl(method = "cv"),
                      tuneGrid = expand.grid(alpha = model_ridge1$bestTune))
model_lasso1<-train(Y ~ ., data = train_df,method = "glmnet",
                    trControl = trainControl(method = "cv"),
                    tuneGrid = expand.grid(alpha = alpha_grid,lambda=0))
model_lasso2 <- train(Y ~ ., data = train_df,method = "glmnet"
                      ,tuneGrid = expand.grid(alpha = model_lasso1$bestTune))
model_rf <- randomForest(Y ~ ., data = train_df)
model_tree <- train(Y ~ ., data =train_df, method = "rpart")
model_pcr <- train(Y ~ ., data = train_df, method = "pcr")
nn_model<-neuralnet(Y ~ .,data = train_df,hidden = c(10, 5),
                         act.fct = "logistic",linear.output = FALSE)

#predictions
predictions_lm<-predict(model_linear,newdata = test_df)
predictions_lasso1<-predict(model_lasso1,newdata = test_df)
predictions_lasso2<-predict(model_lasso2,newdata = test_df)
predictions_ridge1<-predict(model_ridge1,newdata = test_df)
predictions_ridge2<-predict(model_ridge2,newdata = test_df)
predictions_rf<-predict(model_rf,newdata = test_df)
predictions_tree<-predict(model_tree,newdata = test_df)
predictions_pcr<-predict(model_pcr,newdata = test_df)
predictions_nn<-predict(model_nn,newdata = test_df)

final_prediction<-data.frame(predictions_rf)
final_prediction
write.table(final_prediction,file="Final.csv",sep=",",row.names=F,col=F)

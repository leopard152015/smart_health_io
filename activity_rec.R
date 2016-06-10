


.activityPrediction <- function(model){
  prediction <- predict(model, newdata=test_data)
  cm <- confusionMatrix(prediction, test_data$Activity)
  print(cm)  
}

.activityRF <- function() {
  fitControl <- trainControl(method="cv", number=5)
  set.seed(123)
  tstart <- Sys.time()
  forest_full <- train(Activity~., data=train_data,
                        method="rf", do.trace=10, ntree=100,
                        trControl = fitControl)
  tend <- Sys.time()
  print(tend-tstart)

## predict and control Accuracy
  .activityPrediction(forest_full)
#  prediction <- predict(forest_full, newdata=test_data)
#  cm <- confusionMatrix(prediction, test_data$Activity)
#  print(cm)
  return (forest_full)
}

.activitySVM <- function() {
  fitControl <- trainControl(method="cv", number=5)
  tstart <- Sys.time()
  svm_full <- train(Activity~., data=train_data,
                     method="svmRadial",
                     trControl = fitControl)
  tend <- Sys.time()
  print(tend-tstart)

## predict and control Accuracy
  .activityPrediction(svm_full)
#  prediction <- predict(svm_full, newdata=test_data)
#  cm <- confusionMatrix(prediction, test_data$Activity)
#  print(cm)
}
#  get important variables from random forest 
.activityIMP <- function(model){
  plot(varImp(forest_full),20, scales=list(cex=1.1))

#  variable extraction 
  imp <- varImp(model)[[1]]
  imp_vars <- rownames(imp)[order(imp$Overall, decreasing=TRUE)]
  vars <- imp_vars[1:490] # % features
  return(vars)
}
# check important variables from random forest on SVM
.activitySVMimp <- function(vars){
  fitControl <- trainControl(method="cv", number=5)
  tstart <- Sys.time()
  svm_imp <- train(Activity~., data=train_data[,c("Activity", vars)],
                 method="svmRadial",
                 trControl = fitControl)
  tend <- Sys.time()
  print(tend-tstart)
  
  .activityPrediction(svm_imp)
#prediction <- predict(svm_imp, newdata=test_data)
#cm <- confusionMatrix(prediction, test_data$Activity)
#print(cm)
}
# get important variables from Information gain 
.activityIGR <- function(){
  inf_g <- information.gain(Activity~., train_data)
  inf_gain <- cbind.data.frame(new_names, inf_g, stringsAsFactors=F)
  names(inf_gain) <- c("vars", "ratio")
  row.names(inf_gain) <- NULL
# arrange by ratio descending and plot top-20 variables
  inf_gain <- inf_gain[order(inf_gain$ratio, decreasing=TRUE),]
  dotplot(factor(vars, levels=rev(inf_gain[1:20,1])) ~ ratio, 
        data=inf_gain[1:20,],
        scales=list(cex=1.1))

  inf_gain[10,1]
# [1] "tBodyAccmadX"
  plot(train_data[,inf_gain[10,1]], ylab=inf_gain[10,1],
  #   pch=20, col=train_data[,1], main="IGR = 0.87")
  pch=20, col="blue", main="IGR = 0.87")
  legend("topright", pch=20, col=grey, #col=activity_names[,1],
       legend=activity_names[,2], cex=0.8)

  inf_gain[551,1]
# [1] "tBodyAccJerkMagarCoeff4"
  plot(train_data[,inf_gain[551,1]], ylab=inf_gain[551,1],
  #   pch=20, col=train_data[,1], main="IGR = 0.03")
  pch=20, col=blue, main="IGR = 0.03")
  legend("topright", pch=20, col="grey",#activity_names[,1],
       legend=activity_names[,2], cex=0.8)

# select variables (igr cutoff)  
  vars <- inf_gain$vars[1:547]
  return(vars)
}
# check igr impritant variables on SVM 
.activitySVMigr <- function(vars){
# for parallel processing
library(doMC) # don't use for Windows
registerDoMC(cores=3) # don't use for Windows

  fitControl <- trainControl(method="cv", number=5, allowParallel = TRUE)
  tstart <- Sys.time()
  svm_igr <- train(Activity~., data=train_data[,c("Activity", vars)],
                 method="svmRadial",
                 trControl = fitControl)
  tend <- Sys.time()
  print(tend-tstart)
  
  .activityPrediction(swm_igr)
}  
#prediction <- predict(svm_igr, newdata=test_data)
#cm <- confusionMatrix(prediction, test_data$Activity)
#print(cm)

## Random Forest ################################
.activityRFigr <- function(vars){
  vars <- inf_gain$vars[1:526] # Accuracy = 0.9243
  fitControl <- trainControl(method="cv", number=5)
  set.seed(123)
  tstart <- Sys.time()
  forest_igr <- train(Activity~., data=train_data[,c("Activity", vars)],
                    method="rf", do.trace=10, ntree=100,
                    trControl = fitControl)
  tend <- Sys.time()
  print(tend-tstart)

## predict and control Accuracy
  .activityPrediction(forect_igr)
}
#prediction <- predict(forest_igr, newdata=test_data)
#cm <- confusionMatrix(prediction, test_data$Activity)
#print(cm)

.activityPCA <-function(){
  pca_mod <- preProcess(train_data[,-1],
                      method="pca",
                      thresh = 0.95)

  summary(pca_mod)

  pca_train_data <- predict(pca_mod, newdata=train_data[,-1])
  dim(pca_train_data)

  pca_train_data$Activity <- train_data$Activity
  pca_test_data <- predict(pca_mod, newdata=test_data[,-1])
  pca_test_data$Activity <- test_data$Activity
  return(pca_train_data)
}
# check PCA with RF 
.activityRFpca <- function(pca_train_data){ 
  fitControl <- trainControl(method="cv", number=5)
  set.seed(123)
  tstart <- Sys.time()
  forest_pca <- train(Activity~., data=pca_train_data,
                    method="rf", do.trace=10, ntree=100,
                    trControl = fitControl)
  tend <- Sys.time()
  print(tend-tstart)
  
  .activityPrediction(forest_pca)
  
## predict and control Accuracy
}
#prediction <- predict(forest_pca, newdata=pca_test_data)
#cm <- confusionMatrix(prediction, test_data$Activity)
#print(cm)

# check PCA with SVM  
.activitySVMpca <- function(pca_train_data){
  fitControl <- trainControl(method="cv", number=5)
  set.seed(123)
  tstart <- Sys.time()
  svm_pca <- train(Activity~., data=pca_train_data,
                    method="svmRadial",
                    trControl = fitControl)
  tend <- Sys.time()
  print(tend-tstart)
  .activityPrediction(svm_pca)
  
}
## predict and control Accuracy
#prediction <- predict(svm_pca, newdata=pca_test_data)
#cm <- confusionMatrix(prediction, test_data$Activity)
#print(cm)
.activityTree <- function() {
#  fitControl <- trainControl(method = "none",
 #                            classProbs = TRUE, summaryFunction = twoClassSummary)
  tree_full <- train(Activity~., data=train_data,
                  method="xgbTree")#,  trControl = fitControl)
  tend <- Sys.time()
  print(tend-tstart)
  fancyRpartPlot(tree_full$finalModel)
}



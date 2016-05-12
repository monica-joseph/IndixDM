#####################################################
# Collection of Examples of the different algorithms
# that are available to build classification models
# in R.
#
# includes:
#
# Logistic Regression
# Linear Regression
# RLM
# Support Vector Machine
# Decision Tree
# Random Forests
# Gradient Boosting Machine
# Multivariate Adaptive Regression Splines
#
#####################################################




#####################################################
# 1. SETUP DATA
#####################################################

#clear worksace
rm(list = ls(all = TRUE))

#set working directory 
setwd("D:\\Learn R\\IndixDM") 

#load the data
IndixDM_Train <- read.csv("data.csv", header=TRUE)
colnames(IndixDM_Train)
nrow(IndixDM_Train)#91845
#load the test data
IndixDM_Test <- read.csv("blindset_table_out.csv", header=TRUE)
colnames(IndixDM_Test)
nrow(IndixDM_Test)#11842

fullData <- read.csv("fullData.csv", header=TRUE)
sparse_matrix <- sparse.model.matrix(response ~ .-1, data = fullData)
mymatrix=model.matrix(~ -1 + . , data=fullData)
#load the formatted Test+Train data
fullData <- read.csv("fullDataN.csv", header=TRUE)
colnames(fullData)
nrow(fullData)#103687
head(fullData)

fullData[fullData['label']=='1',]['label']=0
fullData[fullData['label']=='2',]['label']=1

trainset[trainset['label']=='1',]['label']=0
trainset[trainset['label']=='2',]['label']=1

DMTrain[DMTrain['label']=='1',]['label']=0
DMTrain[DMTrain['label']=='2',]['label']=1

DMVal[DMVal['label']=='1',]['label']=0
DMVal[DMVal['label']=='2',]['label']=1

#create train and test sets
trainset = fullData[fullData$label != 'Test',]
testset = fullData[fullData$label == 'Test',]
trainset[,'label']=as.numeric(trainset[,'label'])

trainset[,'V1']=as.numeric(trainset[,'V1'])
trainset[,'V2']=as.numeric(trainset[,'V2'])
trainset[,'V3']=as.numeric(trainset[,'V3'])
trainset[,'V4']=as.numeric(trainset[,'V4'])
trainset[,'V5']=as.numeric(trainset[,'V5'])
trainset[,'V6']=as.numeric(trainset[,'V6'])
trainset[,'V7']=as.numeric(trainset[,'V7'])
trainset[,'V8']=as.numeric(trainset[,'V8'])
trainset[,'V9']=as.numeric(trainset[,'V9'])
trainset[,'V10']=as.numeric(trainset[,'V10'])
trainset[,'table.text']=as.numeric(trainset[,'table.text'])
trainset[,'url']=as.numeric(trainset[,'url'])


testset[,'V1']=as.numeric(testset[,'V1'])
testset[,'V2']=as.numeric(testset[,'V2'])
testset[,'V3']=as.numeric(testset[,'V3'])
testset[,'V4']=as.numeric(testset[,'V4'])
testset[,'V5']=as.numeric(testset[,'V5'])
testset[,'V6']=as.numeric(testset[,'V6'])
testset[,'V7']=as.numeric(testset[,'V7'])
testset[,'V8']=as.numeric(testset[,'V8'])
testset[,'V9']=as.numeric(testset[,'V9'])
testset[,'V10']=as.numeric(testset[,'V10'])
testset[,'table.text']=as.numeric(testset[,'table.text'])
testset[,'url']=as.numeric(testset[,'url'])
XY=c('label','V1','V10','V2','V3','V4','V5','V6','V7','V8','V9','table.text','url','Root')
features=c('V1','V10','V2','V3','V4','V5','V6','V7','V8','V9','url','table.text','Root')

#####################################################
# 3. Now just apply the algorithms
#####################################################


#####################################################
#####################################################
############################################
#xgb with all features
############################################
xgb <- xgboost(data = as.matrix(trainset[,features]), 
 label = trainset[,'label'], 
 eta = 0.1,
 max_depth = 4, 
 nround=25, 
 objective = "binary:logistic"
)
y_pred <- predict(xgb, as.matrix(testset[,features]),outputmargin=TRUE)
temp = ifelse(y_pred>0.5,'yes','no')
write.csv(temp, "XGB_submit1.csv", row.names=FALSE)
#####################################################
# SVM
#####################################################
library('e1071')
what <- "SVM"
SVM_model <- svm(label~table.text+Root+V1+V2, data=trainset[,XY],type='C',kernel='linear',probability = TRUE)

outTest <- predict(SVM_model, testset[,features], probability = TRUE)
#####################################################
# GBM
#####################################################
library(gbm)
GBM_NTREES = 150
GBM_SHRINKAGE = 0.1
GBM_DEPTH = 4
GBM_MINOBS = 50
GBM_model <- gbm.fit(
 x = trainset[,features] 
 ,y = trainset[,'label'] 
 ,distribution = "gaussian"
 ,n.trees = GBM_NTREES
 ,shrinkage = GBM_SHRINKAGE ,interaction.depth = GBM_DEPTH
 ,n.minobsinnode = GBM_MINOBS
 ,verbose = TRUE)
prediction <- predict.gbm(object = GBM_model,newdata = testset[,features] ,GBM_NTREES) 
range(prediction)
temp = ifelse(prediction>1.5,'yes','no')
write.csv(temp, "GBM_submit1.csv", row.names=FALSE)
##anothre gbm
GBM_model = gbm(label~tabe.text+V1+V2+V3+url,data=trainset[,features],n.trees=50,shrinkage=0.005 ,cv.folds=10)
###################################################################
#create train and validation dataset
##################################################################
library(caTools)
set.seed(100)
spl = sample.split(trainset$label, SplitRatio = 0.7)
DMTrain = subset(trainset, spl==TRUE)
DMVal = subset(trainset, spl==FALSE)
nrow(DMTrain)#6173
nrow(DMVal)#2350
################################################################
#calculate f-score
################################################################
#First I create a data set as
predict <- sample(c(0, 1), 20, replace=T)
true <- sample(c(0, 1), 20, replace=T)
#I suppose those 1's in the predicted values are the retrieved. The total number of retrieved is
retrieved <- sum(predict)
#Precision which is the fraction of retrieved instances that are relevant, is
precision <- sum(predict & true) / retrieved
#Recall which is the fraction of relevant instances that are retrieved, is
recall <- sum(predict & true) / sum(true)
#F-measure is 2 * precision * recall / (precision + recall) is
Fmeasure <- 2 * precision * recall / (precision + recall)
measurePrecisionRecall <- function(predict, actual_labels){
  precision <- sum(predict & actual_labels) / sum(predict)
  recall <- sum(predict & actual_labels) / sum(actual_labels)
  fmeasure <- 2 * precision * recall / (precision + recall)

  cat('precision:  ')
  cat(precision * 100)
  cat('%')
  cat('\n')

  cat('recall:     ')
  cat(recall * 100)
  cat('%')
  cat('\n')

  cat('f-measure:  ')
  cat(fmeasure * 100)
  cat('%')
  cat('\n')
}
measurePrecisionRecall(predict,true)
#####################################################
# 2. set the formula
#####################################################

theTarget <- "label"
theFormula <- as.formula(paste("as.factor(",theTarget, ") ~ . "))
theFormula1 <- as.formula(paste(theTarget," ~ . "))
#####################################################
# Logisic Regression
#####################################################
what <- "Logistic Regression"
LOGISTIC_model <- glm(theFormula, data=DMTrain[,XY], family=binomial(link="logit"))

train_pred <- predict(LOGISTIC_model, type="response", DMTrain)
test_pred <- predict(LOGISTIC_model, type="response", DMVal)

measurePrecisionRecall(ifelse(test_pred>.5,1,0),DMVal$label)
#####################################################
# GBM
#####################################################
library(gbm)
GBM_NTREES = 150
GBM_SHRINKAGE = 0.1
GBM_DEPTH = 4
GBM_MINOBS = 50
GBM_model <- gbm.fit(
 x = DMTrain[,features] 
 ,y = DMTrain[,'label'] 
 ,distribution = "gaussian"
 ,n.trees = GBM_NTREES
 ,shrinkage = GBM_SHRINKAGE ,interaction.depth = GBM_DEPTH
 ,n.minobsinnode = GBM_MINOBS
 ,verbose = TRUE)
prediction <- predict.gbm(object = GBM_model,newdata = DMVal[,features] ,GBM_NTREES) 
range(prediction)
temp = ifelse(prediction>0.5,'yes','no')
measurePrecisionRecall(ifelse(prediction>0.5,1,0),DMVal$label)
#                  var    rel.inf
#table.text table.text 59.8415496
#url               url 16.7163003
#V2                 V2  8.3246101
#Root             Root  3.0022941
#V4                 V4  2.3495066
#V3                 V3  2.0049378
#V5                 V5  1.7531756
#V6                 V6  1.6449391
#V7                 V7  1.0377003
#V1                 V1  0.9880549
#V8                 V8  0.9213043
#V9                 V9  0.8321679
#V10               V10  0.5834595
library(gbm)
GBM_NTREES = 150
GBM_SHRINKAGE = 0.1
GBM_DEPTH = 4
GBM_MINOBS = 50
GBM_model <- gbm.fit(
 x = trainset[,features] 
 ,y = trainset[,'label'] 
 ,distribution = "gaussian"
 ,n.trees = GBM_NTREES
 ,shrinkage = GBM_SHRINKAGE ,interaction.depth = GBM_DEPTH
 ,n.minobsinnode = GBM_MINOBS
 ,verbose = TRUE)
prediction <- predict.gbm(object = GBM_model,newdata = testset[,features] ,GBM_NTREES) 
range(prediction)
temp = ifelse(prediction>0.5,'yes','no')
measurePrecisionRecall(ifelse(prediction>0.5,1,0),DMVal$label)
#####################################################
# SVM
#####################################################
library('e1071')
what <- "SVM"
SVM_model <- svm(label~table.text+Root+V1+V2+V3+V4, data=DMTrain[,XY],type='C',kernel='linear',probability = TRUE)

outTest <- predict(SVM_model, DMVal[,features], probability = TRUE)
#####################################################
# Robust Fitting of Linear Models
#####################################################
library(MASS)
what <- "RLM"
RLM_model <- rlm(label ~ table.text + Root + url + V1, data=DMTrain)
test_pred <- predict(RLM_model, type="response", DMVal)


measurePrecisionRecall(ifelse(test_pred>0.5,1,0),DMVal$label)
###f-score
# Function: evaluation metrics
    ## True positives (TP) - Correctly idd as success
    ## True negatives (TN) - Correctly idd as failure
    ## False positives (FP) - success incorrectly idd as failure
    ## False negatives (FN) - failure incorrectly idd as success
    ## Precision - P = TP/(TP+FP) how many idd actually success/failure
    ## Recall - R = TP/(TP+FN) how many of the successes correctly idd
    ## F-score - F = (2 * P * R)/(P + R) harm mean of precision and recall
prf <- function(predAct){
    ## predAct is two col dataframe of pred,act
    preds = predAct[,1]
    trues = predAct[,2]
    xTab <- matrix(c(table(preds, trues),0,0,0,0),nrow=2,ncol=2,byrow=TRUE)
    clss <- as.character(sort(unique(preds)))
    r <- matrix(NA, ncol = 7, nrow = 1, 
        dimnames = list(c(),c('Acc',
        paste("P",clss[1],sep='_'), 
        paste("R",clss[1],sep='_'), 
        paste("F",clss[1],sep='_'), 
        paste("P",clss[2],sep='_'), 
        paste("R",clss[2],sep='_'), 
        paste("F",clss[2],sep='_'))))
    r[1,1] <- sum(xTab[1,1],xTab[2,2])/sum(xTab) # Accuracy
    r[1,2] <- xTab[1,1]/sum(xTab[,1]) # Miss Precision
    r[1,3] <- xTab[1,1]/sum(xTab[1,]) # Miss Recall
    r[1,4] <- (2*r[1,2]*r[1,3])/sum(r[1,2],r[1,3]) # Miss F
    r[1,5] <- xTab[2,2]/sum(xTab[,2]) # Hit Precision
    r[1,6] <- xTab[2,2]/sum(xTab[2,]) # Hit Recall
    r[1,7] <- (2*r[1,5]*r[1,6])/sum(r[1,5],r[1,6]) # Hit F
    r}
pred <- rbinom(100,1,.7)
act <- rbinom(100,1,.7)
predAct <- data.frame(pred,act)
prf(predAct)
###
predAct<-data.frame(ifelse(test_pred>0.5,1,0),DMVal$label)
prf(predAct)
measurePrecisionRecall(ifelse(test_pred>0.5,1,0),DMVal$label
#####################################################
# Tree
#####################################################
library(rpart)
what <- "TREE"
TREE_model <- rpart(theFormula1, data=DMTrain[,XY], method="class")
test_pred <- predict(TREE_model, DMVal)[,2]
range(test_pred)
predAct<-data.frame(ifelse(test_pred>.5,1,0),DMVal$label)
prf(predAct)
measurePrecisionRecall(ifelse(test_pred>.5,1,0),DMVal$label)
#####################################################
# Random Forest
#####################################################
library(randomForest)
what <- "Random Forest"
FOREST_model <- randomForest(theFormula1, data=DMTrain[,XY], ntree=50)

test_pred <- predict(FOREST_model, testset, type="response")[,2]
range(test_pred)
predAct<-data.frame(ifelse(test_pred>.5,1,0),DMVal$label)
prf(predAct)
measurePrecisionRecall(ifelse(test_pred>.5,1,0),DMVal$label)
######################################################
DMTrain[,'V1']=as.numeric(DMTrain[,'V1'])
DMTrain[,'V2']=as.numeric(DMTrain[,'V2'])
DMTrain[,'V3']=as.numeric(DMTrain[,'V3'])
DMTrain[,'V4']=as.numeric(DMTrain[,'V4'])
DMTrain[,'V5']=as.numeric(DMTrain[,'V5'])
DMTrain[,'V6']=as.numeric(DMTrain[,'V6'])
DMTrain[,'V7']=as.numeric(DMTrain[,'V7'])
DMTrain[,'V8']=as.numeric(DMTrain[,'V8'])
DMTrain[,'V9']=as.numeric(DMTrain[,'V9'])
DMTrain[,'V10']=as.numeric(DMTrain[,'V10'])
DMTrain[,'table.text']=as.numeric(DMTrain[,'table.text'])
DMTrain[,'url']=as.numeric(DMTrain[,'url'])


DMVal[,'V1']=as.numeric(DMVal[,'V1'])
DMVal[,'V2']=as.numeric(DMVal[,'V2'])
DMVal[,'V3']=as.numeric(DMVal[,'V3'])
DMVal[,'V4']=as.numeric(DMVal[,'V4'])
DMVal[,'V5']=as.numeric(DMVal[,'V5'])
DMVal[,'V6']=as.numeric(DMVal[,'V6'])
DMVal[,'V7']=as.numeric(DMVal[,'V7'])
DMVal[,'V8']=as.numeric(DMVal[,'V8'])
DMVal[,'V9']=as.numeric(DMVal[,'V9'])
DMVal[,'V10']=as.numeric(DMVal[,'V10'])
DMVal[,'table.text']=as.numeric(DMVal[,'table.text'])
DMVal[,'url']=as.numeric(DMVal[,'url'])
############################################
#xgb with all features
############################################
xgb <- xgboost(data = as.matrix(DMTrain[,features]), 
 label = DMTrain[,'label'], 
 eta = 0.01,
 max_depth = 10, 
 nrounds=500, 
 objective = "binary:logistic",
 eval_metric="error"
)
y_pred <- predict(xgb, as.matrix(DMVal[,features]),type="response",n.trees=100)
temp = ifelse(y_pred>0.5,'yes','no')
mdat <- matrix(y_pred, nrow = 13777, ncol = 2, byrow = TRUE,
dimnames = list(1:13777,c("C.1", "C.2")))
max_mdat=apply(mdat,1,max)
for( var in 1:13777){
    if(max_mdat[var]==mdat[var,1]){
    	xgb_y[var]=0}
    else {
	    xgb_y[var]=1
		
	}}
predAct<-data.frame(xgb_y,DMVal$label)
prf(predAct)
measurePrecisionRecall(xgb_y,DMVal$label)


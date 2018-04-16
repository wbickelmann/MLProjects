library(rrcov)
library(caret)
library(ca)
library(plyr)
library(FactoMineR)
library(tidyverse)
library(rlist)
library(data.table)
library(mlbench)
library(randomForest)
library(dplyr)
library(mlr)

dim(combi_c)
combi_c<-read.csv("C_hhold_train.csv",stringsAsFactors = TRUE, header = TRUE)
combi_c_indiv <- read.csv("C_indiv_train.csv", stringsAsFactors = TRUE, header = TRUE)
combi_c_indiv <- subset( combi_c_indiv, select = -c(iid, poor,country ) )
combi_c_indiv <- combi_c_indiv[!duplicated(combi_c_indiv$id), ]

combi_c <-join(combi_c, combi_c_indiv, by='id', type='inner')
dim(combi_c)
combi_c$id<-NULL
combi_c1<-(na.omit(combi_c))

poor<-data.frame(combi_c1$poor)
names(poor) <- ("poor")
combi_c1$poor<-NULL

nzv <- nearZeroVar(combi_c1)
combi_c1<-combi_c1[,-nzv]
dim(combi_c1)
combi_c1<-cbind(combi_c1,poor)

levels_detect<-function(x){
  if(length(levels(x)) > 34){
    return(FALSE)
  }else{
    return(TRUE)
  }
}
excess_levels<-lapply(combi_c1,levels_detect)
name_removec<-names(which(excess_levels==FALSE))
combi_c1<-combi_c1[ , !(names(combi_c1) %in% name_removec)]


control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
resultsc <- rfe(combi_c1[,1:173], combi_c1[,174], rfeControl=control, metric = "Accuracy")
# summarize the results
print(resultsc)
# list the chosen features
predictors(resultsc)->topvarsc
# plot the results
plot(resultsc, type=c("g", "o"))
summary(resultsc)

combi_c1%>%select(topvarsc,'poor')->combi_c2

trainc<-combi_c2
n = nrow(trainc)
train.set = sample(n, size = .8*n)
test.set = setdiff(1:n, train.set)


trainTask = makeClassifTask(data = trainc, target = "poor")
trainTask = createDummyFeatures(trainTask)


xgb_learner <- makeLearner(
  "classif.xgboost",
  predict.type = "response",
  par.vals = list(objective = "binary:logistic", eval_metric = "error"))

xgb_params <- makeParamSet(
  # The number of trees in the model (each one built sequentially)
  makeIntegerParam("nrounds", lower = 100, upper = 500),
  # number of splits in each tree
  makeIntegerParam("max_depth", lower = 1, upper = 10),
  # "shrinkage" - prevents overfitting
  makeNumericParam("eta", lower = .1, upper = .5),
  # L2 regularization - prevents overfitting
  makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x)
)
control <- makeTuneControlRandom(maxit = 1)
resample_desc <- makeResampleDesc("CV", iters = 10)

tuned_params <- tuneParams(
  learner = xgb_learner,
  task = trainTask,
  resampling = resample_desc,
  par.set = xgb_params,
  control = control
)
xgb_tuned_learner <- setHyperPars(
  learner = xgb_learner,
  par.vals = tuned_params$x
)
# Re-train parameters using tuned hyperparameters (and full training set)
xgb_model <- train(xgb_tuned_learner, task=trainTask,subset=train.set)

# Make a new prediction
pred = predict(xgb_model, task=trainTask, subset=test.set)
performance(pred, measures = list(mmce, acc))

estimateRelativeOverfitting(resample_desc, acc, trainTask, xgb_learner)

calculateROCMeasures(pred)

inTrain <- createDataPartition(combi_c2$poor, p = .80, list = FALSE)
Xtrainc<-combi_c2[inTrain,c(1:4)]
Ytrainc <- combi_c2[inTrain,5]
Xtestc <- combi_c2[-inTrain,c(1:4)]
Ytestc <- combi_c2[-inTrain,5]
combi_c2.navg <- caret::train(Xtrainc, 
                       Ytrainc, method = "avNNet",
                       trControl = trainControl(method = "cv"))
Accuraciesa<- confusionMatrix(Ytestc, 
                                  predict(combi_c2.navg, newdata=Xtestc))$overall["Accuracy"]

combi_t<-read.csv("C_hhold_test.csv",stringsAsFactors = TRUE, header = TRUE)
combi_t_indiv <- read.csv("C_indiv_test.csv", stringsAsFactors = TRUE, header = TRUE)
combi_t_indiv <- subset( combi_t_indiv, select = -c(iid, country ) )
combi_t_indiv <- combi_t_indiv[!duplicated(combi_t_indiv$id), ]

combi_t <-inner_join(combi_t, combi_t_indiv, by='id')

id<-data.frame(combi_t$id)
names(id) <- ("id")

country<-data.frame(combi_t$country)
names(country) <- ("country")


combi_t1<-select(combi_t,topvarsc)
combi_c3<-combi_c2[-17]

combi_t1 <- rbind(combi_c3[1, ] , combi_t1)
combi_t1 <- combi_t1[-1,]

predict(combi_c2.navg,newdata=combi_t1,type='prob')->poor_pred
cprediction<-cbind(id,country,poor_pred[2])
names(cprediction)<-c("id","country","poor")



library(caret)
library(tidyverse)
library(xgboost)
setwd('C:/Users/Willi/OneDrive/Documents/MLProjects')

combi_c<-read.csv("C_hhold_train.csv",stringsAsFactors = TRUE, header = TRUE)
combi_c_indiv <- read.csv("C_indiv_train.csv", stringsAsFactors = TRUE, header = TRUE)
combi_c_indiv <- subset( combi_c_indiv, select = -c(iid, poor,country ) )
combi_c_indiv <- combi_c_indiv[!duplicated(combi_c_indiv$id), ]

combi_c <-plyr::join(combi_c, combi_c_indiv, by='id', type='inner')

combi_c$id<-NULL
combi_c1<-(na.omit(combi_c))

poor<-data.frame(as.factor(combi_c1$poor))
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
resultsc <- rfe(combi_c1[,1:(length(colnames(combi_c1))-1)], 
                combi_c1[,length(colnames(combi_c1))], 
                rfeControl=control, metric = "Accuracy")

predictors(resultsc)->topvarsc
plot(resultsc, type=c("g", "o"))

combi_c1%>%select(topvarsc,'poor')->combi_c2

preProcValues <- preProcess(combi_c2, method = c("center", "scale"))
dfc <- predict(preProcValues, combi_c2)
dfc2=gather(dfc2)
dfc2=as.data.frame(dfc2=!NULL)

inTrain <- createDataPartition(dfc2$poor, p = .80, list = FALSE)
Xtrainc<-dfc2[inTrain,c(1:dim(dfc2)[2]-1)]
Ytrainc <- dfc2[inTrain,dim(dfc2)[2]]
Xtestc <- dfc2[-inTrain,c(1:dim(dfc2)[2]-1)]
Ytestc <- dfc2[-inTrain,dim(dfc2)[2]]
trainc=cbind(Xtrainc,Ytrainc)

names(trainc)[dim(dfc2)[2]]="poor"

fitControl <-trainControl(method = "cv",
             number = 5,
             savePredictions = TRUE,
             classProbs = TRUE,
             summaryFunction = twoClassSummary
)

dfc.xgb <- caret::train(poor~.,data=trainc, method = "xgbTree",
                       trControl=fitControl, metric="ROC")
dfc.logit <- caret::train(poor~.,data=trainc, method = "LogitBoost")
dfc.gbm <- caret::train(poor~.,data=trainc, method = "gbm")




qplot(data=dfc,x=poor,geom = 'bar')

trellis.par.set(caretTheme())
plot(dfc.xgb)

confusionMatrix(Ytestc, predict(dfc.logit, newdata=Xtestc))






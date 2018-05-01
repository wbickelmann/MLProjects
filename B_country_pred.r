library(plyr)
library(tidyverse)
library(caret)
library(dplyr)
library(naniar)
library(rlist)
library(randomForest)
library(strin)
setwd('C:/Users/Willi/OneDrive/Documents/MLProjects')
combi_b<-read.csv("B_hhold_train.csv",stringsAsFactors = TRUE, header = TRUE)
combi_b_indiv <- read.csv("B_indiv_train.csv", stringsAsFactors = TRUE, header = TRUE)
combi_b_indiv%>%rename(wJthinfa_indiv=wJthinfa)->combi_b_indiv
combi_b_indiv <- subset( combi_b_indiv, select = -c(iid, poor,country ) )
combi_b_indiv <- combi_b_indiv[!duplicated(combi_b_indiv$id), ]

combi_b <-join(combi_b, combi_b_indiv, by='id', type='inner')
combi_b$id<-NULL


gg_miss_var(combi_b) + labs(y = "Number of Missing NAs") + theme(axis.text.y=element_blank())

#removing columns with excessive NAs
gtg<-which(colSums(is.na(combi_b))>300)
gtg<-list.names(gtg)
(combi_b[,FALSE==(colnames(combi_b) %in% gtg)])->combi_b1
#removing NAs all-together
na.omit(combi_b1)->combi_b1

poor<-(data.frame(combi_b1$poor))
colnames(poor)<-'poor'
combi_b1$poor<-NULL
#removing variables with little variance and too many levels
nzv <- nearZeroVar(combi_b1)
combi_b1<-combi_b1[,-nzv]
dim(combi_b1)
combi_b1<-cbind(combi_b1,poor)
poor<-NULL
levels_detect<-function(x){
  if(length(levels(x)) > 34){
    return(FALSE)
  }else{
    return(TRUE)
  }
}


excess_levels<-lapply(combi_b1,levels_detect)
name_removeb<-names(which(excess_levels==FALSE))
combi_b1<-combi_b1[ , !(names(combi_b1) %in% name_removeb)]


#feature engineering with Simulated Annealing
ctrl <- safsControl(functions = rfSA)
objb <- safs(x = combi_b1[,1:dim(combi_b1)[2]-1], 
            y = combi_b1[,dim(combi_b1)[2]],
            iters = 30,
            safsControl = ctrl,
            metric = "Accuracy",
            method = "xgbTree")
objb$optVariables->topvarsb1

combi_b1%>%select(topvarsb1,'poor')->dfb
preProcValues <- preProcess(dfb, method = c("center", "scale"))
dfb <- predict(preProcValues, dfb)

#Selecting important variables for sake of exploratory analysis

inTrain <- createDataPartition(combi_b1$poor, p = .70, list = FALSE)
rftrainb<-combi_b1[inTrain,]
rftestby<-combi_b1[-inTrain,420]
rftestbx<-combi_b1[-inTrain,1:419]
dfb.rf <- caret::train(poor~.,data=rftrainb, method = "rf")

confusionMatrix(rftestby, 
                predict(dfb.rf, newdata=rftestbx))



ggplot(impbdf1, aes(x=variables,y=Overall))+ 
  geom_bar(stat = "identity",fill = "Blue")+
  coord_flip()


impb<-varImp(dfb.rf)
impb$importance->impbdf
impbdf<-(arrange(impbdf,desc(Overall)))
impbdf1<-head(impcdf,10)

##visualizing variable importance
ggplot(impbdf1, aes(x=variables,y=Overall))+ 
  geom_bar(stat = "identity",fill = "Blue")+
  coord_flip()

##cleaning up variables used for exploratory analysis

explore_varsb2<-c()
explore_varsb1<-(impbdf1$variables)
for(i in explore_varsb1){
    str_sub(i,start=1,end=8)->explore_varsb2[length(explore_varsb2)+1]
}
select(dfb,explore_varsb2,"poor")->explore_varsb3

write.csv(explore_varsb3,"explore_variablesb.csv")


dfb$yy
fitControl <-trainControl(method = "cv",
                          number = 5)

parametersGrid <-  expand.grid(eta = 0.1, 
                               colsample_bytree=c(0.5,0.7),
                               max_depth=c(15,18),
                               nrounds=100,
                               gamma=1,
                               min_child_weight=2,
                               subsample = c(0.5, 0.75)
)



Accuraciesb.xgb <- c(0.00)
Accuraciesb.rf <- c(0.00)
Accuraciesb.logit <-c(0.00)


inTrain <- createDataPartition(dfb$poor, p = .70, list = FALSE)
Xtrainb<-dfb[inTrain,c(1:dim(dfb)[2]-1)]
Ytrainb <- dfb[inTrain,(dim(dfb)[2])]
Xtestb<-dfb[-inTrain,c(1:dim(dfb)[2]-1)]
Ytestb <- dfb[-inTrain,(dim(dfb)[2])]
trainb=cbind(Xtrainb,Ytrainb)
names(trainb)[dim(dfb)[2]]="poor"

table(dfb$poor)
dfb.xgb <- caret::train(poor~.,data=trainb, method = "xgbTree",
                        trControl=fitControl, metric="Accuracy",
                        tuneGrid = parametersGrid, 
                        parms = list(prior = c(0.9825,.0175)))
dfb.rf <- caret::train(poor~.,data=trainb, method = "rf",
                        trControl=fitControl, metric="kappa")
dfb.logit <- caret::train(poor~.,data=trainb, method = "LogitBoost",
                        trControl=fitControl, metric="kappa")
dfb.knn <- caret::train(poor~.,data=trainb, method = "knn",
                        trControl=fitControl, metric="kappa")

qplot(data=dfb,x=poor,geom = 'bar')



Accuraciesb.xgb
confusionMatrix(Ytestb, 
                  predict(dfb.xgb, newdata=Xtestb))
Accuraciesb.rf <- confusionMatrix(Ytestb, 
                  predict(dfb.rf, newdata=Xtestb))$overall["Accuracy"]
Accuraciesb.logit <- confusionMatrix(Ytestb, 
                  predict(dfb.logit, newdata=Xtestb))$overall["Accuracy"]
Accuraciesb.knn <- confusionMatrix(Ytestb, 
                  predict(dfb.knn, newdata=Xtestb))$overall["Accuracy"]


accuraciesb<-rbind(Accuraciesb.xgb,Accuraciesb.rf,Accuraciesb.logit,Accuraciesb.knn)
accuraciesb<-as.data.frame(accuraciesb)
accuraciesb$model<-rownames(accuraciesb)
accuraciesb$Accuracy<-round(accuraciesb$Accuracy,digits = 3)
ggplot(accuraciesb, aes(x=model,y=Accuracy))+ 
  geom_bar(stat = "identity",fill = "#FF6666")+
  geom_text(aes(label = Accuracy))

plot(dfb.xgb)
plot(dfb.rf)
plot(dfb.logit)
plot(dfb.knn)

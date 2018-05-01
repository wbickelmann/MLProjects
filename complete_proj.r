
library(plyr)
library(tidyverse)
library(caret)
library(dplyr)
library(naniar)
library(rlist)
library(randomForest)
library(knitr)
library(kableExtra)
setwd('C:/Users/Willi/OneDrive/Documents/MLProjects')
combi_b<-read.csv("B_hhold_train.csv",stringsAsFactors = TRUE, header = TRUE)
combi_b_indiv <- read.csv("B_indiv_train.csv", stringsAsFactors = TRUE, header = TRUE)
combi_b_indiv%>%rename(wJthinfa_indiv=wJthinfa)->combi_b_indiv
combi_b_indiv <- subset( combi_b_indiv, select = -c(iid, poor,country ) )
combi_b_indiv <- combi_b_indiv[!duplicated(combi_b_indiv$id), ]
combi_b <-join(combi_b, combi_b_indiv, by='id', type='inner')
combi_b$id<-NULL
combi_c<-read.csv("C_hhold_train.csv",stringsAsFactors = TRUE, header = TRUE)
combi_c_indiv <- read.csv("C_indiv_train.csv", stringsAsFactors = TRUE, header = TRUE)
combi_c_indiv <- subset( combi_c_indiv, select = -c(iid, poor,country ) )
combi_c_indiv <- combi_c_indiv[!duplicated(combi_c_indiv$id), ]
combi_c <-plyr::join(combi_c, combi_c_indiv, by='id', type='inner')
combi_c$id<-NULL
combi_a<-read.csv("A_hhold_train.csv",stringsAsFactors = TRUE, header = TRUE)
combi_a_indiv <- read.csv("A_indiv_train.csv", stringsAsFactors = TRUE, header = TRUE)
combi_a_indiv <- subset( combi_a_indiv, select = -c(iid, poor,country ) )
combi_a_indiv <- combi_a_indiv[!duplicated(combi_a_indiv$id), ]
combi_a <-join(combi_a, combi_a_indiv, by='id', type='inner')
combi_a$id<-NULL

Country<-c("A","A","B","B","C","C")
Grouping<-c("Household","Individual","Household","Individual","Household","Individual")
Variables<-c(346,44,443,227,165,44)
Observations<-c(8203,37560,3255,20252,6469,29913)
data.table(Country,Grouping,Variables,Observations)%>%
  kable("html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))%>%
  footnote(general = "This table describes the dimensions of each dataset released by Driven Data")

lapply(combi_a,class)%>%
  data.frame()%>%
  gather()%>%
  group_by(value)%>%
  summarise(Amount=n())%>%
  kable("html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))%>%
  footnote(general = "This table describes the variables of Country A")

lapply(combi_b,class)%>%
  data.frame()%>%
  gather()%>%
  group_by(value)%>%
  summarise(Amount=n())%>%
  kable("html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))%>%
  footnote(general = "This table describes the variables of Country B")


lapply(combi_c,class)%>%
  data.frame()%>%
  gather()%>%
  group_by(value)%>%
  summarise(Amount=n())%>%
  kable("html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))%>%
  footnote(general = "This table describes the variables of Country C")

gg_miss_var(combi_a) + labs(y = "Number of Missing NAs in Country A") + theme(axis.text.y=element_blank())
gg_miss_var(combi_b) + labs(y = "Number of Missing NAs in Country B") + theme(axis.text.y=element_blank())
gg_miss_var(combi_c) + labs(y = "Number of Missing NAs in Country c") + theme(axis.text.y=element_blank())


#removing columns with excessive NAs
gtg<-which(colSums(is.na(combi_b))>((dim(combi_b)[1])/10))
gtg<-list.names(gtg)
(combi_b[,FALSE==(colnames(combi_b) %in% gtg)])->combi_b1
#removing NAs all-together
na.omit(combi_b1)->combi_b1

#removing columns with excessive NAs
gtg<-which(colSums(is.na(combi_a))>((dim(combi_a)[1])/10))
gtg<-list.names(gtg)
(combi_a[,FALSE==(colnames(combi_a) %in% gtg)])->combi_a1
#removing NAs all-together
na.omit(combi_a1)->combi_a1

#removing columns with excessive NAs
gtg<-which(colSums(is.na(combi_c))>((dim(combi_c)[1])/10))
gtg<-list.names(gtg)
(combi_c[,FALSE==(colnames(combi_c) %in% gtg)])->combi_c1
#removing NAs all-together
na.omit(combi_c1)->combi_c1


levels_detect<-function(x){
  if(length(levels(x)) > 34){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

excess_levels<-lapply(combi_b1,levels_detect)
name_remove<-names(which(excess_levels==FALSE))
combi_b1<-combi_b1[ , !(names(combi_b1) %in% name_remove)]

excess_levels<-lapply(combi_a1,levels_detect)
name_remove<-names(which(excess_levels==FALSE))
combi_a1<-combi_a1[ , !(names(combi_a1) %in% name_remove)]

excess_levels<-lapply(combi_c1,levels_detect)
name_remove<-names(which(excess_levels==FALSE))
combi_c1<-combi_c1[ , !(names(combi_c1) %in% name_remove)]



poorb<-(data.frame(combi_b1$poor))
colnames(poorb)<-'poor'
combi_b1$poor<-NULL
nzv <- nearZeroVar(combi_b1)
combi_b1<-combi_b1[,-nzv]
combi_b1<-cbind(combi_b1,poorb)
poorb<-NULL

poora<-(data.frame(combi_a1$poor))
colnames(poora)<-'poor'
combi_a1$poor<-NULL
nzv <- nearZeroVar(combi_a1)
combi_a1<-combi_a1[,-nzv]
combi_a1<-cbind(combi_a1,poora)
poora<-NULL
poorc<-(data.frame(combi_c1$poor))
colnames(poorc)<-'poor'
combi_c1$poor<-NULL
nzv <- nearZeroVar(combi_c1)
combi_c1<-combi_c1[,-nzv]
combi_c1<-cbind(combi_c1,poorc)
poorc<-NULL



ctrl <- safsControl(functions = rfSA)
objb <- safs(x = combi_b1[,1:dim(combi_b1)[2]-1], 
             y = combi_b1[,dim(combi_b1)[2]],
             iters = 30,
             safsControl = ctrl,
             metric = "Accuracy",
             method = "xgbTree")
objb$optVariables->topvarsb1
combi_b1%>%select(topvarsb1,'poor')->dfb

obja <- safs(x = combi_a1[,1:dim(combi_a1)[2]-1],
             y = combi_a1[,dim(combi_a1)[2]],
             iters = 15,
             safsControl = ctrl,
             metric = "Accuracy",
             method = "xgbTree")
obja$optVariables->topvarsa
combi_a1%>%select(topvarsa,'poor')->dfa

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
resultsc <- rfe(combi_c1[,1:(length(colnames(combi_c1))-1)], 
                combi_c1[,length(colnames(combi_c1))], 
                rfeControl=control, metric = "Accuracy")
predictors(resultsc)->topvarsc
plot(resultsc, type=c("g", "o"))
combi_c1%>%select(topvarsc,'poor')->dfc

preProcValues <- preProcess(dfb, method = c("center", "scale"))
dfb <- predict(preProcValues, dfb)

preProcValues <- preProcess(dfa, method = c("center", "scale"))
dfa <- predict(preProcValues, dfa)

preProcValues <- preProcess(dfc, method = c("center", "scale"))
dfc <- predict(preProcValues, dfc)



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


inTrain <- createDataPartition(dfb$poor, p = .70, list = FALSE)
trainb<-dfb[inTrain,]
Xtestb<-dfb[-inTrain,c(1:dim(dfb)[2]-1)]
Ytestb <- dfb[-inTrain,(dim(dfb)[2])]

dfb.xgb <- caret::train(poor~.,data=trainb, method = "xgbTree",
                        trControl=fitControl, metric="Accuracy",
                        tuneGrid = parametersGrid, 
                        parms = list(prior = c(0.9825,.0175)))
dfb.rf <- caret::train(poor~.,data=trainb, method = "rf",
                       trControl=fitControl, metric="Accuracy",
                       parms = list(prior = c(0.9825,.0175)))
dfb.logit <- caret::train(poor~.,data=trainb, method = "LogitBoost",
                          trControl=fitControl, metric="Accuracy",
                          parms = list(prior = c(0.9825,.0175)))

dfb.knn <- caret::train(poor~.,data=trainb, method = "knn",
                        trControl=fitControl, metric="Accuracy")
dfb.avn <- caret::train(poor~.,data=trainb, method = "avNNet",
                        trControl=fitControl, metric="Accuracy")


qplot(data=dfb,x=poor,geom = 'bar',main="Ratio of impoverished households in Country B")


Accuraciesb.xgb<-confusionMatrix(Ytestb, 
                                 predict(dfb.xgb, newdata=Xtestb))$overall["Accuracy"]
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
  geom_text(aes(label = Accuracy))+
  ggtitle("Country B Accurasies")


impb<-varImp(dfb.rf)
impb$importance->impbdf
impbdf<-(arrange(impbdf,desc(Overall)))
impbdf1<-head(impcdf,10)

ggplot(impbdf1, aes(x=variables,y=Overall))+ 
  geom_bar(stat = "identity",fill = "Blue")+
  coord_flip()



inTrain <- createDataPartition(dfa$poor, p = .70, list = FALSE)
traina<-dfa[inTrain,]
Xtesta<-dfa[-inTrain,c(1:dim(dfa)[2]-1)]
Ytesta <- dfa[-inTrain,(dim(dfa)[2])]

dfa.xgb <- caret::train(poor~.,data=traina, method = "xgbTree",
                        trControl=fitControl, metric="Accuracy",
                        tuneGrid = parametersGrid, 
                        parms = list(prior = c(0.9825,.0175)))
dfa.rf <- caret::train(poor~.,data=traina, method = "rf",
                       trControl=fitControl, metric="Accuracy",
                       parms = list(prior = c(0.9825,.0175)))
dfa.logit <- caret::train(poor~.,data=traina, method = "LogitBoost",
                          trControl=fitControl, metric="Accuracy",
                          parms = list(prior = c(0.9825,.0175)))

dfa.knn <- caret::train(poor~.,data=traina, method = "knn",
                        trControl=fitControl, metric="Accuracy")

dfa.avn <- caret::train(poor~.,data=traina, method = "avNNet",
                        trControl=fitControl, metric="Accuracy")


qplot(data=dfa,x=poor,geom = 'bar',main="Ratio of impoverished households in Country A")




Accuraciesa.xgb
confusionMatrix(Ytesta, predict(dfa.xgb, newdata=Xtesta))
Accuraciesa.rf <- confusionMatrix(Ytesta, 
                                  predict(dfa.rf, newdata=Xtesta))$overall["Accuracy"]
Accuraciesa.logit <- confusionMatrix(Ytesta, 
                                     predict(dfa.logit, newdata=Xtesta))$overall["Accuracy"]
Accuraciesa.knn <- confusionMatrix(Ytesta, 
                                   predict(dfa.knn, newdata=Xtesta))$overall["Accuracy"]
Accuraciesa.avn <- confusionMatrix(Ytesta, 
                                   predict(dfa.avn, newdata=Xtesta))$overall["Accuracy"]


accuraciesa<-rbind(Accuraciesa.xgb,Accuraciesa.rf,Accuraciesa.logit,Accuraciesa.knn,Accuraciesa.avn)
accuraciesa<-as.data.frame(accuraciesa)

accuraciesa$model<-rownames(accuraciesa)
accuraciesa$Accuracy<-round(accuraciesa$Accuracy,digits = 3)
ggplot(accuraciesa, aes(x=model,y=Accuracy))+ 
  geom_bar(stat = "identity",fill = "#FF6666")+
  geom_text(aes(label = Accuracy))+
  ggtitle("Country A Accurasies")


impa<-varImp(dfa.rf)
impa$importance->impadf
impadf<-(arrange(impadf,desc(Overall)))
impadf1<-head(impadf,10)

ggplot(impadf1, aes(x=variables,y=Overall))+ 
  geom_bar(stat = "identity",fill = "Blue")+
  coord_flip()


inTrain <- createDataPartition(dfc$poor, p = .70, list = FALSE)
trainc<-dfc[inTrain,]
Xtestc<-dfc[-inTrain,c(1:dim(dfc)[2]-1)]
Ytestc <- dfc[-inTrain,(dim(dfc)[2])]

dfc.xgb <- caret::train(poor~.,data=trainc, method = "xgbTree",
                        trControl=fitControl, metric="Accuracy",
                        tuneGrid = parametersGrid, 
                        parms = list(prior = c(0.9825,.0175)))
dfc.rf <- caret::train(poor~.,data=trainc, method = "rf",
                       trControl=fitControl, metric="Accuracy",
                       parms = list(prior = c(0.9825,.0175)))
dfc.logit <- caret::train(poor~.,data=trainc, method = "LogitBoost",
                          trControl=fitControl, metric="Accuracy",
                          parms = list(prior = c(0.9825,.0175)))

dfc.knn <- caret::train(poor~.,data=trainc, method = "knn",
                        trControl=fitControl, metric="Accuracy")

dfc.avn <- caret::train(poor~.,data=trainc, method = "avNNet",
                        trControl=fitControl, metric="Accuracy")


qplot(data=dfc,x=poor,geom = 'bar',main="Ratio of impoverished households in Country C")




confusionMatrix(Ytestc, predict(dfc.xgb, newdata=Xtestc))
Accuraciesc.rf <- confusionMatrix(Ytestc, 
                                  predict(dfc.rf, newdata=Xtestc))$overall["Accuracy"]
Accuraciesc.logit <- confusionMatrix(Ytestc, 
                                     predict(dfc.logit, newdata=Xtestc))$overall["Accuracy"]
Accuraciesc.knn <- confusionMatrix(Ytestc, 
                                   predict(dfc.knn, newdata=Xtestc))$overall["Accuracy"]
Accuraciesc.avn <- confusionMatrix(Ytestc, 
                                   predict(dfc.avn, newdata=Xtestc))$overall["Accuracy"]
summary(dfc.avn)

accuraciesc<-rbind(Accuraciesc.xgb,Accuraciesc.rf,Accuraciesc.logit,Accuraciesc.knn,Accuraciesc.avn)
accuraciesc<-as.data.frame(accuraciesc)

accuraciesc$model<-rownames(accuraciesc)
accuraciesc$Accuracy<-round(accuraciesc$Accuracy,digits = 3)
ggplot(accuraciesc, aes(x=model,y=Accuracy))+ 
  geom_bar(stat = "identity",fill = "#FF6666")+
  geom_text(aes(label = Accuracy))+
  ggtitle("Country C Accurasies")


impc<-varImp(dfc.rf)
impc$importance->impcdf
impcdf<-(arrange(impcdf,desc(Overall)))
impcdf1<-head(impcdf,10)

ggplot(impcdf1, aes(x=variables,y=Overall))+ 
  geom_bar(stat = "identity",fill = "Blue")+
  coord_flip()


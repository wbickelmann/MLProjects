library(caret)
library(ca)
library(plyr)
library(tidyverse)
library(randomForest)
combi_a<-read.csv("A_hhold_train.csv",stringsAsFactors = TRUE, header = TRUE)
combi_a_indiv <- read.csv("A_indiv_train.csv", stringsAsFactors = TRUE, header = TRUE)
combi_a_indiv <- subset( combi_a_indiv, select = -c(iid, poor,country ) )
combi_a_indiv <- combi_a_indiv[!duplicated(combi_a_indiv$id), ]

combi_a <-join(combi_a, combi_a_indiv, by='id', type='inner')

poor<-(data.frame(combi_a$poor))
colnames(poor)<-'poor'

combi_a$id<-NULL
combi_a$poor<-NULL
combi_a$poor

na_count<-function(x){
  sum(is.na(x))/length(x)
}

na_variables<-lapply(combi_a,na_count)
na_variables<-gather(data.frame(na_variables))
ggplot(na_variables, aes(value))+
  geom_histogram()+ xlim(-1, 5)

nzv <- nearZeroVar(combi_a)
combi_a<-combi_a[,-nzv]
dim(combi_a)
combi_a1<-cbind(combi_a,poor)
table(combi_a1$poor)
control <- rfeControl(functions=rfFuncs, method="cv", number=15)
# run the RFE algorithm
resultsa <- rfe(combi_a1[,1:length(colnames(combi_a1))-1], combi_a1[,length(colnames(combi_a1))], 
                rfeControl=control)
# summarize the results
print(resultsa)
# list the chosen features
predictors(resultsa)-> topvarsa
# plot the results
plot(resultsa, type=c("g", "o"))
summary(resultsa)

combi_a1%>%select(topvarsa,'poor')->combi_a2

ctrl <- safsControl(functions = rfSA)
obja <- safs(x = combi_a2[,1:230], 
            y = combi_a2[,231],
            iters = 15,
            safsControl = ctrl,
            metric = "Accuracy",
            method = "xgbTree")
obja$optVariables->dfa
combi_a2%>%select(dfa,'poor')->dfa
Accuraciesa <- c(0.00)

  inTrain <- createDataPartition(combi_a2$poor, p = .80, list = FALSE)
  Xtrainc<-combi_a2[inTrain,c(1:length(colnames(combi_a2))-1)]
  Ytrainc <- combi_a2[inTrain,length(colnames(combi_a2))]
  Xtestc <- combi_a2[-inTrain,c(1:length(colnames(combi_a2))-1)]
  Ytestc <- combi_a2[-inTrain,21]
  combi_a2.navg <- train(Xtrainc, 
                         Ytrainc, method = "xgbTree",
                         trControl = trainControl(method = "cv"))
  Accuraciesa <- confusionMatrix(Ytestc, 
                                   predict(combi_a2.navg, newdata=Xtestc))$overall["Accuracy"]


library(rrcov)
library(caret)
library(ca)
library(plyr)
library(FactoMineR)
library(tidyverse)
library(rlist)
library(data.table)
library(FactoInvestigate)
library(factoextra)
library(mlbench)
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

str(combi_a)
dim(combi_a)

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

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(combi_a1[,1:230], combi_a1[,231], rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)->topvars
# plot the results
plot(results, type=c("g", "o"))
summary(results)
topvars<-head(results$variables$var,n=20)


combi_a1%>%select(topvars,'poor')->combi_a2

Accuracies <- c(0.00)
for (i in seq(20))
{
  # sample
  inTrain <- createDataPartition(combi_a2$poor, p = .80, list = FALSE)
  combi_a2.navg <- randomForest(combi_a2[c(1:20)], 
                              combi_a2$poor, subset = inTrain)
  Accuracies[i] <- confusionMatrix(combi_a2$poor[-inTrain], predict(combi_a2.navg, combi_a2[-inTrain,-26], type = "class"))$overall["Accuracy"]
}
# turn output on again
summary(Accuracies)
plot(density(Accuracies))

aholdtest<-read.csv('A_hhold_test.csv',stringsAsFactors = TRUE, header = TRUE)
aindivtest<-read.csv('A_indiv_test.csv',stringsAsFactors = TRUE, header = TRUE)

aindivtest <- subset(aindivtest, select = -c(iid,country) )
aindivtest <- aindivtest[!duplicated(aindivtest$id), ]

aholdtest <-join(aholdtest, aindivtest, by='id', type='inner')
aholdtest<-select(aholdtest,topvars)
predict(combi_a2.navg,newdata=aholdtest)


combi_a2[-inTrain,]

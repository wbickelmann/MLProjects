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
library(dplyr)

combi_b<-read.csv("B_hhold_train.csv",stringsAsFactors = TRUE, header = TRUE)
combi_b_indiv <- read.csv("B_indiv_train.csv", stringsAsFactors = TRUE, header = TRUE)
combi_b_indiv <- subset( combi_b_indiv, select = -c(iid, poor,country ) )
combi_b_indiv <- combi_b_indiv[!duplicated(combi_b_indiv$id), ]

combi_b <-join(combi_b, combi_b_indiv, by='id', type='inner')

combi_b$id<-NULL


na_count<-function(x){
  sum(is.na(x))/length(x)
}

na_variables<-lapply(combi_b,na_count)
na_variables<-gather(data.frame(na_variables))
ggplot(na_variables, aes(value))+
  geom_histogram()+ xlim(-1, 5)


gtg<-which(colSums(is.na(combi_b))>300)
combi_b1<-na.omit(combi_b[,-c(gtg)])
combi_b1$wJthinfa.1<-NULL
colnames(combi_b1)[colnames(combi_b1)=="wJthinfa"] <- "wJthinfa.x"


poor<-(data.frame(combi_b1$poor))
colnames(poor)<-'poor'
combi_b1$poor<-NULL



nzv <- nearZeroVar(combi_b1)
combi_b1<-combi_b1[,-nzv]
dim(combi_b1)
combi_b1<-cbind(combi_b1,poor)

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



control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
resultsb <- rfe(combi_b1[,1:418], combi_b1[,419], rfeControl=control)
# summarize the results
print(resultsb)
# list the chosen features
predictors(resultsb)->topvarsb
# plot the results
plot(resultsb, type=c("g", "o"))
summary(resultsb)

combi_b1%>%select(topvarsb,'poor')->combi_b2
Accuraciesb <- c(0.00)
table(combi_b1$poor)

for (i in seq(2))
{
  inTrain <- createDataPartition(combi_b2$poor, p = .80, list = FALSE)
  Xtrainc<-combi_b2[inTrain,c(1:20)]
  Ytrainc <- combi_b2[inTrain,21]
  Xtestc <- combi_b2[-inTrain,c(1:20)]
  Ytestc <- combi_b2[-inTrain,21]
  combi_b2.navg <- train(Xtrainc, 
                         Ytrainc, method = "avNNet",
                         trControl = trainControl(method = "cv"))
  Accuraciesb[i] <- confusionMatrix(Ytestc, 
                                   predict(combi_b2.navg, newdata=Xtestc))$overall["Accuracy"]
}
summary(Accuraciesb)

plot(density(Accuraciesb))


combi_t<-read.csv("B_hhold_test.csv",stringsAsFactors = TRUE, header = TRUE)
combi_t_indiv <- read.csv("B_indiv_test.csv", stringsAsFactors = TRUE, header = TRUE)
combi_t_indiv <- subset( combi_t_indiv, select = -c(iid,country ))
combi_t_indiv <- combi_t_indiv[!duplicated(combi_t_indiv$id), ]

combi_t <-inner_join(combi_t, combi_t_indiv, by='id')
combi_t <- combi_t[, !duplicated(colnames(combi_t))]

id<-data.frame(combi_t$id)
names(id) <- ("id")
country<-data.frame(combi_t$country)
names(country) <- ("country")

combi_t1<-select(combi_t,topvars)

combi_b3<-combi_b2[-11]

combi_t1 <- rbind(combi_b3[1, ], combi_t1)
combi_t1 <- combi_t1[-1,]
predict(combi_b2.navg,newdata=combi_t1,type='prob')->poor_pred
bprediction<-cbind(id,country,poor_pred[2])
names(bprediction)<-c("id","country","poor")
View(bprediction)

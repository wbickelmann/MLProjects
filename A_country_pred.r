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

dim(combi_a)
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
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
resultsa <- rfe(combi_a1[,1:230], combi_a1[,231], rfeControl=control)
# summarize the results
print(resultsa)
# list the chosen features
predictors(resultsa)-> topvarsa
# plot the results
plot(resultsa, type=c("g", "o"))
summary(resultsa)

combi_a1%>%select(topvarsa,'poor')->combi_a2
Accuraciesa <- c(0.00)

for (i in seq(2))
{
  inTrain <- createDataPartition(combi_a2$poor, p = .80, list = FALSE)
  Xtrainc<-combi_a2[inTrain,c(1:20)]
  Ytrainc <- combi_a2[inTrain,21]
  Xtestc <- combi_a2[-inTrain,c(1:20)]
  Ytestc <- combi_a2[-inTrain,21]
  combi_a2.navg <- train(Xtrainc, 
                         Ytrainc, method = "avNNet",
                         trControl = trainControl(method = "cv"))
  Accuraciesa[i] <- confusionMatrix(Ytestc, 
                                   predict(combi_a2.navg, newdata=Xtestc))$overall["Accuracy"]
}
summary(Accuraciesa)


combi_t<-read.csv("A_hhold_test.csv",stringsAsFactors = TRUE, header = TRUE)
combi_t_indiv <- read.csv("A_indiv_test.csv", stringsAsFactors = TRUE, header = TRUE)
combi_t_indiv <- subset( combi_t_indiv, select = -c(iid,country ))
combi_t_indiv <- combi_t_indiv[!duplicated(combi_t_indiv$id), ]

combi_t <-inner_join(combi_t, combi_t_indiv, by='id')
combi_t <- combi_t[, !duplicated(colnames(combi_t))]

id<-data.frame(combi_t$id)
names(id) <- ("id")
country<-data.frame(combi_t$country)
names(country) <- ("country")

combi_t1<-select(combi_t,topvarsa)

combi_a3<-combi_a2[-21]
combi_t1 <- rbind(combi_a3[1, ], combi_t1)
combi_t1 <- combi_t1[-1,]
predict(combi_a2.navg,newdata=combi_t1,type='prob')->poor_pred
aprediction<-cbind(id,country,poor_pred[2])
names(aprediction)<-c("id","country","poor")
View(aprediction)

pred_dataframe<-rbind(aprediction,bprediction,cprediction)
pred_dataframe<-pred_dataframe %>% mutate(poor = round(poor, 1))
write.csv(pred_dataframe,file="submission.csv",row.names = FALSE)

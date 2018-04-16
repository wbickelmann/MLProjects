library(caret)
library(ca)
library(FactoMineR)
library(tidyverse)
library(rlist)
library(data.table)
library(rpart)
library(rpart.plot)
Ahold<-read.csv("A_hhold_train.csv",stringsAsFactors = TRUE, header = TRUE)

str(Ahold)
#first step is to reduct dimensions of this very high dimesional dataset. 
#The easiest way is to remove variables with high amount of NAs

na_count<-function(x){
  sum(is.na(x))/length(x)
}

na_variables<-lapply(Ahold,na_count)
na_variables<-gather(data.frame(na_variables))
ggplot(na_variables, aes(value))+
  geom_histogram()+ xlim(-1, 5)



#results of above option reveal no NAs in the dataset. Next step is to try to use a dimension reduction technique
#Next to eliminate variables with little variable variation with nearZeroVar()
Ahold$id<-NULL
nzv <- nearZeroVar(Ahold)
Ahold1<-Ahold[,-nzv]
Ahold1[,206]<-Ahold1$poor
Ahold1[99]<-NULL
k<-lapply(Ahold1,is.factor)
which(k==FALSE)

MCA(Ahold1,quanti.sup=c(19,66,98,174))
MCA_DIMS<-MCA(Ahold1,quanti.sup=c(19,66,98,174))
summary(MCA_DIMS)
?barplot
eig.val <- MCA_DIMS$eig
barplot(eig.val[, 2], 
        names.arg = 1:nrow(eig.val), 
        main = "Variances Explained by Dimensions (%)",
        xlab = "Principal Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue",
        xlim = c(0,150))
# Add connected line segments to the plot
lines(x = 1:nrow(eig.val), eig.val[, 2], 
      type = "b", pch = 19, col = "red")

plot(MCA_DIMS, indivisible=c("var","qual.sup"),cex=.07,select="contrib 20", axes=1:2)
eig.val[,3]
MCA_DIMS

weights<-data.frame(MCA_DIMS$call$marge.col)
setDT(weights, keep.rownames = TRUE)
names(weights)<-c("Variable","Weight")
keep<-as.character(tail(arrange(weights,desc(Weight)),n=100)$Variable)
print(keep)
Ahold2<-Ahold1[,keep]
dplyr::select(Ahold1,"RpXJc")
tree1<-rpart(poor~., data=Ahold1)
rpart.plot(tree1)
varImp(tree1)

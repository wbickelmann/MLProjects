---
title: "Dimension Reduction"
author: "William Bickelmann"
date: "February 7, 2018"
output: html_document
---
## The packages
Loading inpackages necesarry for the analysis

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(caret)
library(ca)
library(FactoMineR)
library(tidyverse)
library(rlist)
library(data.table)
```

## Loading and summarizing the data
Given the complexity of the dataset, it is necesarry to represent the data as downloaded originally

```{r}
Ahold<-read.csv("A_hhold_train.csv",stringsAsFactors = TRUE, header = TRUE)
str(Ahold)
dim(Ahold)
```

## Addressing NAs

At this stage dimension reduction is the priority, before one can make analysis
```{r pressure, echo=FALSE}

na_count<-function(x){
  sum(is.na(x))/length(x)
}

na_variables<-lapply(Ahold,na_count)
na_variables<-gather(data.frame(na_variables))
ggplot(na_variables, aes(value))+
  geom_histogram()+ xlim(-1, 5)
```
Results of above option reveal no NAs in the dataset. Next step is to try to use a dimension reduction technique which will reveal and remove variables which have little to no variability. A caret function can be used for this

#nearZeroVar
```{r}
Ahold$id<-NULL
nzv <- nearZeroVar(Ahold)
Ahold1<-Ahold[,-nzv]
dim(Ahold1)
Ahold1[,206]<-Ahold1$poor
Ahold1[99]<-NULL
```

This removed 140 variables. 
The dataset now has 205b variables. Still too large though

##Pricipal Componets for factors: Multiple Correspondence Analysis

```{r}
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

```

## Isolating Important Variables

```{r}

weights<-data.frame(MCA_DIMS$call$marge.col)
setDT(weights, keep.rownames = TRUE)
names(weights)<-c("Variable","Weight")
keep<-as.character(tail(arrange(weights,desc(Weight)),n=100)$Variable)
print(keep)
```


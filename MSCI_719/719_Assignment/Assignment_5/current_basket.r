---
title: "MSCI 719 - Assignment 5"
author: "Student ID - 20864394"
output: pdf_document
---
  
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
#install.packages("csv")
#install.packages("xlsx")
#install.packages("arules")
#install.packages("arulesViz")
#install.packages("datasets")

# Load the Libraries
library(dplyr)
library(ggplot2)
library(readxl)     # Read excel
library(readr)      # Read csv
library(arules)
library(arulesViz)
library(datasets)
```

mydata=read.csv(file.choose())
colnames(mydata)
summary(mydata$Created.On)
mydata$Created.On=as.Date(mydata$Created.On)
mydata$Created.On
table=as.data.frame(table(mydata$Member))
a=table[1:20,]
a
mydata2=mydata[mydata$Member%in%a$Var1,]
View(mydata2)
unique(mydata2$Member)
library(arules)
colnames(mydata2)
tData <- as (mydata2, "transactions")
mydata2[,c(2,5)]
################
################
transactions_org=read.csv(file.choose())
colnames(transactions_org)
transactions=transactions_org[,c("Order","Description")]
transactions_merge=transactions_org[,c("Order","Member")]
View(transactions_merge)
View(transactions)
transactions=transactions[order(transactions$Order),]
View(transactions)
#?order
transactions$time=unlist(tapply(transactions$Order,transactions$Order))
View(transactions)
transactions_final=reshape(transactions, idvar="Order", direction="wide")
nrow(transactions_final)
View(transactions_final)
colnames(transactions_final)
a=merge(transactions_final,transactions_merge,by="Order")
View(f)
f=unique(a)
nrow(f)
?unlist
?tapply
?reshape
setwd('C:/Users/Tahmid Bari/Documents')
write.csv(a,
C:/Users/Tahmid Bari/Documents/transactions_final.csv')
quote = FALSE,row.names = FALSE)
transactions_final=read.transactions(C:/Users/Tahmid Bari/Documents/transactions_final.csv',format = 'basket',sep = ',',quote = "")
f=c("Bread", "Glucose", "Honey")
transactions_final
assocation.rules <- apriori(transactions_final, parameter = list(supp=0.001, conf = 0.01, maxlen=1000),appearance = list(lhs=p,default="rhs"))
summary(assocation.rules)
b[1:2]
?order
s=b[order(b$support,decreasing = TRUE),]
k
h=k[3,2:5]
h
p=unname(unlist(h[1,]))
p=na.omit(p)
pp
is.na(p)
?inspect

l=DATAFRAME(association.rules,seperate = TRUE, setStart = '', itemSep = ',', setEnd = '')
colnames(1)
e=1[!1$LHS=="",]
e
?unname
?unlist
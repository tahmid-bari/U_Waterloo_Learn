---
title: "Assignment 6"
subject: "MSCI 719"
author: "20864394"
output: pdf_document
---

#install.packages("csv")
#install.packages("data.table")
#install.packages("factoextra")
#install.packages("NbClust")
#install.packages("factoextra")
#install.packages("ggplot2")
library(dplyr)
library("data.table")
library(factoextra)
library(NbClust)
library(ggpubr)
library(factoextra)
library(ggplot2)

## Set working directory
setwd("C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_6")
getwd() ## Check working directory

AS6Data <- read.csv("Assignment6_Data.csv") ## Read the data file {.build}
file.exists("C:\\Users\\Tahmid Bari\\Desktop\\UWaterloo\\Study\\MSCI_719\\719_Assignment\\Assignment_6\\Assignment6_Data.csv")

AS6Data=read.csv(file.choose()) #Reading and Loading csv file

#Converting Categorical data into Numerical data 
df_AS6Data = data.frame(AS6Data)
x <- data.matrix(df_AS6Data)
x <- as.data.frame(x)
View(x)

#Calculating Sum
Total=rowSums(x[,5:28])
AS6Data=cbind(x,Total)
str(AS6Data)
summary(AS6Data)
View(AS6Data)

#Calculating Stockouts and Not-stockouts
stockouts=AS6Data[(AS6Data$Total>=1 & AS6Data$hour.24==0),]
not_stockouts=AS6Data[!(AS6Data$Total>=1 & AS6Data$hour.24==0),]

#Converting into Data Table
a=stockouts[,5:28]
b=setDT(a)

#Converting back into Data Frame
b[, names(a) := Reduce(`+`, b, accumulate = TRUE)]
stockouts_cumulative=as.data.frame(b)

#View(stockouts_cumulative)
stockouts_cumulative[5:28,]
stockout_time=vector(mode="numeric")
for (i in 1:nrow(stockouts_cumulative))
  {
    stockout_time[i]=match(max(stockouts_cumulative[i,]),stockouts_cumulative[i,])
  }
stockouts=cbind(stockouts,stockout_time)
View(stockouts)

#Elbow Method (include categorical variable)
fviz_nbclust(not_stockouts[,3:28], kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)+
labs(subtitle = "Elbow method")

#K-means Clustering
clusters <- kmeans(not_stockouts[,3:28],4, nstart = 20)
not_stockouts=cbind(not_stockouts,clusters$cluster)
centroids=clusters$center
centroids[4,]

#Plot Cluster
fviz_cluster(clusters, data = not_stockouts[,3:28],
             palette = c("#2E9FDF", "#BB0035", "#E7B800", "#0000FF"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())

#Find true demands
true_demand=vector(mode="numeric")
View(stockouts)
nrow(stockouts)
for (i in 1:nrow(stockouts))
  {
    p=stockouts[i,5:(stockouts[i,30]+3)]
    rownames(p)="p"
    k_1=centroids[1,1:stockouts[i,30]]
    k_2=centroids[2,1:stockouts[i,30]]
    k_3=centroids[3,1:stockouts[i,30]]
    k_4=centroids[4,1:stockouts[i,30]]
    mat <- rbind(k_1, k_2, k_3,k_4,p)
    mat
    dist_matrix=as.matrix(dist(mat, method = "manhattan"))
    determined_cluster=match(min(dist_matrix[5,1:4]),dist_matrix[5,1:4])
    determined_cluster
    lost_percentage=sum(centroids[determined_cluster,(stockouts[i,30]+2):24])
    lost_percentage
    true_demand[i]=stockouts[i,2]/(1-lost_percentage)
}
true_demand
stockouts=cbind(stockouts,true_demand)
View(stockouts)
colnames(stockouts)
colnames(not_stockouts)
View(stockouts[,c(2,31)])
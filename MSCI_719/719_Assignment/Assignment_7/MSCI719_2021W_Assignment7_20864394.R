---
title: "Assignment 7"
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
#install.packages("feather")
#install.packages("party")
#install.packages("forecast")
#install.packages("data.table")
#install.packages("ggforce")
#install.packages("animation")
#install.packages(pkgs)

library(dplyr)
library("data.table")
library(factoextra)
library(NbClust)
library(ggpubr)
library(factoextra)
library(ggplot2)
library(feather) # data import
library(data.table) # data handle
library(rpart) # decision tree method
library(rpart.plot) # tree plot
library(party) # decision tree method
library(forecast) # forecasting methods
library(ggforce) # visualization tools
library(plotly) # interactive visualizations
library(grid) # visualizations
library(animation) # gif
library(reshape2)
library(magrittr)


## Set working directory
setwd("C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_7")
getwd() ## Check working directory

AS7Data <- read.csv("Assignment7-First-exposures.csv") ## Read the data file {.build}
file.exists("C:\\Users\\Tahmid Bari\\Desktop\\UWaterloo\\Study\\MSCI_719\\719_Assignment\\Assignment_6\\Assignment7-First-exposures.csv")

#AS7Data=read.csv(file.choose()) #Reading and Loading csv file
str(AS7Data)
summary(AS7Data)

##############_Read_The_AS6Data_Data_#########
#Taking the (True Demand) values from Assignment 6

AS7Data=read.csv(file.choose())

Total=rowSums(AS7Data[,4:27])
Total
AS7Data=cbind(AS7Data,Total)

stockouts=AS7Data[(AS7Data$Total>=1 & AS7Data$hour.24==0),]
not_stockouts=AS7Data[!(AS7Data$Total>=1 & AS7Data$hour.24==0),]
not_stockouts$Dep_category <- sapply(not_stockouts$Department, function(t){(if (t == 'Polos&Tees') 1 else 0)})
not_stockouts$Part_of_day <- sapply(not_stockouts$Event.Part.of.the.day, function(t){(if (t == 'Morning') 1 else 0)})
not_stockouts <- not_stockouts[,c(1,2,3,30,31,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29)]

#library("data.table")
a=stockouts[,4:27]
b=setDT(a)
b[, names(a) := Reduce(`+`, b, accumulate = TRUE)]
stockouts_cumulative=as.data.frame(b)
stockout_time=vector(mode="numeric")
for (i in 1:nrow(stockouts_cumulative)){
  stockout_time[i]=match(max(stockouts_cumulative[i,]),stockouts_cumulative[i,])
}
stockout_time
stockouts=cbind(stockouts,stockout_time)

##############_Elbow_Method_###########
pkgs <- c("factoextra",  "NbClust")
#library(factoextra)
#library(NbClust)
fviz_nbclust(not_stockouts[,4:29], kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

########_Clustering_K-Means_######
clusters <- kmeans(not_stockouts[,4:29],4, nstart = 20)
not_stockouts=cbind(not_stockouts,clusters$cluster)
clusters$center
centroids=clusters$center

########_Plot_Cluster_########
#library(ggpubr)
#library(factoextra)
fviz_cluster(clusters, data = not_stockouts[,4:29],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#0000FF"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())

##########_Find_True_Demands_########
true_demand=vector(mode="numeric")
for (i in 1:nrow(stockouts)){
  p=stockouts[i,4:(stockouts[i,30]+2)]
  rownames(p)="p"
  k_1=centroids[1,1:stockouts[i,30]]
  k_2=centroids[2,1:stockouts[i,30]]
  k_3=centroids[3,1:stockouts[i,30]]
  k_4=centroids[4,1:stockouts[i,30]]
  mat <- rbind(k_1, k_2, k_3,k_4, p)
  mat
  dist_matrix=as.matrix(dist(mat, method = "manhattan"))
  determined_cluster=match(min(dist_matrix[5,1:4]),dist_matrix[5,1:4])
  determined_cluster
  lost_percentage=sum(centroids[determined_cluster,(stockouts[i,30]+1):24])
  lost_percentage
  true_demand[i]=stockouts[i,28]/(1-lost_percentage)
}
true_demand
stockouts=cbind(stockouts,true_demand)
write.csv(stockouts,'C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_7/True_demand_stockouts.csv',quote=FALSE,row.names=FALSE)
write.csv(not_stockouts,'C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_7/True_demand_nonstockouts.csv',quote=FALSE,row.names=FALSE)

##########_True_Demands_Not_Stocks_########
true_demand=not_stockouts$Total.sales
not_stockouts=cbind(not_stockouts,true_demand)
not_stockouts$Dep_category <- NULL
not_stockouts$Part_of_day <- NULL
not_stockouts$`clusters$cluster` <- NULL
stockouts$stockout_time <- NULL
Demands=rbind(stockouts[,c(1,30)],not_stockouts[,c(1,30)])
Demands <- Demands[order(Demands$Item.),]

############_Import_New_Data_########
AS7Data_2=read.csv(file.choose())
AS7Data_2
Prediction_Data <- merge(Demands,AS7Data_2,by="ï..Item.")
colnames(Prediction_Data)

################_Regression_Trees_###########
#library(rpart)
#library(rpart.plot)
Prediction_Data=Prediction_Data[,2:12]
regression_tree <- rpart (formula = true_demand ~., data= Prediction_Data,method  = "anova")
regression_tree
rpart.plot(regression_tree)

################_Optimization_Model_##############
Data_test=read.csv(file.choose())
Data_EventA <- Data_test[(Data_test$Event == "A"),]
Data_EventB <- Data_test[(Data_test$Event == "B"),]

################_Optimization_Model_for_Event_A_##############
Prices_EventA=c(25.99,30.99,35.99,40.99)
Data_test_EventA <- Data_EventA[rep(seq_len(nrow(Data_EventA)), each=4), ]
possible_k_EventA <- seq(nrow(Data_EventA)*min(Prices_EventA), nrow(Data_EventA)*max(Prices_EventA), by=5)
possible_k_EventA

P1=rep(Prices_EventA,nrow(Data_EventA))
P1
D1=vector(mode="numeric")
Final_objectives_A=vector(mode="numeric")
Final_solutions_A=matrix(nrow=length(possible_k_EventA),ncol=length(Prices_EventA)*nrow(Data_EventA))
for (n1 in 1:length(possible_k_EventA)){
  for (i1 in 1:length(P1)){
    Data_EventA$Price=P1[i1]
    Data_EventA$Relative_Price_of_Competing_Styles=P1[i1]/(possible_k_EventA[n1]/4)
    D1[i1]=predict(regression_tree,Data_EventA[i1,])
  }
  library(lpSolve)
  Obj_coeff_A =D1*P1
  Obj_coeff_A
  Cons_coeff_A=matrix(c(1,1,1,1,0,0,0,0,0,0,0,0,
                        0,0,0,0,1,1,1,1,0,0,0,0,
                        0,0,0,0,0,0,0,0,1,1,1,1,
                        P1[1],P1[2],P1[3],P1[4],P1[5],P1[6],P1[7],P1[8],P1[9],P1[10],P1[11],P1[12]), nrow = 4, byrow = TRUE)
  
  Cons_coeff_A
  Dir_A=c("==",
          "==",
          "==", 
          "==")
  Rhs_A=c(1,
          1,
          1,
          possible_k_EventA[n1])
  
  Model_A=lp("max", Obj_coeff_A, Cons_coeff_A, Dir_A, Rhs_A, all.bin = TRUE)
  Model_A
  Final_objectives_A[n1]=Model_A$objval
  Final_solutions_A[n1,]=Model_A$solution
}
D1
Final_solutions_A
Final_objectives_A
Final_solutions_A[match(max(Final_objectives_A),Final_objectives_A),]


################_Optimization_Model_for_Event_B_##############
Prices_EventB=c(45.99,50.99)
Data_test_EventB <- Data_EventB[rep(seq_len(nrow(Data_EventB)), each=2), ]
possible_k_EventB <- seq(nrow(Data_EventB)*min(Prices_EventB), nrow(Data_EventB)*max(Prices_EventB), by=5)
possible_k_EventB

P2=rep(Prices_EventB,nrow(Data_EventB))
P2
D2=vector(mode="numeric")
Final_objectives_B=vector(mode="numeric")
Final_solutions_B=matrix(nrow=length(possible_k_EventB),ncol=length(Prices_EventB)*nrow(Data_EventB))
for (n2 in 1:length(possible_k_EventB)){
  for (i2 in 1:length(P2)){
    Data_EventB$Price=P2[i2]
    Data_EventB$Relative_Price_of_Competing_Styles=P2[i2]/(possible_k_EventB[n2]/2)
    D2[i2]=predict(regression_tree,Data_EventB[i2,])
  }
  library(lpSolve)
  Obj_coeff_B =D2*P2
  Obj_coeff_B
  Cons_coeff_B=matrix(c(1,1,0,0,
                        0,0,1,1,
                        P2[1],P2[2],P2[3],P2[4]), nrow = 3, byrow = TRUE)
  
  Cons_coeff_B
  Dir_B=c("==",
          "==",
          "==")
  Rhs_B=c(1,
          1,
          possible_k_EventB[n2])
  
  Model_B=lp("max", Obj_coeff_B, Cons_coeff_B, Dir_B, Rhs_B, all.bin = TRUE)
  Model_B
  Final_objectives_B[n2]=Model_B$objval
  Final_solutions_B[n2,]=Model_B$solution
}
D2
Final_solutions_B
Final_objectives_B
Final_solutions_B[match(max(Final_objectives_B),Final_objectives_B),]

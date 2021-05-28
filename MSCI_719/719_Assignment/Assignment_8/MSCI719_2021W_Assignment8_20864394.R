---
title: "Assignment 8"
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
#install.packages(fitdistrplus)

library(dplyr)
library(ggplot2)
library("fitdistrplus")


## Set working directory
setwd("C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_8")
getwd() ## Check working directory

AS8Data <- read.csv("Assignment8-Data.csv") ## Read the data file {.build}
file.exists("C:\\Users\\Tahmid Bari\\Desktop\\UWaterloo\\Study\\MSCI_719\\719_Assignment\\Assignment_8\\Assignment8-Data.csv")

#AS7Data=read.csv(file.choose()) #Reading and Loading csv file
str(AS8Data)
summary(AS8Data)

##############_Read_The_AS6Data_Data_#########

AS8Data=read.csv(file.choose())

a=fitdist(AS8Data[,2],"pois")
b=fitdist(AS8Data[,2],"geom")
a
plot.legend <- c("poisson", "geometric")
denscomp(list(a, b), legendtext = plot.legend)

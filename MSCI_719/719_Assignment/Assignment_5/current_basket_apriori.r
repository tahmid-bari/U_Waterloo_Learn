---
title: "MSCI 719 - Assignment 5"
author: "Student ID - 20864394"
output: pdf_document
---

#install.packages("csv")
#install.packages("xlsx")
#install.packages("arules")
#install.packages("arulesViz")
#install.packages("datasets")
#install.packages("utils")
  
# Load the Libraries
library(dplyr)
library(ggplot2)
library(readxl)     # Read excel
library(readr)      # Read csv
library(arules)
library(arulesViz)
library(datasets)

mydata=read.csv(file.choose())
str(mydata)
view(mydata)
colnames(mydata)
summary(mydata$Member)
mydata$Member
table=as.data.frame(table(mydata$Member))
a=table[1:20,]
a
mydata$Count <- ave(1:nrow(mydata), mydata$Member, FUN = length) # Creating a count variable

trans_with_count=mydata[order(-mydata$Count),] # Ordering the count added
View(trans_with_count)


# An array consisting of 'Order' and 'description'
trans1 = trans_with_count[,c("Member","Order","Count")]
View(trans1)

unique_trans1 <-head(trans1[!duplicated(trans1$Member),],20) # List of unique 'Member' by taking top 20 values
View(unique_trans1)

# Importing current basket data set and merging with the member ID
current_basket = read.csv(file.choose())
View(current_basket)
str(current_basket)
names(current_basket)[names(current_basket) == "Member"] <- "S..No"
current_basket$Member <- unique_trans1$Member
View(current_basket)
Obj_merge = current_basket[,c("S..No","Item1","Item2","Item3","Item4","Item5","Member")]
View(Obj_merge)
Obj_unique_trans1 = unique_trans1[,c("Member","Order","Count")]
View(Obj_unique_trans1)
merged_current_basket = merge(Obj_merge,Obj_unique_trans1)
View(merged_current_basket)

merged_current_basket=na.omit(merged_current_basket) # Omitting the NA
is.na(merged_current_basket)

trans2=trans1[,c("Order","Description")] # Ordering the trans1 based on "Order"
trans2=trans2[order(trans2$Order),]
View(trans2)

trans2$time = unlist(tapply(trans2$Order,trans2$Order, function(x) seq(1, len = length(x)))) #Adding a time stamp
trans_final = reshape(trans2,idvar ="Order", direction = "wide" )
View(trans_final)

trans_merge = trans1[,c("Order","Member","Count")] #Creating a merged object comprising of Member ID & Order ID
View(trans_merge)
a = merge(trans_final,trans_merge, by="Order")
View(a)
b= unique(a)
View(b)

trans2=trans1[,c("Order","Description")] #Ordering the trans1 on the basis of "Order"
trans2=trans2[order(trans2$Order),]
View(trans2)

trans2$time = unlist(tapply(trans2$Order,trans2$Order, function(x) seq(1, len = length(x)))) #Adding a time stamp
trans_final = reshape(trans2,idvar ="Order", direction = "wide" )
View(trans_final)

trans_merge = trans1[,c("Order","Member","Count")] #Creating a merged object comprising of MemberId and OrderId
View(trans_merge)
a = merge(trans_final,trans_merge, by="Order")
View(a)
b= unique(a)
View(b)


# View 1st Member
Mem1 <- b%>%
  filter(Member=="M38622")
View(Mem1)

# Data transformation of 1member into transaction object
setwd('C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5')
write.csv(Mem1[,2:43],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem1.csv',quote = FALSE,row.names = FALSE)
mem1_b=read.transactions('/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem1.csv', format = 'basket', sep=',', quote="")
View(mem1_b)
summary(mem1_b)
vector=current_basket[1,2:6]
h <- unname(unlist(vector))
h<- na.omit(h)
View(h)
#k=c("Raw Rice","Organic F&V")

##########association rules mining###################
association.rules <- apriori(mem1_b, parameter = list(supp=0.001, conf=0.001,maxlen=10000),appearance = list(lhs=h[],default="rhs"))
inspect(association.rules)
Final_rules= DATAFRAME(association.rules,separate = TRUE, setStart = '', itemSep = ',', setEnd = '')
Final_rules
t=Final_rules[!Final_rules$LHS=="",]
t1=t[order(-t$support),]
top5 <- head(t1,5)
View(top5)
write.csv(top5[,1:7],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/top5_mem1.csv', quote = TRUE,row.names = FALSE)

# View 2nd Member
Mem2 <- b%>%
  filter(Member=="M33064")
View(Mem2)

# Data transformation of 2member into transaction object
setwd('/Users/taniachauhan/Desktop/UoW/Winter 2021/MSCI 719/Assignments/Assignment 5/')
write.csv(Mem2[,2:43],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem2.csv',quote = FALSE,row.names = FALSE)
mem2_b=read.transactions('/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem2.csv', format = 'basket', sep=',', quote="")
View(mem2_b)
summary(mem2_b)
vector=current_basket[2,2:6]
h <- unname(unlist(vector))
h<- na.omit(h)
View(h)
#k=c("Raw Rice","Organic F&V")

##########association rules mining###################
association.rules <- apriori(mem2_b, parameter = list(supp=0.001, conf=0.001,maxlen=10000),appearance = list(lhs=h[],default="rhs"))
inspect(association.rules)
Final_rules= DATAFRAME(association.rules,separate = TRUE, setStart = '', itemSep = ',', setEnd = '')
Final_rules
t=Final_rules[!Final_rules$LHS=="",]
t1=t[order(-t$support),]
top52 <- head(t1,5)
View(top52)
write.csv(top52[,1:7],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/top5_mem2.csv', quote = TRUE,row.names = FALSE)


# View 3rd Member
Mem3 <- b%>%
  filter(Member=="M41747")
View(Mem3)

# Data transformation of 3member into transaction object
setwd('C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/')
write.csv(Mem3[,1:43],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem3.csv',quote = FALSE,row.names = FALSE)
mem3_b=read.transactions('/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem3.csv', format = 'basket', sep=',', quote="")
View(mem3_b)
summary(mem3_b)
vector=current_basket[3,2:6]
h <- unname(unlist(vector))
h<- na.omit(h)
View(h)

##########association rules mining###################
association.rules <- apriori(mem3_b, parameter = list(supp=0.001, conf=0.001,maxlen=10000),appearance = list(lhs=h[],default="rhs"))
inspect(association.rules)
Final_rules= DATAFRAME(association.rules,separate = TRUE, setStart = '', itemSep = ',', setEnd = '')
Final_rules
t=Final_rules[!Final_rules$LHS=="",]
t1=t[order(-t$support),]
top53 <- head(t1,5)
View(top53)
write.csv(top53[,1:7],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/top5_mem3.csv', quote = TRUE,row.names = FALSE)


# View 4th Member
Mem4 <- b%>%
  filter(Member=="M32409")
View(Mem4)

# Data transformation of 4member into transaction object
setwd('C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/')
write.csv(Mem4[,1:43],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem4.csv',quote = FALSE,row.names = FALSE)
mem4_b=read.transactions('/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem4.csv', format = 'basket', sep=',', quote="")
View(mem4_b)
summary(mem4_b)
vector=current_basket[4,2:6]
h <- unname(unlist(vector))
h<- na.omit(h)
View(h)

##########association rules mining###################
association.rules <- apriori(mem4_b, parameter = list(supp=0.001, conf=0.001,maxlen=10000),appearance = list(lhs=h[],default="rhs"))
inspect(association.rules)
Final_rules= DATAFRAME(association.rules,separate = TRUE, setStart = '', itemSep = ',', setEnd = '')
Final_rules
t=Final_rules[!Final_rules$LHS=="",]
t1=t[order(-t$support),]
top54 <- head(t1,5)
View(top54)
write.csv(top54[,1:7],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/top5_mem4.csv', quote = TRUE,row.names = FALSE)


# View 5th Member
Mem5 <- b%>%
  filter(Member=="M31966")
View(Mem5)

# Data transformation of 5member into transaction object
setwd('C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/')
write.csv(Mem5[,1:43],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem5.csv',quote = FALSE,row.names = FALSE)
mem5_b=read.transactions('/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem5.csv', format = 'basket', sep=',', quote="")
View(mem5_b)
summary(mem5_b)
vector=current_basket[5,2:6]
h <- unname(unlist(vector))
h<- na.omit(h)
View(h)

##########association rules mining###################
association.rules <- apriori(mem5_b, parameter = list(supp=0.001, conf=0.001,maxlen=10000),appearance = list(lhs=h[],default="rhs"))
inspect(association.rules)
Final_rules= DATAFRAME(association.rules,separate = TRUE, setStart = '', itemSep = ',', setEnd = '')
Final_rules
t=Final_rules[!Final_rules$LHS=="",]
t1=t[order(-t$support),]
top55 <- head(t1,5)
View(top55)
write.csv(top55[,1:7],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/top5_mem5.csv', quote = TRUE,row.names = FALSE)


# View 6th Member
Mem6 <- b%>%
  filter(Member=="M56368")
View(Mem6)

# Data transformation of 6member into transaction object
setwd('C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/')
write.csv(Mem6[,1:43],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem6.csv',quote = FALSE,row.names = FALSE)
mem6_b=read.transactions('/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem6.csv', format = 'basket', sep=',', quote="")
View(mem6_b)
summary(mem6_b)
vector=current_basket[6,2:6]
h <- unname(unlist(vector))
h<- na.omit(h)
View(h)

##########association rules mining###################
association.rules <- apriori(mem6_b, parameter = list(supp=0.001, conf=0.001,maxlen=10000),appearance = list(lhs=h[],default="rhs"))
inspect(association.rules)
Final_rules= DATAFRAME(association.rules,separate = TRUE, setStart = '', itemSep = ',', setEnd = '')
Final_rules
t=Final_rules[!Final_rules$LHS=="",]
t1=t[order(-t$support),]
top56 <- head(t1,5)
View(top56)
write.csv(top56[,1:7],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/top5_mem6.csv', quote = TRUE,row.names = FALSE)


# View 7th Member
Mem7 <- b%>%
  filter(Member=="M36432")
View(Mem7)

# Data transformation of 7member into transaction object
setwd('C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/')
write.csv(Mem7[,1:43],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem7.csv',quote = FALSE,row.names = FALSE)
mem7_b=read.transactions('/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem7.csv', format = 'basket', sep=',', quote="")
View(mem7_b)
summary(mem7_b)
vector=current_basket[7,2:6]
h <- unname(unlist(vector))
h<- na.omit(h)
View(h)

##########association rules mining###################
association.rules <- apriori(mem7_b, parameter = list(supp=0.001, conf=0.001,maxlen=10000),appearance = list(lhs=h[],default="rhs"))
inspect(association.rules)
Final_rules= DATAFRAME(association.rules,separate = TRUE, setStart = '', itemSep = ',', setEnd = '')
Final_rules
t=Final_rules[!Final_rules$LHS=="",]
t1=t[order(-t$support),]
top57 <- head(t1,5)
View(top57)
write.csv(top57[,1:7],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/top5_mem7.csv', quote = TRUE,row.names = FALSE)


# View 8th Member
Mem8 <- b%>%
  filter(Member=="M41781")
View(Mem8)

# Data transformation of 8member into transaction object
setwd('C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/')
write.csv(Mem8[,1:43],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem8.csv',quote = FALSE,row.names = FALSE)
mem8_b=read.transactions('/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem8.csv', format = 'basket', sep=',', quote="")
View(mem8_b)
summary(mem8_b)
vector=current_basket[8,2:6]
h <- unname(unlist(vector))
h<- na.omit(h)
View(h)

##########association rules mining###################
association.rules <- apriori(mem8_b, parameter = list(supp=0.001, conf=0.001,maxlen=10000),appearance = list(lhs=h[],default="rhs"))
inspect(association.rules)
Final_rules= DATAFRAME(association.rules,separate = TRUE, setStart = '', itemSep = ',', setEnd = '')
Final_rules
t=Final_rules[!Final_rules$LHS=="",]
t1=t[order(-t$support),]
top58 <- head(t1,5)
View(top58)
write.csv(top58[,1:7],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/top5_mem8.csv', quote = TRUE,row.names = FALSE)


# View 9th Member
Mem9 <- b%>%
  filter(Member=="M35538")
View(Mem9)

# Data transformation of 9member into transaction object
setwd('C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/')
write.csv(Mem9[,1:43],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem9.csv',quote = FALSE,row.names = FALSE)
mem9_b=read.transactions('/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem9.csv', format = 'basket', sep=',', quote="")
View(mem9_b)
summary(mem9_b)
vector=current_basket[9,2:6]
h <- unname(unlist(vector))
h<- na.omit(h)
View(h)

##########association rules mining###################
association.rules <- apriori(mem9_b, parameter = list(supp=0.001, conf=0.001,maxlen=10000),appearance = list(lhs=h[],default="rhs"))
inspect(association.rules)
Final_rules= DATAFRAME(association.rules,separate = TRUE, setStart = '', itemSep = ',', setEnd = '')
Final_rules
t=Final_rules[!Final_rules$LHS=="",]
t1=t[order(-t$support),]
top59 <- head(t1,5)
View(top59)
write.csv(top59[,1:7],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/top5_mem9.csv', quote = TRUE,row.names = FALSE)


# View 10th Member
Mem10 <- b%>%
  filter(Member=="M33491")
View(Mem10)

# Data transformation of 10member into transaction object
setwd('C:/Users/taniachauhan/Desktop/UoW/Winter 2021/MSCI 719/Assignments/Assignment 5/')
write.csv(Mem10[,1:43],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem10.csv',quote = FALSE,row.names = FALSE)
mem10_b=read.transactions('/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem10.csv', format = 'basket', sep=',', quote="")
View(mem10_b)
summary(mem10_b)
vector=current_basket[10,2:6]
h <- unname(unlist(vector))
h<- na.omit(h)
View(h)

##########association rules mining###################
association.rules <- apriori(mem10_b, parameter = list(supp=0.001, conf=0.001,maxlen=10000),appearance = list(lhs=h[],default="rhs"))
inspect(association.rules)
Final_rules= DATAFRAME(association.rules,separate = TRUE, setStart = '', itemSep = ',', setEnd = '')
Final_rules
t=Final_rules[!Final_rules$LHS=="",]
t1=t[order(-t$support),]
top510 <- head(t1,5)
View(top510)
write.csv(top510[,1:7],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/top5_mem10.csv', quote = TRUE,row.names = FALSE)


# View 11th Member
Mem11 <- b%>%
  filter(Member=="M48101")
View(Mem11)

# Data transformation of 11member into transaction object
setwd('C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/')
write.csv(Mem11[,1:43],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem11.csv',quote = FALSE,row.names = FALSE)
mem11_b=read.transactions('/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem11.csv', format = 'basket', sep=',', quote="")
View(mem11_b)
summary(mem11_b)
vector=current_basket[11,2:6]
h <- unname(unlist(vector))
h<- na.omit(h)
View(h)

##########association rules mining###################
association.rules <- apriori(mem11_b, parameter = list(supp=0.001, conf=0.001,maxlen=10000),appearance = list(lhs=h[],default="rhs"))
inspect(association.rules)
Final_rules= DATAFRAME(association.rules,separate = TRUE, setStart = '', itemSep = ',', setEnd = '')
Final_rules
t=Final_rules[!Final_rules$LHS=="",]
t1=t[order(-t$support),]
top511 <- head(t1,5)
View(top511)
write.csv(top511[,1:7],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/top5_mem11.csv', quote = TRUE,row.names = FALSE)


# View 12th Member
Mem12 <- b%>%
  filter(Member=="M35649")
View(Mem12)

# Data transformation of 12member into transaction object
setwd('C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/')
write.csv(Mem12[,1:43],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem12.csv',quote = FALSE,row.names = FALSE)
mem12_b=read.transactions('/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem12.csv', format = 'basket', sep=',', quote="")
View(mem12_b)
summary(mem12_b)
vector=current_basket[12,2:6]
h <- unname(unlist(vector))
h<- na.omit(h)
View(h)

##########association rules mining###################
association.rules <- apriori(mem12_b, parameter = list(supp=0.001, conf=0.001,maxlen=10000),appearance = list(lhs=h[],default="rhs"))
inspect(association.rules)
Final_rules= DATAFRAME(association.rules,separate = TRUE, setStart = '', itemSep = ',', setEnd = '')
Final_rules
t=Final_rules[!Final_rules$LHS=="",]
t1=t[order(-t$support),]
top512 <- head(t1,5)
View(top512)
write.csv(top512[,1:7],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/top5_mem12.csv', quote = TRUE,row.names = FALSE)


# View 13th Member
Mem13 <- b%>%
  filter(Member=="M45470")
View(Mem13)

# Data transformation of 13member into transaction object
setwd('C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/')
write.csv(Mem13[,1:43],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem13.csv',quote = FALSE,row.names = FALSE)
mem13_b=read.transactions('/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem13.csv', format = 'basket', sep=',', quote="")
View(mem13_b)
summary(mem13_b)
vector=current_basket[13,2:6]
h <- unname(unlist(vector))
h<- na.omit(h)
View(h)

##########association rules mining###################
association.rules <- apriori(mem13_b, parameter = list(supp=0.001, conf=0.001,maxlen=10000),appearance = list(lhs=h[],default="rhs"))
inspect(association.rules)
Final_rules= DATAFRAME(association.rules,separate = TRUE, setStart = '', itemSep = ',', setEnd = '')
Final_rules
t=Final_rules[!Final_rules$LHS=="",]
t1=t[order(-t$support),]
top513 <- head(t1,5)
View(top513)
write.csv(top513[,1:7],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/top5_mem13.csv', quote = TRUE,row.names = FALSE)


# View 14th Member
Mem14 <- b%>%
  filter(Member=="M43831")
View(Mem14)

# Data transformation of 14member into transaction object
setwd('C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/')
write.csv(Mem14[,1:43],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem14.csv',quote = FALSE,row.names = FALSE)
mem14_b=read.transactions('/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem14.csv', format = 'basket', sep=',', quote="")
View(mem14_b)
summary(mem14_b)
vector=current_basket[14,2:6]
h <- unname(unlist(vector))
h<- na.omit(h)
View(h)

##########association rules mining###################
association.rules <- apriori(mem14_b, parameter = list(supp=0.001, conf=0.001,maxlen=10000),appearance = list(lhs=h[],default="rhs"))
inspect(association.rules)
Final_rules= DATAFRAME(association.rules,separate = TRUE, setStart = '', itemSep = ',', setEnd = '')
Final_rules
t=Final_rules[!Final_rules$LHS=="",]
t1=t[order(-t$support),]
top514 <- head(t1,5)
View(top514)
write.csv(top514[,1:7],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/top5_mem14.csv', quote = TRUE,row.names = FALSE)


# View 15th Member
Mem15 <- b%>%
  filter(Member=="M33558")
View(Mem15)

# Data transformation of 15member into transaction object
setwd('C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/')
write.csv(Mem15[,1:43],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem15.csv',quote = FALSE,row.names = FALSE)
mem15_b=read.transactions('/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem15.csv', format = 'basket', sep=',', quote="")
View(mem15_b)
summary(mem15_b)
vector=current_basket[15,2:6]
h <- unname(unlist(vector))
h<- na.omit(h)
View(h)

##########association rules mining###################
association.rules <- apriori(mem15_b, parameter = list(supp=0.001, conf=0.001,maxlen=10000),appearance = list(lhs=h[],default="rhs"))
inspect(association.rules)
Final_rules= DATAFRAME(association.rules,separate = TRUE, setStart = '', itemSep = ',', setEnd = '')
Final_rules
t=Final_rules[!Final_rules$LHS=="",]
t1=t[order(-t$support),]
top515 <- head(t1,5)
View(top515)
write.csv(top515[,1:7],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/top5_mem15.csv', quote = TRUE,row.names = FALSE)


# View 16th Member
Mem16 <- b%>%
  filter(Member=="M32449")
View(Mem16)

# Data transformation of 16member into transaction object
setwd('C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/')
write.csv(Mem16[,1:43],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem16.csv',quote = FALSE,row.names = FALSE)
mem16_b=read.transactions('/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem16.csv', format = 'basket', sep=',', quote="")
View(mem16_b)
summary(mem16_b)
vector=current_basket[16,2:6]
h <- unname(unlist(vector))
h<- na.omit(h)
View(h)

##########association rules mining###################
association.rules <- apriori(mem16_b, parameter = list(supp=0.001, conf=0.001,maxlen=10000),appearance = list(lhs=h[],default="rhs"))
inspect(association.rules)
Final_rules= DATAFRAME(association.rules,separate = TRUE, setStart = '', itemSep = ',', setEnd = '')
Final_rules
t=Final_rules[!Final_rules$LHS=="",]
t1=t[order(-t$support),]
top516 <- head(t1,5)
View(top516)
write.csv(top516[,1:7],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/top5_mem16.csv', quote = TRUE,row.names = FALSE)


# View 17th Member
Mem17 <- b%>%
  filter(Member=="M52629")
View(Mem17)

# Data transformation of 17member into transaction object
setwd('C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/')
write.csv(Mem17[,1:43],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem17.csv',quote = FALSE,row.names = FALSE)
mem17_b=read.transactions('/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem17.csv', format = 'basket', sep=',', quote="")
View(mem17_b)
summary(mem17_b)
vector=current_basket[17,2:6]
h <- unname(unlist(vector))
h<- na.omit(h)
View(h)

##########association rules mining###################
association.rules <- apriori(mem17_b, parameter = list(supp=0.001, conf=0.001,maxlen=10000),appearance = list(lhs=h[],default="rhs"))
inspect(association.rules)
Final_rules= DATAFRAME(association.rules,separate = TRUE, setStart = '', itemSep = ',', setEnd = '')
Final_rules
t=Final_rules[!Final_rules$LHS=="",]
t1=t[order(-t$support),]
top517 <- head(t1,5)
View(top517)
write.csv(top517[,1:7],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/top5_mem17.csv', quote = TRUE,row.names = FALSE)


# View 17th Member
Mem17 <- b%>%
  filter(Member=="M52629")
View(Mem17)

# Data transformation of 17member into transaction object
setwd('C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/')
write.csv(Mem17[,1:43],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem17.csv',quote = FALSE,row.names = FALSE)
mem17_b=read.transactions('/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem17.csv', format = 'basket', sep=',', quote="")
View(mem17_b)
summary(mem17_b)
vector=current_basket[17,2:6]
h <- unname(unlist(vector))
h<- na.omit(h)
View(h)

##########association rules mining###################
association.rules <- apriori(mem17_b, parameter = list(supp=0.001, conf=0.001,maxlen=10000),appearance = list(lhs=h[],default="rhs"))
inspect(association.rules)
Final_rules= DATAFRAME(association.rules,separate = TRUE, setStart = '', itemSep = ',', setEnd = '')
Final_rules
t=Final_rules[!Final_rules$LHS=="",]
t1=t[order(-t$support),]
top517 <- head(t1,5)
View(top517)
write.csv(top517[,1:7],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/top5_mem17.csv', quote = TRUE,row.names = FALSE)


# View 18th Member
Mem18 <- b%>%
  filter(Member=="M55932")
View(Mem18)

# Data transformation of 18member into transaction object
setwd('C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/')
write.csv(Mem18[,1:43],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem18.csv',quote = FALSE,row.names = FALSE)
mem18_b=read.transactions('/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem18.csv', format = 'basket', sep=',', quote="")
View(mem18_b)
summary(mem18_b)
vector=current_basket[18,2:6]
h <- unname(unlist(vector))
h<- na.omit(h)
View(h)

##########association rules mining###################
association.rules <- apriori(mem18_b, parameter = list(supp=0.001, conf=0.001,maxlen=10000),appearance = list(lhs=h[],default="rhs"))
inspect(association.rules)
Final_rules= DATAFRAME(association.rules,separate = TRUE, setStart = '', itemSep = ',', setEnd = '')
Final_rules
t=Final_rules[!Final_rules$LHS=="",]
t1=t[order(-t$support),]
top518 <- head(t1,5)
View(top518)
write.csv(top518[,1:7],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/top5_mem18.csv', quote = TRUE,row.names = FALSE)


# View 19th Member
Mem19 <- b%>%
  filter(Member=="M78720")
View(Mem19)

# Data transformation of 19member into transaction object
setwd('C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/')
write.csv(Mem19[,1:43],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem19.csv',quote = FALSE,row.names = FALSE)
mem19_b=read.transactions('/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem19.csv', format = 'basket', sep=',', quote="")
View(mem19_b)
summary(mem19_b)
vector=current_basket[19,2:6]
h <- unname(unlist(vector))
h<- na.omit(h)
View(h)

##########association rules mining###################
association.rules <- apriori(mem19_b, parameter = list(supp=0.001, conf=0.001,maxlen=10000),appearance = list(lhs=h[],default="rhs"))
inspect(association.rules)
Final_rules= DATAFRAME(association.rules,separate = TRUE, setStart = '', itemSep = ',', setEnd = '')
Final_rules
t=Final_rules[!Final_rules$LHS=="",]
t1=t[order(-t$support),]
top519 <- head(t1,5)
View(top519)
write.csv(top519[,1:7],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/top5_mem19.csv', quote = TRUE,row.names = FALSE)

# View 20th Member
Mem20 <- b%>%
  filter(Member=="M43977")
View(Mem20)

# Data transformation of 20member into transaction object
setwd('C:/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/')
write.csv(Mem20[,1:43],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem20.csv',quote = FALSE,row.names = FALSE)
mem20_b=read.transactions('/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/mem20.csv', format = 'basket', sep=',', quote="")
View(mem20_b)
summary(mem20_b)
vector=current_basket[20,2:6]
h <- unname(unlist(vector))
h<- na.omit(h)
View(h)

##########association rules mining###################
association.rules <- apriori(mem20_b, parameter = list(supp=0.001, conf=0.001,maxlen=10000),appearance = list(lhs=h[],default="rhs"))
inspect(association.rules)
Final_rules= DATAFRAME(association.rules,separate = TRUE, setStart = '', itemSep = ',', setEnd = '')
Final_rules
t=Final_rules[!Final_rules$LHS=="",]
t1=t[order(-t$support),]
top520 <- head(t1,5)
View(top520)
write.csv(top520[,1:7],'/Users/Tahmid Bari/Desktop/UWaterloo/Study/MSCI_719/719_Assignment/Assignment_5/top5_mem20.csv', quote = TRUE,row.names = FALSE)

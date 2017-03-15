## Housing Prediction data set
install.packages("corrplot")
install.packages("missForest")
install.packages("Amelia")
install.packages("VIM")
library(dplyr)
library(psych)
library(Amelia)
library(Hmisc)
library(corrplot)
library(corrgram)
library(VIM)
house_train=read.csv("C:/Users/Aoos/Documents/Projects/HousePrice/train.csv")
str(house_train)

library(utils)
install.packages("utils")

summary(house_train)
sort(sapply(house_train,function(x) sum(is.na(x))),decreasing = TRUE)
# NA -259 LotFrontage
# Alley -1369
# MasVnrArea-8
# BsmtFinType1-37 &BsmtFinType2-38
# FireplaceQu-690
# GarageType GarageYrBlt GarageQual GarageCond-81
# PoolQC -1453
# Fence1179
# MiscFeature 1406
# SalePrice Outcome

hist()

summary(house_train$BsmtFinType2)

describe(log(house_train$SalePrice))

hist(house_train$SalePrice)

hist(log(house_train$SalePrice))

house_train$SalePrice=log(house_train$SalePrice)

opar=par()
par(mfrow=c(1,2))     
hist(log(house_train$SalePrice))

ncol(house_train)

corrgram(house_train)

cor(house_train)
## for loop to get numeric columns and exclude id and sales price
identify_numeric=vector()
for(i in 2:ncol(house_train)-1)
{
  #ifelse(is.numeric(house[,i]),colnames(house)[i],)
  identify_numeric[i]<-is.numeric(house_train[,i])
}

identify_numeric
cor.names=colnames(house_train)[identify_numeric][2:ncol(house_train)-1]
cor.names
cor.data=na.omit(house_train[, (names(house_train) %in% cor.names)])

## creating the correlation plot
cor(cor.data)
corrplot.mixed(cor(cor.data),lower = "circle",tl.pos = "lt", order="hclust", hclust.method="complete")


# creating relationship plots with outcome- sales price
relate=function(data,var)
{
  plot(data[,var],data$SalePrice,xlab =paste(var),ylab ="Log Sales Price")
}
# plot all predictors against Log Sales Price
for(i in 1:(ncol(house)-1)){
  relate(house,colnames(house)[i])  
}
#odd relations for miscVal,PoolArea,Utilities,MiscFeature,KitchenAbvGr,1stFlrSf,TotalBsmtSF,BsmtFinSF1,LotArea

#miscVal,PoolArea,Utilities,MiscFeature,KitchenAbvGr,x1stFlrSf,TotalBsmtSF,BsmtFinSF1,LotArea)
#for(i in 1:length(v)){
#  plot(log(house_train$MiscVal),house_train$SalePrice)  
#}

describe(house_train$MiscVal)

## Missing Value Imputation
# combining test and train dataset
house_test=read.csv("C:/Users/Aoos/Documents/Projects/HousePrice/test.csv")

colnames(house_train[1])
str(house_test)

data_raw<-rbind(house_train[,-81],house_test)

# converting MSSubClass,MoSold,YrSold to categorical
str(data_raw$MSSubClass)
data_raw$MSSubClass <- as.factor(data_raw$MSSubClass)
data_raw$MoSold <- as.factor(data_raw$MoSold)
data_raw$YrSold <- as.factor(data_raw$YrSold)

# Creating map of missing values
missmap(data_raw,col=c("Red","Green"),y.cex=0.2,x.cex=0.8, main = "Before Imputation")

# aggr plot for visualizing missing values
aggr_plot <- aggr(house_train, col = c('navyblue','red'), numbers = TRUE, sortVars = TRUE, 
                  labels= names(train),cex.axis = .9, gap = 2, ylab =c("Histogram of missing data","Pattern"))

# Missing Value impuation
unique(house_train$Alley)
levels(house_train$Alley)=c(levels(house_train$Alley),"None")
house_train[is.na(house_train$Alley),"Alley"] = "None"

house_train[is.na(house_train$MasVnrType),"MasVnrType"]="None"

# house_train[is.na(house_train$MasVnrArea),"MasVnrArea"]=0
# omit bsmtvalues as only 37 
summary(house_train[,30:40])
head(house_train[is.na(house_train$BsmtQual),30:40])

house_train[is.na(house_train$Electrical),"Electrical"] = "SBrkr"

levels(house_train$PoolQC)=c(levels(house_train$PoolQC),"None")
house_train[is.na(house_train$PoolQC),"PoolQC"] = "None"

summary(house_train$Fence)
levels(house_train$Fence)=c(levels(house_train$Fence),"None")
house_train[is.na(house_train$Fence),"Fence"] = "None"

summary(house_train$Fireplaces)# nothing rerquired

summary(house_train$MiscFeature)
levels(house_train$MiscFeature)=c(levels(house_train$MiscFeature),"None")
house_train[is.na(house_train$MiscFeature),"MiscFeature"] = "None"

sum(is.na(house_train$GarageArea))


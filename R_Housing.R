## Housing Prediction data set
install.packages("corrplot")
library(psych)
library(Hmisc)
library(corrplot)
library(corrgram)
house=read.csv("C:/Users/Aoos/Documents/Projects/HousePrice/train.csv")
str(house)

library(utils)
install.packages("utils")

summary(house)
# NA -259 LotFrontage
# Alley -1369
# MasVnrArea-8
# BsmtFinType1-37 &BsmtFinType2-38
# FireplaceQu-690
# GarageType GarageYrBlt GarageQual GarageCond-81
# PoolQC -1453
# Fence1179
# MiscFeature 1406
# SalePrice

hist()

describe(house$MSSubClass)

describe(log(house$SalePrice))

hist(house$SalePrice)

hist(log(house$SalePrice))

house$SalePrice=log(house$SalePrice)

opar=par()
par(mfrow=c(1,2))     
hist(log(house$SalePrice))

ncol(house)

corrgram(house)

cor(house)
## for loop to get numeric columns and exclude id and sales price
identify_numeric=vector()
for(i in 2:ncol(house)-1)
{
  #ifelse(is.numeric(house[,i]),colnames(house)[i],)
  identify_numeric[i]<-is.numeric(house[,i])
}

identify_numeric
cor.names=colnames(house)[identify_numeric][2:ncol(house)-1]
cor.names
cor.data=na.omit(house[, (names(house) %in% cor.names)])

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
# odd relations for miscVal,PoolArea,Utilities,MiscFeature,KitchenAbvGr,1stFlrSf,TotalBsmtSF,BsmtFinSF1,LotArea




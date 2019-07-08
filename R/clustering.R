### This file applies various types of clustering analysis to country-level data

rm(list = ls())

library(mice)
library(VIM)

setwd("Q:/DATA/SPRAIMU/4_SysRisk/R Code")
folder <- "Q:/DATA/SPRAIMU/4_SysRisk/Data/Output/"
file <-'fulldata_panel.Rda'

load(paste(folder,file,sep=""))
mydata<- saveDataR

cut-off <- 2000

##=================================================
## 1. Data Cleaning
##=================================================

mydata <- mydata%>%filter(year(Quarter)>=2000)

mydata$missing <- rowSums(is.na(mydata))

#hist(mydata$missing) 
## Lots of observations miss more than 50 variables

mydata<- mydata%>%filter(missing <=50)             # left with 13584 observations (1/3)

##=================================================
## 2. Data Preprocessing (imputate, standardize)
##=================================================

aggr_plot <- aggr(mydata, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(mydata), cex.axis=.5, gap=1, ~
                      ylab=c("Histogram of missing data","Pattern"))

tempdata <- mice(mydata[c(4:59)], method="cart", seed = 500)

# Some inspection

xyplot(tempdata, Credit_ ~ Credit_Real_Growth+Credit_Growth+Equity_Growth, pch=18, cex =1)
densityplot(tempdata)

completeddata <- complete(tempdata,1)   #complete using 1st of 5 dataset

mydata <- tbl_df(cbind(mydata[c(1:3)], completeddata))

mydata <- mydata%>%select(-c(EQ_Return_vol, Spot_Return_vol, GGR_Return_vol))

mydata <- na.omit(mydata)
mydata[c(4:56)] <- scale(mydata[c(4:56)])            # reduced to 3140 observations

##======================================================================================
## 3. PCA for dimension reeuction and orthogonization (equal-distance across dimensions)
##======================================================================================

mydata.pca <- prcomp(mydata[c(4:56)])

##========================================================================
## 4. Hierachical Clustering (useful for identifying similar observations)
##========================================================================

d<- dist(mydata.pca$x[c(1:30)], method ="euclidean")
fit <- hclust(d, method ="ward.D")
plot(fit)
groups<- cutree(fit, k=5)
rect.hclust(fit, k=5, boarder ="red")


##=================================================
## 5. K-Means Analysis
##=================================================

# Determine number of clusters
wss <- (nrow(mydata.pca$x)-1)*sum(apply(mydata.pca$x,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata.pca$x, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster
fit <- kmeans(mydata.pca$x, 9) # 5 cluster solution
# get cluster means 
aggregate(mydata.pca$x,by=list(fit$cluster),FUN=mean)

# append cluster assignment
result <- data.frame(mydata[c(1:3)], fit$cluster)

# look at an example (Turkey, 2017Q4)
result%>%filter(fit.cluster ==3)%>%select(Country)%>%unique()

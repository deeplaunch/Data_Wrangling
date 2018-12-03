### This file applies various types of clustering analysis to country-level data

rm(list = ls())

setwd("Q:/DATA/SPRAIMU/4_SysRisk/R Code")
folder <- "Q:/DATA/SPRAIMU/4_SysRisk/Data/Output/"
file <-'fulldata_panel.Rda'
saveFolder <- "Q:/DATA/SPRAIMU/4_SysRisk/Data/Output/"


load(paste(folder,file,sep= ""))
mydata<- saveData_Panel


### Clean up suspecious outlisers ###

mydata$BL_HH_g[which(mydata$BL_HH_g>1000)] <- NA

mydata$BL_ONC_g[which(mydata$BL_ONC_g>1000)] <- NA
mydata$BL_ONC_g[which(mydata$BL_ONC_g<-99)] <- NA

mydata$BL_PUB_g[which(mydata$BL_PUB_g>1000)] <- NA
mydata$BL_PUB_g[which(mydata$BL_PUB_g<-99)] <- NA

mydata$OFIL_HH_g[which(mydata$OFIL_HH_g>1000)] <- NA
mydata$OFIL_HH_g[which(mydata$OFIL_HH_g<-99)] <- NA

mydata$OFIL_ONC_g[which(mydata$OFIL_ONC_g>1000)] <- NA
mydata$OFIL_ONC_g[which(mydata$OFIL_ONC_g<-99)] <- NA

mydata$OFIL_PRV_g[which(mydata$OFIL_PRV_g>1000)] <- NA
mydata$OFIL_PRV_g[which(mydata$OFIL_PRV_g<-99)] <- NA

mydata$Gov_Debt_v_GDP[which(mydata$Gov_Debt_v_GDP>1000)] <- NA

mydata$Gov_Ext_v_Debt[which(mydata$Gov_Ext_v_Debt>100)] <- NA



### Missing Numbers Count

mydata%>%
  filter(Country =="Greece" & Quarter ==ymd('2017/12/31'))%>%
  apply(1, function(x) sum(!is.na(x)))


### Save Results

des<- summary(mydata)

write.xlsx(des, paste(saveFolder,'summary_statistics.xlsx',sep=""))



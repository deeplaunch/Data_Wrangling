### This file applies various types of clustering analysis to country-level data

rm(list = ls())

setwd("Q:/DATA/SPRAIMU/4_SysRisk/R Code")
folder <- "Q:/DATA/SPRAIMU/4_SysRisk/Data/Output/"
file <-'fulldata_panel.Rda'
saveFolder <- "Q:/DATA/SPRAIMU/4_SysRisk/Data/Output/"


load(paste(folder,file,sep= ""))
mydata<- saveData_Panel

des<- summary(mydata)

write.xlsx(des, paste(saveFolder,'summary_statistics.xlsx',sep=""))

### 

mydata%>%filter(Country =="Greece" & Quarter ==ymd('2017/12/31'))%>%apply(1, function(x) sum(!is.na(x)))
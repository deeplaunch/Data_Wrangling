
rm(list=ls())
setwd("Q:/DATA/SPRAIMU/4_SysRisk/R Code")

library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(openxlsx)

source('clean_excel.R')
source('calc_percentile_panel.R')
source('calc_2_table.R')
source('calc_growth.R')
source('back_fill.R')

folder<- "Q:/DATA/SPRAIMU/4_SysRisk/Data/"


############## Test merging quarterly with annual data ##########
# folder<- "Q:/DATA/SPRAIMU/MacroPru/Systemic_Risk/Data/"
# file<- "Input_SectorDebt.xlsx" 
# sheet1<- "NGDP_A"
# sheet2<- "NGDP_Q"
# range<- "A6:IV196"
# 
# dataTable1 <- clean_excel(folder=folder, file=file, sheet= sheet1, range= range, freq = "A")
# dataTable2 <- clean_excel(folder=folder, file=file, sheet= sheet2, range= range, freq = "Q")
# 
# tableA <- dataTable1
# tableQ <- dataTable2
# 
# test <- back_fill(tableA, tableQ)


##Test table sum/division calculation###
# folder<- "Q:/DATA/SPRAIMU/MacroPru/Systemic_Risk/Data/"
# file<- "Input_ExnDebt.xlsx" 
# sheet1<- "LOAN_ST_GOV"
# sheet2<- "SEC_ST_GOV"
# range<- "A6:IV196"
# 
# dataTable1 <- clean_excel(folder=folder, file=file, sheet= sheet1, range= range)
# dataTable2 <- clean_excel(folder=folder, file=file, sheet= sheet2, range= range)
# 
# test <- calc_2_table(dataTable1, dataTable2, operator='/')


##Test percentile calculation###
# source('calc_percentile_panel.R')
# 
# setwd('Q:/DATA/SPRAIMU/MacroPru/Systemic_Risk/R Code')
# 
# test <- calc_percentile_panel()
# 
# pasteSheet= paste("DO_FX_v_All","%",sep ="_")
# 
# write.xlsx(test, file = "Q:/DATA/SPRAIMU/MacroPru/Systemic_Risk/Data/test.xlsx", sheetName= pasteSheet,
#            range = "A6:IV196", col.names=TRUE, append=FALSE)
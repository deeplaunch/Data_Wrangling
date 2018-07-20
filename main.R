########### @Auther: Harry Peng Zhao ###############
########### Main scripts that call for individual functions to produce varuos SysRi results in percentiel #########
########### Load, clean, manipulate, and save results ############

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

saveFile = paste(folder, "Result_Percentile_", gsub("-","",today()), ".xlsx", sep ='')
saveData = list()

############################# 1.1 Bank Loan by Sector to GDP ################################################################

# Load GDP
file<- "Input_GDP.xlsx"
range<- "A3:IV196"
dt_GDP<- clean_excel(folder=folder, file=file, sheet= "GDP_Q", range= range, freq ="Q")

# Load Bank Loan Sheets by Sector and Currency
file<- "Input_BankLoans.xlsx"
range<- "A5:IV196"
sheet<-list()
sheet[c("L_HH","L_ONC")]<- list("NC_LOANS_HH_Q","NC_LOANS_ONC_Q")      #Local Currency
sheet[c("F_HH","F_ONC")]<- list("FX_LOANS_HH_Q", "FX_LOANS_ONC_Q")    #Foreign Currency

dt_BL<- lapply (sheet, clean_excel, folder=folder, file=file, range= range, freq ="Q") # Bank Loans Sector, Currency

# Add up LC and FX loans for each sector
dt_BL_Sector<- list()

dt_BL_Sector["HH"]<- list(calc_2_table(dt_BL$L_HH, dt_BL$F_HH, operator='+')) ##HouseHold Banks Loans
dt_BL_Sector["ONC"] <- list(calc_2_table(dt_BL$L_ONC, dt_BL$F_ONC, operator='+')) ## Other Non-Financial

## Calculate Ratio to GDP for each sector
dt_BL_Sector_v_GDP <- lapply(dt_BL_Sector, calc_2_table, dt_GDP, operator ='/')

## Calculate Percentile for each sector
formulaList <- c("NC_LOANS_HH_Q+FX_LOANS_HH_Q/GDP_Q", "NC_LOANS_ONC_Q+FX_LOANS_ONC_Q/GDP_Q")
dt_BL_Sector_v_GDP_ptile <- mapply(calc_percentile_panel, dataTable= dt_BL_Sector_v_GDP, 
                                   calcName = formulaList, SIMPLIFY = FALSE)

############################# 1.2 Bank Loan by Sector (FX Loan/Total Loan) ###############################################################

## Calculate Ratio of FX/overall bank loan
dt_BL_F<- dt_BL[c("F_HH"  , "F_ONC")]
#dt_BL_F[["F_ALL"]]<- dt_F_ALL

dt_BL_Sector_FX_v_TOT<- mapply(calc_2_table, dt_BL_F, dt_BL_Sector, operator ='/', SIMPLIFY = FALSE)
## Calculate Percentile
formulaList <- c("FX_LOANS_HH_Q/NC_LOANS_HH_Q+FX_LOANS_HH_Q", "FX_LOANS_ONC_Q/NC_LOANS_ONC_Q+FX_LOANS_ONC_Q")

dt_BL_Sector_FX_v_TOT_ptile <- mapply(calc_percentile_panel, dataTable= dt_BL_Sector_FX_v_TOT, 
                                   calcName = formulaList, SIMPLIFY = FALSE)

############################# 1.3 Bank Loan by Sector Growth adjusted by CPI index ##########################################

# Load CPI index
file<- "Input_CPI.xlsx"
range<- "A3:IV196"
dt_CPI<- clean_excel(folder=folder, file=file, sheet = "CPI_Q", range= range, freq ="Q")

# Calculate real growth
dt_BL_Secotr_g <- lapply(dt_BL_Sector, calc_2_table, dt_CPI, operator='/')
dt_BL_Secotr_g <- lapply(dt_BL_Secotr_g, calc_growth)

# Calculate percentile
formulaList <- c("NC_LOANS_HH_Q+FX_LOANS_HH_Q/CPI_Q_g", "NC_LOANS_ONC_Q+FX_LOANS_ONC_Q/CPI_Q_g")

dt_BL_Secotr_g_ptile <- mapply(calc_percentile_panel, dataTable= dt_BL_Secotr_g, 
                                   calcName = formulaList, SIMPLIFY = FALSE)

############################# 2.1 Other Financial Institution Loan by Sector to GDP ################################################################

# Load OFI Loan Sheets by Sector and Currency
file<- "Input_OFILoans.xlsx"
range<- "A5:IV196"
sheet<-list()
sheet[c("L_HH","L_ONC")]<- list("NC_LOANS_HH_Q", "NC_LOANS_ONC_Q")      #Local Currency for Household and Private Companies
sheet[c("F_HH","F_ONC")]<- list("FX_LOANS_HH_Q", "FX_LOANS_ONC_Q")      #Foreign Currency

dt_OFIL<- lapply (sheet, clean_excel, folder=folder, file=file, range= range, freq ="Q") # Bank Loans Sector, Currency

# Add up LC and FX loans for each sector
dt_OFIL_Sector<- list()

dt_OFIL_Sector["HH"]<- list(calc_2_table(dt_OFIL$L_HH, dt_OFIL$F_HH, operator='+')) ##HouseHold Banks Loans
dt_OFIL_Sector["ONC"] <- list(calc_2_table(dt_OFIL$L_ONC, dt_OFIL$F_ONC, operator='+')) ## Other Non-Financial

dt_OFIL_Sector["PRV"] <- list(calc_2_table(dt_OFIL_Sector$HH, dt_OFIL_Sector$ONC , operator='+'))   ## Private Sector

## Calculate Ratio to GDP for each sector
dt_OFIL_Sector_v_GDP <- lapply(dt_OFIL_Sector, calc_2_table, dt_GDP, operator ='/')

## Calculate Percentile for each sector
formulaList <- c("NC_LOANS_HH_Q+FX_LOANS_HH_Q/GDP_Q", "NC_LOANS_ONC_Q+FX_LOANS_ONC_Q/GDP_Q",
                 "NC_LOANS_HH_Q+FX_LOANS_HH_Q+NC_LOANS_ONC_Q+FX_LOANS_ONC_Q/GDP_Q"
)
dt_OFIL_Sector_v_GDP_ptile <- mapply(calc_percentile_panel, dataTable= dt_OFIL_Sector_v_GDP, 
                                     calcName = formulaList, SIMPLIFY = FALSE)

############################# 2.2 Other Financial Sector Loan by Sector Growth adjusted by CPI index ##########################################

# Calculate real growth
dt_OFIL_Secotr_g <- lapply(dt_OFIL_Sector, calc_2_table, dt_CPI, operator='/')
dt_OFIL_Secotr_g <- lapply(dt_OFIL_Secotr_g, calc_growth)

# Calculate percentile
formulaList <- c("NC_LOANS_HH_Q+FX_LOANS_HH_Q/CPI_Q_g",  "NC_LOANS_ONC_Q+FX_LOANS_ONC_Q/CPI_Q_g",
                 "NC_LOANS_HH_Q+FX_LOANS_HH_Q+NC_LOANS_ONC_Q+FX_LOANS_ONC_Q/CPI_Q_g"
)

dt_OFIL_Secotr_g_ptile <- mapply(calc_percentile_panel, dataTable= dt_OFIL_Secotr_g, 
                                 calcName = formulaList, SIMPLIFY = FALSE)

########################################## 3. Bank Loans to Public Sector ##########################################

# Load Bank Loan Sheets by Sector and Currency
file<- "Input_BankLoans.xlsx"
range<- "A5:IV196"
sheet<-list()
sheet[c("LOAN_CG","LOAN_PC","LOAN_LG")]<- list("LOAN_CG","LOAN_PC","LOAN_LG")      
# Bank Loans to Central Gov, Public Companies, Local Government

dt_BL<- lapply (sheet, clean_excel, folder=folder, file=file, range= range, freq ="Q") # Bank Loans Sector

# Add up bank loans to all sectors
dt_BL_Sector<- list()

dt_BL_Sector["ALL"]<- list(calc_2_table(dt_BL$LOAN_CG, dt_BL$LOAN_PC, operator='+')) 
dt_BL_Sector["ALL"] <- list(calc_2_table(dt_BL_Sector$ALL, dt_BL$LOAN_LG, operator='+')) 

## Calculate Ratio to GDP for each sector
dt_BL_Sector_v_GDP <- lapply(dt_BL_Sector, calc_2_table, dt_GDP, operator ='/')

## Calculate Percentile 
dt_BL_Sector_v_GDP_ptile <- mapply(calc_percentile_panel, dataTable= dt_BL_Sector_v_GDP, 
                                   calcName = "LOAN_CG+LOAN_PC+LOAN_LG/GDP_Q", SIMPLIFY = FALSE)

############################################### Save results for 1.1 to 3, clear memory #################################

names(dt_BL_Sector_v_GDP_ptile)<- paste( "BL_", names(dt_BL_Secotr_g_ptile), "_v_GDP_ptile", sep ="")
names(dt_BL_Sector_FX_v_TOT_ptile)<- paste( "BL_", names(dt_BL_Sector_FX_v_TOT_ptile), '_v_FL_ptile', sep ="")
names(dt_BL_Secotr_g_ptile)<- paste("BL_", names(dt_BL_Secotr_g_ptile), "_g_ptile", sep ="")

names(dt_OFIL_Sector_v_GDP_ptile)<- paste( "OFIL_", names(dt_OFIL_Secotr_g_ptile), "_v_GDP_ptile", sep ="")
names(dt_OFIL_Secotr_g_ptile)<- paste( "OFIL_", names(dt_OFIL_Secotr_g_ptile), "_g_ptile", sep ="")

names(dt_BL_Sector_v_GDP_ptile)<- "BL_PB_v_GDP_ptile"

saveData = append(saveData, c(dt_BL_Sector_v_GDP_ptile, dt_BL_Sector_FX_v_TOT_ptile, dt_BL_Secotr_g_ptile, dt_OFIL_Sector_v_GDP_ptile, dt_OFIL_Secotr_g_ptile, dt_BL_Sector_v_GDP_ptile))

rm(list = ls()[grep("dt_", ls())] )                 
rm(sheet)

############################################## 4. FSI Indicators #######################################################

# Load FSI file
file<- "Input_FSI.xlsx"
range<- "A6:IV196"
sheet<-list()
sheet[c('CAR','Leverage_Rat','NPL_CAP','LIQ_Ass_ST_Liab','NPL','ROE','L_D_Ratio','NOP_CAP','FX_Loans')]<- 
  list('CAR','Leverage_Rat','NPL_CAP','LIQ_Ass_ST_Liab','NPL','ROE','L_D_Ratio','NOP_CAP','FX_Loans')

dt_FSI<- lapply(sheet, clean_excel, folder=folder, file=file, range= range, freq ="Q")

# Calculate percentile and save
dt_FSI_ptile <- lapply(sheet, function(x) calc_percentile_panel(dt_FSI[[x]], x) )
names(dt_FSI_ptile)<- paste(names(dt_FSI_ptile),"_ptile", sep ='')

saveData<- append(saveData, dt_FSI_ptile)

############################################## 5. Net Foreign Asset % GDP #######################################################

# Load GDP
file<- "Input_GDP.xlsx"
range<- "A3:IV196"
dt_GDP<- clean_excel(folder=folder, file=file, sheet= "GDP_Q", range= range, freq ="Q")

# Load NFA sheet
file<- "Input_OthVar.xlsx"
range<- "A3:IV196"
sheet<-list()
sheet[c('NFA_Q')]<- list('NFA_Q')

dt_NFA<- lapply(sheet, clean_excel, folder=folder, file=file, range= range, freq ="Q")
dt_NFA_v_GDP<- lapply(dt_NFA, calc_2_table, dt_GDP, operator ='/')

# Calculate percentile and save
dt_NFA_v_GDP_ptile <- lapply(dt_NFA_v_GDP, function(x) calc_percentile_panel(x,'NFA_Q/GDP_Q') )
names(dt_NFA_v_GDP_ptile)<- paste(names(dt_NFA_v_GDP),"_ptile", sep ='')

saveData<- append(saveData, dt_NFA_v_GDP_ptile)

rm(list = ls()[grep("dt_", ls())] )                 
rm(sheet, formulaList)

############################################## 6.1 External Debt by Sector (Short-term % All) ###########################
#Load External Debt file
file<- "Input_Exn_Debt.xlsx"
range<- "A6:IV196"
sheet<-list()
sheet[c('SEC_ST_GOV','SEC_ST_OTH','SEC_LT_GOV','SEC_LT_OTH',
        'LOAN_ST_GOV','LOAN_ST_OTH','LOAN_LT_GOV','LOAN_LT_OTH')]<-
  list('SEC_ST_GOV','SEC_ST_OTH','SEC_LT_GOV','SEC_LT_OTH',
       'LOAN_ST_GOV','LOAN_ST_OTH','LOAN_LT_GOV','LOAN_LT_OTH') #Name convention: Security Type_Term_ReceiveEntity

dt_Exn_Debt<- lapply(sheet, clean_excel, folder=folder, file=file, range= range, freq ="Q")

# Add up loans and securities for ST and LT in each sector
dt_Exn_Debt_ST<- list()
dt_Exn_Debt_ST["GOV"]<- list(calc_2_table(dt_Exn_Debt$SEC_ST_GOV, dt_Exn_Debt$LOAN_ST_GOV, operator='+')) 
dt_Exn_Debt_ST["OTH"]<- list(calc_2_table(dt_Exn_Debt$SEC_ST_OTH, dt_Exn_Debt$LOAN_ST_OTH, operator='+')) 

dt_Exn_Debt_LT<- list()
dt_Exn_Debt_LT["GOV"]<- list(calc_2_table(dt_Exn_Debt$SEC_LT_GOV, dt_Exn_Debt$LOAN_LT_GOV, operator='+')) 
dt_Exn_Debt_LT["OTH"]<- list(calc_2_table(dt_Exn_Debt$SEC_LT_OTH, dt_Exn_Debt$LOAN_LT_OTH, operator='+')) 

# Add up ST and LT in each sector
dt_Exn_Debt_Sector<- list()
dt_Exn_Debt_Sector["GOV"]<- list(calc_2_table(dt_Exn_Debt_ST$GOV, dt_Exn_Debt_LT$GOV, operator='+')) 
dt_Exn_Debt_Sector["OTH"]<- list(calc_2_table(dt_Exn_Debt_ST$OTH, dt_Exn_Debt_LT$OTH, operator='+')) 

# Calculate Short-Term Debt as % of Total External Debt for each sector (Loans & Debt Securities)
dt_Exn_Debt_ST_v_Sector<- list()
dt_Exn_Debt_ST_v_Sector['GOV']<- list(calc_2_table(dt_Exn_Debt_ST$GOV, dt_Exn_Debt_Sector$GOV, operator='/'))
dt_Exn_Debt_ST_v_Sector['OTH']<- list(calc_2_table(dt_Exn_Debt_ST$OTH, dt_Exn_Debt_Sector$OTH, operator='/'))

# Calculate percentile and save to saveData
formulaList <- c("SEC_ST_GOV+LOAN_ST_GOV/SEC_ST_GOV+LOAN_ST_GOV+SEC_LT_GOV+LOAN_LT_GOV",  
                 "SEC_ST_OTH+LOAN_ST_OTH/SEC_ST_OTH+LOAN_ST_OTH+SEC_LT_OTH+LOAN_LT_OTH")
dt_Exn_Debt_ptile <- mapply(calc_percentile_panel, dt_Exn_Debt_ST_v_Sector, formulaList, SIMPLIFY = FALSE)

names(dt_Exn_Debt_ptile)<- paste("Exn_Debt_ST_v_ALL_",names(dt_Exn_Debt_ptile),"_ptile", sep ='')

saveData<- append(saveData, dt_Exn_Debt_ptile)

########################################### 6.2 External Debt Growth adjusted by CPI index ##########################

# Load FX rate
file<- "Input_Exn_Debt.xlsx"
range<- "A6:IV196"
dt_FX<- clean_excel(folder=folder, file=file, sheet = "FX_RATE", range= range, freq ="Q")

# Load CPI index
file<- "Input_CPI.xlsx"
range<- "A3:IV196"
dt_CPI<- clean_excel(folder=folder, file=file, sheet = "CPI_Q", range= range, freq ="Q")

# Calculate external debt in local currency 
dt_Exn_Debt_Sector <- lapply(dt_Exn_Debt_Sector, calc_2_table, dt_FX, operator='*')

# Calculate real growth
dt_Exn_Debt_Sector_g <- lapply(dt_Exn_Debt_Sector, calc_2_table, dt_CPI, operator='/')
dt_Exn_Debt_Sector_g <- lapply(dt_Exn_Debt_Sector_g, calc_growth)

# Calculate percentile and save data, clear memory
formulaList <- c("SEC_ST_GOV+LOAN_ST_GOV+SEC_LT_GOV+LOAN_LT_GOV*FX_RATE/CPI_Q_g",
                 "SEC_ST_OTH+LOAN_ST_OTH+SEC_LT_OTH+LOAN_LT_OTH*FX_RATE/CPI_Q_g")

dt_Exn_Debt_Sector_g_ptile <- mapply(calc_percentile_panel, dataTable= dt_Exn_Debt_Sector_g,
                               calcName = formulaList, SIMPLIFY = FALSE)

names(dt_Exn_Debt_Sector_g_ptile)<- paste("Exn_Debt_g_",names(dt_Exn_Debt_Sector_g_ptile),"_ptile", sep ='')

saveData<- append(saveData, dt_Exn_Debt_Sector_g_ptile)

########################################### 6.3 External Debt by Sector, % of GDP ##########################################

# Load GDP
file<- "Input_GDP.xlsx"
range<- "A3:IV196"
dt_GDP<- clean_excel(folder=folder, file=file, sheet= "GDP_Q", range= range, freq ="Q")

## Calculate Ratio to GDP for each sector
dt_Exn_Debt_Sector_v_GDP <- lapply(dt_Exn_Debt_Sector, calc_2_table, dt_GDP, operator ='/')

## Calculate Percentile for each sector
formulaList <- c("SEC_ST_GOV+LOAN_ST_GOV+SEC_LT_GOV+LOAN_LT_GOV*FX_RATE/GDP_Q",
                 "SEC_ST_OTH+LOAN_ST_OTH+SEC_LT_OTH+LOAN_LT_OTH*FX_RATE/GDP_Q"
)
dt_Exn_Debt_Sector_v_GDP_ptile <- mapply(calc_percentile_panel, dataTable= dt_Exn_Debt_Sector_v_GDP, 
                                   calcName = formulaList, SIMPLIFY = FALSE)

names(dt_Exn_Debt_Sector_v_GDP_ptile)<- paste("Exn_Debt_v_GDP_",names(dt_Exn_Debt_Sector_v_GDP_ptile),"_ptile", sep ='')

saveData<- append(saveData, dt_Exn_Debt_Sector_v_GDP_ptile)

rm(list = ls()[grep("dt_", ls())] )
rm(sheet, formulaList)

########################################### 7.1 Government Debt to GDP ##########################################

# Load Government Debt to GDP sheet
file<- "Input_OthVar.xlsx"
range<- "A3:IV196"
sheet<-list()
sheet[c('GOV_Debt_GDP_Q')]<- list('GOV_Debt_GDP_Q')
dt_Gov_Debt_v_GDP<- lapply(sheet, clean_excel, folder=folder, file=file, range= range, freq ="Q")

# Calculate percentile and save
dt_Gov_Debt_v_GDP_ptile <- lapply(dt_Gov_Debt_v_GDP, function(x) calc_percentile_panel(x,'GOV_Debt_GDP_Q') )

########################################### 7.2 Fiscal Balance to GDP ##########################################

# Load Fiscal Balance sheet
file<- "Input_OthVar.xlsx"
range<- "A3:IV196"
sheet<-list()
sheet[c('FB_Q')]<- list('FB_Q')

dt_FB<- lapply(sheet, clean_excel, folder=folder, file=file, range= range, freq ="Q")

# Load GDP
file<- "Input_GDP.xlsx"
range<- "A3:IV196"
dt_GDP<- clean_excel(folder=folder, file=file, sheet= "GDP_Q", range= range, freq ="Q")

## Calculate Ratio to GDP for each sector
dt_FB_v_GDP <- lapply(dt_FB, calc_2_table, dt_GDP, operator ='/')

# Calculate percentile and save
dt_FB_v_GDP_ptile <- lapply(dt_FB_v_GDP, function(x) calc_percentile_panel(x,'FB_Q/GDP_Q') )


# Rename and save data for Section 7
names(dt_Gov_Debt_v_GDP_ptile)<- paste(names(dt_Gov_Debt_v_GDP),"_ptile", sep ='')
names(dt_FB_v_GDP_ptile)<- paste(names(dt_FB_v_GDP),"_v_GDP_ptile", sep ='')

saveData<- append(saveData, dt_Gov_Debt_v_GDP_ptile)
saveData<- append(saveData, dt_Gov_Debt_v_GDP_ptile)


################################################# 8.1 BOP Net FLows #####################################################
############## Other Net Inflow to Official Sector & Non-official, non-bank sector and net debt portflio inflow#########

# Load BOP Items
file<- "Input_BOP.xlsx"
range<- "A3:IV196"
sheet<-list()
sheet[c('BOP_OF_GDP','BOP_NOF_GDP','BOP_DT_GDP')]<- list('BOP_OF_GDP','BOP_NOF_GDP','BOP_DT_GDP')


dt_BOP<- lapply(sheet, clean_excel, folder=folder, file=file, range= range, freq ="Q")

# Calculate percentile and save
formulaList <- c("BOP_OF_GDP", "BOP_NOF_GDP", "BOP_DT_GDP")

dt_BOP_ptile <- mapply(calc_percentile_panel, dataTable= dt_BOP, 
       calcName = formulaList, SIMPLIFY = FALSE)

saveData<- append(saveData, dt_BOP_ptile)

################################################### 8.2 REER Grwoth #######################################################

# Load REER value
sheet<-c('REER')
dt_REER<- lapply(sheet, clean_excel, folder=folder, file=file, range= range, freq ="Q")

# Calculate REER growth
dt_REER_g<- lapply(dt_REER, calc_growth)

# Calculate percentile and save
dt_REER_g_ptile <- lapply(dt_REER_g, function(x) calc_percentile_panel(x,'REER_g') )
names(dt_REER_g_ptile)<- "dt_REER_g_ptile"

saveData<- append(saveData, dt_REER_g_ptile)

################################################## 9.1 Housing Real Price Growth ##########################################

# Load Real House Price
file<- "Input_Housing.xlsx"
range<- "A3:IV196"
sheet<-c('REAL_PRICE')
dt_RHP<- lapply(sheet, clean_excel, folder=folder, file=file, range= range, freq ="Q")

# Calculate REER growth
dt_RHP_g<- lapply(dt_RHP, calc_growth)

# Calculate percentile and save
dt_RHP_g_ptile <- lapply(dt_RHP_g, function(x) calc_percentile_panel(x,'REAL_PRICE_g') )
names(dt_RHP_g_ptile)<- "dt_RHP_g_ptile"

saveData<- append(saveData, dt_RHP_g_ptile)

################################################## 9.2 Housing Valuation ##########################################

# Load Housing Valuation
sheet<- list()
sheet[c('PRICE_RENT','PRICE_INCOME')]<- list('PRICE_RENT','PRICE_INCOME')
dt_HV<- lapply(sheet, clean_excel, folder=folder, file=file, range= range, freq ="Q")

# Calculate percentile (Data is indexed for each country, so use within-country percentile instead)
dt_HV_ptile <- mapply(calc_percentile_panel, dt_HV, c('PRICE_RENT','PRICE_INCOME') , by_column = TRUE, SIMPLIFY = FALSE)
names(dt_HV_ptile)<- paste(names(dt_HV_ptile),'_ptile_c', sep = '')

saveData<- append(saveData, dt_HV_ptile)

rm(list = ls()[grep("dt_", ls())] )                 
rm(sheet)

################################################## 10 ARA Metric ##########################################

# Load ARA Metric
file<- "SPRIRU_ARA_METRIC.xlsx"
range<- "G201:IV396"
sheet<- list()
sheet[c('ARA_A')]<-list('ARA')
dt_ARA_A<- lapply(sheet, clean_excel, folder=folder, file=file, range= range, freq ="A")

#Convert annual data to quarterly data
dt_ARA_Q<-list()
dt_ARA_Q[['ARA']]<-dt_ARA_A$ARA%>%
  mutate(Q.1 = ARA, Q.2 =ARA, Q.3 =ARA, Q.4 =ARA)%>%
  select(-ARA)%>%
  gather(Quarter, ARA, -c(Country, Code, Year))%>%
  mutate(Year_Quarter = as.numeric(gsub('Q.','',Quarter)) + Year *100)%>%
  select(-Quarter)%>%
  mutate(Quarter = yq((as.character(Year_Quarter)))+ months(3)-days(1))%>%
  select(-Year, -Year_Quarter)

# Calculate percentile and save
dt_ARA_Q_ptile <- lapply(dt_ARA_Q, function(x) calc_percentile_panel(x,'ARA') )
names(dt_ARA_Q_ptile)<- "dt_ARA_ptile"

saveData<- append(saveData, dt_ARA_Q_ptile)

############################# Write Results to Excel  ##########################################

openxlsx::write.xlsx(saveData, file = saveFile)
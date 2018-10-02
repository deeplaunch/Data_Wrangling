



##Main scripts that call individual data/ functions to produce varuos SysRi results in percentiles and raw

##@Auther: Harry Peng Zhao

##===========================================
##0. Load, clean, manipulate, and save results
##===========================================

rm(list = ls())

setwd("Q:/DATA/SPRAIMU/4_SysRisk/R Code")
folder <- "Q:/DATA/SPRAIMU/4_SysRisk/Data/"
by_country <- TRUE     ##Calculate country-wise/group percentiles
suffix <- "_ptile"

library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(openxlsx)

source('clean_excel.R')
source('calc_percentile_panel.R')
source('calc_2_table.R')
source('calc_growth.R')
source('calc_vol.R')
source('back_fill.R')
source('to_wide.R')

##==========================================
## 1.1 Bank Loan by Private Sector to GDP
##==========================================

# Load GDP
file <- "Input_GDP.xlsx"
range <- "A3:IV196"
dt_GDP <-
    clean_excel(
        folder = folder,
        file = file,
        sheet = "GDP_Q",
        range = range,
        freq = "Q"
    )

# Load Bank Loan Sheets by Sector and Currency
file <- "Input_BankLoans.xlsx"
range <- "A5:IV196"
sheet <- list()
sheet[c("L_HH", "L_ONC")] <-
    list("NC_LOANS_HH_Q", "NC_LOANS_ONC_Q")      #Local Currency
sheet[c("F_HH", "F_ONC")] <-
    list("FX_LOANS_HH_Q", "FX_LOANS_ONC_Q")    #Foreign Currency

dt_BL <-
    lapply (
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    ) # Bank Loans Sector, Currency

# Add up LC and FX loans for each sector
dt_BL_Private <- list()
dt_BL_Private["HH"] <-
    list(calc_2_table(dt_BL$L_HH, dt_BL$F_HH, operator = '+')) ##HouseHold Banks Loans
dt_BL_Private["ONC"] <-
    list(calc_2_table(dt_BL$L_ONC, dt_BL$F_ONC, operator = '+')) ## Other Non-Financial

## Calculate Ratio to GDP for each sector
dt_BL_Private_v_GDP <-
    lapply(dt_BL_Private, calc_2_table, dt_GDP, operator = '/')


##=====================================================
## 1.2 Bank Loan by Private Sector (FX Loan/Total Loan)
##=====================================================

## Calculate Ratio of FX/overall bank loan
dt_BL_F <- dt_BL[c("F_HH"  , "F_ONC")]
#dt_BL_F[["F_ALL"]]<- dt_F_ALL

dt_BL_Private_FX_v_TOT <-
    mapply(calc_2_table,
           dt_BL_F,
           dt_BL_Private,
           operator = '/',
           SIMPLIFY = FALSE)

##==============================================================
## 1.3 Bank Loan by Private Sector Growth adjusted by CPI index
##==============================================================

# Load CPI index
file <- "Input_CPI.xlsx"
range <- "A3:IV196"
dt_CPI <-
    clean_excel(
        folder = folder,
        file = file,
        sheet = "CPI_Q",
        range = range,
        freq = "Q"
    )

# Calculate real growth
dt_BL_Private_g <-
    lapply(dt_BL_Private, calc_2_table, dt_CPI, operator = '/')
dt_BL_Private_g <- lapply(dt_BL_Private_g, calc_growth)

##==========================================
##2.1 Bank Loan to Public Sector to GDP
##==========================================

# Load Bank Loan Sheets by Sector and Currency
file <- "Input_BankLoans.xlsx"
range <- "A5:IV196"
sheet <- list()
sheet[c("LOAN_CG", "LOAN_PC", "LOAN_LG")] <-
    list("NC_LOANS_CG_Q", "NC_LOANS_PNC_Q", "NC_LOANS_LG_Q")
# Bank Loans to Central Gov, Public Companies, Local Government

dt_BL <-
    lapply (
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    ) # Bank Loans Sector

# Add up bank loans to all public sectors
dt_BL_Public <- list()

dt_BL_Public["PUB"] <-
    list(calc_2_table(dt_BL$LOAN_CG, dt_BL$LOAN_PC, operator = '+'))
dt_BL_Public["PUB"] <-
    list(calc_2_table(dt_BL_Public$PUB, dt_BL$LOAN_LG, operator = '+'))

## Calculate Ratio to GDP for each sector
dt_BL_Public_v_GDP <-
    lapply(dt_BL_Public, calc_2_table, dt_GDP, operator = '/')

## Calculate Real growth

dt_BL_Public_g <-
    lapply(dt_BL_Public, calc_2_table, dt_CPI, operator = '/')
dt_BL_Public_g <- lapply(dt_BL_Public_g, calc_growth)

##==========================================
##2.2 Bank Loan Sector to Total
##==========================================

dt_BL_ALL_Sector <- list()
dt_BL_ALL_Sector[c('HH', 'ONC', 'PUB')] <-
    list(dt_BL_Private$HH, dt_BL_Private$ONC, dt_BL_Public$PUB)

dt_BL_ALL <-
    calc_2_table(dt_BL_ALL_Sector$HH, dt_BL_ALL_Sector$ONC, '+')
dt_BL_ALL <- calc_2_table(dt_BL_ALL, dt_BL_ALL_Sector$PUB, '+')

dt_BL_ALL_Sector_v_ALL <-
    lapply(dt_BL_ALL_Sector, calc_2_table, dt_BL_ALL, operator = '/')



##==========================================================
##3.1 Other Financial Institution Loan by Sector to GDP
##===========================================================

# Load OFI Loan Sheets by Sector and Currency
file <- "Input_OFILoans.xlsx"
range <- "A5:IV196"
sheet <- list()
sheet[c("L_HH", "L_ONC")] <-
    list("NC_LOANS_HH_Q", "NC_LOANS_ONC_Q")      #Local Currency for Household and Private Companies
sheet[c("F_HH", "F_ONC")] <-
    list("FX_LOANS_HH_Q", "FX_LOANS_ONC_Q")      #Foreign Currency

dt_OFIL <-
    lapply (
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    ) # Bank Loans Sector, Currency

# Add up LC and FX loans for each sector
dt_OFIL_Sector <- list()

dt_OFIL_Sector["HH"] <-
    list(calc_2_table(dt_OFIL$L_HH, dt_OFIL$F_HH, operator = '+')) ##HouseHold Banks Loans
dt_OFIL_Sector["ONC"] <-
    list(calc_2_table(dt_OFIL$L_ONC, dt_OFIL$F_ONC, operator = '+')) ## Other Non-Financial

dt_OFIL_Sector["PRV"] <-
    list(calc_2_table(dt_OFIL_Sector$HH, dt_OFIL_Sector$ONC , operator = '+'))   ## Private Sector

## Calculate Ratio to GDP for each sector
dt_OFIL_Sector_v_GDP <-
    lapply(dt_OFIL_Sector, calc_2_table, dt_GDP, operator = '/')

##============================================================================
##3.2 Other Financial Institution Loan by Sector Growth adjusted by CPI index
##============================================================================

# Calculate real growth
dt_OFIL_Secotr_g <-
    lapply(dt_OFIL_Sector, calc_2_table, dt_CPI, operator = '/')
dt_OFIL_Secotr_g <- lapply(dt_OFIL_Secotr_g, calc_growth)


##==========================================
##4. FSI Indicators
##==========================================

# Load FSI file
file <- "Input_FSI.xlsx"
range <- "A6:IV196"
sheet <- list()
sheet[c(
    'CAR',
    'Leverage_Rat',
    'NPL_PRO',
    'LIQ_Ass_ST_Liab',
    'NPL_TOT',
    'ROA',
    'L_D_Ratio',
    'FX_CAP',
    'FX_Loans',
    'FX_Liability'
)] <-
    list(
        'CAR',
        'Leverage_Rat',
        'NPL_PRO',
        'LIQ_Ass_ST_Liab',
        'NPL_TOT',
        'ROA',
        'L_D_Ratio',
        'FX_CAP',
        'FX_Loans',
        'FX_Liability'
    )

dt_FSI <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    )
##==========================================
##5. Net Foreign Asset % GDP
##==========================================
# Load GDP
# file<- "Input_GDP.xlsx"
# range<- "A3:IV196"
# dt_GDP<- clean_excel(folder=folder, file=file, sheet= "GDP_Q", range= range, freq ="Q")
#
# # Load NFA sheet
# file<- "Input_OthVar.xlsx"
# range<- "A3:IV196"
# sheet<-list()
# sheet[c('NFA_Q')]<- list('NFA_Q')
#
# dt_NFA<- lapply(sheet, clean_excel, folder=folder, file=file, range= range, freq ="Q")
# dt_NFA_v_GDP<- lapply(dt_NFA, calc_2_table, dt_GDP, operator ='/')
#
# # Calculate percentile and save
# dt_NFA_v_GDP_ptile <- lapply(dt_NFA_v_GDP, function(x) calc_percentile_panel(x,'NFA_Q/GDP_Q') )
# names(dt_NFA_v_GDP_ptile)<- paste(names(dt_NFA_v_GDP),"_", suffix, sep ='')
#
# saveData<- append(saveData, dt_NFA_v_GDP_ptile)
#
# rm(list = ls()[grep("dt_", ls())] )
# rm(sheet, formulaList)

##==============================================
##6.1 External Debt by Sector (Short-term % All)
##==============================================

#Load External Debt file
file <- "Input_Ext_Debt.xlsx"
range <- "A6:IV196"
sheet <- list()
sheet[c(
    'SEC_ST_GOV',
    'SEC_ST_OTH',
    'SEC_LT_GOV',
    'SEC_LT_OTH',
    'LOAN_ST_GOV',
    'LOAN_ST_OTH',
    'LOAN_LT_GOV',
    'LOAN_LT_OTH'
)] <-
    list(
        'SEC_ST_GOV',
        'SEC_ST_OTH',
        'SEC_LT_GOV',
        'SEC_LT_OTH',
        'LOAN_ST_GOV',
        'LOAN_ST_OTH',
        'LOAN_LT_GOV',
        'LOAN_LT_OTH'
    ) #Name convention: Security Type_Term_ReceiveEntity

dt_Exn_Debt <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    )

# Add up loans and securities for ST and LT in each sector
dt_Exn_Debt_ST <- list()
dt_Exn_Debt_ST["GOV"] <-
    list(calc_2_table(dt_Exn_Debt$SEC_ST_GOV, dt_Exn_Debt$LOAN_ST_GOV, operator =
                          '+'))
dt_Exn_Debt_ST["OTH"] <-
    list(calc_2_table(dt_Exn_Debt$SEC_ST_OTH, dt_Exn_Debt$LOAN_ST_OTH, operator =
                          '+'))

dt_Exn_Debt_LT <- list()
dt_Exn_Debt_LT["GOV"] <-
    list(calc_2_table(dt_Exn_Debt$SEC_LT_GOV, dt_Exn_Debt$LOAN_LT_GOV, operator =
                          '+'))
dt_Exn_Debt_LT["OTH"] <-
    list(calc_2_table(dt_Exn_Debt$SEC_LT_OTH, dt_Exn_Debt$LOAN_LT_OTH, operator =
                          '+'))

# Add up ST and LT in each sector
dt_Exn_Debt_Sector <- list()
dt_Exn_Debt_Sector["GOV"] <-
    list(calc_2_table(dt_Exn_Debt_ST$GOV, dt_Exn_Debt_LT$GOV, operator = '+'))
dt_Exn_Debt_Sector["OTH"] <-
    list(calc_2_table(dt_Exn_Debt_ST$OTH, dt_Exn_Debt_LT$OTH, operator = '+'))

# Calculate Short-Term Debt as % of Total External Debt for each sector (Loans & Debt Securities)
dt_Exn_Debt_ST_v_ALL_Sector <- list()
dt_Exn_Debt_ST_v_ALL_Sector['GOV'] <-
    list(calc_2_table(dt_Exn_Debt_ST$GOV, dt_Exn_Debt_Sector$GOV, operator =
                          '/'))
dt_Exn_Debt_ST_v_ALL_Sector['OTH'] <-
    list(calc_2_table(dt_Exn_Debt_ST$OTH, dt_Exn_Debt_Sector$OTH, operator =
                          '/'))


##==============================================
##6.2 External Debt Growth adjusted by CPI index
##==============================================


# Load FX rate
file <- "Input_Ext_Debt.xlsx"
range <- "A6:IV196"
dt_FX <-
    clean_excel(
        folder = folder,
        file = file,
        sheet = "FX_RATE",
        range = range,
        freq = "Q"
    )

# Load CPI index
file <- "Input_CPI.xlsx"
range <- "A3:IV196"
dt_CPI <-
    clean_excel(
        folder = folder,
        file = file,
        sheet = "CPI_Q",
        range = range,
        freq = "Q"
    )

# Calculate external debt in local currency
dt_Exn_Debt_Sector <-
    lapply(dt_Exn_Debt_Sector, calc_2_table, dt_FX, operator = '*')

# Calculate real growth
dt_Exn_Debt_Sector_g <-
    lapply(dt_Exn_Debt_Sector, calc_2_table, dt_CPI, operator = '/')

dt_Exn_Debt_Sector_g <- lapply(dt_Exn_Debt_Sector_g, calc_growth)


##==========================================
##6.3 External Debt by Sector, % of GDP
##==========================================

# Load GDP
file <- "Input_GDP.xlsx"
range <- "A3:IV196"
dt_GDP <-
    clean_excel(
        folder = folder,
        file = file,
        sheet = "GDP_Q",
        range = range,
        freq = "Q"
    )

## Calculate Ratio to GDP for each sector
dt_Exn_Debt_Sector_v_GDP <-
    lapply(dt_Exn_Debt_Sector, calc_2_table, dt_GDP, operator = '/')

##==========================================
##7.1 Government Debt to GDP
##==========================================

# Load Government Debt to GDP sheet
file <- "Input_Gov.xlsx"
range <- "A3:IV196"
sheet <- list()
sheet[c('GOV_Debt_GDP_Q')] <- list('GOV_Debt_GDP_Q')
dt_Gov_Debt_v_GDP <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    )

##==========================================
##7.2 Fiscal Balance to GDP
##==========================================

# Load Fiscal Balance sheet
file <- "Input_Gov.xlsx"
range <- "A3:IV196"
sheet <- list()
sheet[c('FB_Q')] <- list('FB_Q')

dt_FB <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    )

# Load GDP
file <- "Input_GDP.xlsx"
range <- "A3:IV196"

dt_GDP <-
    clean_excel(
        folder = folder,
        file = file,
        sheet = "GDP_Q",
        range = range,
        freq = "Q"
    )

## Calculate Ratio to GDP for each sector
dt_FB_v_GDP <- lapply(dt_FB, calc_2_table, dt_GDP, operator = '/')


##==========================================
##8.1 BOP Net FLows
##==========================================

# Other Net Inflow to Official Sector & Non-official, non-bank sector and net debt portflio inflow
# Load BOP Items
file <- "Input_BOP.xlsx"
range <- "A3:IV196"
sheet <- list()
sheet[c('BOP_OF_GDP', 'BOP_NOF_GDP', 'BOP_DT_GDP')] <-
    list('BOP_OF_GDP', 'BOP_NOF_GDP', 'BOP_DT_GDP')

dt_BOP <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    )


##==========================================
##8.2 REER Grwoth
##==========================================

# Load REER value
sheet <- c('REER')
dt_REER <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    )

# Calculate REER growth
dt_REER_g <- lapply(dt_REER, calc_growth)

##==========================================
##9.1 Housing Real Price Growth
##==========================================

# Load Real House Price
file <- "Input_Housing.xlsx"
range <- "A3:IV196"
sheet <- c('REAL_PRICE')
dt_RHP <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    )

# Calculate growth
dt_RHP_g <- lapply(dt_RHP, calc_growth)

##==========================================
##9.2 Housing Valuation Ratios
##==========================================

# Load Housing Valuation
sheet <- list()
sheet[c('PRICE_RENT', 'PRICE_INCOME')] <-
    list('PRICE_RENT', 'PRICE_INCOME')
dt_HV <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    )


##==========================================
##10. Credit-GDP Gap and Credit/GDP change/growth
##==========================================

# Load Credit Gap, etc
file <- 'Input_CreditGDP.xlsx'
sheet <- list()
sheet[c('Credit_Gap', 'Credit_GDP_Change','Credit_GDP_Growth')] <-
    list('Credit_Gap', 'Credit_GDP_Change','Credit_GDP_Growth')
dt_Credit <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    )

# Load Credit Growth
sheet <- list()
sheet[c('Credit_Growth')] <-
    list('Credit_Growth')
dt_Credit_Growth <- lapply(sheet, clean_excel, folder = folder, file = file, range = range, freq = "Q")

dt_CPI_Growth <- calc_growth(dt_CPI, q_lag= 4) #calculate yoy growth
dt_Credit_Real_Growth <- lapply(dt_Credit_Growth, calc_2_table, dt_CPI_Growth, operator ='-')


##==========================================
##11. ARA Metric
##==========================================

# Load ARA Metric
file <- "SPRIRU_ARA_METRIC.xlsx"
range <- "G201:IV396"
sheet <- list()
sheet[c('ARA_A')] <- list('ARA')
dt_ARA_A <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "A"
    )

#Convert annual data to quarterly data
dt_ARA_Q <- list()
dt_ARA_Q[['ARA']] <- dt_ARA_A$ARA %>%
    mutate(
        Q.1 = ARA,
        Q.2 = ARA,
        Q.3 = ARA,
        Q.4 = ARA
    ) %>%
    select(-ARA) %>%
    gather(Quarter, ARA,-c(Country, Code, Year)) %>%
    mutate(Year_Quarter = as.numeric(gsub('Q.', '', Quarter)) + Year * 100) %>%
    select(-Quarter) %>%
    mutate(Quarter = yq((as.character(Year_Quarter))) + months(3) - days(1)) %>%
    select(Country, Code, Quarter, ARA)

##==========================================
##12. Financial Variables
##==========================================

# Equity Market Cap to GDP ratio
file <- "Input_Financial.xlsx"
range <- "A3:IV196"
dt_EQ_MKT_Q <- list()
sheet <- list()
sheet[c('Equity_Marketcap_Q')] <- list('Equity_Marketcap_Q')

dt_EQ_MKT_Q <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    )
dt_EQ_v_GDP <-
    lapply(dt_EQ_MKT_Q, calc_2_table, dt_GDP, operator = '/')

# Real Stock Market Return
sheet <- list()
sheet[c('Equity_Index')] <- list('Equity_Index')
dt_EQ_Index <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    )
dt_EQ_Growth <-
    lapply(dt_EQ_Index, calc_2_table, dt_CPI, operator = '/')
dt_EQ_Growth <- lapply(dt_EQ_Growth, calc_growth)

# Bond Indices: Sovereign FX debt Spread/ Sovereign Risk Premium/ Government Bond Yield
sheet <- list()
sheet[c('EMBIG_Spread', 'CDS_Spread', 'GGR_Yield')] <-
    list('EMBIG_Spread', 'CDS_Spread', 'GGR_Yield')
dt_Bond <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    )

# Asset Volatility
sheet <- list()
sheet[c('EQ_Return', 'Spot_Return', 'GGR_Return')] <-
    list('EQ_Return', 'Spot_Return', 'GGR_Return')
range="A3:NZ196"
dt_Asset_Return_M <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "M"
    )
dt_Asset_Vol_M <-
    lapply(dt_Asset_Return_M, calc_vol, freq = "M", lag = 6)

dt_Asset_Vol <-
    lapply(dt_Asset_Vol_M, filter, month(Month) %in% c(3, 6, 9, 12))
dt_Asset_Vol <- lapply(dt_Asset_Vol, rename, Quarter = Month)

# Real Overnight Interbank Rate
sheet<-list()
sheet[c('ON_Rate')]<- list("ON_Rate")
dt_ON_Rate <- lapply(sheet, clean_excel, folder = folder,  file = file,  range = range, freq = "Q")
dt_ON_Rate <- lapply(dt_ON_Rate, calc_2_table, dt_CPI_Growth, operator ='-')

# 3Mo Libor-OIS Spread
sheet<- list()
sheet[c('LIBOR_OIS')]<- list("LIBOR_OIS")
dt_LIBOR_OIS <- lapply(sheet, clean_excel, folder = folder,  file = file,  range = range, freq = "Q")

##==========================================
## Lastly, Write All results to Excel
##==========================================

#rename variables

names(dt_BL_Private_v_GDP) <-
    paste("BL_", names(dt_BL_Private_v_GDP), "_v_GDP",  sep = "")
names(dt_BL_Private_FX_v_TOT) <-
    paste("BL_", names(dt_BL_Private_FX_v_TOT), '_v_FL',  sep = "")
names(dt_BL_Private_g) <-
    paste("BL_", names(dt_BL_Private_g), "_g",  sep = "")

names(dt_BL_Public_g) <- "BL_PUB_g"

names(dt_BL_ALL_Sector_v_ALL) <-
    paste("BL_",
          names(dt_BL_ALL_Sector_v_ALL),
          "_v_ALL_BL",
          sep = "")

names(dt_OFIL_Sector_v_GDP) <-
    paste("OFIL_", names(dt_OFIL_Secotr_g), "_v_GDP",  sep = "")
names(dt_OFIL_Secotr_g) <-
    paste("OFIL_", names(dt_OFIL_Secotr_g), "_g",  sep = "")

names(dt_Exn_Debt) <-
    paste("Exn_Debt_ST_v_ALL_", names(dt_Exn_Debt),  sep = '')

names(dt_Exn_Debt_Sector_g) <-
    paste("Exn_Debt_Sector_g_", names(dt_Exn_Debt_Sector_g),  sep = '')

names(dt_Exn_Debt_ST_v_ALL_Sector) <-
    paste("Exn_Debt_ST_v_ALL_",
          names(dt_Exn_Debt_ST_v_ALL_Sector),
          sep = '')

names(dt_Exn_Debt_Sector_v_GDP) <-
    paste("Exn_Debt_Sector_v_GDP_",
          names(dt_Exn_Debt_Sector_v_GDP),
          sep = '')

names(dt_Credit_Real_Growth)<- "Credit_Real_Growth"

names(dt_REER_g) <- 'REER_g'
names(dt_RHP_g) <- 'RHP_g'

names(dt_FB_v_GDP) <-
    paste(names(dt_FB_v_GDP), "_v_GDP",  sep = '')

names(dt_ARA_Q) <- "ARA"

names(dt_EQ_v_GDP) <- "dt_EQ_v_GDP"

names(dt_EQ_Growth) <- "Equity_Growth"

names(dt_EQ_v_GDP) <- "EQ_v_GDP"

names(dt_Asset_Vol) <- paste(names(dt_Asset_Vol), "_vol", sep = "")

## Save Raw Data and Percentiles

saveData = list()

saveData = append(
    saveData,
    c(
        dt_BL_Private_v_GDP,
        dt_BL_Private_FX_v_TOT,
        dt_BL_Private_g,
        dt_BL_Public_g,
        dt_BL_ALL_Sector_v_ALL,
        dt_OFIL_Sector_v_GDP,
        dt_OFIL_Secotr_g,
        dt_FSI,
        dt_Exn_Debt_ST_v_ALL_Sector,
        dt_Exn_Debt_Sector_g,
        dt_Exn_Debt_Sector_v_GDP,
        dt_Gov_Debt_v_GDP,
        dt_BOP,
        dt_REER_g,
        dt_RHP_g,
        dt_HV,
        dt_Credit,
        dt_Credit_Real_Growth,
        dt_Credit_Growth,
        dt_ARA_Q,
        dt_EQ_v_GDP,
        dt_EQ_Growth,
        dt_Bond,
        dt_Asset_Vol,
        dt_ON_Rate,
        dt_LIBOR_OIS
    )
)

saveDataWide <- lapply(saveData, to_wide)


# #### Save to existing files
#
saveFile <- "Q:/DATA/SPRAIMU/4_SysRisk/Data/Result_Raw.xlsx"

wb <- loadWorkbook(saveFile)
sheetName <- getSheetNames(saveFile)

for (n in sheetName[c(2:length(sheetName))]) {
    writeData(wb, x = saveDataWide[[n]], sheet = n)
}

# check consistency and save
if (! FALSE %in% (names(wb)[2:length(names(wb))] == names(saveDataWide))) {
    saveWorkbook(wb, saveFile, overwrite = TRUE)
}



# saveFile <- "Q:/DATA/SPRAIMU/4_SysRisk/Data/Result_Percentile.xlsx"
# wb <- loadWorkbook(saveFile)
# sheetName <- getSheetNames(saveFile)
#
# for (n in sheetName[c(2:57)]) {
#     writeData(wb, x = savePercentileWide[[n]], sheet = n)
# }
#
# saveWorkbook(wb, saveFile, overwrite = TRUE)

# Save Raw with date

# saveFile = paste(folder, "Result_Raw_", gsub("-", "", today()), ".xlsx", sep =
#                      '')
# openxlsx::write.xlsx(saveDataWide, file = saveFile)


# #### Calculate and save percentiles
# 
# savePercentileWide <-
#     lapply(saveData, calc_percentile_panel, by_column = TRUE)
# 
# names(savePercentileWide) <-
#     paste(names(savePercentileWide), suffix, sep = "")
# 
# saveFile = paste(folder,
#                  "Result_Percentile_",
#                  gsub("-", "", today()),
#                  ".xlsx",
#                  sep = '')
# openxlsx::write.xlsx(savePercentileWide, file = saveFile)
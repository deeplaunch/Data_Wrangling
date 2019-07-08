##@Auther: Harry Peng Zhao

##===Main scripts that call individual data/ functions to produce various System Risk indicators===
##===Load, clean, manipulate, and save results to R and Excel for Tableau processing===##

##===========================================
## 0. Setup
##===========================================

rm(list = ls())

setwd("Q:/DATA/SPRAIMU/4_SysRisk/R Code")
folder <- "Q:/DATA/SPRAIMU/4_SysRisk/Data/"
saveFolder <- "Q:/DATA/SPRAIMU/4_SysRisk/Data/Output/"
daily_price_folder <- 'Q:/DATA/SPRAIMU/4_SysRisk/Data/Input_Databases/Bloomberg/'
daily_price_file <- 'SysRisk_Market_Bloomberg Data.xlsx'

library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(openxlsx)

myfunc <-c('load_and_clean.R','single_table_func.R','reshaping.R','two_table_func.R','process_for_tableau.R')

lapply(myfunc, source, verbose = FALSE)

##==========================================
## 1.1 Bank Loan by Private Sector to GDP
##==========================================

# Load GDP (already annualized)
file <- "Input_GDP.xlsx"
range <- "A3:IV200"
df_GDP <-
    clean_excel(
        folder = folder,
        file = file,
        sheet = "GDP",
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

df_BL <-
    lapply (
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    ) # Bank Loans Sector, Currency

# Add up LC and FX loans for each sector
df_BL_Private <- list()
df_BL_Private["HH"] <-
    list(calc_2_table(df_BL$L_HH, df_BL$F_HH, operator = '+')) ##HouseHold Banks Loans
df_BL_Private["ONC"] <-
    list(calc_2_table(df_BL$L_ONC, df_BL$F_ONC, operator = '+')) ## Other Non-Financial

## Calculate Ratio to GDP for each sector
df_BL_Private_v_GDP <-
    lapply(df_BL_Private, calc_2_table, df_GDP, operator = '/')


##=====================================================
## 1.2 Bank Loan by Private Sector (FX Loan/Total Loan)
##=====================================================

## Calculate Ratio of FX/overall bank loan
df_BL_F <- df_BL[c("F_HH"  , "F_ONC")]
#df_BL_F[["F_ALL"]]<- df_F_ALL

df_BL_Private_FX_v_TOT <-
    mapply(calc_2_table,
           df_BL_F,
           df_BL_Private,
           operator = '/',
           SIMPLIFY = FALSE)

##==============================================================
## 1.3 Bank Loan by Private Sector Growth adjusted by CPI index
##==============================================================

# Load CPI index
file <- "Input_CPI.xlsx"
range <- "A3:IV196"
df_CPI_Q <-
    clean_excel(
        folder = folder,
        file = file,
        sheet = "IFS_Q",
        range = range,
        freq = "Q"
    )

df_CPI_A <-
    clean_excel(
        folder = folder,
        file = file,
        sheet = "IFS_A",
        range = range,
        freq = "A"
    )

df_CPI<- back_fill(tableA = df_CPI_A, tableQ =  df_CPI_Q)

# Calculate real growth
df_BL_Private_g <-
    lapply(df_BL_Private, calc_2_table, df_CPI, operator = '/')
df_BL_Private_g <- lapply(df_BL_Private_g, calc_growth, lags = 4)

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

df_BL <-
    lapply (
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    ) # Bank Loans Sector

# Add up bank loans to all public sectors
df_BL_Public <- list()

df_BL_Public["PUB"] <-
    list(calc_2_table(df_BL$LOAN_CG, df_BL$LOAN_PC, operator = '+'))
df_BL_Public["PUB"] <-
    list(calc_2_table(df_BL_Public$PUB, df_BL$LOAN_LG, operator = '+'))

## Calculate Ratio to GDP for each sector
df_BL_Public_v_GDP <-
    lapply(df_BL_Public, calc_2_table, df_GDP, operator = '/')

## Calculate Real growth

df_BL_Public_g <-
    lapply(df_BL_Public, calc_2_table, df_CPI, operator = '/')
df_BL_Public_g <- lapply(df_BL_Public_g, calc_growth, lags = 4)

##==========================================
##2.2 Bank Loan Sector to Total
##==========================================

df_BL_ALL_Sector <- list()
df_BL_ALL_Sector[c('HH', 'ONC', 'PUB')] <-
    list(df_BL_Private$HH, df_BL_Private$ONC, df_BL_Public$PUB)

df_BL_ALL <-
    calc_2_table(df_BL_ALL_Sector$HH, df_BL_ALL_Sector$ONC, '+')
df_BL_ALL <- calc_2_table(df_BL_ALL, df_BL_ALL_Sector$PUB, '+')

df_BL_ALL_Sector_v_ALL <-
    lapply(df_BL_ALL_Sector, calc_2_table, df_BL_ALL, operator = '/')

# Below is to avoid cases where shares are 100% because other sectors are NAs
df_BL_ALL_Sector_v_ALL <-
    lapply(df_BL_ALL_Sector_v_ALL, replace_value, value = 100) 

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

df_OFIL <-
    lapply (
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    ) # Bank Loans Sector, Currency

# Add up LC and FX loans for each sector
df_OFIL_Sector <- list()

df_OFIL_Sector["HH"] <-
    list(calc_2_table(df_OFIL$L_HH, df_OFIL$F_HH, operator = '+')) ##HouseHold Banks Loans
df_OFIL_Sector["ONC"] <-
    list(calc_2_table(df_OFIL$L_ONC, df_OFIL$F_ONC, operator = '+')) ## Other Non-Financial

df_OFIL_Sector["PRV"] <-
    list(calc_2_table(df_OFIL_Sector$HH, df_OFIL_Sector$ONC , operator = '+'))   ## Private Sector

## Calculate Ratio to GDP for each sector
df_OFIL_Sector_v_GDP <-
    lapply(df_OFIL_Sector, calc_2_table, df_GDP, operator = '/')

##============================================================================
##3.2 Other Financial Institution Loan by Sector Growth adjusted by CPI index
##============================================================================

# Calculate real growth
df_OFIL_Secotr_g <-
    lapply(df_OFIL_Sector, calc_2_table, df_CPI, operator = '/')
df_OFIL_Secotr_g <- lapply(df_OFIL_Secotr_g, calc_growth, lags = 4)


##==========================================
##4. FSI Indicators
##==========================================

# Load FSI file
file <- "Input_FSI.xlsx"
range <- "A9:IV206"
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
        'CAR_Q',
        'Leverage_Rat_Q',
        'NPL_PRO_Q',
        'LIQ_Ass_ST_Liab_Q',
        'NPL_TOT_Q',
        'ROA_Q',
        'L_D_Ratio_Q',
        'FX_CAP_Q',
        'FX_Loans_Q',
        'FX_Liability_Q'
    )

df_FSI_Q <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    )

sheet<- list()

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
        'CAR_A',
        'Leverage_Rat_A',
        'NPL_PRO_A',
        'LIQ_Ass_ST_Liab_A',
        'NPL_TOT_A',
        'ROA_A',
        'L_D_Ratio_A',
        'FX_CAP_A',
        'FX_Loans_A',
        'FX_Liability_A'
    )

df_FSI_A <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    )

# Some rename of annual table
df_FSI_A <- lapply(df_FSI_A, mutate, Year = year(Quarter))
df_FSI_A <- lapply(df_FSI_A, select, -Quarter)

# Merge Quarterly with annual
df_FSI <- mapply(merge_2_table, df_FSI_A, df_FSI_Q, SIMPLIFY = FALSE)

# Some formatting
df_FSI$FX_CAP$FX_CAP_Q <- abs(df_FSI$FX_CAP$FX_CAP_Q) # Take absolute value of Net open FX position to capital
df_FSI$L_D_Ratio$L_D_Ratio_Q <- 10000/df_FSI$L_D_Ratio$L_D_Ratio_Q # Convert FSI Deposit-to-Loan ratio to Loan-to-Deposit

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

df_Exn_Debt <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q",
        include_zero = TRUE
    )

# Add up loans and securities for ST and LT in each sector, make total NA if any sub-category is NA using removeNA = FALSE
df_Exn_Debt_ST <- list()
df_Exn_Debt_ST["GOV"] <-
    list(calc_2_table(df_Exn_Debt$SEC_ST_GOV, df_Exn_Debt$LOAN_ST_GOV, operator ='+', removeNA = FALSE))
df_Exn_Debt_ST["OTH"] <-
    list(calc_2_table(df_Exn_Debt$SEC_ST_OTH, df_Exn_Debt$LOAN_ST_OTH, operator ='+', removeNA = FALSE))

df_Exn_Debt_LT <- list()
df_Exn_Debt_LT["GOV"] <-
    list(calc_2_table(df_Exn_Debt$SEC_LT_GOV, df_Exn_Debt$LOAN_LT_GOV, operator = '+', removeNA = FALSE))
df_Exn_Debt_LT["OTH"] <-
    list(calc_2_table(df_Exn_Debt$SEC_LT_OTH, df_Exn_Debt$LOAN_LT_OTH, operator ='+', removeNA = FALSE))

# Add up ST and LT in each sector
df_Exn_Debt_Sector_USD <- list()
df_Exn_Debt_Sector_USD["GOV"] <-
    list(calc_2_table(df_Exn_Debt_ST$GOV, df_Exn_Debt_LT$GOV, operator = '+', removeNA = FALSE))
df_Exn_Debt_Sector_USD["OTH"] <-
    list(calc_2_table(df_Exn_Debt_ST$OTH, df_Exn_Debt_LT$OTH, operator = '+', removeNA = FALSE))

# Calculate Short-Term Debt as % of Total External Debt for each sector (Loans & Debt Securities)
df_Exn_Debt_ST_v_ALL_Sector <- list()
df_Exn_Debt_ST_v_ALL_Sector['GOV'] <-
    list(calc_2_table(df_Exn_Debt_ST$GOV, df_Exn_Debt_Sector_USD$GOV, operator =
                          '/'))
df_Exn_Debt_ST_v_ALL_Sector['OTH'] <-
    list(calc_2_table(df_Exn_Debt_ST$OTH, df_Exn_Debt_Sector_USD$OTH, operator =
                          '/'))

##==============================================
##6.2 External Debt Growth adjusted by CPI index
##==============================================

# Load FX rate
file <- "Input_Ext_Debt.xlsx"
range <- "A6:IV196"
df_FX <-
    clean_excel(
        folder = folder,
        file = file,
        sheet = "FX_RATE",
        range = range,
        freq = "Q"
    )

# Calculate external debt in local currency
df_Exn_Debt_Sector <-
    lapply(df_Exn_Debt_Sector_USD, calc_2_table, df_FX, operator = '*')

# Calculate real growth
df_Exn_Debt_Sector_g <-
    lapply(df_Exn_Debt_Sector, calc_2_table, df_CPI, operator = '/')

df_Exn_Debt_Sector_g <- lapply(df_Exn_Debt_Sector_g, calc_growth, lags = 4)


##==========================================
##6.3 External Debt by Sector, % of GDP
##==========================================

## Calculate Ratio to GDP for each sector
df_Exn_Debt_Sector_GDP <-
    lapply(df_Exn_Debt_Sector, calc_2_table, df_GDP, operator = '/')

##==========================================
##7.1 Government Debt to GDP
##==========================================

# Load Government Debt sheet
file <- "Input_Gov.xlsx"
range <- "A3:IV196"
sheet <- list()
sheet[c('GOV_Debt')] <- list('GOV_Debt')

df_GOV_Debt <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    )

## Calculate Ratio to GDP
df_Gov_Debt_v_GDP <- lapply(df_GOV_Debt, calc_2_table, df_GDP, operator = '/')

##==================================================
##7.2 External Government Debt/ Total Government Debt
##===================================================

df_Gov_Ext_v_Debt <- lapply(df_Exn_Debt_Sector[1], calc_2_table, df_GOV_Debt$GOV_Debt, operator = '/')

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

df_BOP <-
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
df_REER <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    )

# Calculate REER growth
df_REER_g <- lapply(df_REER, calc_growth, lags = 4)

##==========================================
##9.1 Housing Real Price Growth
##==========================================

# Load Real House Price
file <- "Input_Housing.xlsx"
range <- "A3:IV196"
sheet <- c('REAL_PRICE')
df_RHP <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    )

# Calculate growth
df_RHP_g <- lapply(df_RHP, calc_growth, lags = 4)

##==========================================
##9.2 Housing Valuation Ratios
##==========================================

# Load Housing Valuation
sheet <- list()
sheet[c('PRICE_RENT', 'PRICE_INCOME')] <-
    list('PRICE_RENT', 'PRICE_INCOME')
df_HV <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    )


##===============================================
##10. Credit-GDP Gap and Credit/GDP change/growth
##===============================================

# Load Credit Gap, etc
file <- 'Input_CreditGDP.xlsx'
range <- 'A3:IV200'
sheet <- list()
sheet[c('Credit_Gap_HP', 'Credit_GDP_Change','Credit_GDP_Growth','Credit_Gap_Cubic','Credit_Growth')] <-
    list('Credit_Gap_HP', 'Credit_GDP_Change','Credit_GDP_Growth','Credit_Gap_Cubic','Credit_Growth')
df_Credit <- 
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    )

# Calculate Real Credit Growth
df_CPI_Growth <- calc_growth(df_CPI, lags= 4) #calculate yoy growth
Credit_Real_Growth <- lapply(df_Credit[5], calc_2_table, df_CPI_Growth, operator ='-')
df_Credit["Credit_Real_Growth"]<- Credit_Real_Growth 

# Calculate Flags by Del'Ariccia
Credit_Flag1 <- full_join(df_Credit$Credit_Gap_Cubic, df_Credit$Credit_GDP_Growth, by =c('Country','Code','Quarter'))

Credit_Flag1 <- Credit_Flag1%>%
    mutate(Flag = Credit_Gap_Cubic>1.5 & Credit_GDP_Growth > 10 )%>%
    select(-c(Credit_Gap_Cubic, Credit_GDP_Growth))

Credit_Flag2 <- df_Credit$Credit_GDP_Growth%>%
    mutate(Flag = Credit_GDP_Growth > 20)%>%
    select(-Credit_GDP_Growth)

df_Credit['Flag1'] <- list(Credit_Flag1)
df_Credit['Flag2'] <- list(Credit_Flag2)

##==========================================
##11. ARA Metric
##==========================================

# Load ARA Metric
# file <- "SPRIRU_ARA_METRIC.xlsx"
# range <- "G201:IV396"
# sheet <- list()
# sheet[c('ARA_A')] <- list('ARA')
# df_ARA_A <-
#     lapply(
#         sheet,
#         clean_excel,
#         folder = folder,
#         file = file,
#         range = range,
#         freq = "A"
#     )
# 
# #Convert annual data to quarterly data
# df_ARA_Q <- list()
# df_ARA_Q[['ARA']]<- annual_to_quarter(df_ARA_A[[1]])

##==========================================
##12. Financial Variables
##==========================================

# Equity Market Cap to GDP ratio
file <- "Input_Financial.xlsx"
range <- "A3:IV196"
df_EQ_MKT_Q <- list()
sheet <- list()
sheet[c('Equity_Marketcap_Q')] <- list('Equity_Marketcap_Q')

df_EQ_MKT_Q <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    )
df_EQ_v_GDP <-
    lapply(df_EQ_MKT_Q, calc_2_table, df_GDP, operator = '/')

# Real Stock Market Return
sheet <- list()
sheet[c('Equity_Index')] <- list('Equity_Index')
df_EQ_Index <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    )

df_EQ_Growth <-
    lapply(df_EQ_Index, calc_2_table, df_CPI, operator = '/')
df_EQ_Growth <- lapply(df_EQ_Growth, calc_growth, lags = 4)

# Bond Indices: Sovereign FX debt Spread/ Sovereign Risk Premium/ Government Bond Yield
sheet <- list()
sheet[c('EMBIG_Spread', 'CDS_Spread', 'GGR_Yield')] <-
    list('EMBIG_Spread', 'CDS_Spread', 'GGR_Yield')
df_Bond <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "Q"
    )
# merge EMBIG spread with CDS spread
df_Bond$EMBIG_Spread$EMBIG_Spread<-df_Bond$EMBIG_Spread$EMBIG_Spread * 100
df_spread<- full_join(df_Bond$EMBIG_Spread, df_Bond$CDS_Spread, by = c('Country','Code','Quarter'))
df_spread[is.na(df_spread$EMBIG_Spread),4]<- df_spread[is.na(df_spread$EMBIG_Spread),5]
df_Bond[['Sov_Spread']]<- as_tibble(df_spread%>%
                                        select(-CDS_Spread)%>%
                                        rename(Sov_Spread = EMBIG_Spread))


# Asset Volatility
sheet <- list()
sheet[c('EQ', 'FX', 'Yield')] <-
    list('Equity_Index_Level', 'Spot_Rate', 'GGR_Yield')

df_Asset_Level <-
    lapply(
        sheet,
        clean_excel_transpose,
        folder = daily_price_folder,
        file = daily_price_file,
        freq = "D"
    )

df_Asset_Return <- 
    lapply( df_Asset_Level, calc_growth, freq ='D', lags =1
    )

df_Asset_Return$Yield$Level_g<- log(df_Asset_Return$Yield$Level_g / 100 +1) ## Use log return for bond yield

df_Asset_Vol <-
    lapply(df_Asset_Return, calc_vol, freq = "D", lags = 60)

df_yield <- left_join(df_Asset_Vol$Yield, df_Asset_Level$Yield, by = c('Country','Code','Day'))
df_yield <- df_yield%>%
    mutate(newVol = Vol * Level * 100 )%>%
    select(Country, Code, Day, newVol)%>%
    rename(Vol = newVol) # Special treatment for bond yield volatility

df_Asset_Vol$Yield <- df_yield
df_Asset_Vol <- lapply(df_Asset_Vol, rename, Quarter = Day)

# Real Overnight Interbank Rate
sheet<-list()
sheet[c('ON_Rate')]<- list("ON_Rate")
df_ON_Rate <- lapply(sheet, clean_excel, folder = folder,  file = file,  range = range, freq = "Q", include_zero = TRUE)
df_ON_Rate <- lapply(df_ON_Rate, calc_2_table, df_CPI_Growth, operator ='-')

# 3Mo Libor-OIS Spread
sheet<- list()
sheet[c('LIBOR_OIS')]<- list("LIBOR_OIS")
df_LIBOR_OIS <- lapply(sheet, clean_excel, folder = folder,  file = file,  range = range, freq = "Q")

##==========================================
##13. Financing Needs (Corp & Gov)
##==========================================

file <- "Input_BOP.xlsx"
range <- "A6:IV196"
df_DBT_AMORT <- list()
sheet <- list()
sheet[c('FISC_DEF', 'TOT_DBT_AMORT','TOT_GOV_DBT_AMORT','GDP_USD')] <- 
    list('FISC_DEF_WEO_A','TOT_DBT_AMORT_WEO_A','TOT_GOV_DBT_AMORT_WEO_A','GDP_WEO_A')

df_DBT_AMORT <-
    lapply(
        sheet,
        clean_excel,
        folder = folder,
        file = file,
        range = range,
        freq = "A"
    )

df_DBT_AMORT<- lapply(df_DBT_AMORT, annual_to_quarter)

##Corporate external debt amortization to GDP ratio (%)
df_DBT_AMORT[["TOT_COP_DBT_AMORT"]] <-
    calc_2_table(df_DBT_AMORT$TOT_DBT_AMORT, df_DBT_AMORT$TOT_GOV_DBT_AMORT, operator = '-')
df_DBT_AMORT[['COP_DBT_AMORT_v_GDP']]<- 
    calc_2_table(df_DBT_AMORT$TOT_COP_DBT_AMORT, df_DBT_AMORT$GDP_USD, operator = '/')

df_DBT_NEED<- list()
df_DBT_NEED[['COP_DBT_AMORT_v_GDP']] <- df_DBT_AMORT[["COP_DBT_AMORT_v_GDP"]]

## General government financing needs (% of GDP)
df_DBT_NEED[['GOV_FIN_NEED_v_GDP']]<- 
    calc_2_table(df_DBT_AMORT$TOT_GOV_DBT_AMORT, df_DBT_AMORT$GDP_USD, operator = '/')
df_DBT_NEED[['GOV_FIN_NEED_v_GDP']]<- 
    calc_2_table(df_DBT_NEED[['GOV_FIN_NEED_v_GDP']],df_DBT_AMORT$FISC_DEF, operator ='-' )

##==========================================
##14. Sectoral Debt Servicing
##==========================================

##====
## a. Calculate Debt to Equity for HH and Corporates
##====

## Load Debt Data
file <- "EU_Sectoral_Financial_Accounts.xlsx"
range <- "A8:IV196"
df_DBT_SEC_Q <- list()
sheet <- list()
sheet[c('HH','NFC')] <- 
    list('DO_HH_Debt_N_Q','DO_NFC_Debt_N_Q')

df_DBT_SEC_Q <- lapply(
    sheet,
    clean_excel,
    folder = folder,
    file = file,
    range = range,
    freq = "Q"
)

df_DBT_SEC_A <- list()
sheet <- list()
sheet[c('HH','NFC')] <- 
    list('DO_HH_Debt_N_A','DO_NFC_Debt_N_A')

df_DBT_SEC_A <- lapply(
    sheet,
    clean_excel,
    folder = folder,
    file = file,
    range = range,
    freq = "A"
)

df_DBT_SEC_MG <- 
    mapply(merge_2_table, tableA = df_DBT_SEC_A, tableQ = df_DBT_SEC_Q, SIMPLIFY =  FALSE)

# Load Equity Data

sheet <- list()
sheet[c('HH','NFC')]  <- 
    list('DO_HH_NetWorth_N_Q','DO_NFC_Equity_N_Q')

df_EQ_SEC_Q <- lapply(
    sheet,
    clean_excel,
    folder = folder,
    file = file,
    range = range,
    freq = "Q"
)

df_EQ_SEC_A <- list()
sheet <- list()
sheet[c('HH','NFC')]  <- 
    list('DO_HH_NetWorth_N_A','DO_NFC_Equity_N_A')

df_EQ_SEC_A <- lapply(
    sheet,
    clean_excel,
    folder = folder,
    file = file,
    range = range,
    freq = "A"
)

df_EQ_SEC_MG <- 
    mapply(merge_2_table, tableA = df_EQ_SEC_A, tableQ = df_EQ_SEC_Q, SIMPLIFY =  FALSE)

# Calculate Debt to Equity Ratio
df_DBT_EQ <- 
    mapply(calc_2_table, table1 = df_DBT_SEC_MG, table2 = df_EQ_SEC_MG, operator ='/', SIMPLIFY = FALSE)


##================================================================
## b. Calculate Debt to Income for HH, Corporates and Government
##================================================================

# Load Income for all sectors
file <- "EU_Sectoral_NonFinancial_Accounts.xlsx"
range <- "A7:IV196"
df_INC_SEC_Q <- list()
sheet <- list()
sheet[c('HH','NFC','GOV')] <- 
    list('DO_HH_GDI_Adj_N_Q','DO_NFC_GDI_Adj_N_Q','DO_GOV_GDI_Adj_N_Q')

df_INC_SEC_Q <- lapply(
    sheet,
    clean_excel,
    folder = folder,
    file = file,
    range = range,
    freq = "Q"
)
# Calculate trailing 4-quarter sums
df_INC_SEC_Q <- lapply(df_INC_SEC_Q, calc_trailing_sum )

# Load annual 
sheet <- list()
sheet[c('HH','NFC','GOV')] <- 
    list('DO_HH_GDI_Adj_N_A','DO_NFC_GDI_Adj_N_A','DO_GOV_GDI_Adj_N_A')

df_INC_SEC_A <- lapply(
    sheet,
    clean_excel,
    folder = folder,
    file = file,
    range = range,
    freq = "A"
)
##Merging trailing 4 quarter with annual
df_INC_SEC_MG <- 
    mapply(merge_2_table, tableA = df_INC_SEC_A, tableQ = df_INC_SEC_Q, SIMPLIFY =  FALSE)

## Add GOV to Debt List
file <- "EU_Sectoral_Financial_Accounts.xlsx"
range <- "A8:IV196"
sheet <- 'DO_GOV_Debt_N_Q'
df_DBT_GOV_Q <- clean_excel(folder = folder, file = file, sheet = sheet,range = range,freq = "Q")

sheet <- 'DO_GOV_Debt_N_A'
df_DBT_GOV_A <- clean_excel(folder = folder, file = file, sheet = sheet,range = range,freq = "A")

df_DBT_GOV_MG <- merge_2_table(tableA = df_DBT_GOV_A, tableQ = df_DBT_GOV_Q)
df_DBT_SEC_MG[['GOV']]<- df_DBT_GOV_MG

# Calculate Debt to Income
df_DBT_v_INC <- 
    mapply(calc_2_table, table1 = df_DBT_SEC_MG, table2 = df_INC_SEC_MG, operator ='/', SIMPLIFY = FALSE)


##============================================
## c. Calculate Interest Payment to Income
##============================================

# Load Interest Expense for all sectors
file <- "EU_Sectoral_Financial_Accounts.xlsx"
range <- "A8:IV196"
df_INT_EXP_Q <- list()
sheet <- list()
sheet[c('HH','NFC','GOV')] <- 
    list('HH_D4G_N_Q','NFC_D4G_N_Q','GOV_D4G_N_Q')

df_INT_EXP_Q <- lapply(
    sheet,
    clean_excel,
    folder = folder,
    file = file,
    range = range,
    freq = "Q"
)

# Calculate trailing 4-quarter sums
df_INT_EXP_Q <- lapply(df_INT_EXP_Q, calc_trailing_sum )

# Load annual 
sheet <- list()
sheet[c('HH','NFC','GOV')] <- 
    list('HH_D4G_N_A','NFC_D4G_N_A','GOV_D4G_N_A')

df_INT_EXP_A <- lapply(
    sheet,
    clean_excel,
    folder = folder,
    file = file,
    range = range,
    freq = "A"
)
##Merging trailing 4 quarter with annual
df_INT_EXP_MG <- 
    mapply(merge_2_table, tableA = df_INT_EXP_A, tableQ = df_INT_EXP_Q, SIMPLIFY =  FALSE)

##Calculate Interest Expense to Income
df_INT_EXP_v_INC <- 
    mapply(calc_2_table, table1 = df_INT_EXP_MG, table2 = df_INC_SEC_MG, operator ='/', SIMPLIFY = FALSE)

##============================================
## d. Calculate Interest Rate - Income Growth
##============================================

## Load interest rate
file <- "EU_Sectoral_Financial_Accounts.xlsx"
range <- "A8:IV196"
df_INT_RT_Q <- list()
sheet <- list()
sheet[c('HH','NFC','GOV')] <- 
    list('DO_HH_INT_RAT_N_Q','DO_NFC_INT_RAT_N_Q','DO_GOV_INT_RAT_N_Q')

df_INT_RT_Q <- lapply(
    sheet,
    clean_excel,
    folder = folder,
    file = file,
    range = range,
    freq = "Q"
)

# Load annual data
sheet <- list()
sheet[c('HH','NFC','GOV')] <- 
    list('DO_HH_INT_RAT_N_A','DO_NFC_INT_RAT_N_A','DO_GOV_INT_RAT_N_A')

df_INT_RT_A <- lapply(
    sheet,
    clean_excel,
    folder = folder,
    file = file,
    range = range,
    freq = "A"
)

# Merge quarterly and annual
df_INT_RT_MG <- mapply(merge_2_table, tableA = df_INT_RT_A, tableQ = df_INT_RT_Q, SIMPLIFY =  FALSE)

# Calculate income growth
df_INC_G <- lapply(df_INC_SEC_MG, calc_growth, lags = 4)

# Take difference between Interest Rate and Income Growth
df_INT_m_INC_G <- 
    mapply(calc_2_table, table1 = df_INT_RT_MG, table2 = df_INC_G, operator ='-', SIMPLIFY =  FALSE)

##====================================
## 15. Probabilty of Default (PD)
##====================================

## Load PD Data
file <- "PD_EconomySector_mean.xlsx"
range <- "A5:IV196"
df_PD <- list()
sheet <- list()
sheet[c('BANK','XBANK','CORP')] <- 
    list('BANK','XBANK','CORP')

df_PD <- lapply(
    sheet,
    clean_excel,
    folder = folder,
    file = file,
    range = range,
    freq = "Q"
)

##==========================================
## 16. Rename Variables
##==========================================

names(df_BL_Private_v_GDP) <-
    paste("BL_", names(df_BL_Private_v_GDP), "_v_GDP",  sep = "")
names(df_BL_Private_FX_v_TOT) <-
    paste("BL_", names(df_BL_Private_FX_v_TOT), '_v_FL',  sep = "")
names(df_BL_Private_g) <-
    paste("BL_", names(df_BL_Private_g), "_g",  sep = "")

names(df_BL_Public_g) <- "BL_PUB_g"
names(df_BL_ALL_Sector_v_ALL) <-
    paste("BL_",
          names(df_BL_ALL_Sector_v_ALL),
          "_v_ALL_BL",
          sep = "")

names(df_OFIL_Sector_v_GDP) <-
    paste("OFIL_", names(df_OFIL_Secotr_g), "_v_GDP",  sep = "")
names(df_OFIL_Secotr_g) <-
    paste("OFIL_", names(df_OFIL_Secotr_g), "_g",  sep = "")

names(df_Exn_Debt) <-
    paste("Exn_Debt_ST_v_ALL_", names(df_Exn_Debt),  sep = '')

names(df_Exn_Debt_Sector_g) <-
    paste("Exn_Debt_Sector_g_", names(df_Exn_Debt_Sector_g),  sep = '')

names(df_Exn_Debt_ST_v_ALL_Sector) <-
    paste("Exn_Debt_ST_v_ALL_",
          names(df_Exn_Debt_ST_v_ALL_Sector),
          sep = '')

names(df_Exn_Debt_Sector_GDP) <-
    paste("Exn_Debt_Sector_v_GDP_",
          names(df_Exn_Debt_Sector_GDP),
          sep = '')

names(df_Gov_Debt_v_GDP) <- 'Gov_Debt_v_GDP'
names(df_Gov_Ext_v_Debt) <- 'Gov_Ext_v_Debt'
names(df_REER_g) <- 'REER_g'
names(df_RHP_g) <- 'RHP_g'
# names(df_ARA_Q) <- "ARA"
names(df_EQ_v_GDP) <- "df_EQ_v_GDP"
names(df_EQ_Growth) <- "Equity_Growth"
names(df_EQ_v_GDP) <- "EQ_v_GDP"
names(df_Asset_Vol) <- paste(names(df_Asset_Vol), "_vol", sep = "")

names(df_DBT_EQ) <- paste("DBT_v_EQ_", names(df_DBT_EQ), sep ="")
names(df_DBT_v_INC)<- paste("DBT_v_INC_", names(df_DBT_v_INC), sep ="")
names(df_INT_EXP_v_INC)<- paste("INT_Exp_v_INC_", names(df_INT_EXP_v_INC), sep ="")
names(df_INT_m_INC_G) <- paste("INT_Rate_m_INC_G_", names(df_INT_m_INC_G), sep ="")

names(df_PD) <- paste("PD36_",names(df_PD), sep ="")

##===================================================================
## 17. Data Cleaning
##===================================================================

saveData = list()

saveData = append(
    saveData,
    c(
        df_BL_Private_v_GDP,
        df_BL_Private_FX_v_TOT,
        df_BL_Private_g,
        df_BL_Public_g,
        df_BL_ALL_Sector_v_ALL,
        df_OFIL_Sector_v_GDP,
        df_OFIL_Secotr_g,
        df_FSI,
        df_Exn_Debt_ST_v_ALL_Sector,
        df_Exn_Debt_Sector_g,
        df_Exn_Debt_Sector_GDP,
        df_Gov_Debt_v_GDP,
        df_Gov_Ext_v_Debt,
        df_BOP,
        df_REER_g,
        df_RHP_g,
        df_HV,
        df_Credit,
        #df_ARA_Q,
        df_EQ_v_GDP,
        df_EQ_Growth,
        df_Bond,
        df_Asset_Vol,
        df_ON_Rate,
        df_LIBOR_OIS,
        df_DBT_NEED,
        df_DBT_EQ,
        df_DBT_v_INC,
        df_INT_EXP_v_INC,
        df_INT_m_INC_G,
        df_PD
    )
)


# Take care of trading days not including quarter-end days
saveData<- lapply(saveData, mutate, Quarter = rollback(Quarter+5)) 
saveData_Panel<- 
    saveData%>%Reduce(function(dtf1,dtf2) full_join(dtf1,dtf2,by=c("Country","Code","Quarter")), .)
colnames(saveData_Panel)<- c("Country","Code","Quarter", names(saveData))

## And merge different country names with same code
country_code <- saveData_Panel%>%
    distinct(Country,Code)%>%
    group_by(Code)%>%
    summarise_all(max, na.rm = TRUE)

saveData_Panel <- saveData_Panel%>%
    select(-'Country')%>%
    group_by(Code, Quarter)%>%
    summarise_all(mean, na.rm = TRUE)

saveData_Panel<- left_join(country_code, saveData_Panel, by = 'Code')

# cleaning (see validation file)

saveData_Panel$BL_HH_g[which(saveData_Panel$BL_HH_g>2000)] <- NA

saveData_Panel$BL_ONC_g[which(saveData_Panel$BL_ONC_g>1000)] <- NA
saveData_Panel$BL_ONC_g[which(saveData_Panel$BL_ONC_g< -99)] <- NA

saveData_Panel$BL_PUB_g[which(saveData_Panel$BL_PUB_g>1000)] <- NA
saveData_Panel$BL_PUB_g[which(saveData_Panel$BL_PUB_g< -99)] <- NA

saveData_Panel$OFIL_HH_g[which(saveData_Panel$OFIL_HH_g>1000)] <- NA
saveData_Panel$OFIL_HH_g[which(saveData_Panel$OFIL_HH_g< -99)] <- NA

saveData_Panel$OFIL_ONC_g[which(saveData_Panel$OFIL_ONC_g>1000)] <- NA
saveData_Panel$OFIL_ONC_g[which(saveData_Panel$OFIL_ONC_g< -99)] <- NA

saveData_Panel$OFIL_PRV_g[which(saveData_Panel$OFIL_PRV_g>1000)] <- NA
saveData_Panel$OFIL_PRV_g[which(saveData_Panel$OFIL_PRV_g< -99)] <- NA

saveData_Panel$Gov_Debt_v_GDP[which(saveData_Panel$Gov_Debt_v_GDP>1000)] <- NA

saveData_Panel$Gov_Ext_v_Debt[which(saveData_Panel$Gov_Ext_v_Debt>100)] <- NA

View(saveData_Panel%>%filter(FX_vol ==0)%>%group_by(Country)%>%
         summarize(zeros =n(), earliest = min(Quarter), latest = max(Quarter))%>%
         arrange(desc(zeros)))

saveData_Panel <- saveData_Panel%>%filter(Quarter < today())

##===================================================================
## 18. Save panel data in R and long data in Excel for Tableau
##===================================================================
# Remvoe hidden object other than dataframe, this needs to be done for Tableau to load 
saveData_Panel<- as.data.frame(saveData_Panel) 
save(saveData_Panel, file = paste(saveFolder, "fulldata_panel.Rda", sep =""))
# Process results for Tableau
process_for_tableau()
### This file calcualtes weighted credit gdp gap 
### by weighting country-level credit to gdp data using BIS cross-border-banking statistics

rm(list = ls())

setwd("Q:/DATA/SPRAIMU/4_SysRisk/R Code")
folder <- "Q:/DATA/SPRAIMU/4_SysRisk/Data/"
saveFolder <- "Q:/DATA/SPRAIMU/4_SysRisk/Data/Output/"

library(zoo)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(openxlsx)

myfunc <-c('load_and_clean.R','single_table_func.R','reshaping.R','two_table_func.R','process_for_tableau.R')
lapply(myfunc, source, verbose = FALSE)

exposure_file <- "Q:/DATA/SPRAIMU/4_SysRisk/Data/Input_Databases/Cross-border GDP gap/full_bis_cbs_csv/WEBSTATS_CBS_DATAFLOW_csv_col.csv"
mapping_file <- 'Q:/DATA/SPRAIMU/4_SysRisk/Data/Input_Databases/Cross-border GDP gap/country_code_bis.xlsx'

# Load Credit-to-GDP gap
credit_to_gdp_file <- 'Input_CreditGDP.xlsx'
range <- 'A3:IV200'
sheet <- 'Credit_Gap_HP'

df_Credit_GDP_Gap <- clean_excel(sheet = sheet, folder = folder, file = credit_to_gdp_file, range = range, freq = "Q")

# Load Country-to-Country cross-border bank holdings
df_Exposure <- 
    read.csv(exposure_file)

df_Exposure = df_Exposure%>%
    filter(CBS.reporting.basis =='Ultimate risk basis')%>%
    filter(Counterparty.sector == 'All sectors')%>%
    filter(Type.of.instruments =='All instruments')%>%
    filter(Balance.sheet.position =='Total claims')%>%
    filter(CBS.bank.type == 'Domestic banks')


df_Exposure<- df_Exposure%>%
    filter(!Counterparty.country %in% c('International organisations','Residents',
                                        'Offshore centres','British Overseas Territories',
                                        'West Indies UK','Residual developing Europe',
                                        'Residual former Serbia and Montenegro','Residual former Netherlands Antilles',
                                        'Residual developing Latin America and Caribbean','Residual offshore centres',
                                        'Residual developing Asia and Pacific','Residual developed countries',
                                        'Residual developing Africa and Middle East','Developing Europe',
                                        'All countries excluding residents','Developing countries',
                                        'Developing Latin America and Caribbean','Developing Africa and Middle East',
                                        'Developing Asia and Pacific','Euro area',
                                        'European developed countries','Unallocated location',
                                        'Developed countries'))

df_Exposure<- df_Exposure%>%select(L_REP_CTY, Reporting.country, L_CP_COUNTRY, Counterparty.country,
                     starts_with('X'))

#View(df_Exposure%>%filter(Reporting.country =='United States'))

df_Exposure_long <- df_Exposure%>%
    gather(key = 'Quarter', value = 'Amount', colnames(select(df_Exposure, starts_with('X'))))

df_Exposure_long$Quarter <- as.Date(as.yearqtr(paste(substr(df_Exposure_long$Quarter,2,5), substr(df_Exposure_long$Quarter,8,8), sep ='-')), frac = 1)


df_Exposure_long <- df_Exposure_long%>%mutate(Total.amount = Amount)

df_Exposure_long ['Total.amount'][which(df_Exposure_long$Counterparty.country != 'All countries'),1] <- NA

df_Exposure_long <- df_Exposure_long%>%group_by(Reporting.country, Quarter)%>%
    fill(Total.amount, .direction = 'up')%>%
    fill(Total.amount, .direction = 'down')

df_Exposure_long <- df_Exposure_long%>%mutate(Share = Amount/ Total.amount)

df_Exposure_long<- df_Exposure_long%>%ungroup()


## Merge with Country_Code file, first for country_party_country and, then for reporting_country

df_country_code <- read.xlsx(mapping_file)

df_country_code <- df_country_code%>%
    select(c('ISO-2.code','Code'))

df_Exposure_long <- left_join(df_Exposure_long, df_country_code, by = c('L_CP_COUNTRY' = 'ISO-2.code'))

colnames(df_Exposure_long)[which(colnames(df_Exposure_long) == 'Code')] <- 'L_CP_COUNTRY_Code'

df_Exposure_long <- left_join(df_Exposure_long, df_country_code, by = c('L_REP_CTY' = 'ISO-2.code'))

colnames(df_Exposure_long)[which(colnames(df_Exposure_long) == 'Code')] <- 'L_REP_COUNTRY_Code'

## Merge with Credit_to_GDP file

df_result <- left_join(df_Exposure_long, df_Credit_GDP_Gap, by = c('L_CP_COUNTRY_Code' = 'Code', 'Quarter' = 'Quarter'))


df_result <- df_result%>%mutate(Credit_Gap_HP_Contrib = Credit_Gap_HP * Share)

df_result <- df_result%>%group_by(Reporting.country,L_REP_COUNTRY_Code, Quarter)%>%
    summarise(Weighted_Gap = sum(Credit_Gap_HP_Contrib, na.rm = TRUE))

df_result['Weighted_Gap'][which(df_result['Weighted_Gap']==0, 1)] <- NA

## Some Analysis ##

df_analysis <- left_join(df_Credit_GDP_Gap, df_result, by = c('Code'='L_REP_COUNTRY_Code','Quarter'='Quarter'))

View(df_analysis)

df_analysis <- df_analysis%>%drop_na()

plot(df_analysis[which(df_analysis$Country =='Belgium'),c('Credit_Gap_HP','Weighted_Gap')])

df_analysis <- df_analysis%>%filter(Quarter >= as.Date('2014-12-31'))

plot(df_analysis[which(df_analysis$Country =='Singapore'),c('Credit_Gap_HP','Weighted_Gap')])

linearMod <- lm(Weighted_Gap ~Credit_Gap_HP, data = df_analysis)

summary(linearMod)

plot(df_analysis[c('Credit_Gap_HP','Weighted_Gap')])
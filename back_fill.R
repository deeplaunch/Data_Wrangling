####################### @Anthor: Harry Peng Zhao #################

back_fill<- function (tableA, tableQ ) {
  ####Takes quarterly and annual tables, back-fill quarterly table with annual growth for missing###
  ####return the back-filled quarterly table ####
  
  source('back_fill_cols.R')
  
  tableA <- tableA%>%arrange(Country, Year)
  tableQ <- tableQ%>%arrange(Country, Quarter)
  
  nameA<- colnames(tableA%>%select(-c('Country','Code','Year')))
  nameQ<- colnames(tableQ%>%select(-c('Country','Code','Quarter')))
  nameA_lag<- paste(nameA, "_1", sep ='')
  nameA_growth<- paste(nameA, "_g", sep ='')
  
  ######Calculate yoy growth from pervious year on annual table######
  
  nameList<- c(nameA, nameQ, nameA_lag, nameA_growth)
  
  symbol<- lapply(nameList, sym)
  names(symbol)<- nameList
  
  tableA <- tableA%>%group_by(Country)%>%
    mutate((!!symbol[[nameA_lag]]) := lag(!!symbol[[nameA]]))%>%
    ungroup()%>%
    mutate((!!symbol[[nameA_growth]]) := (!!symbol[[nameA]]/ (!!symbol[[nameA_lag]])-1))
  
  ###### Merge quarterly and annual tables based on year #########
  
  tableQ<- tableQ%>%mutate(Year= year(Quarter))
  
  table3<- tbl_df(full_join(tableA, tableQ, by = c('Code', 'Year')))%>%
    mutate(Q=quarter(Quarter))%>%
    arrange(Code, Q, Year)
  
 
  ###### Fill quarterly result based on annual growth (backward and forward for missing quarterly data)
  
  table3<- table3%>%select(Code, Year, Quarter, Q, !!symbol[[nameQ]], !!symbol[[nameA_growth]])%>% 
    group_by(Code, Q)%>%
    mutate( (!!symbol[[nameQ]]) := back_fill_cols(!!symbol[[nameQ]], !!symbol[[nameA_growth]]))%>%
    ungroup()## replace quarterly with filled column
  
  return(table3)
  
}
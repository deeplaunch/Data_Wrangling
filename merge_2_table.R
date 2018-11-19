####################### @Anthor: Harry Peng Zhao #################

merge_2_table<- function (tableA, tableQ ) {
  ####Takes quarterly and annual tables, merging quarterly table with annual table where missing###
  ####return the merged quarterly table ####
  
  tableA <- tableA%>%arrange(Country, Year)
  tableQ <- tableQ%>%arrange(Country, Quarter)
  
  nameA<- colnames(tableA%>%select(-c('Country','Code','Year')))
  nameQ<- colnames(tableQ%>%select(-c('Country','Code','Quarter')))
  
  ###### Merge quarterly and annual tables based on year #########
  
  tableQ<- tableQ%>%mutate(Year= year(Quarter))
  
  table_merged<- tbl_df(full_join(tableA, tableQ, by = c('Country','Code', 'Year')))%>%
    arrange(Code, Year)
  
  ###### Fill quarterly missing field with annual data
  
  table_merged<- table_merged%>%
      mutate( !!sym(nameQ) := coalesce(!!sym(nameQ), !!sym(nameA)))%>%
      select(Country, Code, Quarter, nameQ)
  
  return(table_merged)
  
}
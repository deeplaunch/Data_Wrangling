####################### @Anthor: Harry Peng Zhao #################

calc_growth<- function (table ) {
  ####Takes quarterly tables, calculate q-o-q growth for each country###
  
  table <- table%>%arrange(Country, Quarter)
  
  tname<- colnames(table%>%select(-c('Country','Code','Quarter')))
  tname_lag<- paste(tname, "_1", sep ='')
  tname_growth<- paste(tname, "_g", sep ='')
  
  ######Calculate yoy growth from pervious year on annual table######
  
  nameList<- c(tname, tname_lag, tname_growth)
  
  symbol<- lapply(nameList, sym)
  names(symbol)<- nameList
  
  table <- table%>%group_by(Country)%>%
    mutate((!!symbol[[tname_lag]]) := lag(!!symbol[[tname]]))%>%
    ungroup()%>%
    mutate((!!symbol[[tname_growth]]) := (!!symbol[[tname]]/ (!!symbol[[tname_lag]])-1))%>%
    select(-c(tname, tname_lag))
  
  return(table)
  
}
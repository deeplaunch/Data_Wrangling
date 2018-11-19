####################### @Anthor: Harry Peng Zhao #################

calc_growth<- function (table, lags= 1, freq = "Q" ) {
  ####Takes quarterly tables, calculate q-o-q growth (annualized) for each country###
  if (freq =="Q"){
      table <- table%>%arrange(Code, Quarter)
      
      tname<- colnames(table%>%select(-c('Country','Code','Quarter')))
      tname_lag<- paste(tname, "_1", sep ='')
      tname_growth<- paste(tname, "_g", sep ='')
      
      ######Calculate yoy growth from pervious year on annual table######
      
      nameList<- c(tname, tname_lag, tname_growth)
      
      symbol<- lapply(nameList, sym)
      names(symbol)<- nameList
      
      table <- table%>%group_by(Country)%>%
          mutate((!!symbol[[tname_lag]]) := lag(!!symbol[[tname]], lags))%>%
          ungroup()%>%
          mutate((!!symbol[[tname_growth]]) := ((!!symbol[[tname]]/ (!!symbol[[tname_lag]]))^(4 / lags) *100-100))%>%
          select(-c(tname, tname_lag))
  } else if (freq =="D"){
      table <- table%>%arrange(Code, Day)
      tname<- colnames(table%>%select(-c('Country','Code','Day')))
      tname_lag<- paste(tname, "_1", sep ='')
      tname_growth<- paste(tname, "_g", sep ='')
      
      ######Calculate daily growth rate in percentage (without annualization)######
      nameList<- c(tname, tname_lag, tname_growth)
      symbol<- lapply(nameList, sym)
      names(symbol)<- nameList
      
      table <- table%>%group_by(Code)%>%
          mutate((!!symbol[[tname_lag]]) := lag(!!symbol[[tname]], lags))%>%
          ungroup()%>%
          mutate((!!symbol[[tname_growth]]) := ((!!symbol[[tname]]/ (!!symbol[[tname_lag]]))*100-100))%>%
          select(-c(tname, tname_lag))
      
  }
  
  
  return(table)
  
}
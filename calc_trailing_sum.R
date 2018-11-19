####################### @Anthor: Harry Peng Zhao #################

calc_trailing_sum<- function (table, lags= 4 ) {
  ####Takes quarterly tables, calculate rolling sum country###
      table <- table%>%arrange(Code, Quarter)
      
      tname<- colnames(table%>%select(-c('Country','Code','Quarter')))
      
      tname_trailing <- paste(tname, "_trailing_", lags, "_sum", sep = '')
      
      ######Calculate trailing sum over time ######
      
      table <- table%>%group_by(Code)%>%
          mutate( (!!sym(tname_trailing) ):= rollapply(!!sym(tname), width = lags, fill = NA, FUN = sum, align = 'r'))%>%
          select(-tname)%>%
          ungroup()
 
  
  return(table)
  
}
####Takes quarterly tables, replace value as NaN###

replace_value <- function (table, value) {
    
    table <- table%>%arrange(Code, Quarter)
    
    vname<- colnames(table%>%select(-c('Country','Code','Quarter')))
    
    ###### Replace 100 with NaN ######
    
    table[vname][which(table[vname]== value),1]<- NA
    
    return(table)
    
}
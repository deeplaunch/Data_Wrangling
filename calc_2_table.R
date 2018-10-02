####################### @Anthor: Harry Peng Zhao #################

calc_2_table<- function (table1, table2, operator, removeNa = TRUE) {
  ####Takes two tables, do calculations (+, /, *), return the resulted table ####

  table3<- tbl_df(full_join(table1, table2, by = c('Country', 'Code', 'Quarter')))
  
  name1<- colnames(table1%>%select(-c('Country','Code','Quarter')))
  name2<- colnames(table2%>%select(-c('Country','Code','Quarter')))
  name3<- paste(name1, operator, name2, sep ="")
  
  
  if(operator == '+'){
    x <- rowSums(cbind(table3[name1], table3[name2]), na.rm = removeNa)
    table4<- table3%>%mutate(!!sym(name3):= x)         ##Use symbol variables & !! for dynamic naming
    table4[name3][which(table4[name3]== 0),1]<- NA
  }else if (operator =='/'){
    table4<- table3%>%
      mutate(!!sym(name3) := (!!sym(name1)) / (!!sym(name2)) * 100) 
    table4[name3][which(table4[name3]== -100),1]<- NA
  }else if (operator =='*'){
    table4<- table3%>%
      mutate(!!sym(name3) := (!!sym(name1)) * (!!sym(name2)))
  }else if (operator =='-'){
      table4<- table3%>%
          mutate(!!sym(name3) := (!!sym(name1)) - (!!sym(name2))) 
  }
  
  table4<- table4%>%select(-c(name1,name2))
  
  return(table4)
  
}
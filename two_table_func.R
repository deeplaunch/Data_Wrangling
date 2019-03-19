####Takes two tables, do calculations (+, /, *), return the resulted table ####

calc_2_table<- function (table1, table2, operator, removeNA = TRUE) {
 
    table3<- tbl_df(full_join(table1, table2, by = c('Country', 'Code', 'Quarter')))
    
    name1<- colnames(table1%>%select(-c('Country','Code','Quarter')))
    name2<- colnames(table2%>%select(-c('Country','Code','Quarter')))
    name3<- paste(name1, operator, name2, sep ="")
    
    
    if(operator == '+'){
        x <- rowSums(cbind(table3[name1], table3[name2]), na.rm = removeNA)
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


####Takes quarterly and annual tables, merging quarterly table with annual table where missing###
####return the merged quarterly table ####

merge_2_table<- function (tableA, tableQ ) {

    
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

####Takes quarterly and annual tables, back-fill quarterly table with annual growth for missing###

####return the back-filled quarterly table ####

back_fill_cols<- function(fill_col, growth_col) {
    #### Back fill a list based on growth list (first backwaerd, then forward) ###
    #### return the back-filled list #### 
    
    ##Fill backward (up)
    for (i in (length(fill_col)-1):1) {
        if (is.na(fill_col[i])){
            fill_col[i] <- fill_col[i+1]/ (1 + growth_col[i+1])
        }
    }
    
    ##Fill forward (down)
    for (i in (2: length(fill_col))) {
        if (is.na(fill_col[i])){
            fill_col[i] <- fill_col[i-1] * (1 + growth_col[i])
        }
    } 
    
    return (fill_col)
}

back_fill<- function (tableA, tableQ ) {
    
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
    
    table3<- tbl_df(full_join(tableQ, tableA, by = c('Code', 'Year')))%>%
        mutate(Q=quarter(Quarter))%>%
        arrange(Code, Quarter)
    
    
    ###### Fill quarterly result based on annual growth (backward and forward for missing quarterly data)
    
    table3<- table3%>%
        select(Country.x, Code, Year, Quarter, Q, !!symbol[[nameQ]], !!symbol[[nameA_growth]])%>%
        arrange(Code, Quarter)%>%
        group_by(Code)%>%
        mutate( (!!symbol[[nameQ]]) := back_fill_cols(!!symbol[[nameQ]], !!symbol[[nameA_growth]]))%>%
        ungroup()## replace quarterly with filled column
    
    table3<- table3%>%select(Country.x, Code, Quarter, nameQ)%>%
        rename(Country = Country.x)
    
    return(table3)
    
}

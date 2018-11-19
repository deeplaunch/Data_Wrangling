####################### @Anthor: Harry Peng Zhao #################

annual_to_quarter<- function (tableA ) {
    ####Takes  annual tables, converting to quarterly
    
    tableA <- tableA%>%arrange(Country, Year)
    
    nameA<- colnames(tableA%>%select(-c('Country','Code','Year')))
    
    tableQ <- list()
    tableQ <- tableA %>%
        mutate(
            Q.1 = !!sym(nameA),
            Q.2 = !!sym(nameA),
            Q.3 = !!sym(nameA),
            Q.4 = !!sym(nameA)
        ) %>%
        select(-nameA) %>%
        gather(Quarter, !!sym(nameA),-c(Country, Code, Year)) %>%
        mutate(Year_Quarter = as.numeric(gsub('Q.', '', Quarter)) + Year * 100) %>%
        select(-Quarter) %>%
        mutate(Quarter = yq((as.character(Year_Quarter))) + months(3) - days(1)) %>%
        select(Country, Code, Quarter, !!sym(nameA))
    
    return(tableQ)
    
}
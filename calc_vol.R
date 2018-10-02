####################### @Anthor: Harry Peng Zhao #################

calc_vol <- function (table , freq = "M", lag, in_percent = TRUE) {
    ####Takes tables, calculate trailing volalitities for each country###
    require(zoo)
    
    if (freq == 'M') {
        table <- table %>% arrange(Country, Month)
        tname <- colnames(table %>% select(-c('Country', 'Code', 'Month')))
    }
    
    tname_vol <- paste(tname, lag, freq, "_vol", sep = '')
    
    ######Calculate volatility for each country######
    
    nameList <- c(tname, tname_vol)
    
    symbol <- lapply(nameList, sym)
    names(symbol) <- nameList
    
    # compute volatity as standard deviation
    # table <- table%>%group_by(Country)%>%
    #   mutate((!!symbol[[tname_vol]]) := rollapply(!!symbol[[tname]], width = lag, FUN = sd, fill = 0, align ='r'))%>%
    #   ungroup()%>%
    #   select(-c(tname))
    
    # compute volatilty as rolling average of squared return/change
    table <- table %>% group_by(Country) %>%
        mutate((!!symbol[[tname]]) := (!!symbol[[tname]]) ^2 )%>%
        mutate((!!symbol[[tname_vol]]) := rollapply(
            !!symbol[[tname]],
            width = lag,
            FUN = mean,
            fill = 0,
            align = 'r'
        )) %>%
        ungroup() %>%
        select(-c(tname))
    
    # convert to annulized standard deviation in %
    if(in_percent){
        table[tname_vol] <- sqrt(table[tname_vol]) * sqrt(12) *100
    }
    
    table[tname_vol] <- table[tname_vol]%>% replace(.== 0, NA)
    
    return(table)
    
}
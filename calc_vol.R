####################### @Anthor: Harry Peng Zhao #################

calc_vol <- function (table , freq = "M", lags, in_percent = TRUE, ceiling = 10) {
    ####Takes tables, calculate trailing volalitities for each country###
    require(zoo)
    
    if (freq == 'M') {
        table <- table %>% arrange(Country, Month)
        tname <- colnames(table %>% select(-c('Country', 'Code', 'Month')))
    
        tname_vol <- paste(tname, lags, freq, "_vol", sep = '')
        
        ######Calculate volatility for each country######
        
        nameList <- c(tname, tname_vol)
        
        symbol <- lapply(nameList, sym)
        names(symbol) <- nameList
        
        # compute volatity as standard deviation
        # table <- table%>%group_by(Country)%>%
        #   mutate((!!symbol[[tname_vol]]) := rollapply(!!symbol[[tname]], width = lags, FUN = sd, fill = 0, align ='r'))%>%
        #   ungroup()%>%
        #   select(-c(tname))
        
        table[tname] <- table[tname]%>% replace(.> ceiling, NA)  # remove extremetly large returns due to bad starting data
        
        # compute volatilty as rolling average of squared return/change
        table <- table %>% group_by(Country) %>%
            mutate((!!symbol[[tname]]) := (!!symbol[[tname]]) ^2 )%>%
            mutate((!!symbol[[tname_vol]]) := rollapply(
                !!symbol[[tname]],
                width = lags,
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
    } else if (freq =="D"){
        table <- table %>% 
            arrange(Code, Day)%>%
            filter( (!is.na(Day)) & Day>ymd(19940101))#%>%
            ##filter(Level_g!=0)
        table <- table%>%group_by(Code)%>%
            mutate(Vol = rollapply(Level_g, width = lags, FUN = sd, fill = 0, align = 'r'))
        table <- table%>%
            select(-Level_g)%>%
            filter(month(Day) %in% c(3,6,9,12) & year(Day)>=1995)%>%
            mutate(YearMonth= month(Day) + year(Day) * 100 )%>%
            group_by(Code, YearMonth)%>%
            summarise(Country = last(Country), Day = last(Day), Vol= last(Vol))%>%
            select(Country, Code, Day, Vol)
        table$Vol = table$Vol *sqrt(250)
    }
    
    return(table)
    
}
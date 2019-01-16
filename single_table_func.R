
####################### @Anthor: Harry Peng Zhao #################

####Takes  annual tables, converting to quarterly

annual_to_quarter<- function (tableA ) {
    
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

####Takes quarterly tables, calculate rolling sum country###

calc_trailing_sum<- function (table, lags= 4 ) {
    
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

calc_percentile_panel <-
    function (dataTable, by_column = FALSE) {
        #### Takes a long tidyr dataframe and return calculated percentile for a given column name=calcName#########
        #### by_column = FALSE: calculate percentile on entire panel, by_column = True: calcualte percentile on each country
        
        library(tidyr)
        library(dplyr)
        library(stringr)
        library(lubridate)
        library(reshape2)
        
        calcName = colnames(dataTable)[!colnames(dataTable) %in% c("Country","Code","Quarter")]
        
        #Transfer 0 value to NA
        dataTable[calcName][dataTable[calcName] == 0] <- NA
        
        #use built-in ntile function to bin vector to 100 buckets #rank <- ntile(dataTable[calcName][[1]], 100)
        
        if (by_column == FALSE) {
            dataTable <- dataTable %>%
                mutate(Percentile = ntile(!!sym(calcName), 100)) %>%
                select(-!!calcName)
        } else if (by_column == TRUE) {
            dataTable <- dataTable %>%
                group_by(Code) %>%
                mutate(Percentile = ntile(!!sym(calcName), 100)) %>%
                select(-!!calcName) %>%
                ungroup()
        }
        
        #filldown missing variables for each country group
        dataTable <- dataTable %>% arrange(Code, Quarter) %>%
            group_by(Code) %>%
            fill(Percentile, .direction = 'down') %>%
            ungroup()
        
        #spread to wide format
        dataTable$Quarter <-
            paste(year(dataTable$Quarter),
                  "Q",
                  quarter(dataTable$Quarter),
                  sep = '')
        dataTable <- dataTable %>% spread(Quarter, Percentile)
        
        return (dataTable)
        
    }

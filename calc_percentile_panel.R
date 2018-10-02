####################### @Anthor: Harry Peng Zhao #################

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

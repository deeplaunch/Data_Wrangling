####################### @Anthor: Harry Peng Zhao #################

to_wide <-
    function (dataTable) {
        #### Takes a long tidyr dataframe and return wide format#########
        require(tidyr)
        require(dplyr)
        require(stringr)
        require(lubridate)
        require(reshape2)
        
        calcName <- colnames(dataTable)[4]
        
        #Transfer 0 value to NA
        dataTable[calcName][dataTable[calcName] == 0] <- NA
        
        # #filldown missing variables for each country group
        # dataTable <- dataTable %>% arrange(Code, Quarter) %>%
        #     group_by(Code) %>%
        #     fill(Percentile, .direction = 'down') %>%
        #     ungroup()
        
        #spread to wide format
        dataTable$Quarter <-
            paste(year(dataTable$Quarter),
                  "Q",
                  quarter(dataTable$Quarter),
                  sep = '')
        dataTable <- dataTable %>% spread(Quarter, calcName)
        
        return (dataTable)
        
    }

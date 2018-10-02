####################### @Anthor: Harry Peng Zhao #################

to_long <-
    function (dataTable) {
        #### Takes a wide tidyr dataframe and return long format#########
        require(tidyr)
        require(dplyr)
        require(stringr)
        require(lubridate)
        require(reshape2)
        
        dataTable.long <- dataTable %>%
            melt(
                id.var = c("Country", "Code"),
                variable.name = "Quarter",
                value.name = "data"
            ) #converting form wide to long format
        dataTable.long$Quarter <-
            yq((as.character(dataTable.long$Quarter))) + months(3) - days(1) 
        
        return (dataTable.long)
        
    }

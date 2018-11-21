####Takes an excel sheet, cleans data and converts to tidyr format (long) for later vectorized processing#########
####################### @Anthor: Harry Peng Zhao #########################################

clean_excel <-
    function (folder = "Q:/DATA/SPRAIMU/4_SysRisk/Data/",
              file = "Input_BankLoans.xlsx" ,
              sheet = "NC_LOANS_HH_Q",
              range = "A5:IV196",
              freq = "Q",
              cutoff = 19950101,
              raw_shape = "W",
              include_zero = FALSE) {
        
        library(tidyr)
        library(dplyr)
        library(stringr)
        library(lubridate)
        library(openxlsx)
        library(reshape2)
        
        ##Load from Excel sheet
        if (include_zero == FALSE){
            dataTable <-
                readxl::read_xlsx(
                    path = paste(folder, file, sep = ''),
                    sheet,
                    range,
                    na = c('n.a.', '', '.', '#TSREF!','0','#N/A N/A')
                    
                )
        } else{
            dataTable <-
                readxl::read_xlsx(
                    path = paste(folder, file, sep = ''),
                    sheet,
                    range,
                    na = c('n.a.', '', '.', '#TSREF!','#N/A N/A')
                    
                )
        }
      
        dataTable <- dataTable %>% filter(!is.na(Code))
        
        if (freq == 'A') {
            ##Cleaning
            temp <- dataTable %>%
                select(-starts_with('X')) %>%
                select_if(is.numeric) %>%
                filter(!is.na(Code))
            dataTable <-
                dataTable['Country'] %>% bind_cols(temp) %>% filter(!is.na(Country))
            dataTable.long <-
                dataTable %>% melt(
                    id.var = c("Country", "Code"),
                    variable.name = "Year",
                    value.name = sheet,
                    variable.factor = FALSE
                )
            
            # In case of exception where Year is in the format of excel date
            if (max(levels(dataTable.long$Year))>2050) {
                dataTable.long$Year<- year(as.Date(as.numeric(as.character(dataTable.long$Year)), origin = "1899-12-30"))
            }
            
            dataTable.long$Year <-
                as.numeric(as.character(dataTable.long$Year))
            
            dataTable.long <-
                dataTable.long %>% filter(Year >= 1995 &
                                              Year <= year(today()))
            
        } else if (freq == "Q") {
            ##Cleaning
            dataTable <- dataTable %>%
                select(-starts_with('X')) %>%
                filter(!is.na(Code))
            if ('.excel_last' %in% colnames(dataTable)) {
                dataTable <- dataTable %>% select(-'.excel_last')
            }
            
            if (dim(dataTable %>% select(contains('Q', ignore.case = FALSE)))[2] !=
                0) {
                #Quarter in "1990Q1" format
                dataTable.long <- dataTable %>%
                    select(Country, Code, contains('Q', ignore.case = FALSE)) %>%
                    filter(!is.na(Country)) %>%
                    melt(
                        id.var = c("Country", "Code"),
                        variable.name = "Quarter",
                        value.name = sheet
                    ) #converting form wide to long format
                dataTable.long$Quarter <-
                    yq((as.character(dataTable.long$Quarter))) + months(3) - days(1) #calculate end of quarter
                
            } else{
                #Quarter in "19900331" format
                dataTable.long <- dataTable %>%
                    filter(!is.na(Country)) %>%
                    melt(
                        id.var = c("Country", "Code"),
                        variable.name = "Quarter",
                        value.name = sheet
                    )
                dataTable.long$Quarter <-
                    as.Date(as.numeric(as.character(dataTable.long$Quarter)), origin = '1899-12-30')
            }
            
            dataTable.long <-
                dataTable.long %>% filter(Quarter >= ymd(cutoff) &
                                              Quarter <= today()) # Select based on this Cut-Off
        } else if (freq == "M") {
            ##cleaning
            temp <- dataTable %>%
                select(-starts_with('X')) %>%
                select_if(is.numeric) %>%
                filter(!is.na(Code))
            dataTable <-
                dataTable['Country'] %>% bind_cols(temp) %>% filter(!is.na(Country))
            dataTable.long <-
                dataTable %>% melt(
                    id.var = c("Country", "Code"),
                    variable.name = "Month",
                    value.name = sheet
                )
            ##Convert month to ymd format
            dataTable.long$Month <-
                ymd(paste(as.character(dataTable.long$Month), "D1", sep = ""))
            dataTable.long <-
                dataTable.long %>% filter(Month >= ymd(cutoff) &
                                              Month <= today())
        }
        
        # use standardized country names
        country_code <- read.xlsx('Q:/DATA/SPRAIMU/4_SysRisk/Data/Output/country_code.xlsx')

        dataTable.long<- dataTable.long%>%
            select(-Country)%>%
            left_join(country_code, by='Code')%>%
            select(c(4,1,2,3))
        
        return (dataTable.long)
        
    }

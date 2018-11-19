clean_excel_transpose <-
    function (folder = "Q:/DATA/SPRAIMU/4_SysRisk/Data/Input_Databases/Bloomberg/",
              file = "SysRisk_Market_Bloomberg Data.xlsx" ,
              sheet = "Spot_Rate",
              range = "A4:FU30000",
              freq = "D",
              cutoff = 19940101) {
        ##Load raw data from Excel sheet
        dataTable <-
            readxl::read_xlsx(
                path = paste(folder, file, sep = ''),
                sheet,
                range,
                na = c('n.a.', '', '.', '#TSREF!','0','#N/A N/A')
            )

        ##Take Transpose
        size.length <- dim(dataTable)[1]
        title <- dataTable[1:2,]
        dataTable <- as_tibble(dataTable[4:size.length,])
        
        dataTable<- dataTable%>%select(-starts_with('X'))
        
        size.width <- dim(dataTable)[2]
 
        dataTable<- dataTable%>%gather("Day", "Level", 2:size.width)
        names(dataTable) <- c("Day", "Code", "Level")
        dataTable <- as_tibble(sapply(dataTable, type.convert))
        dataTable$Day <- as.Date(dataTable$Day, origin = "1899-12-30")
        dataTable<- dataTable%>%arrange(Code,Day)


        #Add Country name
        
        title[3, ]<- names(title)
        title<- t(title)
    
        title<- as_tibble(x = title[2:size.width, ])
        names(title)<- c("Country" ,"Ticker","Code")
        
        title$Code <- as.double(title$Code)
        
        dataTable<- full_join(dataTable, title, by= 'Code')
        
        dataTable<- dataTable%>%select(Country,Code, Day, Level)
        
        country_code <- read.xlsx('Q:/DATA/SPRAIMU/4_SysRisk/Data/Output/country_code.xlsx')
        
        dataTable<- dataTable%>%
            select(-Country)%>%
            left_join(country_code, by='Code')%>%
            select(c(4,1,2,3))


        return (dataTable)
    }
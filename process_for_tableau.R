process_for_tableau <- function(){
    
    
    library(tidyr)
    library(dplyr)
    library(stringr)
    library(lubridate)
    library(openxlsx)
    
    panel_file <- "Q:/DATA/SPRAIMU/4_SysRisk/Data/Output/fulldata_panel.Rda"
    
    mapping_file <- 'Q:/DATA/SPRAIMU/4_SysRisk/Data/Tableau/Mapping File.xlsx'
    country_group_sheet <- 'Country Group'
    country_group_range <-'A1:E199'
    
    variable_group_sheet <- 'Variable Group'
    variable_group_range <-'E1:F64'
    
    saveFolder <- "Q:/DATA/SPRAIMU/4_SysRisk/Data/Output/"
    
    # load datafile
    load(panel_file)
    
    # load country group mapping file
    
    df_country_group <- readxl::read_xlsx(
        path = mapping_file,
        sheet = country_group_sheet,
        range = country_group_range,
        na = c('n.a.', '', '.', '#TSREF!','0','#N/A N/A')
        
    )
    
    df_country_group <- df_country_group%>%
        select(ccode, Income)%>%
        rename(Code =ccode, Group=Income)
    
    
    # Save long-format to excel for Tableau processing later
    saveData_Long <- gather(saveData_Panel, "VariableName", "VariableValue", 4:dim(saveData_Panel)[2])
    saveData_Long <- saveData_Long%>%drop_na()
    
    # merge data file with country group mapping file
    
    saveData_Long<- left_join(saveData_Long, df_country_group, by =c('Code'))
    
    # Calculate Precentiles within country and within country group (call it "All_Group")
    
    saveData_Long<- saveData_Long%>%
        group_by(VariableName,Group)%>%
        mutate(Percentile_All_Country = ntile(VariableValue,100))%>%
        ungroup()
    
    saveData_Long<- saveData_Long%>%
        group_by(VariableName,Code)%>%
        mutate(Percentile_In_Country = ntile(VariableValue,100))%>%
        ungroup()
    
    # Calculate High Risk Flag (most recent risk >80 or change in risk >20)
    
    # Merge with variable grou mapping
    
    df_variable_group <- readxl::read_xlsx(
        path = mapping_file,
        sheet = variable_group_sheet,
        range = variable_group_range,
        na = c('n.a.', '', '.', '#TSREF!','0','#N/A N/A')
        
    )
    
    colnames(df_variable_group) <- c('VariableName','RiskDirection')
    
    saveData_Long<- left_join(saveData_Long, df_variable_group, by =c('VariableName'))
    
    # Most Recent Percentile (last and last-4)
    saveData_Long<- saveData_Long%>%
        arrange(Country, VariableName,Quarter)%>%
        group_by(Country,VariableName)%>%
        mutate(Ptile_1 = nth(Percentile_In_Country, -1))%>%
        mutate(Ptile_5 = nth(Percentile_In_Country, -5))%>%
        ungroup()%>%
        mutate(PtileChange = Ptile_1 -Ptile_5 )
    
    # Flip Percentile to 101- Percentile if Risk Direction is '-'  
    saveData_Long[which(saveData_Long$RiskDirection =='-'),'Ptile_1'] <- 101 - saveData_Long[which(saveData_Long$RiskDirection =='-'),'Ptile_1']
    # Flip Change to -  if Risk Direction is '-' 
    saveData_Long[which(saveData_Long$RiskDirection =='-'),'PtileChange'] <- (- saveData_Long[which(saveData_Long$RiskDirection =='-'),'PtileChange'])
    
    saveData_Long<- saveData_Long%>%
        mutate(RiskFlag = Ptile_1 >=80 | PtileChange >=20 )
    # Svae Results
    saveData_Long<- saveData_Long%>%select(-c('RiskDirection','Ptile_1','Ptile_5','PtileChange'))
    
    write.xlsx(saveData_Long, paste(saveFolder,'fulldata_long.xlsx',sep=""))
    
}
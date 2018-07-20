####################### @Anthor: Harry Peng Zhao #################

calc_percentile_panel_old<- function (dataTable, calcName) {
#### Takes a long tidyr dataframe and return calculated percentile for a given column name=calcName#########
  
  library(tidyr)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(reshape2)

  ##create one vector that contains all values (sorted, none-zero)
  fullList<- unlist(dataTable[calcName], use.names = FALSE)
  fullList<-sort(fullList[(!is.na(fullList)) & (fullList!=0)])

  #vectorization, each element is one value
  source("rank_in_percent.R")
  pctile<- sapply(X= dataTable[[calcName]], FUN= rank_in_percent, l= fullList)
  
  dataTable<- dataTable%>%mutate("Percentile"= pctile)%>%select(-!!calcName)
  
  #spread to wide format
  
  dataTable$Quarter <- paste(year(dataTable$Quarter),"Q",quarter(dataTable$Quarter),sep ='')
  dataTable <- dataTable%>%spread(Quarter, Percentile)
  
  return (dataTable)

}

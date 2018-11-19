
#Convert data to metatable format

saveDataWide <- lapply(saveData, to_wide)

saveFile <- paste(saveFolder, "Result_Raw.xlsx", sep ="")
wb <- loadWorkbook(saveFile)
sheetName <- getSheetNames(saveFile)

for (n in sheetName[c(2:length(sheetName))]) {
    deleteData(wb, sheet = n, cols = 1:500, rows = 1:500)
    writeData(wb, x = saveDataWide[[n]], sheet = n)
}
# check consistency and save raw data to existing template file
if (! FALSE %in% (names(wb)[2:length(names(wb))] == names(saveDataWide))) {
    saveWorkbook(wb, saveFile, overwrite = TRUE)
} else {
    print('Inconsistency in raw save file!')
}

##===================================================================
## Save Percentile data (within country and by time)
##===================================================================
saveData_P <- lapply(saveData, calc_percentile_panel, by_column = TRUE)

saveFile_P <- paste(saveFolder, "Result_Percentile.xlsx", sep ="")
wb <- loadWorkbook(saveFile_P)
sheetName <- getSheetNames(saveFile_P)

for (n in sheetName[c(2:length(sheetName))]) {
    deleteData(wb, sheet = n, cols = 1:500, rows = 1:500)
    writeData(wb, x = saveData_P[[n]], sheet = n)
}
# check consistency and save raw data to existing template file
if (! FALSE %in% (names(wb)[2:length(names(wb))] == names(saveData_P))) {
    saveWorkbook(wb, saveFile_P, overwrite = TRUE)
}

##===================================================================
## Save Percentile data (cross country and time)
##===================================================================
saveData_P <- lapply(saveData, calc_percentile_panel, by_column = FALSE)

saveFile_P <- paste(saveFolder, "Result_Percentile_All.xlsx", sep ="")
wb <- loadWorkbook(saveFile_P)
sheetName <- getSheetNames(saveFile_P)

for (n in sheetName[c(2:length(sheetName))]) {
    deleteData(wb, sheet = n, cols = 1:500, rows = 1:500)
    writeData(wb, x = saveData_P[[n]], sheet = n)
}
# check consistency and save raw data to existing template file
if (! FALSE %in% (names(wb)[2:length(names(wb))] == names(saveData_P))) {
    saveWorkbook(wb, saveFile_P, overwrite = TRUE)
}


#write.xlsx(country_code, paste(saveFolder,'country_code.xlsx',sep=""))

##==========================================================================
## Copy Raw data with Date
##==========================================================================
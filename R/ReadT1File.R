#' ReadT1File
#' 
#' Read a Thomson ONE excel output file (.xlsx format) into memory
#' @param file.name Name of .xlsx file
#' @param sheet.types A character vector of sheet types to look for.
#' @return A data.frame containing the contents of the .xlsx file.

ReadT1File <- function(file.name,sheet.types=c("Balance Sheet","Cash Flow Statement","Income Statement")) {
  require(XLConnect)
  xlsx.file <- loadWorkbook(file.name)
  setMissingValue(xlsx.file, value = c("--","-"))
  contains.sheet <- existsSheet(xlsx.file,sheet.types)
  
  df <- data.frame()
  if(sum(contains.sheet,na.rm=TRUE)==1){
    sheet.type <- names(contains.sheet)[contains.sheet]
    df <- readWorksheet(xlsx.file, sheet=sheet.type, header=FALSE)
    attributes(df) <- MakeAttributes(sheet.type,df)
    return(df)      
  } else {
    stop("Improperly formatted workbook. Workbook must contain a single Balance Sheet, Cash Flow Statement or Income Statement")
  }
}
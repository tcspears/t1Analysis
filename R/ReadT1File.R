#' ReadT1File
#' 
#' Read a Thomson ONE excel output file (.xlsx format) into memory
#' @param file.name Name of .xlsx file
#' @param sheet.types A character vector of sheet types to look for.
#' @return A data.frame containing the contents of the .xlsx file.

ReadT1File <- function(file.name,sheet.types=c("Balance Sheet","Cash Flow Statement","Income Statement")) {
  require(XLConnect)
  
  # Load workbook 
  xlsx.file <- loadWorkbook(file.name)
  
  # Specifies that '--' and '-' should be treated as NA values.
  setMissingValue(xlsx.file, value = c("--","-"))
  
  # Check to see if workbook contains sheets of the type listed in sheet.types
  contains.sheet <- existsSheet(xlsx.file,sheet.types)
  
  df <- data.frame()
  if(sum(contains.sheet,na.rm=TRUE)==1){
    
    # Set the sheet type (used to set attributes)
    sheet.type <- names(contains.sheet)[contains.sheet]
    
    # Read the sheet into a data.frame
    df <- readWorksheet(xlsx.file, sheet=sheet.type, header=FALSE)
    
    # Set sheet attributes
    attributes(df) <- MakeAttributes(sheet.type,df)
    
    # Return data.frame containing the sheet.
    return(df)      
  } else {
    # Throw an error if the workbook contains more/less than one matching sheet.
    stop("Improperly formatted workbook. Workbook must contain a single Balance Sheet, Cash Flow Statement or Income Statement")
  }
}
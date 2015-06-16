#' GetStructuralParameters
#'  
#' @param sheet.type Type of sheet (i.e. Balance Sheet)
#' @return A list of structural parameters depending on the sheet type.

GetStructuralParameters <- function(sheet.type){
  # At the moment, the only allowed file types are Balance Sheets, Cash Flow Statements, and Income Statements.
  if(sheet.type %in% c("Balance Sheet","Cash Flow Statement","Income Statement")){
    return(list(data.begins.column=3,firm.name.location=c(2,1),first.row=4,period.end.date.row=5))
  } else {
    stop("Unknown file type.")
  }
}
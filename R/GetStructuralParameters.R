#' GetStructuralParameters
#'  
#' @param sheet.type Type of sheet (i.e. Balance Sheet)
#' @return A list of structural parameters depending on the sheet type.

GetStructuralParameters <- function(sheet.type){
  if(sheet.type %in% c("Balance Sheet","Cash Flow Statement","Income Statement")){
    return(list(first.row=4,years.begin.column=3,firm.name.location=c(2,1)))
  } else {
    stop("Unknown file type.")
  }
}
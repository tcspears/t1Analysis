#' GetSheet
#' 
#' Grabs a particular T1 sheet from a list of sheets that were previously read into memory. 
#' @param firm.name Name of the firm
#' @param sheet.type Type of sheet (e.g. Balance Sheet)
#' @param sheets List of sheets

GetSheet <- function(firm.name,sheet.type,sheets){
  # First condition: sheet type specified in sheet.type
  con.a <- sapply(sheets, function(x) attributes(x)$type==sheet.type)
  
  # Second condition: firm name specified in firm.name
  con.b <- sapply(sheets, function(x) attributes(x)$firm==firm.name)
  
  # Create a subsetted list of sheets matching the first and second conditions
  out <- sheets[con.a & con.b]
  
  # If the subsetted list contains anything other than a single element, then there is a problem and an error should be thrown.
  if(length(out)==0){
    stop("No matching sheet exists.")
  } else if(length(out) > 1){
    stop("More than one sheet matches the specified conditions.")
  } else {
    out <- out[[1]]
    return(out)
  }
}
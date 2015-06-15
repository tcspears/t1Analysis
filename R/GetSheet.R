#' GetSheet
#' 
#' Grabs a particular T1 sheet from a list of sheets that were previously read into memory
#' @param firm.name Name of the firm
#' @param sheet.type Type of sheet (e.g. Balance Sheet)
#' @param sheets List of sheets

GetSheet <- function(firm.name,sheet.type,sheets){
  con.a <- sapply(sheets, function(x) attributes(x)$type==sheet.type)
  con.b <- sapply(sheets, function(x) attributes(x)$firm==firm.name)
  out <- sheets[con.a & con.b]
  if(length(out)==0){
    stop("No matching sheet exists.")
  } else if(length(out) > 1){
    stop("More than one sheet matches the specified conditions.")
  } else {
    out <- out[[1]]
    return(out)
  }
}
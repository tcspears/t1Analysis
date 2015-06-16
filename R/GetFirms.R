#' GetFirms
#' 
#' Returns a character vector of firms whose financial statements are contained in a list of sheets.
#' @param sheets A list of T1 excel sheets
#' @return A character vector of firms whose financial statements are contained in sheets.
GetFirms <- function(sheets){
  return(unique(sapply(sheets,FUN=function(x) attributes(x)$firm)))
}
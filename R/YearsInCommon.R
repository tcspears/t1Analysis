#' YearsInCommon
#' 
#' Finds the years in common between a vector of fundamentals. If years is specified, then the function will only check those years.
#' @param firm.name Name of firm
#' @param fundamentals.names A character vector of fundamentals
#' @param sheets A list of T1 sheets
#' @param years (optional) A character vector of years.
#' @return A character vector of matching years.

YearsInCommon <- function(firm.name,fundamentals.names,sheets,years=NULL){
  info <- lapply(fundamentals.names,FundamentalsInfo)
  sheet <- lapply(info,FUN= function(x) GetSheet(firm.name,x[3],sheets))
  years.in.common <- Reduce(f=intersect,lapply(sheet,function(x) attributes(x)$years))
  
  if(is.null(years)){
    if(length(years.in.common)==0){
      stop("No years in common.")
    }
    return(years.in.common)
  } else {
    if(length(intersect(years.in.common,years))==0){
      stop("No years in common.")
    }
    return(intersect(years.in.common,years))
  }
}
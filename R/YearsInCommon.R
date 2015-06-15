#' YearsInCommon
#' 
#' Finds the years in common between a vector of fundamentals. If years is specified, then the function will only check those years.
#' @param firm.name Name of firm
#' @param fundamentals.names A character vector of fundamentals
#' @param sheets A list of T1 sheets
#' @param years (optional) A character vector of years.
#' @return A character vector of matching years.

YearsInCommon <- function(firm.name,fundamentals.names,sheets,years=NULL){
  # Create a list containing fundamentals information for each element of fundamentals.names
  info <- lapply(fundamentals.names,FundamentalsInfo)
  
  # Create a list of sheets that are required to calculate each fundamental listed in fundamentals.names
  sheet <- lapply(info,FUN= function(x) GetSheet(firm.name,x[3],sheets))
  
  # Determine the years in common between the fundamentals listed in fundamentals.names
  years.in.common <- Reduce(f=intersect,lapply(sheet,function(x) attributes(x)$years))
  
  # If years is left un-specified, then check years in common for all available years. Otherwise, check only for
  # the years specified in the years variable.
  
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
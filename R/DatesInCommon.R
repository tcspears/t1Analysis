#' DatesInCommon
#' 
#' Finds the reporting periods in common between a vector of fundamentals. If years is specified, then the function will only check those years.
#' @param firm.name Name of firm
#' @param fundamentals.names A character vector of fundamentals
#' @param sheets A list of T1 sheets
#' @param dates (optional) A character vector of reporting dates.
#' @return A character vector of matching years.

DatesInCommon <- function(firm.name,fundamentals.names,sheets,dates=NULL){
  # Create a list containing fundamentals information for each element of fundamentals.names
  info <- lapply(fundamentals.names,FUN=function(x) FundamentalsInfo(fundamental.name=x))
  
  # Create a list of sheets that are required to calculate each fundamental listed in fundamentals.names
  sheet <- lapply(info,FUN= function(x) GetSheet(firm.name,x[3],sheets))
  
  # Determine the years in common between the fundamentals listed in fundamentals.names
  dates.in.common <- Reduce(f=intersect,lapply(sheet,function(x) attributes(x)$reporting.dates))
  
  # If years is left un-specified, then check years in common for all available years. Otherwise, check only for
  # the years specified in the years variable.
  
  if(is.null(dates)){
    if(length(dates.in.common)==0){
      stop("No reporting dates in common.")
    }
    return(as.Date(dates.in.common))
  } else {
    if(length(intersect(dates.in.common,dates))==0){
      stop("No years in common.")
    }
    return(as.Date(intersect(dates.in.common,dates)))
  }
}
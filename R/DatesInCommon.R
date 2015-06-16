#' DatesInCommon
#' 
#' Finds the reporting periods in common between a vector of fundamentals. If years is specified, then the function will only check those years.
#' @param firm.name Name of firm
#' @param fundamentals.names A character vector of fundamentals
#' @param sheets A list of T1 sheets
#' @param dates (optional) A character vector of reporting dates. These can either be years (e.g. '2014') or specific dates (e.g. '2014-05-12).
#' @return A character vector of matching dates. 

DatesInCommon <- function(firm.name,fundamentals.codes,sheets,dates=NULL){
  require(lubridate)
  
  # Create a list containing fundamentals information for each element of fundamentals.names
  info <- lapply(fundamentals.codes,FUN=function(x) FundamentalsInfo(firm.name,fundamental.code=x,sheets))
  
  # Create a list of sheets that are required to calculate each fundamental listed in fundamentals.names
  sheet <- lapply(info,FUN=function(x) GetSheet(firm.name,x[1],sheets))
  
  # Determine the reporting periods in common between the fundamentals listed in fundamentals.names
  dates.in.common <- Reduce(f=intersect,lapply(sheet,function(x) attributes(x)$reporting.dates))
  
  # If years is left un-specified, then check years in common for all available years. Otherwise, check only for
  # the years specified in the years variable.
  
  if(is.null(dates)){
    if(length(dates.in.common)==0){
      stop("No reporting dates in common.")
    }
    return(as.Date(dates.in.common))
  # If the user specified dates contain hyphens (and refer to particular dates)
  } else if(all(grepl("-",dates))) {
    intersection <- as.Date(intersect(as.Date(dates.in.common),as.Date(dates)))
    if(length(intersection)==0){
      stop("No reporting dates in common.")
    } else {
      # Grab the subset of dates.in.common that match the dates listed in dates.  
      out <- as.Date(dates.in.common[as.character(as.Date(dates.in.common)) %in% dates])
      return(out)
    }
  # If the user specified dates are neither null, nor do they contain any hyphens, then they will be evaluated as years.
  } else {
    intersection <- intersect(year(as.Date(dates.in.common)),dates)
    if(length(intersection)==0){
      stop("No reporting dates in common")
    } else {
      # Grab the subset of dates.in.common that share the same year as the years listed in dates.
      out <- as.Date(dates.in.common)[year(as.Date(dates.in.common)) %in% dates]
      return(out)
    }
  }
}

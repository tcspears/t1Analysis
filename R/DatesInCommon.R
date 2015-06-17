#' DatesInCommon
#' 
#' Finds the reporting periods in common between a list of sheets. If years is specified, then the function will only check those years.
#' @param sheet A list of T1 sheets
#' @param dates (optional) A character vector of reporting dates. These can either be years (e.g. '2014') or specific dates (e.g. '2014-05-12).
#' @return A character vector of matching dates. 

DatesInCommon <- function(sheet,dates=NULL){
  require(lubridate)
  
  # Determine the reporting periods in common between the fundamentals listed in fundamentals.names
  dates.in.common <- as.Date(Reduce(f=intersect,lapply(sheet,function(x) attributes(x)$reporting.dates)),origin="1970-01-01")
  
  # If years is left un-specified, then check years in common for all available years. Otherwise, check only for
  # the years specified in the years variable.
  
  if(is.null(dates)){
    if(length(dates.in.common)==0){
      stop("No reporting dates in common.")
    }
    return(dates.in.common)
  # If the user specified dates contain hyphens (and refer to particular dates)
  } else if(all(grepl("-",dates))) {
    intersection <- as.Date(intersect(dates.in.common,as.Date(dates)),origin="1970-01-01")
    if(length(intersection)==0){
      stop("No reporting dates in common.")
    } else {
      # Grab the subset of dates.in.common that match the dates listed in dates.  
      out <- as.Date(dates.in.common[as.character(as.Date(dates.in.common)) %in% dates],origin="1970-01-01")
      return(out)
    }
  # If the user specified dates are neither null, nor do they contain any hyphens, then they will be evaluated as years.
  } else {
    intersection <- intersect(year(as.Date(dates.in.common)),dates)
    if(length(intersection)==0){
      stop("No reporting dates in common")
    } else {
      # Grab the subset of dates.in.common that share the same year as the years listed in dates.
      out <- as.Date(dates.in.common,origin="1970-01-01")[year(as.Date(dates.in.common,origin="1970-01-01")) %in% dates]
      return(out)
    }
  }
}

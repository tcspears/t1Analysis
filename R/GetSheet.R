#' GetSheet
#' 
#' Grabs a particular T1 sheet from a list of sheets that were previously read into memory. 
#' @param fundamental.info A character vector of fundamental info, output from FundamentalsInfo.
#' @param sheets List of sheets

GetSheet <- function(fundamental.info,sheets){
  firm.name <- attributes(fundamental.info)$firm
  # First, check to see if sheet.type is 'Stock Timeseries'
  if(fundamental.info[1] == "Stock Timeseries"){
    
    # Grab sheets that match firm.name. We need this to grab the correct dates
    con.a <- sapply(sheets, function(x) attributes(x)$firm==firm.name)
    firm.sheets <- sheets[con.a]
    
    # Find the max/min dates in those sheets
    dates.in.sheets <- lapply(firm.sheets,FUN=function(x) attributes(x)$reporting.dates)
    date.min <- min(as.Date(unlist(dates.in.sheets),origin="1970-01-01"))
    date.max <- max(as.Date(unlist(dates.in.sheets),origin="1970-01-01"))
    
    # Grab the corresponding stock price history data for those dates
    out <- GetStockTimeseries(symbol=fundamental.info[3],from.date=date.min,to.date=date.max)
    return(out)
  } 
  
  # First condition: sheet type specified in sheet.type
  con.a <- sapply(sheets, function(x) attributes(x)$type==fundamental.info[1])
  
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
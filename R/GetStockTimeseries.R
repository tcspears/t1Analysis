#' GetStockTimeseries
#' 
#' @param symbol A ticker symbol
#' @param from.date Beginning date
#' @param to.date End date
#' @return A data.frame of closing prices

GetStockTimeseries <- function(symbol,from.date,to.date){
  
  # Grab prices, and transform resulting object into format compatible with other sheets.
  out <- quantmod::getSymbols(Symbols=symbol,from=from.date,to=to.date,src="google",auto.assign=FALSE)
  out <- as.data.frame(out)
  out <- out[order(row.names(out),decreasing=TRUE),]
  out <- t(out)
  
  # Set attributes of price sheet
  attributes(out) <- MakeAttributes("Stock Timeseries",out)
  
  # Return price sheet
  return(out)
}
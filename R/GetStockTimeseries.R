#' GetStockTimeseries
#' 
#' @param symbol A ticker symbol
#' @param from.date Beginning date
#' @param to.date End date
#' @return A data.frame of closing prices

GetStockTimeseries <- function(symbol,from.date,to.date){
  
  # Grab prices, and transform resulting object into format compatible with other sheets.
  time.series <- quantmod::getSymbols(Symbols=symbol,from=from.date,to=to.date,src="yahoo",auto.assign=FALSE)
  time.series.df <- as.data.frame(time.series)
  time.series.sorted <- time.series.df[order(row.names(time.series.df),decreasing=TRUE),]
  out <- t(time.series.sorted)
  
  # Set attributes of price sheet
  attributes(out)$type <- "Stock Timeseries"
  attributes(out)$reporting.dates <- as.Date(colnames(out))
  attributes(out)$reporting.dates.columns <- seq(GetStructuralParameters(attributes(out)$type)$data.begins.column,dim(out)[2])
  attributes(out)$years <- lubridate::year(attributes(out)$reporting.dates)
  
  # Return price sheet
  return(out)
}
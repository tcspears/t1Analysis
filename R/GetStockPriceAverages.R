#' GetStockPriceAverages
#' 
#' Retrieves the average stock price for a given ticker symbol across a series of dates.
#' @param symbol A ticker symbol (e.g. 'AAPL')
#' @param start.date Start date for analysis.
#' @param forward.dates A character vector of relative forward dates (e.g. 5 days from start.date, 10 days from start.date)
#' @return A list of average stock prices

GetStockPriceAverages <- function(symbol,start.date,forward.dates=c(5,10)){
  
  # If the format of start.date is not Date, transform it.
  if(class(start.date)!="Date"){
    start.date <- as.Date(start.date)
  }
  
  prices <- lapply(forward.dates,FUN=function(x) quantmod::getSymbols(Symbols=symbol,
                                                  from=start.date,
                                                  to=start.date+x,
                                                  src="google",
                                                  auto.assign=FALSE))
  
  averages <- lapply(prices,FUN=function(x) mean(x[,4]))
  names(averages) <- lapply(forward.dates,FUN=function(x) as.character(start.date+x))
  return(averages)
}
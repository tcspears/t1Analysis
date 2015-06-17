#' RatioInfo
#' 
#' Returns basic information about how to calculate fundamentals ratios. This function is called by GetRatios.
#' @param ratio.name (optional) A ratio name (e.g. Earnings per Share) If none is specified, entire list is returned
#' @return A list containing basic information about each ratio.

RatioInfo <- function(ratio.name=NULL){
  
  # Generate a list of ratio information. Each element of the list corresponds to a ratio. Each element of r
  # contains a sub-list with two elements: (1) a character vector of fundamentals used to calculate the ratio in question;
  # (2) a formula that specifies how to calculate that ratio from those fundamentals.
  
  r <- list()
  r[[1]] <- list(fundamentals=c("Net Income","Total Revenue"),formula=function(x) x[,1]/x[,2])
  names(r)[[1]] <- "Net Margin"
  r[[2]] <- list(fundamentals=c("Net Income","Total Assets"),formula=function(x) x[,1]/x[,2])
  names(r)[[2]] <- "Return on Assets"
  r[[3]] <- list(fundamentals=c("Total Revenue","Total Assets"),formula=function(x) x[,1]/x[,2])
  names(r)[[3]] <- "Sales to Total Assets"
  r[[4]] <- list(fundamentals=c("Current Assets","Current Liabilities"),formula=function(x) x[,1]/x[,2])
  names(r)[[4]] <- "Current Ratio"
  r[[5]] <- list(fundamentals=c("Long Term Debt","Total Assets"),formula=function(x) x[,1]/x[,2])
  names(r)[[5]] <- "Long Term Debt to Total Assets"
  r[[6]] <- list(fundamentals=c("Total Debt","Total Equity"),formula=function(x) x[,1]/x[,2])
  names(r)[[6]] <- "Leverage Ratio"
  r[[7]] <- list(fundamentals=c("Cash Flow From Operations","Current Liabilities"),formula=function(x) x[,1]/x[,2])
  names(r)[[7]] <- "Operating Cash Flow Ratio"
  r[[8]] <- list(fundamentals=c("Long Term Debt","Total Equity"),formula=function(x) x[,1]/x[,2])
  names(r)[[8]] <- "Long Term Debt To Total Equity Ratio"
  r[[9]] <- list(fundamentals=c("Net Income","Preferred Dividends","Average Outstanding Shares"),formula=function(x) (x[,1]-x[,2])/x[,3])
  names(r)[[9]] <- "Earnings Per Share"
  r[[10]] <- list(fundamentals=c("Operating Income","Total Assets","Current Liabilities"),formula=function(x) x[,1]/(x[,2]-x[,3]))
  names(r)[[10]] <- "Return on Capital Employed"
  r[[11]] <- list(fundamentals=c("Net Income","Total Equity"),formula=function(x) x[,1]/x[,2])
  names(r)[[11]] <- "Return on Equity"
  r[[12]] <- list(fundamentals=c("Stock Price","Total Equity","Shares Outstanding"),formula=function(x) x[,1]/(x[,2]/x[,3]))
  names(r)[[12]] <- "Book to Market Value"
  r[[13]] <- list(fundamentals=c("Stock Price","Earnings Per Share"),formula=function(x) x[,1]/x[,2])
  names(r)[[13]] <- "Price Earnings Ratio"
  r[[14]] <- list(fundamentals=c("Stock Price","Shares Outstanding","Total Revenue"),formula=function(x) x[,1]*x[,2]/x[,3])
  names(r)[[14]] <- "Price to Sales Ratio"
  
  # If ratio.name is left unspecified, then return the entire list. Otherwise, return a subsetted list containing only the
  # ratio specified by ratio.name
  
  if(is.null(ratio.name)){
    return(r)
  } else {
    return(r[names(r)==ratio.name])
  }
}

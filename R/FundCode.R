#' FundCode
#' 
#' Maps names of common fundamentals to their four digit T1 codes
#' @param fundamental.names A character vector of names of fundamentals (e.g. 'Total Revenue')
#' @return A character vector of codes.

FundCode <- function(fundamental.names){
  matrix <- structure(c("Current Assets", "Current Liabilities", "Net Income", 
                        "Total Revenue", "Total Assets", "Long Term Debt", "Total Debt", 
                        "Total Equity", "Cash Flow From Operations", "Preferred Dividends", 
                        "Average Outstanding Shares", "Earnings Per Share", "Operating Income", 
                        "Stock Price", "Shares Outstanding", "ATCA", "LTCL", "NINC", 
                        "RTLR", "ATOT", "LTTD", "STLD", "QTLE", "OTLO", "CPRD", "SBAS", 
                        "SBBF", "SOPI", "Common Stock", "QTCO"), .Dim = c(15L, 2L), .Dimnames = list(
                          NULL, c("Fundamental", "Code")))
    
  GetCode <- function(fundamental.name){
    if(fundamental.name %in% matrix[,1]){
      out <- matrix[matrix[,1]==fundamental.name,2]
      return(out)
    } else {
      out <- fundamental.name
      return(out)
    }
  }
  return(sapply(fundamental.names,FUN=function(x) GetCode(x)))
}
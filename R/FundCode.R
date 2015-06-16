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
              "ATCA", "LTCL", "NINC", "RTLR", "ATOT", "LTTD", "STLD", "QTLE", 
              "OTLO", "CPRD", "SBAS", "SBBF", "SOPI"), .Dim = c(13L, 2L), .Dimnames = list(
                NULL, c("Fundamental", "Code")))
  
  return(sapply(fundamental.names,FUN=function(x) matrix[matrix[,1]==x,2]))
}
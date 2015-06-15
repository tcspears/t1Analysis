#' FundamentalsInfo
#' 
#' Returns basic information about the location of various fundamentals within the T1 sheets
#' @param fundamental.name (optional) A name of a fundamental (e.g. Net Income). If none is specified, entire fundamentals matrix is returned.
#' @return A matrix of information about one or more fundamentals.

FundamentalsInfo <- function(fundamental.name=NULL){
  # Matrix of information about each fundamental
  matrix <- structure(c("Current Assets", "Current Liabilities", "Net Income", 
                        "Total Revenue", "Total Assets", "Long Term Debt", "Total Debt", 
                        "Total Equity", "Cash Flow From Operations", "Preferred Dividends", 
                        "Average Outstanding Shares", "Earnings Per Share", "Operating Income", 
                        "ATCA", "LTCL", "NINC", "RTLR", "ATOT", "LTTD", "STLD", "QTLE", 
                        "OTLO", "CPRD", "SBAS", "SBBF", "SOPI", "Balance Sheet", "Balance Sheet", 
                        "Income Statement", "Income Statement", "Balance Sheet", "Balance Sheet", 
                        "Balance Sheet", "Balance Sheet", "Cash Flow Statement", "Income Statement", 
                        "Income Statement", "Income Statement", "Income Statement", "39", 
                        "82", "90", "26", "67", "85", "86", "117", "45", "91", "99", 
                        "100", "63"), .Dim = c(13L, 4L), .Dimnames = list(NULL, c("Fundamental", 
                                                                                  "Code", "Sheet", "Location")))                                                                                
  # If fundamental.name is left unspecified, return the entire matrix.
  if(is.null(fundamental.name)){
    out <- matrix
    return(out)
  # If fundamental.name consists of a four letter T1 code, match this to the 2nd column of the matrix and return the 
  # resulting sub-matrix.  
  } else if(nchar(fundamental.name)==4) {
    out <- matrix[matrix[,2]==fundamental.name,]
    return(out)
  # Otherwise, match fundamental.name to the 1st column of the matrix and return the resulting sub-matrix.
  } else {
    out <- matrix[matrix[,1]==fundamental.name,]
    return(out)
  }
}
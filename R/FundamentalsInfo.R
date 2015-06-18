#' FundamentalsInfo
#' 
#' Determines the location of data on a fundamental for a particular firm, as well as its full name.
#' @param firm.name Name of firm
#' @param fundamental.code Four digit code for fundamental (e.g. 'NINC' for 'Net Income')
#' @param A list of T1 excel sheets
#' @return A character vector consisting of information on the specified fundamental (e.g. location within sheets)

FundamentalsInfo <- function(firm.name,fundamental.code,sheets){
  # Extract a sub-list of sheets matching to firm.name
  firm.sheets.con <- sapply(sheets,FUN=function(x) attributes(x)$firm == firm.name)
  firm.sheets <- sheets[firm.sheets.con]
  
  # Name the sheets based on their type
  names(firm.sheets) <- sapply(firm.sheets,FUN=function(x) attributes(x)$type)
  
  # Determine which sheet contains the desired fundamental.
  matching.location <- lapply(firm.sheets,FUN=function(x) which(x[,1]==fundamental.code))
  
  # Throw an error if there are no matching sheets/locations
  if(length(unlist(matching.location)) == 0){
    stop("No such fundamental exists within the sheets")
  }
  
  # If the fundamental.code is of type Common Stock, then run this code instead.
  # (We have to grab the stock prices sheet from Google (via the quantmod package) rather than from the list of T1 sheets.)
  if(fundamental.code == "Common Stock"){
    
    # Find the ticker symbol from the T1 excel sheets. Generally 'Common Stock' matches to both the ticker symbol 
    # (in multiple locations)and NA values. This drops the NA values and multiple values.
    matches <- mapply(firm.sheets,matching.location,FUN=function(x,y) x[y,2])
    matches <- unique(matches[!is.na(matches)])
    
    # If there is a unique ticker symbol associated with Common Stock, then grab the data.row from GetStructuralParameters,
    # combine it together with the sheet type and ticker symbol and return to the user.
    if(length(unlist(matches)) == 1){
      out <- c("Stock Timeseries",GetStructuralParameters("Stock Timeseries")$data.row,matches)
      attributes(out)$firm <- firm.name
      return(out)
    } else {
    # Throw an error if there are multiple matching ticker symbols/entries.
      stop("Error: Multiple matching entries with different values")
    }
  }
  
  # Throw an error if the funudamental.code is *not* Common Stock but there are multiple matches
  if(length(unlist(matching.location)) > 1 & fundamental.code != "Common Stock"){
    stop("Error: Multiple matching entries with different values")
  }
  
  # Determine the location of the fundamental within the sheet and its full name.
  which.sheet <- which(matching.location!=0)
  which.row <- matching.location[[which(matching.location!=0)]]
  full.name <- firm.sheets[[which.sheet]][which.row,2]
  
  # Return the sheet, location, and full name.
  out <- c(names(which.sheet),which.row,full.name)
  attributes(out)$firm <- firm.name
  return(out)
}
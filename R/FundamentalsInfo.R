#' FundamentalsInfo
#' 
#' Determines the sheet and row containing a fundamental for a particular firm, as well as its full name.
#' @param firm.name Name of firm
#' @param fundamental.code Four digit code for fundamental (e.g. 'NINC' for 'Net Income')
#' @param A list of T1 excel sheets

FundamentalsInfo <- function(firm.name,fundamental.code,sheets){
  # Extract a sub-list of sheets matching to firm.name
  firm.sheets.con <- sapply(sheets,FUN=function(x) attributes(x)$firm == firm.name)
  firm.sheets <- sheets[firm.sheets.con]
  
  # Name the sheets based on their type
  names(firm.sheets) <- sapply(firm.sheets,FUN=function(x) attributes(x)$type)
  
  # Determine which sheet contains the desired fundamental.
  matching.location <- sapply(firm.sheets,FUN=function(x) which(x[,1]==fundamental.code))
  
  # Throw an error if there are no matching sheets/locations
  if(length(unlist(matching.location))==0){
    stop("No such fundamental exists within the sheets")
  }
  
  # Determine the location of the fundamental within the sheet and its full name.
  which.sheet <- which(matching.location!=0)
  which.row <- matching.location[[which(matching.location!=0)]]
  full.name <- firm.sheets[[which.sheet]][which.row,2]
  
  # Return the sheet, location, and full name.
  return(c(names(which.sheet),which.row,full.name))
}
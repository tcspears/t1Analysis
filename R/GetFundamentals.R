#' GetFundamentals
#' 
#' Extracts a set of fundamentals for a given firm across a number of reporting periods.
#' @param firm.name Name of firm
#' @param fundamentals.names A character vector of fundamentals
#' @param sheets A list of T1 sheets
#' @param years (optional) A character vector of years
#' @return A matrix of fundamentals (one per column)
#' 
GetFundamentals <- function(firm.name,fundamentals.names,sheets,years=NULL){
  
  # Extract name/location information on chosen fundamentals, as well as the corresponding sheets.
  info <- lapply(fundamentals.names,FUN=FundamentalsInfo)
  sheet <- lapply(info,FUN= function(x) GetSheet(firm.name,x[3],sheets))
  
  # Determine years in common; reset years to this value
  years <- YearsInCommon(firm.name,fundamentals.names,sheets,years)
  
  # Determine the years.location for chosen years
  years.location <- lapply(sheet,function(x) attributes(x)$years.location[which(attributes(x)$years %in% years)])
  
  # Extract the specified fundamentals
  fundamentals <- mapply(sheet,info,years.location,FUN=function(x,y,z) StripFormatting(x[as.numeric(y[4]),z]))
  
  # Set attributes for fundamentals. 
  rownames(fundamentals) <- years
  colnames(fundamentals) <- sapply(info,function(x) x[1])
  attributes(fundamentals)$firm <- firm.name
  
  # Return fundamentals
  return(fundamentals)
}
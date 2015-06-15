#' GetFundamentals
#' 
#' Extracts a set of fundamentals for a given firm across a number of reporting periods.
#' @param firm.name Name of firm
#' @param sheets A list of T1 sheets
#' @param fundamentals.names (optional) A character vector of fundamentals
#' @param years (optional) A character vector of years
#' @return A matrix of fundamentals (one per column)
#' 
GetFundamentals <- function(firm.name,sheets,fundamentals.names=NULL,years=NULL){
  
  # If fundamentals.names is left unspecified, then return information on all fundamentals.
  if(is.null(fundamentals.names)){
    fundamentals.names <- FundamentalsInfo()[,1]
  }
  
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
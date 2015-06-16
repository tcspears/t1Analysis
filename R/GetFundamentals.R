#' GetFundamentals
#' 
#' Extracts a set of fundamentals for a given firm across a number of reporting periods.
#' @param firm.name Name of firm
#' @param sheets A list of T1 sheets
#' @param fundamentals.names (optional) A character vector of fundamentals
#' @param dates (optional) A character vector of reporting dates
#' @return A matrix of fundamentals (one per column)
#' 
GetFundamentals <- function(firm.name,sheets,fundamentals.names=NULL,dates=NULL){
  
  # If fundamentals.names is left unspecified, then return information on all fundamentals.
  if(is.null(fundamentals.names)){
    fundamentals.names <- FundamentalsInfo()[,1]
  }
  
  # Extract name/location information on chosen fundamentals, as well as the corresponding sheets.
  # We have to do this in a two step process, because the location portion of info depends on the type of sheet.
  info <- lapply(fundamentals.names,FUN=function(x) FundamentalsInfo(fundamental.name=x))
  sheet <- lapply(info,FUN= function(x) GetSheet(firm.name,x[3],sheets))
  info <- mapply(sheet,info,FUN=function(x,y) FundamentalsInfo(sheet=x,fundamental.name=y[1]),SIMPLIFY=FALSE)
  
  # Determine dates in common; reset dates to this value
  dates <- DatesInCommon(firm.name,fundamentals.names,sheets,dates)
  
  # Determine the dates.location for chosen years
  dates.location <- lapply(sheet,function(x) attributes(x)$reporting.dates.columns[which(attributes(x)$reporting.dates %in% dates)])
  
  # Extract the specified fundamentals
  fundamentals <- mapply(sheet,info,dates.location,FUN=function(x,y,z) StripFormatting(x[as.numeric(y[4]),z]))
  
  # Set attributes for fundamentals. 
  rownames(fundamentals) <- as.character(dates)
  colnames(fundamentals) <- sapply(info,function(x) x[1])
  attributes(fundamentals)$firm <- firm.name
  
  # Return fundamentals
  return(fundamentals)
}
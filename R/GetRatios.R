#' GetRatios
#' 
#' Calculates a set of fundamental ratios for a given firm
#' @param firm.name Name of firm
#' @param sheets A list of T1 sheets
#' @param ratio.names (optional) A character vector of ratio names. If none is specified, all available ratios are returned.
#' @param dates (optional) A character vector of reporting dates. These can either be years (e.g. '2014') or specific dates (e.g. '2014-05-12).
#' @return A matrix of ratios (one per column)

GetRatios <- function(firm.name,sheets,ratio.names=NULL,dates=NULL){
  # If ratio.names is left unspecified, then return information on all ratios.
  if(is.null(ratio.names)){
    ratio.names <- names(RatioInfo())
  }
  
  # Generate a list containing a matrix of fundamentals needed to calculate each ratio specified in ratio.names
  # (using information provided by RatioInfo())
  fundamentals <- lapply(ratio.names,FUN= function(x) GetFundamentals(firm.name,RatioInfo(x)[[1]][[1]],sheets,dates))
  
  # Generate a matrix of ratios using fundamentals by applying the ratio functions produced by RatioInfo()
  out <- mapply(ratio.names,fundamentals,FUN=function(x,y) RatioInfo(x)[[1]][[2]](y))
  
  # Set the firm.name attribute for the ratios.
  attributes(out)$firm <- firm.name
  
  # Return the ratios.
  return(out)
}
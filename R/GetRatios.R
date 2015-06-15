#' GetRatio
#' 
#' Calculates a set of fundamental ratios for a given firm
#' @param firm.name Name of firm
#' @param sheets A list of T1 sheets
#' @param ratio.names (optional) A character vector of ratio names. If none is specified, all available ratios are returned.
#' @param years (optional) A character vector of years.
#' @return A matrix of ratios (one per column)

GetRatios <- function(firm.name,sheets,ratio.names=NULL,years=NULL){
  if(is.null(ratio.names)){
    ratio.names <- names(RatioInfo())
  }
  fundamentals <- lapply(ratio.names,FUN= function(x) GetFundamentals(firm.name,RatioInfo(x)[[1]][[1]],sheets,years))
  out <- mapply(ratio.names,fundamentals,FUN=function(x,y) RatioInfo(x)[[1]][[2]](y))
  attributes(out)$firm <- firm.name
  return(out)
}
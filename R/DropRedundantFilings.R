#' DropRedundantFilings
#' 
#' Drops redundant filings from a data.frame containing a T1 sheet. Redundancy is defined as multiple filings on the same date.
#' @param df A data.frame containing a T1 sheet
#' @return A data.frame without redundant filings

DropRedundantFilings <- function(df){
  columns.to.drop <- which(duplicated(attributes(df)$years))
  years.begin.column <- GetStructuralParameters(attributes(df)$type)$years.begin.column
  
  # Drops columns with redundant years
  df.new <- df[,-(years.begin.column-1+columns.to.drop)]
  
  # Sets attributes of new data.frame.
  attributes(df.new) <- MakeAttributes(sheet.type=attributes(df)$type,df.new)
  
  # Return new data.frame.
  return(df.new)
}
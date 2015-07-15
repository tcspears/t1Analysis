#' DropRedundantFilings
#' 
#' Drops redundant filings from a data.frame containing a T1 sheet. Redundancy is defined as multiple filings on the same date.
#' @param df A data.frame containing a T1 sheet
#' @return A data.frame without redundant filings

DropRedundantFilings <- function(df){
  # Determine the location of columns that are duplicates
  columns.to.drop <- which(duplicated(attributes(df)$reporting.dates))
  
  # Extract the column at which reporting periods begin to appear given the type of sheet.
  data.begins.column <- GetStructuralParameters(attributes(df)$type)$data.begins.column
  
  # Drops columns with redundant reporting periods, if such redundancies exist.
  if(length(columns.to.drop)>0){
    out <- df[,-(data.begins.column-1+columns.to.drop)]
    
    # Sets attributes of new data.frame.
    # Set attributes
    attributes(out)$firm <- attributes(df)$firm
    attributes(out)$type <- attributes(df)$type
    attributes(out)$reporting.dates <- as.Date(colnames(out)[-(1:2)],format="%d/%m/%Y")
    attributes(out)$reporting.dates.columns <- seq(GetStructuralParameters(attributes(out)$type)$data.begins.column,dim(out)[2])
    attributes(out)$years <- lubridate::year(attributes(out)$reporting.dates)  
    
    # Return new data.frame.
    return(out)
  } else {
    # If there are no redundant columns, then just return the original dataset. 
    out <- df
    return(out)
  }
}
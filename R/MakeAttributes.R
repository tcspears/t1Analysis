#' MakeAttributes
#' 
#' Produces a set of attributes for a T1 sheet data.frame, depending on its sheet type
#' @param sheet.type Type of T1 sheet (e.g. Balance Sheet)
#' @param df Data.frame containing a T1 sheet
#' @return A list of attributes

MakeAttributes <- function(sheet.type,df){
  # Sheet type (e.g. Balance Sheet, Income Statement)
  attributes(df)$type <- sheet.type
  # Firm name, based on the location of the firm name in the given sheet type.
  attributes(df)$firm <- df[GetStructuralParameters(sheet.type)$firm.name.location[1],
                            GetStructuralParameters(sheet.type)$firm.name.location[2]]
  
  # Reporting periods, based on the sheet type and the number of columns in the data.frame
  attributes(df)$years <- as.numeric(df[GetStructuralParameters(sheet.type)$first.row,
                                        GetStructuralParameters(sheet.type)$years.begin.column:dim(df)[2]])
  
  # Column numbers corresponding to the reporting periods
  attributes(df)$years.location <- seq(GetStructuralParameters(sheet.type)$years.begin.column,dim(df)[2])
  
  # Period end dates (in Date format), based on the sheet.type. Read in only first 10 characters of period end dates
  # (i.e drop times)
  attributes(df)$period.end.dates <- as.Date(substr(df[GetStructuralParameters(sheet.type)$period.end.date.row,
                                                       GetStructuralParameters(sheet.type)$years.begin.column:dim(df)[2]],1,10))
  return(attributes(df))
}

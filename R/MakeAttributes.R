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
  
  # Period end dates (in Date format), based on the sheet.type. Read in only first 10 characters of period end dates
  # (i.e drop times)
  attributes(df)$reporting.dates <- as.Date(substr(df[GetStructuralParameters(sheet.type)$period.end.date.row,
                                                       GetStructuralParameters(sheet.type)$data.begins.column:dim(df)[2]],1,10))
  
  # Column numbers corresponding to the reporting periods
  attributes(df)$reporting.dates.columns <- seq(GetStructuralParameters(sheet.type)$data.begins.column,dim(df)[2])
  
  # Years corresponding to period reporting dates
  attributes(df)$years <- as.numeric(df[GetStructuralParameters(sheet.type)$first.row,
                                        GetStructuralParameters(sheet.type)$data.begins.column:dim(df)[2]])
  
  return(attributes(df))
}

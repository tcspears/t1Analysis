#' MakeAttributes
#' 
#' Produces a set of attributes for a T1 sheet data.frame, depending on its sheet type
#' @param sheet.type Type of T1 sheet (e.g. Balance Sheet)
#' @param df Data.frame containing a T1 sheet
#' @return A list of attributes

MakeAttributes <- function(sheet.type,df){
  attributes(df)$type <- sheet.type
  attributes(df)$firm <- df[GetStructuralParameters(sheet.type)$firm.name.location[1],
                            GetStructuralParameters(sheet.type)$firm.name.location[2]]
  attributes(df)$years <- as.numeric(df[GetStructuralParameters(sheet.type)$first.row,
                                        GetStructuralParameters(sheet.type)$years.begin.column:dim(df)[2]])
  attributes(df)$years.location <- seq(GetStructuralParameters(sheet.type)$years.begin.column,dim(df)[2])
  attributes(df)$period.end.dates <- as.Date(substr(df[GetStructuralParameters(sheet.type)$first.row+1,
                                                       GetStructuralParameters(sheet.type)$years.begin.column:dim(df)[2]],1,10))
  return(attributes(df))
}

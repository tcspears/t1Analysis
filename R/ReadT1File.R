#' ReadT1File
#' 
#' Read a Thomson ONE excel output file (.xlsx format) into memory
#' @param file.name Name of .xlsx file
#' @param sheet.types A character vector of sheet types to look for.
#' @return A data.frame containing the contents of the .xlsx file.

ReadT1Excel <- function(file.name){
  # Read raw HTML
  file <- readLines(file.name)
  
  # Parse raw HTML
  pagetree <- XML::htmlTreeParse(file, useInternalNodes = TRUE)
  
  # Read tables from pagetree
  initial.read <- XML::readHTMLTable(pagetree,stringsAsFactors = FALSE)
  
  # Determine which of the elements of initial.read contains the data.
  # Note: this will have to be modified once we start reading in combined files.
  data.location <- which.max(sapply(initial.read,length))
  
  # Only keep rows that start with 4 capital letters (i.e. a fundamentals code)
  
  to.keep <- grepl("[A-Z]{4}",initial.read[[data.location]][,1])
  out <- initial.read[[data.location]][to.keep,]
  
  # Set attributes
  attributes(out)$firm <- xpathSApply(pagetree, "//*/table[@id='companyPageHeading']/tr/td", xmlValue)[1]
  attributes(out)$sheet.type <- xpathSApply(pagetree, "//*/div[@class='mso_stitle']", xmlValue)[1]
  attributes(out)$years <- xpathSApply(pagetree, "//*/td[@class='mso_ghr-c']", xmlValue)[-1]
  attributes(out)$reporting.dates <- as.Date(as.character(initial.read[[data.location]][1,-c(1,length(initial.read[[data.location]][1,]))]),format="%d/%m/%Y")
  attributes(out)$reporting.dates.columns <- seq(GetStructuralParameters(attributes(out)$sheet.type)$data.begins.column,dim(out)[2])
  
  # Return the file 
  return(out)
}
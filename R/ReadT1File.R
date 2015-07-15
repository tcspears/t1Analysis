#' ReadT1File
#' 
#' Read a Thomson ONE excel/HTML output file into memory
#' @param file.name Name of T1 excel/HTML file
#' @return A data.frame containing the contents of the .xlsx file.

ReadT1File <- function(file.name){
  # Read raw HTML
  file <- readLines(file.name)
  
  # Parse raw HTML
  pagetree <- XML::htmlTreeParse(file, useInternalNodes = TRUE)
  
  # Read tables from pagetree
  initial.read <- XML::readHTMLTable(pagetree,stringsAsFactors = FALSE)
  
  # Only keep rows that start with 4 or 5 capital letters and numbers (i.e. a fundamentals code)
  data.rows <- lapply(initial.read,FUN=function(x)  grepl("[A-Z0-9]{4,5}|Common Stock",x[,1]))
  
  # Drop empty sheets and then combine them all together
  modified <- initial.read[lapply(initial.read,length)>0]
  combined <- plyr::rbind.fill(modified)
  
  # Set column names for combined sheet (to enable merging later) and re-organise the Update Date row
  # (shift date entries to the right by one column and insert an empty description column)
  colnames(combined) <- c("Fundamentals Code","Description",combined[1,-c(1,length(combined[1,]))])
  combined[combined[,1]=="Update Date:",] <- c("Update Date:","",combined[combined[,1]=="Update Date:",2:(length(combined[1,])-1)])
  
  # Drop rows that don't contain fundamentals data
  data.rows <- grepl("[A-Z0-9]{4,5}|Common Stock|Update Date:",combined[,1])
  out <- combined[data.rows,]
  rownames(out) <- NULL
  
  # Set attributes
  attributes(out)$firm <- XML::xpathSApply(pagetree, "//*/table[@id='companyPageHeading']/tr/td", xmlValue)[1]
  attributes(out)$type <- XML::xpathSApply(pagetree, "//*/div[@class='mso_stitle']", xmlValue)[1]
  attributes(out)$reporting.dates <- as.Date(colnames(out)[-(1:2)],format="%d/%m/%Y")
  attributes(out)$reporting.dates.columns <- seq(GetStructuralParameters(attributes(out)$type)$data.begins.column,dim(out)[2])
  attributes(out)$years <- lubridate::year(attributes(out)$reporting.dates)
  
  # Return the file 
  return(out)
}
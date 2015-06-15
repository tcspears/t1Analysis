#' StripFormatting
#' 
#' Performs the following operations on a character input: removes commas, coverts numbers in parentheses to negative numbers, and removes trailing whitespace.
#' @param x A character vector
#' @return A modified character vector

StripFormatting <- function(x){
  out <- x
  out <- gsub(",", "", out)
  out <- gsub("\\(", "-", out)
  out <- gsub("\\)", "", out)
  out <- gsub("^\\s+|\\s+$", "", out)
  return(as.numeric(out))
}
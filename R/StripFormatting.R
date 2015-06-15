#' StripFormatting
#' 
#' Performs the following operations on a character input: removes commas, coverts numbers in parentheses to negative numbers, and removes trailing whitespace.
#' @param x A character vector
#' @return A modified character vector

StripFormatting <- function(x){
  out <- x
  # Remove commas
  out <- gsub(",", "", out)
  # Convert open parentheses '(' to negative signs '-'
  out <- gsub("\\(", "-", out)
  # Drop closing parentheses ')'
  out <- gsub("\\)", "", out)
  # Remove leading and trailing whitespace
  out <- gsub("^\\s+|\\s+$", "", out)
  # Return x
  return(as.numeric(out))
}
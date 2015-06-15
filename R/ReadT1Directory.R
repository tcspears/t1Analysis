#' ReadT1Directory
#' 
#' Reads a directory of Thomson ONE excel output files (.xlsx format) into memory.
#' @param directory Path to directory
#' @return A list containing all of the data.frames that were read into memory.

ReadT1Directory <- function(directory){
  # Produce a list of sheets by applying ReadT1File to every file contained in the directory specified in directory.
  out <- lapply(dir(directory), FUN=function(x) ReadT1File(file.name=paste(directory,"/",x,sep="")))
  
  # Return the list
  return(out)  
}
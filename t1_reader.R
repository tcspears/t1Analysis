require(XLConnect)

setwd("~/Desktop/")
sheets <- ReadT1Directory("./T1files")
sheets <- lapply(sheets,FUN=DropRedundantFilings)

# This function reads in a T1 excel file and extracts a data.frame using the sheet names listed in sheet.types. If no corresponding sheet.type is found, the function throws an error.

ReadT1File <- function(file.name,sheet.types=c("Balance Sheet","Cash Flow Statement","Income Statement")) {
  xlsx.file <- loadWorkbook(file.name)
  setMissingValue(xlsx.file, value = c("--","-"))
  contains.sheet <- existsSheet(xlsx.file,sheet.types)
  
  df <- data.frame()
  if(sum(contains.sheet,na.rm=TRUE)==1){
    sheet.type <- names(contains.sheet)[contains.sheet]
    df <- readWorksheet(xlsx.file, sheet=sheet.type, header=FALSE)
    attributes(df) <- SetAttributes(sheet.type,df)
    return(df)      
  } else {
    stop("Improperly formatted workbook. Workbook must contain a single Balance Sheet, Cash Flow Statement or Income Statement")
  }
}

ReadT1Directory <- function(directory){
  out <- lapply(dir(directory), FUN=function(x) ReadT1File(file.name=paste(directory,"/",x,sep="")))
  return(out)  
}

GetSheet <- function(firm.name,sheet.type,sheets){
  con.a <- sapply(sheets, function(x) attributes(x)$type==sheet.type)
  con.b <- sapply(sheets, function(x) attributes(x)$firm==firm.name)
  out <- sheets[con.a & con.b]
  if(length(out)==0){
    stop("No matching sheet exists.")
  } else if(length(out) > 1){
    stop("More than one sheet matches the specified conditions.")
  } else {
    out <- out[[1]]
    return(out)
  }
}

SetAttributes <- function(sheet.type,df){
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

  
DropRedundantFilings <- function(df){
  columns.to.drop <- which(duplicated(attributes(df)$years))
  years.begin.column <- GetStructuralParameters(attributes(df)$type)$years.begin.column
  
  # Drops columns with redundant years
  df.new <- df[,-(years.begin.column-1+columns.to.drop)]
  
  # Sets attributes of new data.frame.
  attributes(df.new) <- SetAttributes(sheet.type=attributes(df)$type,df.new)
  
  # Return new data.frame.
  return(df.new)
}

GetStructuralParameters <- function(sheet.type){
  if(sheet.type %in% c("Balance Sheet","Cash Flow Statement","Income Statement")){
    return(list(first.row=4,years.begin.column=3,firm.name.location=c(2,1)))
  } else {
    stop("Unknown file type.")
  }
}

StripFormatting <- function(x){
  out <- x
  out <- gsub(",", "", out)
  out <- gsub("\\(", "-", out)
  out <- gsub("\\)", "", out)
  out <- gsub("^\\s+|\\s+$", "", out)
  return(as.numeric(out))
}

# This function checks to see if the inputs are valid.
# Use this when calculating ratios across multiple sheets.

CheckInputs <- function(input1,input2=NULL,input.types){
  if(is.null(input2)){
    con.a <- attributes(input1)$type == input.types[1]
    if(!con.a){
      stop("Invalid sheet type.")
    }
  } else {
    con.a <- attributes(input1)$type == input.types[1]
    con.b <- attributes(input2)$type == input.types[2]
    con.c <- attributes(input1)$firm == attributes(input2)$firm
    if(!(con.a & con.b & con.c)){
      stop("Invalid sheet types. At least one of the sheet types is invalid, or you are using sheets from different firms.")
    }
  }
}

# Finds the years in common between a vector of fundamentals.

YearsInCommon <- function(firm.name,fundamentals.names,sheets,years=NULL){
  info <- lapply(fundamental.name,FundamentalInfo)
  sheet <- lapply(info,FUN= function(x) GetSheet(firm.name,x[3],sheets))
  years.in.common <- Reduce(f=intersect,lapply(sheet,function(x) attributes(x)$years))
  
  if(is.null(years)){
    if(length(years.in.common)==0){
      stop("No years in common.")
    }
    return(years.in.common)
  } else {
    if(length(intersect(years.in.common,years))==0){
      stop("No years in common.")
    }
    return(intersect(years.in.common,years))
  }
}

GetFundamentals <- function(firm.name,fundamentals.names,sheets,years=NULL){
  
  # Extract name/location information on chosen fundamental, as well as the corresponding sheet.
  info <- lapply(fundamentals.names,FUN=FundamentalInfo)
  sheet <- lapply(info,FUN= function(x) GetSheet(firm.name,x[3],sheets))
  
  # Determine years in common; reset years to this value
  years <- YearsInCommon(firm.name,fundamentals.names,sheets,years)
  
  # Determine the years.location for chosen years
  years.location <- lapply(sheet,function(x) attributes(x)$years.location[which(attributes(x)$years %in% years)])

  # Extract the specified fundamentals
  fundamentals <- mapply(sheet,info,years.location,FUN=function(x,y,z) StripFormatting(x[as.numeric(y[4]),z]))

  # Set attributes for fundamental. 
  rownames(fundamentals) <- years
  colnames(fundamentals) <- sapply(info,function(x) x[1])
  attributes(fundamentals)$firm <- firm.name
  
  # Return fundamental
  return(fundamentals)
}

FundamentalInfo <- function(fundamental.name=NULL){
  matrix <- structure(c("Current Assets", "Current Liabilities", "Net Income", 
                        "Total Revenue", "Total Assets", "Long Term Debt", "Total Debt", 
                        "Total Equity", "Cash Flow From Operations", "Preferred Dividends", 
                        "Average Outstanding Shares", "Earnings Per Share", "Operating Income", 
                        "ATCA", "LTCL", "NINC", "RTLR", "ATOT", "LTTD", "STLD", "QTLE", 
                        "OTLO", "CPRD", "SBAS", "SBBF", "SOPI", "Balance Sheet", "Balance Sheet", 
                        "Income Statement", "Income Statement", "Balance Sheet", "Balance Sheet", 
                        "Balance Sheet", "Balance Sheet", "Cash Flow Statement", "Income Statement", 
                        "Income Statement", "Income Statement", "Income Statement", "39", 
                        "82", "90", "26", "67", "85", "86", "117", "45", "91", "99", 
                        "100", "63"), .Dim = c(13L, 4L), .Dimnames = list(NULL, c("Fundamental", 
                                                                                  "Code", "Sheet", "Location")))
  if(is.null(fundamental.name)){
    out <- matrix
    return(out)
  } else if(nchar(fundamental.name)==4) {
    out <- matrix[matrix[,2]==fundamental.name,]
    return(out)
  } else {
    out <- matrix[matrix[,1]==fundamental.name,]
    return(out)
  }
}


RatioInfo <- function(ratio.name=NULL){
  r <- list()
  r[[1]] <- list(fundamentals=c("Net Income","Total Revenue"),formula=function(x) x[,1]/x[,2])
  names(r)[[1]] <- "Net Margin"
  r[[2]] <- list(fundamentals=c("Net Income","Total Assets"),formula=function(x) x[,1]/x[,2])
  names(r)[[2]] <- "Return on Assets"
  r[[3]] <- list(fundamentals=c("Total Revenue","Total Assets"),formula=function(x) x[,1]/x[,2])
  names(r)[[3]] <- "Sales to Total Assets"
  r[[4]] <- list(fundamentals=c("Current Assets","Current Liabilities"),formula=function(x) x[,1]/x[,2])
  names(r)[[4]] <- "Current Ratio"
  r[[5]] <- list(fundamentals=c("Long Term Debt","Total Assets"),formula=function(x) x[,1]/x[,2])
  names(r)[[5]] <- "Long Term Debt to Total Assets"
  r[[6]] <- list(fundamentals=c("Total Debt","Total Equity"),formula=function(x) x[,1]/x[,2])
  names(r)[[6]] <- "Leverage Ratio"
  r[[7]] <- list(fundamentals=c("Cash Flow From Operations","Current Liabilities"),formula=function(x) x[,1]/x[,2])
  names(r)[[7]] <- "Operating Cash Flow Ratio"
  r[[8]] <- list(fundamentals=c("Long Term Debt","Total Equity"),formula=function(x) x[,1]/x[,2])
  names(r)[[8]] <- "Long Term Debt To Total Equity Ratio"
  r[[9]] <- list(fundamentals=c("Net Income","Preferred Dividends","Average Outstanding Shares"),formula=function(x) (x[,1]-x[,2])/x[,3])
  names(r)[[9]] <- "Earnings Per Share"
  r[[10]] <- list(fundamentals=c("Operating Income","Total Assets","Current Liabilities"),formula=function(x) x[,1]/(x[,2]-x[,3]))
  names(r)[[10]] <- "Return on Capital Employed"
  r[[11]] <- list(fundamentals=c("Net Income","Total Equity"),formula=function(x) x[,1]/x[,2])
  names(r)[[11]] <- "Return on Equity"
  
  if(is.null(ratio.name)){
    return(r)
  } else {
    return(r[names(r)==ratio.name])
  }
}


GetRatios <- function(firm.name,sheets,ratio.names=NULL,years=NULL){
  if(is.null(ratio.names)){
    ratio.names <- names(RatioInfo())
  }
  fundamentals <- lapply(ratio.names,FUN= function(x) GetFundamentals(firm.name,RatioInfo(x)[[1]][[1]],sheets,years))
  out <- mapply(ratio.names,fundamentals,FUN=function(x,y) RatioInfo(x)[[1]][[2]](y))
  attributes(out)$firm <- firm.name
  return(out)
}


bs.new <- drop.redundant.filings(bs)
cf.new <- drop.redundant.filings(cf)
is.new <- drop.redundant.filings(is)






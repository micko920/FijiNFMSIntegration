
DATA_PATH <- "./Data/mrPostAuditOct25/"

getDataPath<-function(filename) {
  result <- paste0(DATA_PATH, filename)
  print(result)
  return(result)
}

getPeriodDataPath<-function(filename, period="", ext="") {
  periodStr <- ""
  extStr <- ""
  if (!missing(period)) {
    periodStr <- paste0("-",period)
  }
  if (!missing(ext)) {
    extStr <- paste0(".",ext)
  }
  result <- getDataPath(paste0(filename, periodStr, extStr))
  #print(result)
  return(result)
}
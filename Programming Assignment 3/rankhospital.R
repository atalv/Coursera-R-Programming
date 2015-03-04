#outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#outcome[, 11] <- as.numeric(outcome[, 11])
#hist(outcome[, 11])

rankhospital <- function(state,outcome,num="best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  ## if invalid then stop the function with a message
  if (sum(data["State"]==state)==0) stop("invalid state")
  if (sum(c("HEART ATTACK","HEART FAILURE", "PNEUMONIA")==toupper(outcome))==0) stop("invalid outcome")
  
  ## Return hospital name in that state with the given rank 30-day death rate
  cn<-colnames(data)
  ## keeping only relevant column identifiers
  reqc<-c(2,grep(paste("^Hospital.30.Day.Death..Mortality..Rates.from.*",gsub(" ",".",outcome),"$",sep=""),cn,ignore.case = T))
  ## subset data for defined state and keep only selected columns
  sdata<-subset(data,State==state,select=reqc)
  ## Convert rquired values to numeric
  sdata[, 2] <- as.numeric(sdata[, 2])
  ##check for invalid 'num'
  if (is.numeric(num)) {
    if (nrow(sdata)<num) return(NA)
  }
  ## sort the data by rate>hospital.name and drop NA cases
  odata<-sdata[order(sdata[2],sdata$Hospital.Name,na.last=NA),]
  ## convert 'best' and 'worst' to respective numeric values
  if (!is.numeric(num)){
    if (num=="best") num=1
    if (num=="worst") num=nrow(odata)
  }
  ## final output of ranke 'num'
  odata[num,1]
}
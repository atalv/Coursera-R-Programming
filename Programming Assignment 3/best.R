#outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#outcome[, 11] <- as.numeric(outcome[, 11])
#hist(outcome[, 11])

best <- function(state,outcome){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (sum(data["State"]==state)==0) stop("invalid state")
  if (sum(c("HEART ATTACK","HEART FAILURE", "PNEUMONIA")==toupper(outcome))==0) stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  cn<-colnames(data)
  reqc<-c(2,grep(paste("^Hospital.30.Day.Death..Mortality..Rates.from.*",gsub(" ",".",outcome),"$",sep=""),cn,ignore.case = T))
  sdata<-subset(data,State==state,select=reqc)
  ## Convert rquired values to numeric
  sdata[, 2] <- as.numeric(sdata[, 2])

  sdata[order(sdata[2],sdata$Hospital.Name),][1,1]
  #head(sdata)
  
}
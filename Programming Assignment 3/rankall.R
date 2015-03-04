#outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#outcome[, 11] <- as.numeric(outcome[, 11])
#hist(outcome[, 11])

rankall <- function(outcome,num="best"){
  ## Read outcome data, read all columns as character
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that 'outcome' is valid
  if (sum(c("HEART ATTACK","HEART FAILURE", "PNEUMONIA")==toupper(outcome))==0) stop("invalid outcome")
  
  ## keeping only required columns namely hospital.name, state and relevant 'outcome'
  cn<-colnames(data)
  ## grep: keep only those column starting with something and ending with something
  ## gsub: substitute values within a string
  ## reqc: contains column identifiers
  reqc<-c(2,7,grep(paste("^Hospital.30.Day.Death..Mortality..Rates.from.*",gsub(" ",".",outcome),"$",sep=""),cn,ignore.case = T))

  ## discarding other columns
  sdata<-subset(data,select=reqc)
  ## Convert rquired values to numeric
  sdata[, 3] <- as.numeric(sdata[, 3])
  ## sort the data by state>rate>hospital.name and drop NA cases
  sdata<-sdata[order(sdata[2],sdata[3],sdata[1],na.last=NA),]
  ## creating a list grouped by state
  olist<-split(sdata,sdata[2])
  ## getting hospital names for each state of rank 'num'
  hname<-sapply(olist,function(x) { if (num=="best") num=1
                                    if (num=="worst") num=nrow(x)
                                    x[1][num,]
                                   })
  data.frame(hospital=hname,state=names(hname))
}
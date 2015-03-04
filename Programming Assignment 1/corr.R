corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  corrv<-vector("numeric",length=0)
  c<-1
  for (i in 1:332) {
    if (i<10){
      file<-paste(getwd(),"/",directory,"/00",i,".csv",sep="")
    }else if (i<100){
      file<-paste(getwd(),"/",directory,"/0",i,".csv",sep="")
    }else {
      file<-paste(getwd(),"/",directory,"/",i,".csv",sep="")
    }
    data<-read.csv(file)
    if  (sum(complete.cases(data))>threshold) {
      corrv<-cbind(corrv,cor(data[,"nitrate"],data[,"sulfate"],use="complete.obs"))
    }
    c<-c+1
  }
  corrv
}
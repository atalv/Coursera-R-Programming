complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  c<-1
  for (i in id) {
    if (i<10){
      file<-paste(getwd(),"/",directory,"/00",i,".csv",sep="")
    }else if (i<100){
      file<-paste(getwd(),"/",directory,"/0",i,".csv",sep="")
    }else {
      file<-paste(getwd(),"/",directory,"/",i,".csv",sep="")
    }
    data<-read.csv(file)
    if (c==1) {
      idn<-i
      nobs<-sum(complete.cases(data))
    }else{
      idn<-rbind(idn,i)
      dimnames(idn) <- list(NULL)
      nobs<-rbind(nobs,sum(complete.cases(data)))
      dimnames(nobs) <- list(NULL)
    }
    c<-c+1
  }
  data.frame(id=idn,nobs=nobs)
}
pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ##  setwd(paste(getwd(),directory,sep="/"))

        c<-1
  for (i in id) {
    if (i<10){
      file<-paste(getwd(),"/",directory,"/00",i,".csv",sep="")
    }else if (i<100){
      file<-paste(getwd(),"/",directory,"/0",i,".csv",sep="")
    }else {
      file<-paste(getwd(),"/",directory,"/",i,".csv",sep="")
    }
    if (c==1) {
    data<-read.csv(file)
    }else{
      data<-rbind(data,read.csv(file))
    }
    c<-c+1
  }
  mean(data[,pollutant],na.rm=T)
}
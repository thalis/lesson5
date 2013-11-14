#Author: Thanos Strantzalis
# Date: November 2013

rm(list = ls())

average<- function(x,y){
  if(is.na(x) && is.na(y)){
    out<-NA
  }
  else if(is.na(x) | is.na(y)){
    out<-max(y,x, na.rm=TRUE)
  }
  else{
    out<-(x+y)/2
  }
  return(out)
}

Vaverage<- function(x,y){
  out <- mapply(FUN=average,x,y)
  return(out)
}


landsatcmp <-function(landsat1,landsat2){
  
  cloud<-calc(x=landsat1[[9]], fun=QA2cloud)
  cloud[cloud==0]<- NA
  cloud2<-calc(x=landsat2[[9]], fun=QA2cloud)
  cloud2[cloud2==0]<- NA
  
  landsat1<-dropLayer(x=landsat1, i=9)
  landsat2<-dropLayer(x=landsat2, i=9)
  landsat1[cloud==1] <- NA
  landsat2[cloud2==1]<-NA
  
  out<-overlay(x=landsat1,y=landsat2, fun=Vaverage)
  
  return(out)
}


library(rasta)
data(taravao)
data(taravao2)

composite<-landsatcmp(taravao,taravao2)
plotRGB(composite,5,3,4)

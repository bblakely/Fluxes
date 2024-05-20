#Compare maize datasets

#Read in Maize Basalt

dir<-getwd()
setwd("Fluxdata")
library(readxl)

maize.raw<-read_excel("Maize_2008_to_2019_L6.xlsx", sheet=2,skip=2) #Maize basalt all the way through; no basalt before 2017
maize.raw[maize.raw==-9999]<-NA;
maize1<-maize.raw


maize2.raw<-read_excel("Maize_2008_to_2016_L6.xlsx", sheet=2,skip=2) #Maize basalt all the way through; no basalt before 2017
maize2.raw[maize2.raw==-9999]<-NA; 
maize2<-maize2.raw

setwd(dir)

###Timestamp unpacker####
unpack.time<-function(dat){
  dat.ts<-dat$xlDateTime
  ts<-strptime(dat.ts, "%Y-%m-%d %H:%M", tz="")
  YEAR<-as.numeric(format(ts, '%Y'))
  DECDOY<-as.numeric(format(ts, '%j'))+(as.numeric(format(ts, '%H'))/24)+(as.numeric(format(ts, '%M'))/1440)
  DOY<-as.numeric(format(ts, '%j'))
  MONTH<-as.numeric(format(ts, '%m'))
  DAY<-as.numeric(format(ts, '%d'))
  HOUR<-as.numeric(format(ts, '%H'))
  DECHR<-as.numeric(format(ts, '%H'))+as.numeric(format(ts, '%M'))/60
  DECYEAR<-YEAR+(DECDOY/366)
  ts.dat<-data.frame(cbind(YEAR,DECDOY,DECYEAR, MONTH, DAY, DOY,DECHR,HOUR))
  
  return(ts.dat)
}

zm1.ts<-unpack.time(maize1)
zm2.ts<-unpack.time(maize2)


zm1.14<-maize1[zm1.ts$YEAR==2014,]
zm2.14<-maize2[zm2.ts$YEAR==2014,]

zm1.15<-maize1[zm1.ts$YEAR==2015,]
zm2.15<-maize2[zm2.ts$YEAR==2015,]

zm1.16<-maize1[zm1.ts$YEAR==2016,]
zm2.16<-maize2[zm2.ts$YEAR==2016,]

years<-c(2009:2016)

par(mfrow=c(2,2))

for(i in 1:length(years)){
  zm1<-maize1[zm1.ts$YEAR==years[i],]
  zm2<-maize2[zm2.ts$YEAR==years[i],]
  
  smoothScatter(zm1$Fc~zm2$Fc, main=years[i], ylab="2008 - 2019 dataset", 
       xlab="2008 - 2016 dataset", ylim=c(-65, 50), xlim=c(-65, 50))

  abline(h=0); abline(v=0)
  
  }



for(i in 1:length(years)){
  zm1<-maize1[zm1.ts$YEAR==years[i],]
  zm2<-maize2[zm2.ts$YEAR==years[i],]
  
  plot(zm1$Fc~zm2$Fc, main=years[i], ylab="2008 - 2019 dataset", 
                xlab="2008 - 2016 dataset", ylim=c(-65, 50), xlim=c(-65, 50))
  
  abline(h=0); abline(v=0)
  
}




for(i in 1:length(years)){
  zm1<-maize1[zm1.ts$YEAR==years[i],]
  zm2<-maize2[zm2.ts$YEAR==years[i],]
  
  plot(zm1$Fc-zm2$Fc~zm2.ts$DECDOY[zm2.ts$YEAR==years[i]], main=years[i], ylab="differences (2019 set - 2016 set)", 
       xlab="DOY", ylim=c(-40,40))
  
  abline(h=0)
  
}

for(i in 1:length(years)){
  zm1<-maize1[zm1.ts$YEAR==years[i],]
  zm2<-maize2[zm2.ts$YEAR==years[i],]
  
  plot(cumsum((zm1$Fc*0.0792*.0027)-(zm2$Fc*0.0792*.0027))~zm2.ts$DECDOY[zm2.ts$YEAR==years[i]], main=years[i], ylab="2008 - 2019 dataset", 
       xlab="2008 - 2016 dataset")
  
  abline(h=0)
  
}



for(i in 1:length(years)){
  zm1<-maize1[zm1.ts$YEAR==years[i],]
  zm2<-maize2[zm2.ts$YEAR==years[i],]
  
  plot(cumsum(zm1$Fc*0.0792*.0027)~zm2.ts$DECDOY[zm2.ts$YEAR==years[i]], main=years[i], type='l', ylab="tc/ha", xlab="DOY", ylim=c(-5, 3))
  
  lines(cumsum(zm2$Fc*0.0792*.0027)~zm2.ts$DECDOY[zm2.ts$YEAR==years[i]], col='blue')
  
  lines(cumsum((zm1$Fc*0.0792*.0027)-(zm2$Fc*0.0792*.0027))~zm2.ts$DECDOY[zm2.ts$YEAR==years[i]], lwd=3, col='red')
  
  abline(h=0)
  
  legend (0, -1, cex=0.8, bty="n", y.intersp=0.3, legend=c("dataset 2008 - 2019", "dataset 2008 - 2016", "difference"), col=c("black", "blue", "red"), lwd=c(1,1,3))
  
}


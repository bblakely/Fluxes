
library(readxl)

sorghum.raw<-read_excel("sorghum_2018_to_2019_L6.xlsx", sheet=2,skip=2)
sorghum.raw.2020<-read_excel("Sorghum_2020_to_2021_L6.xls", sheet=2, skip=2)
sorghum.raw[sorghum.raw==-9999]<-NA; sorghum.raw.2020[sorghum.raw.2020==-9999]<-NA
sorghum<-sorghum.raw; sorghum.2020<-sorghum.raw.2020

sorg.merge.mod<-sorghum.2020; colnames(sorg.merge.mod)[colnames(sorg.merge.mod)=="Fco2"]<-"Fc"
sorg.merge<-merge(sorghum, sorg.merge.mod, all=TRUE)
rm(sorg.merge.mod)

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
  MIN<-as.numeric(format(ts, '%M'))
  DECHR<-as.numeric(format(ts, '%H'))+as.numeric(format(ts, '%M'))/60
  DECYEAR<-YEAR+(DECDOY/366)
  ts.dat<-data.frame(cbind(YEAR,DECDOY,DECYEAR, MONTH, DAY, DOY,DECHR,HOUR, MIN))
  
  return(ts.dat)
}

sorg.ts<-unpack.time(sorg.merge)



sb.all<-cbind(sorg.ts, sorg.merge)


subvars<-c("YEAR","MONTH","DAY", "HOUR", "MIN", "Fsd", "Ta", "RH", "Precip", "Ws")

sb<-sb.all[, which(colnames(sb.all)%in%subvars)]




agroibis.format<-function(dat, year, plots=TRUE){

dat<-dat[dat$YEAR==year,] #subset to year specified

dat.agg<-aggregate(dat, by=list(dat$HOUR, dat$DAY, dat$MONTH), FUN="mean")

dat.clip<-dat.agg[4:ncol(dat.agg)]; dat.clip$MIN<-0 #Remove "group" columns, set MIN to 0 instead of 15
obs<-seq(from=1, to=nrow(dat.clip)) #make sequence of obs column

dat.full<-cbind(dat.clip[1], obs, dat.clip[2:5], dat.clip$Fsd, 
                dat.clip$Ta, dat.clip$RH, dat.clip$Precip, dat.clip$Ws) #splice in obs column, reorder columns
colnames(dat.full)<-c("C_Year","Obs", "C_Month", "C_Day", "C_Hour", 
                         "C_Minute", "dw_psp [Wm-2]", "temp [Co]", "rh [%]", "Precip [mm]", "windspd [ms-1]")
if (plots==TRUE){
  par(mar=c(2,2,4,1), mfrow=c(2,2))
  for(i in c(7:11)){
    plot(dat.full[,i], main=paste(year, colnames(dat.full)[i]))
  }
  par(mar=c(4,4,4,4), mfrow=c(1,1))
}

return(dat.full)
}

sb.2018<-agroibis.format(sb, 2018)
write.csv(sb.2018, "C:/Users/Bethany/Desktop/EF_2018.csv", row.names=FALSE)

sb.2020<-agroibis.format(sb, 2020)
write.csv(sb.2020, "C:/Users/Bethany/Desktop/EF_2020.csv", row.names=FALSE)

sb.2021<-agroibis.format(sb, 2021)
write.csv(sb.2021, "C:/Users/Bethany/Desktop/EF_2021.csv", row.names=FALSE)


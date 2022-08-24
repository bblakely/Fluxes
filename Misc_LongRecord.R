#Miscellaneous long record tasks



#Quick look at whether 2020 was bad?####

years<-unique(as.numeric(format(maize$xlDateTime, "%Y")))
Y<-(as.numeric(format(maize$xlDateTime, "%Y")))
#read in maize
maize.raw.2020<-read_excel("MaizeCon_2020_L6.xls", sheet=2,skip=2)
maize.raw.2020[maize.raw.2020==-9999]<-NA
maize.2020<-maize.raw.2020#[1:(nrow(maize.raw)-1),]

plot(cumsum(maize$Fc[which(Y==2011)])~as.numeric(format(maize$xlDateTime[Y==2011], "%j")), col='white', ylim=c(-30000, 15000))
for (i in c(1:6, 9:length(years))){
  yearsum<-cumsum(maize$Fc[which(Y==years[i])])
  
  lines(yearsum~as.numeric(format(maize$xlDateTime[Y==years[i]], "%j")), col=i)
  
  
}

lines(cumsum(maize.2020$Fco2)~as.numeric(format(maize.2020$xlDateTime, "%j")), col='black', lwd=4, type='l')
lines(cumsum(maize$Fc[Y==2017])~as.numeric(format(maize$xlDateTime[Y==2017], "%j")), col='blue', lwd=4, type='l')
lines(cumsum(maize$Fc[Y==2018])~as.numeric(format(maize$xlDateTime[Y==2018], "%j")), col='red', lwd=4, type='l')

#Nope, corn was just fine being knocked over apparently

#Soil temp at planting time
sbdates<-unpack.time(maize.merge)
subset<-which(sbdates$MONTH==5)

#thing<-aggregate(sorg.merge$Ts_5cm[subset], by=list(sbdates$DOY[subset]), FUN='mean', na.rm=TRUE)

#plot(thing, ylab='temp deg.c', xlab="day of year (May)");(abline(v=135))

interan<-function(dat, sub=c(2008:2021)){
  ts<-unpack.time(dat)
  ind<-which(ts$YEAR%in%sub)
  nee<-aggregate(dat$Fc[ind]*0.0792*0.0027, by=list(ts$YEAR[ind]), FUN='sum', na.rm=TRUE)
}

nee.zm<-interan(maize.merge); nee.zm$id<-"ZM"
nee.mg<-interan(misc.merge);nee.mg$id<-"MG"
nee.sw<-interan(switchgrass); nee.sw$id<-"SW"
nee.sb<-interan(sorg.merge); nee.sb$id<-"SB"

nee.zmc<-interan(maize.c.merge); nee.zmc$id<-"ZMC"
nee.mgc<-interan(misc.c.merge); nee.mgc$id<-"MGC"

nee.comp<-rbind(nee.zm,nee.mg, nee.sb, nee.sw, nee.zmc, nee.mgc)


short.nosoy<- c(2020, 2021)
early.nosoy<-c(2008, 2009, 2011, 2012,  2014, 2015)
long.nosoy<-c(2008, 2009, 2011, 2012, 2014, 2015, 2017, 2018, 2020, 2021)
soy.yr<-c(2010, 2013, 2016, 2019); soy.yr.early<-c(2010, 2013, 2016)


boxplot(x~id, dat=nee.comp[nee.comp$Group.1%in%short.nosoy,], ylab="NEE", main="2019 to 2021 (sorghum, maize, miscanthus)")
boxplot(x~id, dat=nee.comp[nee.comp$Group.1%in%early.nosoy,], ylab="NEE", main="2008 to 2016 (switchgrass, maize, miscanthus")
boxplot(x~id, dat=nee.comp[which(nee.comp$id%in%c("ZM", "MG")& nee.comp$Group.1%in%long.nosoy),], ylab="NEE", main="2008 to 2021 (maize, miscanthus)")

boxplot(x~id, dat=nee.comp[which(nee.comp$id%in%c("ZM", "MG")& nee.comp$Group.1%in%soy.yr),], ylab="NEE", main="2010/13/16/19 (soy years)")
boxplot(x~id, dat=nee.comp[which(nee.comp$Group.1%in%soy.yr.early),], ylab="NEE", main="2010/13/16 (soy years w/switchgrass)")


#back-of-envelope for ecosystem fluxes
mean(nee.zm$x[c(1:6, 8:12)]+maize.yield[c(1:6, 8:12)]); sd((nee.zm$x[c(1:6, 8:12)]+maize.yield[c(1:6, 8:12)]))
mean(nee.mg$x[1:12]+misc.yield); sd(nee.mg$x[1:12]+misc.yield)
mean(nee.sw$x[1:9]+switch.yield); sd(nee.sw$x[1:9]+switch.yield)
mean(nee.sb$x[1:3]+sorg.yield); sd(nee.sb$x[1:3]+sorg.yield)

library(vioplot)


vioplot(x~id, dat=nee.comp[nee.comp$Group.1%in%short.nosoy,], ylab="ER", main="2019 to 2021 (sorghum, maize, miscanthus)")
vioplot(x~id, dat=nee.comp[nee.comp$Group.1%in%early.nosoy,], ylab="ER", main="2008 to 2016 (switchgrass, maize, miscanthus")
vioplot(x~id, dat=nee.comp[which(nee.comp$id%in%c("ZM", "MG")& nee.comp$Group.1%in%long.nosoy),], ylab="ER", main="2008 to 2021 (maize, miscanthus)")


# #extract some stuff for matt
# 
# 
# windind<-function(dat, year, month, day){
# dat.ts<-unpack.time(dat); 
# dat.ind<-which(dat.ts$YEAR==year&dat.ts$MONTH==month&dat.ts$DAY==day)
# return(dat.ind)
# }
# 
# datevec<-data.frame(matrix(nrow=3, ncol=0))
# datevec$years<-c(2018, 2019, 2020); datevec$months<-c(8,8,9); datevec$days<-c(31, 28, 19)
# datenames<-paste(datevec$years, datevec$months,  datevec$days, sep ="-")
# 
# summary.stat<-data.frame(matrix(nrow=nrow(datevec), ncol=3)); colnames(summary.stat)<-c("avg", "max", "min")
# avgwind<-data.frame(matrix(nrow=48, ncol=nrow(datevec)+1)); colnames(avgwind)<-c("hour",datenames)
# 
# 
# for(i in 1:nrow(datevec)){
# 
# 
# sb.ind.1<-windind(dat=sorg.merge, datevec[i,1], datevec[i,2], datevec[i,3])
# mgc.ind.1<-windind(dat=misc.c.merge, datevec[i,1], datevec[i,2], datevec[i,3])
# mgb.ind.1<-windind(dat=misc.merge, datevec[i,1], datevec[i,2], datevec[i,3])
# zmc.ind.1<-windind(dat=maize.c.merge, datevec[i,1], datevec[i,2], datevec[i,3])
# zmb.ind.1<-windind(dat=maize.merge, datevec[i,1], datevec[i,2], datevec[i,3])
# 
# wind.dat<-cbind(sorg.merge$Ws[sb.ind.1],
#                 misc.merge$Ws[mgb.ind.1], misc.c.merge$Ws[mgc.ind.1],
#                 maize.merge$Ws[zmb.ind.1], maize.c.merge$Ws[zmc.ind.1])
# 
# cols<-c("forest green", "blue", "light blue", "orange", "yellow")
# 
# plot(wind.dat[,i], col='white', main=datevec[i,], ylim=c(0, max(wind.dat[,i])*1.3))
# for (p in 1: ncol(wind.dat)){
#   points(wind.dat[,p], col=cols[p])
# }
# 
# avg.wind<-rowMeans(wind.dat)
# avgwind[,i+1]<-avg.wind
# 
# 
# #lines(avg.wind)
# 
# avg.all<-mean(avg.wind); max.all<-max(avg.wind); min.all<-min(avg.wind)
# 
# summary.stat[i,]<-c(avg.all, max.all, min.all)
# }
# 
# time.ex<-unpack.time(sorg.merge)[sb.ind.1,]
# avgwind[,1]<-c(time.ex$DECHR)
# 
# 
# write.csv(avgwind, "Windspeed(ms-2)_dates_requested_by_MS.csv", row.names = FALSE)

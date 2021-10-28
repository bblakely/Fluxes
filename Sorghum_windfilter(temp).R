library(readxl)
library(bigleaf)
library(zoo)
sorg.raw<-read_excel("Sorghum_2018_2020_L6.xls", sheet=2,skip=2)
sorg.raw[sorg.raw==-9999]<-NA

sorg<-sorg.raw[as.numeric(format(sorg.raw$xlDateTime, "%Y"))==2020,]
#sorg.2018.months; sorg$xlDateTime<-sorg.raw$xlDateTime
sorg$H<-as.numeric(format(sorg$xlDateTime, "%H"))


east.deg<-c(15, 165); west.deg<-c(180, 315)
east.recs<-which(sorg$Wd>east.deg[1]&sorg$Wd<east.deg[2])
west.recs<-which(sorg$Wd>west.deg[1]&sorg$Wd<west.deg[2])

sorg.e<-sorg; sorg.e[which(sorg$Wd<east.deg[1]|sorg$Wd>east.deg[2]),]<-NA
sorg.w<-sorg;sorg.w[which(sorg$Wd<west.deg[1]|sorg$Wd>west.deg[2]),]<-NA



sorgflux.e<-umolCO2.to.gC(sorg.e$GPP_LT); sorgflux.w<-umolCO2.to.gC(sorg.w$GPP_LT)

# plot(sorgflux.e~sorg.e$xlDateTime); points(sorgflux.w~sorg.w$xlDateTime, col='red')
# 
# sorgroll.e<-rollapply(sorgflux.e, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
# sorgroll.w<-rollapply(sorgflux.w, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
# 
# plot(sorgroll.e~sorg$xlDateTime, type='l'); lines(sorgroll.w~sorg$xlDateTime, col='red')

#systematic differences in hours...
hours<-sorg$H
hist(hours[east.recs])
hist(hours[west.recs])

east.counts<-hist(hours[east.recs], breaks=c(-1:23))$counts #number obs at each hour
west.counts<-hist(hours[west.recs], breaks=c(-1:23))$counts

diffs<-east.counts-west.counts; diffs #differences in obs at hours

#Try to remove data until each has same # at each hour

remove.west.hours<-which(diffs<0)-1  #minus one makes hour zero  work
remove.east.hours<-which(diffs>0)-1

sorg.w.thin<-sorg.w;sorg.e.thin<-sorg.e

for(i in remove.west.hours){
  thinee<-which(!is.na(sorgflux.w) & hours==i)
  axe<-sample(thinee, abs(diffs[i+1]))
  sorg.w.thin[axe,]<-NA

}

for(i in remove.east.hours){
  thinee<-which(!is.na(sorgflux.e) & hours==i)
  axe<-sample(thinee, abs(diffs[i+1]))
  sorg.e.thin[axe,]<-NA
  
}


hist(sorg.e.thin$H[east.recs], breaks=c(-1:23));hist(sorg.w.thin$H[west.recs], breaks=c(-1:23))

sorgflux.e.thin<-umolCO2.to.gC(sorg.e.thin$GPP_LT); sorgflux.w.thin<-umolCO2.to.gC(sorg.w.thin$GPP_LT)

plot(sorgflux.e.thin~sorg.e$xlDateTime); points(sorgflux.w.thin~sorg.w$xlDateTime, col='red')

sorgroll.e<-rollapply(sorgflux.e.thin, width=48*14, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
sorgroll.w<-rollapply(sorgflux.w.thin, width=48*14, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)

plot(sorgroll.e~sorg$xlDateTime, type='l'); lines(sorgroll.w~sorg$xlDateTime, col='red')
par(new=TRUE)
plot(sorg$GPP_LT~format(sorg$xlDateTime, "%j"), col='', axes="FALSE", ylab='', xlab='')
abline(v=c(132, 163))
# 
# par(new=TRUE)
# plot(sorg$GPP_LT~format(sorg$xlDateTime, "%j"), col='', axes="FALSE", ylab='', xlab='')
# abline(v=(195-1)) #Hailstorm
# abline(v=156)
# abline(v=(181), lty=2) #dry spell start
# abline(v=(190), lty=2) #dry spell end
# abline(v=223)
# text(149,25,"Flood >")
# text(185,18, "Heatwave")
# text(205,25, "< Hailstorm")
# text(233, -20, "< Derecho")

gs<-which(format(sorg$xlDateTime, "%j")>130)
flexmin<-min(min(cumsum(sorgroll.e[gs]/48)), min(cumsum(sorgroll.w[gs]/48)))*1.1
flexmax<-max(max(cumsum(sorgroll.e[gs]/48)), max(cumsum(sorgroll.w[gs]/48)))*1.1

plot(cumsum(sorgroll.e[gs])/48~sorg$xlDateTime[gs], type='l', ylim=c(flexmin, flexmax),col='blue', lwd=2, ylab="Gross Primary Productivity")
lines(cumsum(sorgroll.w[gs])/48~sorg$xlDateTime[gs], col='red', lwd=2)
par(new=TRUE)
plot(sorg$GPP_LT[gs]~format(sorg$xlDateTime, "%j")[gs], col='', axes="FALSE", ylab='', xlab='', lwd=2)
#abline(v=c(132, 163))
legend(x=140, y=70, legend=c("flood", "nonflood"), lwd=2, col=c("red", "blue"))
abline(v=c(195, 210, 233, 249), lty=3)


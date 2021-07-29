#Script for exploring causes of sorghuum respiration

if(!exists("sorg")){source("ReadAll.R")}

sorg.bkup<-sorg
sorg<-sorg.2018

par(mfrow=c(1,1))
plot(sorg$ER~sorg$xlDateTime,ylab=c("Sorghum ER"), xlab="Date");lines(sorg.er~sorg$xlDateTime, lwd=0.5)
ersub<-which(!is.na(sorg$ER)&format(sorg$xlDateTime, "%m")=="08")
hist(as.numeric(format(sorg$xlDateTime, "%H"))[ersub], breaks=c(0:23))
hist(sorg$Fsd[ersub])

plot(sorg$ER[ersub]~sorg$xlDateTime[ersub]);lines(sorg.er~sorg$xlDateTime, lwd=0.5)

#Precip/Sws sensitivity####
titles<-c("January","February","March","April","May","June","July","August","September","October","November","December")
par(mfrow=c(2,2))
for(i in c(7:10)){
ersub<-which(!is.na(sorg$ER)&as.numeric(format(sorg$xlDateTime, "%m"))==i)
monthlim<-range(as.numeric(sorg$xlDateTime[which(as.numeric(format(sorg$xlDateTime, "%m"))==i)]))
#plot(sorg$ER[ersub]~sorg$Ta[ersub], main=titles[i]) 
#plot(sorg$ER[ersub]~sorg$Sws[ersub], main=titles[i])
#par(mfrow=c(1,1))
plot(sorg$ER[ersub]~sorg$xlDateTime[ersub], main=titles[i], ylim=c(0,40), xlim=monthlim, ylab="ER (gm-2d-1)", xlab="date");lines(sorg.er~sorg$xlDateTime, lwd=0.5)
lines(sorg$Precip~sorg$xlDateTime, col='blue', lwd=2);lines(sorg$Sws*100~sorg$xlDateTime, col='brown')
legend(monthlim[1],40, legend=c("Sws (vwc*100)", "Precip", "ER (filled)"), col=c('brown', 'blue', 'black'), lwd=c(1,2, 0.5),bty='n')
legend((monthlim[1]*1.001),40, legend=c("ER 'observations'"),pch=1,bty='n')
}
#####

#Storm events####
xldecdoy<-as.numeric(format(sorg$xlDateTime, "%j"))+as.numeric(format(sorg$xlDateTime, "%H"))/24+as.numeric(format(sorg$xlDateTime, "%M"))/3600
#this may be approximate; check before being specific about times

plot(sorg$ER~xldecdoy,ylab=c("Sorghum ER"), xlab="Date",xlim=c(150,280), ylim=c(0,30));lines(sorg.er~xldecdoy, lwd=0.5)
abline(v=156) #Flood
abline(v=(195-1)) #Hailstorm
abline(v=223) #derecho
#abline(v=(181), lty=2) #dry spell start
#abline(v=(190), lty=2) #dry spell end

#zoom in
par(mfrow=c(1,3))

plot(sorg$ER~xldecdoy,ylab=c("Sorghum ER"), xlab="Date",xlim=c(150,190), ylim=c(0,30));lines(sorg.er~xldecdoy, lwd=0.5)
abline(v=156) #Flood

plot(sorg$ER~xldecdoy,ylab=c("Sorghum ER"), xlab="Date",xlim=c(180,230), ylim=c(0,30));lines(sorg.er~xldecdoy, lwd=0.5)
abline(v=195) #Hail

plot(sorg$ER~xldecdoy,ylab=c("Sorghum ER"), xlab="Date",xlim=c(210,250), ylim=c(0,30));lines(sorg.er~xldecdoy, lwd=0.5)
abline(v=195, lty=2) #Hail
abline(v=223) #derecho
#####


#Delayed bit####
library(bigleaf); library(zoo)
sorg$H<-as.numeric(format(sorg$xlDateTime, "%H"))
east.deg<-c(15, 165); west.deg<-c(180, 315)
east.recs<-which(sorg$Wd>east.deg[1]&sorg$Wd<east.deg[2])
west.recs<-which(sorg$Wd>west.deg[1]&sorg$Wd<west.deg[2])

sorg.e<-sorg; sorg.e[which(sorg$Wd<east.deg[1]|sorg$Wd>east.deg[2]),]<-NA
sorg.w<-sorg;sorg.w[which(sorg$Wd<west.deg[1]|sorg$Wd>west.deg[2]),]<-NA

sorgflux.e<-umolCO2.to.gC(sorg.e$GPP_LT); sorgflux.w<-umolCO2.to.gC(sorg.w$GPP_LT)

#systematic differences in hours...
par(mfrow=c(1,2))
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
#plot(sorgflux.e.thin~sorg.e$xlDateTime); points(sorgflux.w.thin~sorg.w$xlDateTime, col='red')

sorgroll.e<-rollapply(sorgflux.e.thin, width=48*14, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
sorgroll.w<-rollapply(sorgflux.w.thin, width=48*14, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)


#ylim=c(-20, 10)
title<-"GPP (g m-2 d-1)"
titlecum<-"Cumulative GPP (g m-2)"
ylim=c(0,25)

par(mfrow=c(2,1), mar=c(2,4,1,1))
gs<-which(format(sorg$xlDateTime, "%j")>0)#which(format(sorg$xlDateTime, "%j")>130)
plot(sorgroll.e~sorg$xlDateTime, type='l', ylim=ylim, col='blue', lwd=2, ylab=title); lines(sorgroll.w~sorg$xlDateTime, col='red', lwd=2)
par(new=TRUE)
plot(sorg$GPP_LT[gs]~format(sorg$xlDateTime, "%j")[gs], col='', axes="FALSE", ylab='', xlab='', lwd=2)
legend(x=1, y=25, legend=c("flood", "nonflood"), lwd=2, col=c("red", "blue"))
# abline(v=156); text(150,25, "Flood >");#abline(v=156+7, lty=2)
# abline(v=(195-1));text(195,-9, "Hailstorm");#abline(v=195+7, lty=2)
# abline(v=223);text(235,25, "< Derecho");#abline(v=223+7, lty=2)

flexmin<-min(min(cumsum((sorgroll.e[gs]/48)[which(!is.na(sorgroll.e[gs]))]), na.rm=TRUE), min(cumsum((sorgroll.w[gs]/48)[which(!is.na(sorgroll.w[gs]))]), na.rm=TRUE))*1.1
flexmax<-max(max(cumsum((sorgroll.e[gs]/48)[which(!is.na(sorgroll.e[gs]))]), na.rm=TRUE), max(cumsum((sorgroll.w[gs]/48)[which(!is.na(sorgroll.w[gs]))]), na.rm=TRUE))*1.1

plot(cumsum((sorgroll.e[gs]/48)[which(!is.na(sorgroll.e[gs]))])~(sorg$xlDateTime[gs])[which(!is.na(sorgroll.e[gs]))], type='l', ylim=c(flexmin, flexmax),xlim=c(as.numeric(min(sorg$xlDateTime[gs])), as.numeric(max(sorg$xlDateTime[gs]))), col='blue', lwd=2, ylab=titlecum)
lines(cumsum((sorgroll.w[gs]/48)[which(!is.na(sorgroll.w[gs]))])~(sorg$xlDateTime[gs])[which(!is.na(sorgroll.w[gs]))], col='red', lwd=2)
par(new=TRUE)
plot(sorg$GPP_LT[gs]~format(sorg$xlDateTime, "%j")[gs], col='', axes="FALSE", ylab='', xlab='', lwd=2)
legend(x=1, y=25, legend=c("flood", "nonflood"), lwd=2, col=c("red", "blue"))

######

#Up-close look at effect of disasters

sorgroll.e.detail<-rollapply(sorgflux.e.thin, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
sorgroll.w.detail<-rollapply(sorgflux.w.thin, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)


par(mfrow=c(1,1))
gs<-which(format(sorg$xlDateTime, "%j")>0)#which(format(sorg$xlDateTime, "%j")>130)
plot(sorgroll.e.detail[7000:13000]~sorg$xlDateTime[7000:13000], type='l', ylim=(ylim*1.5), col='blue', lwd=2, ylab=title); lines(sorgroll.w.detail~sorg$xlDateTime, col='red', lwd=2)
par(new=TRUE)
plot(sorg$GPP_LT[7000:13000]~format(sorg$xlDateTime, "%j")[7000:13000], col='', axes="FALSE", ylab='', xlab='', lwd=2)
legend(x=1, y=25, legend=c("flood", "nonflood"), lwd=2, col=c("red", "blue"))
# abline(v=156); text(150,25, "Flood >");abline(v=156+3.5, lty=2)
# abline(v=(195-1));text(195,-9, "Hailstorm");abline(v=195+3.5, lty=2)
# abline(v=223);text(235,25, "< Derecho");abline(v=223+3.5, lty=2)

write.csv(sorg.e.thin, "Sorghum_L6_East_Undisturbed.csv")
write.csv(sorg.w.thin, "Sorghum_L6_West_Disturbed.csv")

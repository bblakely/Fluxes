#cmuulative comparisons - functionalize

par(mfrow=c(2,1), mar=c(4,4,3,1))

compare_fluxes<-function(varnameL2, varnameL3=varnameL2){

#Find the right columns
zm.18<-which(colnames(maize.2018)==varnameL2)
zm.17<-which(colnames(maize.2017)==varnameL2)
zm.20<-which(colnames(maize)==varnameL3)

mg.18<-which(colnames(misc.2018)==varnameL2)
mg.17<-which(colnames(misc.2017)==varnameL2)
mg.20<-which(colnames(misc)==varnameL3)

sb.18<-which(colnames(sorg.2018)==varnameL2)
sb.20<-which(colnames(sorg)==varnameL3)

#set universal ylims
ylim.flux<-c(-30, 30)
ylim.cum<-c(-50000/48,5500/48)
ylab.flux<-"NEE (gC m-2 d-1)"; ylab.cum<-"Cumulative NEE (gC m-2)"
if(varnameL2=="GPP_LT"){ylim.cum<-c(0, 120000/48);ylab.flux<-"GPP (gC m-2 d-1)";ylab.cum<-"Cumulative GPP (gC m-2)"}
if(varnameL2=="ER_LT"){ylim.cum<-c(0, 100000/48);ylab.flux<-"ER (gC m-2 d-1)";ylab.cum<-"Cumulative ER (gC m-2)"}

#set PAR
par(mfrow=c(2,1))

##Plot for maize####
zmflux18<-rollmean(umolCO2.to.gC(maize.2018[,zm.18]), k=48, na.rm=TRUE, na.pad=TRUE)
zmflux17<-rollmean(umolCO2.to.gC(maize.2017[,zm.17]), k=48, na.rm=TRUE, na.pad=TRUE)
zmflux20<-rollmean(umolCO2.to.gC(maize[,zm.20]), k=48, na.rm=TRUE, na.pad=TRUE)

#ylim=c(1.2*min(c(zmflux18, zmflux17, zmflux20), na.rm=TRUE),1.2*max(c(zmflux18, zmflux17, zmflux20), na.rm=TRUE))

plot(zmflux18~format(maize.2018$xlDateTime, "%j"), type='l', col='lightpink', lwd=0.5, ylab=ylab.flux, xlab="day of year",  main="Maize",ylim=ylim.flux)
lines(zmflux17~format(maize.2017$xlDateTime,"%j"), col="lightblue", lwd=0.5)
lines(zmflux20~format(maize$xlDateTime, "%j"),col='gray', lwd=0.5)

abline(h=0, col='gray')

zmsm18<-rollmean(umolCO2.to.gC(maize.2018[,zm.18]), k=48*7, fill=NA, na.rm=TRUE)
zmsm17<-rollmean(umolCO2.to.gC(maize.2017[,zm.17]), k=48*7, fill=NA, na.rm=TRUE)
zmsm20<-rollmean(umolCO2.to.gC(maize[,zm.20]), k=48*7, fill=NA, na.rm=TRUE)

lines(zmsm18~format(maize.2018$xlDateTime, "%j"), lwd=3, col="red")
lines(zmsm17~format(maize.2017$xlDateTime, "%j"), lwd=3, col="blue")
lines(zmsm20~format(maize$xlDateTime, "%j"), lwd=3, col='black')
#lines(rollmean(umolCO2.to.gC(maize.2019$Fc), k=48*7, na.rm=TRUE, fill=NA)~format(maize.2019$xlDateTime, "%j"), lwd=3, col='green')

legend(1,ylim.flux[2], legend=c(2020, 2018, 2017),bty='n',cex=0.8, col=c('black', 'red','blue'), lwd=2, y.intersp = 0.8)

maize.20.interp<-unlist(cumsum(umolCO2.to.gC(na.approx(maize[,zm.20])))/48)
#maize.19.interp<-cumsum(umolCO2.to.gC(maize.2019$Fc))/48
maize.18.interp<-unlist(cumsum(umolCO2.to.gC(na.approx(maize.2018[,zm.18])))/48)
maize.17.interp<-unlist(cumsum(umolCO2.to.gC(na.approx(maize.2017[,zm.17])))/48)


plot(maize.20.interp, type='l',ylab=ylab.cum, xaxt='n', xlab='', ylim=ylim.cum, lwd=2)
#lines(maize.19.interp, col='green', lwd=2)
lines(maize.18.interp, col='red', lwd=2)
lines(maize.17.interp, col='blue', lwd=2)
#####

##plot for miscanthus####
#match<-c(199:nrow(misc))\

plot(rollmean(umolCO2.to.gC(misc.2018[,mg.18]), k=48, xlab='',na.rm=TRUE, na.pad=TRUE)~format(misc.2018$xlDateTime, "%j"), type='l',col='lightpink', lwd=0.5, ylab=ylab.flux,ylim=ylim.flux,main="Miscanthus",xlab='day of year')
lines(rollmean(umolCO2.to.gC(misc.2017[,mg.17]), k=48, na.rm=TRUE, na.pad=TRUE)~format(misc.2017$xlDateTime, "%j"), col="lightblue", lwd=0.5)
lines(rollmean(umolCO2.to.gC(misc[,mg.20]), k=48, na.rm=TRUE, na.pad=TRUE)~format(misc$xlDateTime, "%j"),col='gray', lwd=0.5)
abline(h=0, col='gray')
lines(rollmean(umolCO2.to.gC(misc.2018[,mg.18]), k=48*7, fill=NA, na.rm=TRUE)~format(misc.2018$xlDateTime, "%j"), lwd=3, col="red")
lines(rollmean(umolCO2.to.gC(misc.2017[,mg.17]), k=48*7, fill=NA, na.rm=TRUE)~format(misc.2017$xlDateTime, "%j"), lwd=3, col="blue")
lines(rollmean(umolCO2.to.gC(misc[mg.20]), k=48*7, na.rm=TRUE, fill=NA)~format(misc$xlDateTime, "%j"), lwd=3, col='black')

legend(1,ylim.flux[2], legend=c(2020, 2018, 2017),bty='n',cex=0.8, col=c('black', 'red','blue'), lwd=2, y.intersp = 0.8)

misc.20.interp<-unlist(cumsum(na.approx(umolCO2.to.gC(misc[,mg.20])))/48)
misc.18.interp<-unlist(cumsum(na.approx(umolCO2.to.gC(misc.2018[,mg.18])))/48)

#start from avg of 18, 20 
#find the point
sample<-which(format(maize$xlDateTime, "%j")==min(format(misc.2017$xlDateTime, "%j")))[29] #29 is roughly 14:00 of that day
startpoint<-mean(c(misc.18.interp[sample], misc.20.interp[sample]))
#pad with NAs
misc.17.interp<-startpoint+unlist(cumsum(na.approx(umolCO2.to.gC(misc.2017[mg.17])))/48)
misc.17.interp<-c(rep (NA, (sample-2)), misc.17.interp)


plot(misc.20.interp, type='l',ylab=ylab.cum, xaxt='n', xlab='', ylim=ylim.cum, lwd=2)
lines(misc.18.interp, col='red', lwd=2)
lines(misc.17.interp, col='blue', lwd=2)
#####


##plot for Sorghum####
if(!exists('sorg.raw.2018')){sorg.raw.2018<-read_excel("Sorghum_2018_to_2019_L6.xlsx", sheet=2,skip=2)}

sorg.2018.yearmark<-format(sorg.raw.2018$xlDateTime, '%Y')
sorg.2018<-sorg.raw.2018[which(sorg.2018.yearmark=="2018"),]
sorg.2018[sorg.2018==-9999]<-NA
#sorg.2018.months<-sorg.2018[1:6018,]
#sorg.2018.months$Fc[which(is.na(sorg$Fc))]<-NA

na.pad<-matrix(data=NA, nrow=(17520-nrow(sorg.2018)), ncol=ncol(sorg.2018));colnames(na.pad)<-colnames(sorg.2018)
sorg.2018<-rbind(na.pad, sorg.2018)
#sorg.2018$xlDateTime[which(is.na(sorg.2018$xlDateTime))]<-sorg$xlDateTime[which(is.na(sorg.2018$xlDateTime))]
sorg.2018$xlDateTime<-maize.2018$xlDateTime

plot(rollmean(umolCO2.to.gC(sorg.2018[,sb.18]), k=48, na.pad=TRUE, na.rm=TRUE)~format(sorg.2018$xlDateTime, "%j"), type='l', col='lightpink', main="Sorghum", lwd=0.5, ylab=ylab.flux, xlab="day of year", ylim=ylim.flux)
lines(rollmean(umolCO2.to.gC(sorg[,sb.20]), k=48, na.pad=TRUE, na.rm=TRUE)~format(sorg$xlDateTime, "%j"),col='gray', lwd=0.5)

sorgroll18<-rollapply(umolCO2.to.gC(sorg.2018[,sb.18]), width=48*7, fill=NA, FUN='mean',na.rm=TRUE, partial=FALSE)
sorgroll20<-rollmean(umolCO2.to.gC(sorg[,sb.20]), k=48*7, na.rm=TRUE, fill=NA)

lines(sorgroll18~format(sorg.2018$xlDateTime, "%j"), lwd=3, col="red")
lines(sorgroll20~format(sorg$xlDateTime, "%j"), lwd=3, col='black')

text(80,-15, "(No sorghum data Jan 2018 - Jun 2018)", cex=0.8)


legend(1,ylim.flux[2], legend=c(2020, 2018),bty='n',cex=0.8, col=c('black', 'red'), lwd=2, y.intersp = 0.8)

#opt. 1, start from mid-june
# sorg.pairy.20<-unlist(umolCO2.to.gC(sorg[,sb.20])/48);sorg.pairy.20[is.na(sorg.2018[,sb.18])]<-NA
# sorg.pairy.18<-umolCO2.to.gC(sorg.2018[,sb.18])/48;sorg.pairy.18[is.na(sorg.pairy.20)]<-NA
# 
# plot(cumsum(sorg.pairy.20[!is.na(sorg.pairy.20)])~sorg$xlDateTime[!is.na(sorg.pairy.20)], type='l', ylim=ylim.cum,ylab="Cumulative Carbon flux (gC m-2)",xlab='', lwd=2)
# lines(cumsum(sorg.pairy.18[!is.na(sorg.pairy.18)])~sorg$xlDateTime[!is.na(sorg.pairy.18)], col='red', lwd=2)

#opt. 2, start from jan, with both sharing early 2020 fluxes
sorg.add.20<-na.approx(umolCO2.to.gC(sorg[,sb.20]))/48
sorg.add.18<-na.approx(umolCO2.to.gC(c(
  as.numeric(unlist(sorg[which(is.na(sorg.2018[,sb.18])), sb.20])), #2020 sorghum where there is no data for 2018 
  sorg.2018[!is.na(sorg.2018[sb.18]), sb.18])))/48                   #2018 sorghum where we have data for it

sorg.add.18.plot<-cumsum(sorg.add.18); sorg.add.18.plot[is.na(sorg.2018[sb.18])]<-NA

plot(unlist(cumsum(sorg.add.20))~format(sorg$xlDateTime, "%j"), type='l', ylim=ylim.cum, ylab=ylab.cum, xlab='', lwd=2)
lines(sorg.add.18.plot~format(sorg.2018$xlDateTime, "%j"), col='red', lwd=2)



#####

}

compare_fluxes("Fc", "Fco2")
compare_fluxes("GPP_LT")
compare_fluxes("ER_LT")


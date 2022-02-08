
library(readxl)

maize.raw<-read_excel("MaizeNoBasalt_2020_L6.xls", sheet=2,skip=2)
maize.raw[maize.raw==-9999]<-NA
maize<-maize.raw#[1:(nrow(maize.raw)-1),]

sorg.raw<-read_excel("Sorghum_2020_L6.xls", sheet=2,skip=2)
sorg.raw[sorg.raw==-9999]<-NA
sorg<-sorg.raw

misc.raw<-read_excel("MiscanthusNoBasalt_2020_L6.xls", sheet=2,skip=2)
misc<-misc.raw#[1030:6715,]
misc[misc==-9999]<-NA


doylist<-format(maize$xlDateTime, "%j")
hours<-format(sorg$xlDateTime, format="%H")

plot(maize$Fco2~maize$xlDateTime,  type='l', col='orange')
lines(sorg$Fco2~sorg$xlDateTime,  type='l', col='forest green')
lines(misc$Fco2~misc$xlDateTime,  type='l', col='light blue')

library(esquisse)

library(ggplot2)
#NEE####

ggplot(maize) +
 aes(x = xlDateTime, y = Fco2) +
 geom_line(size = 0.5L, colour = "light gray") +
 labs(x = "Timestamp", y = "Fco2 (Carbon Flux)", title = "Maize Control ") +
 stat_smooth(method="loess",span=0.03, size=1.5L,se=FALSE,colour="orange" ) +
 theme_minimal()

ggplot(sorg) +
  aes(x = xlDateTime, y = Fco2) +
  geom_line(size = 0.5L, colour = "light gray") +
  labs(x = "Timestamp", y = "Fc (Carbon Flux)", title = "Sorghum") +
  stat_smooth(method="loess",span=0.03, size=1.5L,se=FALSE,colour="forest green" ) +
  theme_minimal()

ggplot(misc) +
  aes(x = xlDateTime, y = Fco2) +
  geom_line(size = 0.5L, colour = "light gray") +
  labs(x = "Timestamp", y = "Fc (Carbon Flux)", title = "Miscanthus") +
  stat_smooth(method="loess",span=0.03, size=1.5L,se=FALSE,colour="light blue" ) +
  theme_minimal()


#GPP####
ggplot(maize) +
  aes(x = xlDateTime, y = GPP_LT) +
  geom_line(size = 0.5L, colour = "light gray") +
  labs(x = "Timestamp", y = "GPP", title = "Maize Control ") +
  stat_smooth(method="loess",span=0.03, size=1.5L,se=FALSE,colour="orange" ) +
  theme_minimal()

ggplot(sorg) +
  aes(x = xlDateTime, y = GPP_LT) +
  geom_line(size = 0.5L, colour = "light gray") +
  labs(x = "Timestamp", y = "GPP", title = "Sorghum") +
  stat_smooth(method="loess",span=0.03, size=1.5L,se=FALSE,colour="forest green" ) +
  theme_minimal()

ggplot(misc) +
  aes(x = xlDateTime, y = GPP_LT) +
  geom_line(size = 0.5L, colour = "light gray") +
  labs(x = "Timestamp", y = "Fc (Carbon Flux)", title = "Miscanthus") +
  stat_smooth(method="loess",span=0.03, size=1.5L,se=FALSE,colour="light blue" ) +
  theme_minimal()

#Respiration####
#Pull off wild negative resprs
maize$ER_LT[maize$ER_LT<(-10)]<-NA; sorg$ER_LT[sorg$ER_LT<(-10)]<-NA;misc$ER_LT[misc$ER_LT<(-10)]<-NA

ggplot(maize) +
  aes(x = xlDateTime, y = ER_LT) +
  geom_line(size = 0.5L, colour = "light gray") +
  labs(x = "Timestamp", y = "ER", title = "Maize Control ") +
  stat_smooth(method="loess",span=0.03, size=1.5L,se=FALSE,colour="orange" ) +
  theme_minimal()

ggplot(sorg) +
  aes(x = xlDateTime, y = ER_LT) +
  geom_line(size = 0.5L, colour = "light gray") +
  labs(x = "Timestamp", y = "ER", title = "Sorghum") +
  stat_smooth(method="loess",span=0.03, size=1.5L,se=FALSE,colour="forest green" ) +
  theme_minimal()

ggplot(misc) +
  aes(x = xlDateTime, y = ER_LT) +
  geom_line(size = 0.5L, colour = "light gray") +
  labs(x = "Timestamp", y = "Fc (Carbon Flux)", title = "Miscanthus") +
  stat_smooth(method="loess",span=0.03, size=1.5L,se=FALSE,colour="light blue" ) +
  theme_minimal()
#####
library(zoo)
par(mfrow=c(2,1), mar=c(3,4,1,4))

library(bigleaf)
maize.pair.u<-umolCO2.to.gC(maize$Fco2)#Choose a variable here
sorg.pair.u<-umolCO2.to.gC(sorg$Fco2)
misc.pair.u<-umolCO2.to.gC(misc$Fco2)

maize.pair.u<-umolCO2.to.gC(maize$GPP_LT)#Choose a variable here
sorg.pair.u<-umolCO2.to.gC(sorg$GPP_LT)
misc.pair.u<-umolCO2.to.gC(misc$GPP_LT)

maize.pair.u<-umolCO2.to.gC(maize$ER_LT)#Choose a variable here
sorg.pair.u<-umolCO2.to.gC(sorg$ER_LT)
misc.pair.u<-umolCO2.to.gC(misc$ER_LT)
#Unis...

maize.roll<-rollapply(maize.pair.u, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
misc.roll<-rollapply(misc.pair.u, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
sorg.roll<-rollapply(sorg.pair.u, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)


plot(rollmean(maize.pair.u, k=48, na.pad=TRUE, na.rm=TRUE)~maize$xlDateTime, col='peachpuff',type='l',  lwd=0.5, ylab="NEE (gC m-2 d-1)", xlab="Day of Year", ylim=c(-20, 30))# ylab="Carbon Flux (gC m-2 d-1)"
lines(rollmean(sorg.pair.u, na.rm=TRUE, k=48, na.pad=TRUE)~maize$xlDateTime, col='darkseagreen1', lwd=0.5)
lines(rollmean(misc.pair.u, na.rm=TRUE, k=48, na.pad=TRUE)~maize$xlDateTime, col='aliceblue', lwd=0.5)
abline(h=0, col='gray')
lines(maize.roll~maize$xlDateTime, lwd=3, col="orange")
lines(sorg.roll~maize$xlDateTime, lwd=3, col='forest green')
lines(misc.roll~maize$xlDateTime, lwd=3, col='light blue')


legend(1577836800, 30,legend=c("maize", "sorghum", "miscanthus"), col=c('orange','forest green', 'light blue'), lwd=2, bty='n')

par(mfrow=c(1,1))
#C-C-C-combo plot!
maize.gpp<-(umolCO2.to.gC(maize$GPP_LT))*-1
sorg.gpp<-(umolCO2.to.gC(sorg$GPP_LT))*-1
misc.gpp<-(umolCO2.to.gC(misc$GPP_LT))*-1
maize.roll.gpp<-rollapply(maize.gpp, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
sorg.roll.gpp<-rollapply(sorg.gpp, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
misc.roll.gpp<-rollapply(misc.gpp, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)


maize.er<-umolCO2.to.gC(maize$ER_LT)
sorg.er<-umolCO2.to.gC(sorg$ER_LT)
misc.er<-umolCO2.to.gC(misc$ER_LT)
maize.roll.er<-rollapply(maize.er, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
sorg.roll.er<-rollapply(sorg.er, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
misc.roll.er<-rollapply(misc.er, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)


maize.nee<-umolCO2.to.gC(maize$Fco2)
sorg.nee<-umolCO2.to.gC(sorg$Fco2)
misc.nee<-umolCO2.to.gC(misc$Fco2)

plot(rollmean(maize.pair.u, k=48, na.pad=TRUE, na.rm=TRUE)~maize$xlDateTime, col='peachpuff',type='l',  lwd=0.5, ylab="Carbon Flux (gC m-2 d-1)", xlab="Day of Year", ylim=c(-30, 27))
#nee
lines(rollmean(sorg.pair.u, na.rm=TRUE, k=48, na.pad=TRUE)~maize$xlDateTime, col='darkseagreen1', lwd=0.5)
lines(rollmean(misc.pair.u, na.rm=TRUE, k=48, na.pad=TRUE)~maize$xlDateTime, col='aliceblue', lwd=0.5)

#gpp
lines(rollmean(sorg.gpp, na.rm=TRUE, k=48, na.pad=TRUE)~maize$xlDateTime, col='darkseagreen1', lwd=0.5, lty=2)
lines(rollmean(maize.gpp, na.rm=TRUE, k=48, na.pad=TRUE)~maize$xlDateTime, col='peachpuff', lwd=0.5, lty=2)
lines(rollmean(misc.gpp, na.rm=TRUE, k=48, na.pad=TRUE)~maize$xlDateTime, col='aliceblue', lwd=0.5, lty=2)

#er
lines(rollmean(sorg.er, na.rm=TRUE, k=48, na.pad=TRUE)~maize$xlDateTime, col='darkseagreen1', lwd=0.5, lty=3)
lines(rollmean(maize.er, na.rm=TRUE, k=48, na.pad=TRUE)~maize$xlDateTime, col='peachpuff', lwd=0.5, lty=3)
lines(rollmean(misc.er, na.rm=TRUE, k=48, na.pad=TRUE)~maize$xlDateTime, col='aliceblue', lwd=0.5, lty=3)


abline(h=0, col='gray')
#lines(maize.roll~maize$xlDateTime, lwd=3, col="orange")
#lines(sorg.roll~maize$xlDateTime, lwd=3, col='forest green')
#lines(misc.roll~maize$xlDateTime, lwd=3, col='light blue')

#gpp
lines(maize.roll.gpp~maize$xlDateTime, lwd=3, col="orange", lty=2)
lines(sorg.roll.gpp~maize$xlDateTime, lwd=3, col='forest green', lty=2)
lines(misc.roll.gpp~maize$xlDateTime, lwd=3, col='lightblue', lty=2)

#re
lines(maize.roll.er~maize$xlDateTime, lwd=3, col="orange", lty=3)
lines(sorg.roll.er~maize$xlDateTime, lwd=3, col='forest green', lty=3)
lines(misc.roll.er~maize$xlDateTime, lwd=3, col='light blue', lty=3)


legend(230, 23,legend=c("maize", "sorghum", "miscanthus"), col=c('orange','forest green', 'light blue'), lwd=2, cex=0.7)

sorg.interp<-cumsum(sorg.pair.u)/48
maize.interp<-cumsum(maize.pair.u)/48
misc.interp<-cumsum(misc.pair.u)/48

ts.force<-format(maize$xlDateTime,  "%j")
plot(maize.interp~ts.force, type='l', col='orange', lwd=3, ylab="Cumulative C Flux (gC m-2)", xlab="Day of Year", ylim=c(-45000/48, 5000/48))
lines(sorg.interp~ts.force, type='l', col='forest green', lwd=3)
lines(misc.interp~ts.force, type='l', col='light blue', lwd=3)

legend(100, -24000/48, legend=c('maize', 'sorghum', 'miscanthus'),  lwd=2, col=c('orange', 'forest green', 'light blue'), cex=0.8)

par(mfrow=c(1,1))
######

#Pull in other years
if(!exists('maize.raw.2018')){maize.raw.2018<-read_excel("MaizeNoBasalt_2017_to_2019_L6.xlsx", sheet=2,skip=2)}
if(!exists('misc.raw.2018')){misc.raw.2018<-read_excel("MiscanthusNoBasalt_2017_to_2019_L6.xlsx", sheet=2,skip=2)}

maize.years<-format(maize.raw.2018$xlDateTime, "%Y")
misc.years<-format(misc.raw.2018$xlDateTime, "%Y")

#Prepare maize data
maize.2018<-maize.raw.2018[maize.years=="2018",]
maize.2018[maize.2018==-9999]<-NA
#maize.2018.months<-maize.2018[1:14640,]

maize.2017<-maize.raw.2018[maize.years=="2017",]
maize.2017[maize.2017==-9999]<-NA
#maize.2017.months<-maize.2017[1:14640,]

maize.2019<-maize.raw.2018[maize.years=="2019",]
maize.2019[maize.2019==-9999]<-NA
#maize.2019.months<-maize.2019[1:14640,]


#Prepare miscanthus data

misc.2018<-misc.raw.2018[misc.years=="2018",]
misc.2018[misc.2018==-9999]<-NA
#misc.2018.months<-misc.2018[1:14640,]

misc.2017<-misc.raw.2018[misc.years=="2017",]
misc.2017[misc.2017==-9999]<-NA
misc.2017<-
#misc.2017.months<-misc.2017[1:14640,]

misc.2019<-misc.raw.2018[misc.years=="2019",]
misc.2019[misc.2019==-9999]<-NA
#misc.2019.months<-misc.2019[1:14640,]



par(mfrow=c(2,1), mar=c(2,4,3,1))

#Plot for maize
plot(rollmean(umolCO2.to.gC(maize.2018$Fc), k=48, na.rm=TRUE, na.pad=TRUE)~format(maize.2018$xlDateTime, "%j"), type='l', col='lightpink', lwd=0.5, ylab="Carbon flux (gC d-1)", xlab="Date",  main="Maize",ylim=c(-20,15))
lines(rollmean(umolCO2.to.gC(maize.2017$Fc), k=48, na.rm=TRUE, na.pad=TRUE)~format(maize.2017$xlDateTime,"%j"), col="lightblue", lwd=0.5)
lines(rollmean(umolCO2.to.gC(maize$Fco2), k=48, na.rm=TRUE, na.pad=TRUE)~format(maize$xlDateTime, "%j"),col='gray', lwd=0.5)
#lines(rollmean(umolCO2.to.gC(maize.2019$Fc), k=48, na.rm=TRUE, na.pad=TRUE)~format(maize.2019$xlDateTime,"%j"),col='palegreen', lwd=0.5)

abline(h=0, col='gray')
lines(rollmean(umolCO2.to.gC(maize.2018$Fc), k=48*7, fill=NA, na.rm=TRUE)~format(maize.2018$xlDateTime, "%j"), lwd=3, col="red")
lines(rollmean(umolCO2.to.gC(maize.2017$Fc), k=48*7, fill=NA, na.rm=TRUE)~format(maize.2017$xlDateTime, "%j"), lwd=3, col="blue")
lines(rollmean(umolCO2.to.gC(maize$Fco2), k=48*7, na.rm=TRUE, fill=NA)~format(maize$xlDateTime, "%j"), lwd=3, col='black')
#lines(rollmean(umolCO2.to.gC(maize.2019$Fc), k=48*7, na.rm=TRUE, fill=NA)~format(maize.2019$xlDateTime, "%j"), lwd=3, col='green')

legend(1,15, legend=c(2020, 2019, 2018, 2017),bty='n',cex=0.8, col=c('black', 'green','red','blue'), lwd=2)

maize.20.interp<-cumsum(umolCO2.to.gC(maize$Fco2))/48
maize.19.interp<-cumsum(umolCO2.to.gC(maize.2019$Fc))/48
maize.18.interp<-cumsum(umolCO2.to.gC(maize.2018$Fc))/48
maize.17.interp<-cumsum(umolCO2.to.gC(maize.2017$Fc))/48


plot(maize.20.interp, type='l',ylab="Cumulative Carbon flux (gC m-2)", xaxt='n', xlab='', ylim=c(-40000/48,5500/48), lwd=2)
#lines(maize.19.interp, col='green', lwd=2)
lines(maize.18.interp, col='red', lwd=2)
lines(maize.17.interp, col='blue', lwd=2)


#plot for miscanthus
#match<-c(199:nrow(misc))
plot(rollmean(umolCO2.to.gC(misc.2018$Fc), k=48, xlab='',na.rm=TRUE, na.pad=TRUE)~format(misc.2018$xlDateTime, "%j"), type='l',col='lightpink', lwd=0.5, ylab="Carbon flux (gC d-1)",ylim=c(-20, 15),main="Miscanthus",xlab='')
lines(rollmean(umolCO2.to.gC(misc.2017$Fc), k=48, na.rm=TRUE, na.pad=TRUE)~format(misc.2017$xlDateTime, "%j"), col="lightblue", lwd=0.5)
lines(rollmean(umolCO2.to.gC(misc$Fco2), k=48, na.rm=TRUE, na.pad=TRUE)~format(misc$xlDateTime, "%j"),col='gray', lwd=0.5)
abline(h=0, col='gray')
lines(rollmean(umolCO2.to.gC(misc.2018$Fc), k=48*7, fill=NA, na.rm=TRUE)~format(misc.2018$xlDateTime, "%j"), lwd=3, col="red")
lines(rollmean(umolCO2.to.gC(misc.2017$Fc), k=48*7, fill=NA, na.rm=TRUE)~format(misc.2017$xlDateTime, "%j"), lwd=3, col="blue")
lines(rollmean(umolCO2.to.gC(misc$Fco2), k=48*7, na.rm=TRUE, fill=NA)~format(misc$xlDateTime, "%j"), lwd=3, col='black')

legend(1,15, legend=c(2020, 2018, 2017),bty='n',cex=0.8, col=c('black', 'red','blue'), lwd=2)

misc.20.interp<-cumsum(na.approx(umolCO2.to.gC(misc$Fco2)))/48
misc.18.interp<-cumsum(na.approx(umolCO2.to.gC(misc.2018$Fc)))/48

#start from avg of 18, 20 
#find the point
sample<-which(format(maize$xlDateTime, "%j")==min(format(misc.2017$xlDateTime, "%j")))[29] #29 is roughly 14:00 of that day
startpoint<-mean(c(misc.18.interp[sample], misc.20.interp[sample]))
#pad with NAs
misc.17.interp<-startpoint+cumsum(na.approx(umolCO2.to.gC(misc.2017$Fc)))/48
misc.17.interp<-c(rep (NA, (sample-2)), misc.17.interp)


plot(misc.20.interp, type='l',ylab="Cumulative Carbon flux (gC m-2)", xaxt='n', xlab='', ylim=c(-50000/48,4000/48), lwd=2)
lines(misc.18.interp, col='red', lwd=2)
lines(misc.17.interp, col='blue', lwd=2)



##Sorghum
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


plot(rollmean(umolCO2.to.gC(sorg.2018$Fc), k=48, na.pad=TRUE, na.rm=TRUE)~format(sorg.2018$xlDateTime, "%j"), type='l', col='lightpink', main="Sorghum", lwd=0.5, ylab="Carbon Flux (gC d-1)", xlab='date', ylim=c(-20, 15))
lines(rollmean(umolCO2.to.gC(sorg$Fco2), k=48, na.pad=TRUE, na.rm=TRUE)~format(sorg$xlDateTime, "%j"),col='gray', lwd=0.5)

sorgroll18<-rollapply(umolCO2.to.gC(sorg.2018$Fc), width=48*7, fill=NA, FUN='mean',na.rm=TRUE, partial=FALSE)
sorgroll20<-rollmean(umolCO2.to.gC(sorg$Fco2), k=48*7, na.rm=TRUE, fill=NA)

lines(sorgroll18~format(sorg.2018$xlDateTime, "%j"), lwd=3, col="red")
lines(sorgroll20~format(sorg$xlDateTime, "%j"), lwd=3, col='black')

#opt. 1, start from mid-june
sorg.pairy.20<-umolCO2.to.gC(sorg$Fco2)/48;sorg.pairy.20[is.na(sorg.2018$Fc)]<-NA
sorg.pairy.18<-umolCO2.to.gC(sorg.2018$Fc)/48;sorg.pairy.18[is.na(sorg.pairy.20)]<-NA

plot(cumsum(sorg.pairy.20[!is.na(sorg.pairy.20)])~sorg$xlDateTime[!is.na(sorg.pairy.20)], type='l', ylim=c(-50000/48,1000/48),ylab="Cumulative Carbon flux (gC m-2)",xlab='', lwd=2)
lines(cumsum(sorg.pairy.18[!is.na(sorg.pairy.18)])~sorg$xlDateTime[!is.na(sorg.pairy.18)], col='red', lwd=2)

#opt. 2, start from jan, with both sharing early 2020 fluxes
sorg.add.20<-umolCO2.to.gC(sorg$Fco2)/48#sorg.pairy.20[is.na(sorg.2018.months$Fc)]<-NA
sorg.add.18<-umolCO2.to.gC(c(sorg$Fco2[is.na(sorg.2018$Fc)], sorg.2018$Fc[!is.na(sorg.2018$Fc)]))/48#sorg.pairy.18[is.na(sorg.pairy.20)]<-NA

plot(cumsum(sorg.add.20)~sorg$xlDateTime, type='l', ylim=c(-50000/48,4500/48),ylab="Cumulative Carbon flux (gC m-2)",xlab='', lwd=2)
lines(cumsum(sorg.add.18)~sorg$xlDateTime, col='red', lwd=2)
#####


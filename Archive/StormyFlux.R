
library(readxl)

maize.raw<-read_excel("MaizeCon_2020_L6.xls", sheet=2,skip=2)
maize.raw[maize.raw==-9999]<-NA
maize<-maize.raw[1:(nrow(maize.raw)-1),]

sorg.raw<-read_excel("Sorghum_2020_L6.xls", sheet=2,skip=2)
sorg.raw[sorg.raw==-9999]<-NA
sorg<-sorg.raw

misc.raw<-read_excel("MiscanthusNoBasalt_2020_L3.xls", sheet=2,skip=2)
misc<-misc.raw[1030:6715,]
misc[misc==-9999]<-NA


doylist<-format(maize$xlDateTime, "%j")
hours<-format(sorg$xlDateTime, format="%H")

plot(maize$Fco2~maize$xlDateTime,  type='l', col='orange')
lines(sorg$Fco2~sorg$xlDateTime,  type='l', col='forest green')
lines(misc$Fc~misc$xlDateTime,  type='l', col='light blue')

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
  aes(x = xlDateTime, y = Fc) +
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

# ggplot(misc) +
#   aes(x = xlDateTime, y = Fc) +
#   geom_line(size = 0.5L, colour = "light gray") +
#   labs(x = "Timestamp", y = "Fc (Carbon Flux)", title = "Miscanthus") +
#   stat_smooth(method="loess",span=0.03, size=1.5L,se=FALSE,colour="light blue" ) +
#   theme_minimal()

#Respiration####
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

# ggplot(misc) +
#   aes(x = xlDateTime, y = Fc) +
#   geom_line(size = 0.5L, colour = "light gray") +
#   labs(x = "Timestamp", y = "Fc (Carbon Flux)", title = "Miscanthus") +
#   stat_smooth(method="loess",span=0.03, size=1.5L,se=FALSE,colour="light blue" ) +
#   theme_minimal()
#####
library(zoo)
par(mfrow=c(2,1), mar=c(3,4,1,4))

library(bigleaf)
maize.pair.u<-umolCO2.to.gC(maize$Fco2)#Choose a variable here
sorg.pair.u<-umolCO2.to.gC(sorg$Fco2)
misc.pair.u<-umolCO2.to.gC(misc.pair.r)
#Unis...

maize.roll<-rollapply(maize.pair.u, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
misc.roll<-rollapply(misc.pair.u, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
sorg.roll<-rollapply(sorg.pair.u, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)


plot(rollmean(maize.pair.u, k=48, na.pad=TRUE, na.rm=TRUE)~maize$xlDateTime, col='peachpuff',type='l',  lwd=0.5, ylab="Carbon Flux (gC m-2 d-1)", xlab="Day of Year", ylim=c(-20, 60))
lines(rollmean(sorg.pair.u, na.rm=TRUE, k=48, na.pad=TRUE)~maize$xlDateTime, col='darkseagreen1', lwd=0.5)
lines(rollmean(misc.pair.u, na.rm=TRUE, k=48, na.pad=TRUE)~maize$xlDateTime, col='aliceblue', lwd=0.5)
abline(h=0, col='gray')
lines(maize.roll~maize$xlDateTime, lwd=3, col="orange")
lines(sorg.roll~maize$xlDateTime, lwd=3, col='forest green')
lines(misc.roll~maize$xlDateTime, lwd=3, col='light blue')
#used 650x360 for export

par(new=TRUE)
plot(maize.pair.u~format(maize$xlDateTime, "%j"), col='', axes="FALSE", ylab='', xlab='')
abline(v=(195-1)) #Hailstorm
abline(v=156)
abline(v=(181), lty=2) #dry spell start
abline(v=(190), lty=2) #dry spell end
abline(v=223)
legend(230, 23,legend=c("maize", "sorghum", "miscanthus"), col=c('orange','forest green', 'light blue'), lwd=2, cex=0.7)
text(149,60,"Flood >")
text(185,18, "Heatwave")
text(205,60, "< Hailstorm")
text(233, -20, "< Derecho")


#C-C-C-combo plot!
maize.gpp<-(umolCO2.to.gC(maize$GPP_LT))*-1
sorg.gpp<-(umolCO2.to.gC(sorg$GPP_LT))*-1
maize.roll.gpp<-rollapply(maize.gpp, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
sorg.roll.gpp<-rollapply(sorg.gpp, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)

maize.er<-umolCO2.to.gC(maize$ER_LT)
sorg.er<-umolCO2.to.gC(sorg$ER_LT)
maize.roll.er<-rollapply(maize.er, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
sorg.roll.er<-rollapply(sorg.er, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)

maize.nee<-umolCO2.to.gC(maize$Fco2)
sorg.nee<-umolCO2.to.gC(sorg$Fco2)

plot(rollmean(maize.pair.u, k=48, na.pad=TRUE, na.rm=TRUE)~maize$xlDateTime, col='peachpuff',type='l',  lwd=0.5, ylab="Carbon Flux (gC m-2 d-1)", xlab="Day of Year", ylim=c(-30, 27))
#nee
lines(rollmean(sorg.pair.u, na.rm=TRUE, k=48, na.pad=TRUE)~maize$xlDateTime, col='darkseagreen1', lwd=0.5)
#gpp
lines(rollmean(sorg.gpp, na.rm=TRUE, k=48, na.pad=TRUE)~maize$xlDateTime, col='darkseagreen1', lwd=0.5, lty=2)
lines(rollmean(maize.gpp, na.rm=TRUE, k=48, na.pad=TRUE)~maize$xlDateTime, col='peachpuff', lwd=0.5, lty=2)
#er
lines(rollmean(sorg.er, na.rm=TRUE, k=48, na.pad=TRUE)~maize$xlDateTime, col='darkseagreen1', lwd=0.5, lty=3)
lines(rollmean(maize.er, na.rm=TRUE, k=48, na.pad=TRUE)~maize$xlDateTime, col='peachpuff', lwd=0.5, lty=3)

abline(h=0, col='gray')
#lines(maize.roll~maize$xlDateTime, lwd=3, col="orange")
#lines(sorg.roll~maize$xlDateTime, lwd=3, col='forest green')
#gpp
lines(maize.roll.gpp~maize$xlDateTime, lwd=3, col="orange", lty=2)
lines(sorg.roll.gpp~maize$xlDateTime, lwd=3, col='forest green', lty=2)
#re
lines(maize.roll.er~maize$xlDateTime, lwd=3, col="orange", lty=3)
lines(sorg.roll.er~maize$xlDateTime, lwd=3, col='forest green', lty=3)

par(new=TRUE)
plot(maize.pair.u~format(maize$xlDateTime, "%j"), col='', axes="FALSE", ylab='', xlab='')
abline(v=(195-1)) #Hailstorm
abline(v=156)
abline(v=(181), lty=2) #dry spell start
abline(v=(190), lty=2) #dry spell end
abline(v=223)
legend(230, 23,legend=c("maize", "sorghum", "miscanthus"), col=c('orange','forest green', 'light blue'), lwd=2, cex=0.7)
text(149,25,"Flood >")
text(185,18, "Heatwave")
text(205,25, "< Hailstorm")
text(233, -20, "< Derecho")

sorg.interp<-cumsum(sorg.pair.u)/48
maize.interp<-cumsum(maize.pair.u)/48
misc.interp<-cumsum(na.approx(misc.pair.u)[notmissed])/48

ts.force<-format(sorg$xlDateTime,  "%j")
plot(maize.interp~ts.force, type='l', col='orange', lwd=3, ylab="Cumulative C Flux (gC m-2)", xlab="Day of Year", ylim=c(-42000/48, 5000/48))
lines(sorg.interp~ts.force, type='l', col='forest green', lwd=3)
lines(misc.interp~ts.force, type='l', col='light blue', lwd=3)

#abline(v=(195-1)) #Hailstorm
#abline(v=156)
#abline(v=(181), lty=2) #dry spell start
#abline(v=(190), lty=2) #dry spell end
legend(140, -24000/48, legend=c('maize', 'sorghum', 'miscanthus'),  lwd=2, col=c('orange', 'forest green', 'light blue'), cex=0.8)
polygon(y=c(-25000/48, 0, 0, -25000/48), x=c(210,210,220,220), col='white', border='NA')

par(mfrow=c(1,1))

# #up close following disasters####
# par(mfrow=c(1,1))
# doylist<-format(maize$xlDateTime, "%j")
# eventdate<-195
# stormtime<-which(doylist%in%c(eventdate:(eventdate+14)))
# prestorm<-which(doylist%in%c((eventdate-14):eventdate))
# 
# plot(sorg$Fc[stormtime]~sorg$xlDateTime[stormtime], type='l', col='green')
# plot(maize$Fc[stormtime]~maize$xlDateTime[stormtime], col='orange', type='l')
# 
# hours<-format(sorg$xlDateTime, format="%H")
# 
# plot(aggregate(maize.pair[stormtime], by=list(hours[stormtime]), FUN='mean', na.rm=TRUE), type='l')
# lines(aggregate(maize.pair[prestorm], by=list(hours[prestorm]), FUN='mean', na.rm=TRUE), col='blue')
# 
# #plot(aggregate(maize.pair[stormtime], by=list(hours[stormtime]),FUN='mean', na.rm=TRUE)$x-aggregate(maize.pair[prestorm], by=list(hours[prestorm]), FUN='mean', na.rm=TRUE)$x)
# #abline(h=0)
# 
# plot(aggregate(sorg.pair[stormtime], by=list(hours[stormtime]), FUN='mean', na.rm=TRUE),  type='l')
# lines(aggregate(sorg.pair[prestorm], by=list(hours[prestorm]), FUN='mean', na.rm=TRUE), col='blue')
# 
# #plot(aggregate(sorg.pair[stormtime], by=list(hours[stormtime]),FUN='mean', na.rm=TRUE)$x-aggregate(sorg.pair[prestorm], by=list(hours[prestorm]), FUN='mean', na.rm=TRUE)$x)
# #abline(h=0)
# 
# weeks<-format(sorg$xlDateTime, '%W')
# weeklyprod.s<-aggregate(sorg.pair, by=list(weeks), FUN='mean', na.rm=TRUE)
# weeklyprod.m<-aggregate(maize.pair, by=list(weeks), FUN='mean', na.rm=TRUE)
# 
# 
# plot(weeklyprod.s, type='l', col='forest green')
# points(weeklyprod.m,col='orange', type='l')
# abline(v=c(22, 28))
#####

#Pull in other years
##Maize
if(!exists('maize.raw.2018')){maize.raw.2018<-read_excel("MaizeNoBasalt_2017_to_2019_L6.xlsx", sheet=2,skip=2)}
if(!exists('misc.raw.2018')){misc.raw.2018<-read_excel("MiscanthusNoBasalt_2017_to_2019_L6.xlsx", sheet=2,skip=2)}

#Prepare maize data
maize.2018<-maize.raw.2018[17521:35040,]
maize.2018[maize.2018==-9999]<-NA
maize.2018.months<-maize.2018[1:14640,]

maize.2017<-maize.raw.2018[1:17520,]
maize.2017[maize.2017==-9999]<-NA
maize.2017.months<-maize.2017[1:14640,]

maize.2019<-maize.raw.2018[35041:nrow(maize.raw.2018),]
maize.2019[maize.2019==-9999]<-NA
maize.2019.months<-maize.2019[1:14640,]


#Prepare miscanthus data

misc.2018<-misc.raw.2018[10534:28053,]
misc.2018[misc.2018==-9999]<-NA
misc.2018.months<-misc.2018[6790:12475,]
misc.2018.months.cl<-misc.2018.months[199:nrow(misc.2018.months),]
misc.2018.months$Fc[which(is.na(misc$Fc[match]))]<-NA

misc.2017<-misc.raw.2018[1:10533,]
misc.2017[misc.2017==-9999]<-NA
misc.2017.months<-misc.2017[1:5488,]
misc.2017.months$Fc[which(is.na(misc$Fc[match]))]<-NA

par(mfrow=c(2,1), mar=c(2,4,3,1))

#Plot for maize
plot(rollmean(umolCO2.to.gC(maize.2018.months$Fc), k=48, na.rm=TRUE, na.pad=TRUE)~maize$xlDateTime, type='l', col='lightpink', lwd=0.5, ylab="Carbon flux (gC d-1)", xlab="Date",  main="Maize",ylim=c(-20,15))
lines(rollmean(umolCO2.to.gC(maize.2017.months$Fc), k=48, na.rm=TRUE, na.pad=TRUE)~maize$xlDateTime, col="lightblue", lwd=0.5)
lines(rollmean(umolCO2.to.gC(maize$Fco2), k=48, na.rm=TRUE, na.pad=TRUE)~maize$xlDateTime,col='gray', lwd=0.5)
lines(rollmean(umolCO2.to.gC(maize.2019.months$Fc), k=48, na.rm=TRUE, na.pad=TRUE)~maize$xlDateTime,col='palegreen', lwd=0.5)

abline(h=0, col='gray')
lines(rollmean(umolCO2.to.gC(maize.2018.months$Fc), k=48*7, fill=NA, na.rm=TRUE)~maize$xlDateTime, lwd=3, col="red")
lines(rollmean(umolCO2.to.gC(maize.2017.months$Fc), k=48*7, fill=NA, na.rm=TRUE)~maize$xlDateTime, lwd=3, col="blue")
lines(rollmean(umolCO2.to.gC(maize$Fco2), k=48*7, na.rm=TRUE, fill=NA)~maize$xlDateTime, lwd=3, col='black')
lines(rollmean(umolCO2.to.gC(maize.2019.months$Fc), k=48*7, na.rm=TRUE, fill=NA)~maize$xlDateTime, lwd=3, col='green')

par(new=T)
plot(1:10, axes=FALSE, col='', ylab='', xlab='')
legend(1,5, legend=c(2020, 2018, 2017),bty='n',cex=0.8, col=c('black', 'red','blue'), lwd=2)

maize.20.interp<-cumsum(umolCO2.to.gC(maize$Fco2))/48
maize.19.interp<-cumsum(umolCO2.to.gC(maize.2019.months$Fc))/48
maize.18.interp<-cumsum(umolCO2.to.gC(maize.2018.months$Fc))/48
maize.17.interp<-cumsum(umolCO2.to.gC(maize.2017.months$Fc))/48

plot(maize.20.interp, type='l',ylab="Cumulative Carbon flux (gC m-2)", xaxt='n', xlab='', ylim=c(-40000/48,5500/48), lwd=2)
lines(maize.19.interp, col='green', lwd=2)
lines(maize.18.interp, col='red', lwd=2)
lines(maize.17.interp, col='blue', lwd=2)


#plot for miscanthus
match<-c(199:nrow(misc))
plot(rollmean(umolCO2.to.gC(misc.2018.months$Fc[match]), k=48, xlab='',na.rm=TRUE, na.pad=TRUE)~misc$xlDateTime[match], type='l',col='lightpink', lwd=0.5, ylab="Carbon flux (gC d-1)",ylim=c(-20, 15),main="Miscanthus",xlab='')
lines(rollmean(umolCO2.to.gC(misc.2017.months$Fc), k=48, na.rm=TRUE, na.pad=TRUE)~misc$xlDateTime[match], col="lightblue", lwd=0.5)
lines(rollmean(umolCO2.to.gC(misc$Fc[match]), k=48, na.rm=TRUE, na.pad=TRUE)~misc$xlDateTime[match],col='gray', lwd=0.5)
abline(h=0, col='gray')
lines(rollmean(umolCO2.to.gC(misc.2018.months$Fc[match]), k=48*7, fill=NA, na.rm=TRUE)~misc$xlDateTime[match], lwd=3, col="red")
lines(rollmean(umolCO2.to.gC(misc.2017.months$Fc), k=48*7, fill=NA, na.rm=TRUE)~misc$xlDateTime[match], lwd=3, col="blue")
lines(rollmean(umolCO2.to.gC(misc$Fc[match]), k=48*7, na.rm=TRUE, fill=NA)~misc$xlDateTime[match], lwd=3, col='black')

par(new=T)
plot(1:10, axes=FALSE, col='', ylab='', xlab='')
legend(1,5, legend=c(2020, 2018, 2017),bty='n',cex=0.8, col=c('black', 'red','blue'), lwd=2)

misc.20.interp<-cumsum(na.approx(umolCO2.to.gC(misc$Fc[match])))/48
misc.18.interp<-cumsum(na.approx(umolCO2.to.gC(misc.2018.months$Fc[match])))/48
misc.17.interp<-cumsum(na.approx(umolCO2.to.gC(misc.2017.months$Fc)))/48

plot(misc.20.interp, type='l',ylab="Cumulative Carbon flux (gC m-2)", xaxt='n', xlab='', ylim=c(-40000/48,4000/48), lwd=2)
lines(misc.18.interp, col='red', lwd=2)
lines(misc.17.interp, col='blue', lwd=2)



##Sorghum
if(!exists('sorg.raw.2018')){sorg.raw.2018<-read_excel("Sorghum_2018_to_2019_L6.xlsx", sheet=2,skip=2)}

sorg.2018.yearmark<-format(sorg.raw.2018$xlDateTime, '%Y')
sorg.2018<-sorg.raw.2018[which(sorg.2018.yearmark=="2018"),]
sorg.2018[sorg.2018==-9999]<-NA
sorg.2018.months<-sorg.2018[1:6018,]
#sorg.2018.months$Fc[which(is.na(sorg$Fc))]<-NA

na.pad<-matrix(data=NA, nrow=(17520-nrow(sorg.2018)), ncol=ncol(sorg.2018.months));colnames(na.pad)<-colnames(sorg.2018.months)
sorg.2018.months<-rbind(na.pad, sorg.2018.months)
sorg.2018.months$xlDateTime[which(is.na(sorg.2018.months$xlDateTime))]<-sorg$xlDateTime[which(is.na(sorg.2018.months$xlDateTime))]


plot(rollmean(umolCO2.to.gC(sorg.2018.months$Fc), k=48, na.pad=TRUE, na.rm=TRUE)~sorg$xlDateTime, type='l', col='lightpink', main="Sorghum", lwd=0.5, ylab="Carbon Flux (gC d-1)", xlab='date', ylim=c(-20, 15))
lines(rollmean(umolCO2.to.gC(sorg$Fco2), k=48, na.pad=TRUE, na.rm=TRUE)~sorg$xlDateTime,col='gray', lwd=0.5)

sorgroll18<-rollapply(umolCO2.to.gC(sorg.2018.months$Fc), width=48*7, fill=NA, FUN='mean',na.rm=TRUE, partial=FALSE)
sorgroll20<-rollmean(umolCO2.to.gC(sorg$Fco2), k=48*7, na.rm=TRUE, fill=NA)

lines(sorgroll18~sorg$xlDateTime, lwd=3, col="red")
lines(sorgroll20~sorg$xlDateTime, lwd=3, col='black')

#opt. 1, start from mid-june
sorg.pairy.20<-umolCO2.to.gC(sorg$Fco2)/48;sorg.pairy.20[is.na(sorg.2018.months$Fc)]<-NA
sorg.pairy.18<-umolCO2.to.gC(sorg.2018.months$Fc)/48;sorg.pairy.18[is.na(sorg.pairy.20)]<-NA

plot(cumsum(sorg.pairy.20[!is.na(sorg.pairy.20)])~sorg$xlDateTime[!is.na(sorg.pairy.20)], type='l', ylim=c(-50000/48,1000/48),ylab="Cumulative Carbon flux (gC m-2)",xlab='', lwd=2)
lines(cumsum(sorg.pairy.18[!is.na(sorg.pairy.18)])~sorg$xlDateTime[!is.na(sorg.pairy.18)], col='red', lwd=2)

#opt. 2, start from jan, with both sharing early 2020 fluxes
sorg.add.20<-umolCO2.to.gC(sorg$Fco2)/48#sorg.pairy.20[is.na(sorg.2018.months$Fc)]<-NA
sorg.add.18<-umolCO2.to.gC(c(sorg$Fco2[is.na(sorg.2018.months$Fc)], sorg.2018.months$Fc[!is.na(sorg.2018.months$Fc)]))/48#sorg.pairy.18[is.na(sorg.pairy.20)]<-NA

plot(cumsum(sorg.add.20)~sorg$xlDateTime, type='l', ylim=c(-50000/48,4500/48),ylab="Cumulative Carbon flux (gC m-2)",xlab='', lwd=2)
lines(cumsum(sorg.add.18)~sorg$xlDateTime, col='red', lwd=2)


#Make a 2019 dataset for "sorghum"...
sorg.2019<-sorg.raw.2018[which(sorg.2018.yearmark=="2019"),]
sorg.2019.months<-sorg.2019[1:14640,]

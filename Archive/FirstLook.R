
library(readxl)
maize.raw_MJ<-read_excel("MaizeCon_2020_L3_MayJul.xls", sheet=2,skip=2)
maize.raw_AS<-read_excel("MaizeCon_2020_L3_AugSep.xls", sheet=2,skip=2)
maize.raw<-rbind(maize.raw_MJ,  maize.raw_AS)
maize<-maize.raw[1030:6715,]
maize[maize==-9999]<-NA


# sorg.raw_MJ<-read_excel("Sorghum_2020_L3_MayJul.xls", sheet=2,skip=2)
# sorg.raw_AS<-read_excel("Sorghum_2020_L3_AugSep.xls", sheet=2,skip=2)
# sorg.raw<-rbind(sorg.raw_MJ,  sorg.raw_AS)
# sorg<-sorg.raw[1030:6715,]
# sorg[sorg==-9999]<-NA

sorg<-read_excel("Sorghum_L6_improved.xls", sheet=2,skip=2)
sorg[sorg==-9999]<-NA

misc.raw<-read_excel("MiscanthusNoBasalt_2020_L3.xls", sheet=2,skip=2)
misc<-misc.raw[1030:6715,]
misc[misc==-9999]<-NA


excl<-c(1030:6715)
doylist<-format(maize$xlDateTime, "%j")
hours<-format(sorg$xlDateTime, format="%H")

plot(maize$Fc~maize$xlDateTime,  type='l', col='orange')
plot(sorg$Fco2~sorg$xlDateTime,  type='l', col='forest green')
lines(misc$Fc~misc$xlDateTime,  type='l', col='light blue')

library(esquisse)

library(ggplot2)

ggplot(maize) +
 aes(x = xlDateTime, y = Fc) +
 geom_line(size = 0.5L, colour = "light gray") +
 labs(x = "Timestamp", y = "Fc (Carbon Flux)", title = "Maize Control ") +
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

anynan<-which(is.na(maize$Fc)|is.na(sorg$Fc)|is.na(misc$Fc))

sorg.pair<-sorg$Fc
sorg.pair[anynan]<-NA

maize.pair<-maize$Fc
maize.pair[anynan]<-NA

misc.pair<-misc$Fc
misc.pair[anynan]<-NA

maize.refill<-maize$Fc[missed]
maize.pair.r<-maize.pair;maize.pair.r[missed]<-maize.refill

misc.refill<-misc$Fc[missed]
misc.pair.r<-misc.pair;misc.pair.r[missed]<-misc.refill

#Check missing data####

par(mfrow=c(1,1))
colvec<-rep('black', nrow(maize));colvec[is.na(maize$Fc)]<-'red'
plot(rep(2, nrow(maize))~maize$xlDateTime, col=colvec, pch='.', cex=5, ylim=c(0,3))

colvec<-rep('black', nrow(sorg));colvec[is.na(sorg$Fc)]<-'red'
points(rep(3, nrow(sorg))~maize$xlDateTime, col=colvec, pch='.', cex=5)

colvec<-rep('black', nrow(misc));colvec[is.na(misc$Fc)]<-'red'
points(rep(2.5, nrow(misc))~maize$xlDateTime, col=colvec, pch='.', cex=5)



colvec<-rep('black', nrow(sorg));colvec[anynan]<-'red'
points(rep(1, nrow(maize))~maize$xlDateTime, col=colvec, pch='.', cex=5, ylim=c(1,4))
length(which(is.na(maize.pair)))/length(maize.pair)

notmissed<-c(1:3260, 3660:nrow(sorg))
missed<-c(3261:3659)

nonacounts<-aggregate(maize.pair[notmissed], by=list(hours[notmissed]), FUN=function(x){sum(!is.na(x))})
nacounts<-aggregate(maize.pair[notmissed], by=list(hours[notmissed]), FUN=function(x){sum(is.na(x))})
numhour<-nonacounts$x+nacounts$x
barplot(nacounts$x/numhour, names=nacounts$Group.1)
mean(nacounts$x/numhour)
#####
library(zoo)
par(mfrow=c(2,1), mar=c(3,4,1,4))

library(bigleaf)
maize.pair.u<-umolCO2.to.gC(maize.pair.r)
sorg.pair.u<-umolCO2.to.gC(sorg.pair)
misc.pair.u<-umolCO2.to.gC(misc.pair.r)
#Unis...

maize.roll<-rollapply(maize.pair.u, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
#maize.roll[missed]<-NA
misc.roll<-rollapply(misc.pair.u, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
#misc.roll[missed]<-NA
sorg.roll<-rollapply(sorg.pair.u, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
sorg.roll[missed]<-NA

plot(rollmean(maize.pair.u, k=48, na.pad=TRUE, na.rm=TRUE)~maize$xlDateTime, col='peachpuff',type='l',  lwd=0.5, ylab="Carbon Flux (gC m-2 d-1)", xlab="Day of Year", ylim=c(-20, 15))
lines(rollmean(sorg.pair.u, na.rm=TRUE, k=48, na.pad=TRUE)~maize$xlDateTime, col='darkseagreen1', lwd=0.5)
lines(rollmean(misc.pair.u, na.rm=TRUE, k=48, na.pad=TRUE)~maize$xlDateTime, col='aliceblue', lwd=0.5)
abline(h=0, col='gray')
lines(maize.roll~maize$xlDateTime, lwd=3, col="orange")
lines(sorg.roll~maize$xlDateTime, lwd=3, col='forest green')
lines(misc.roll~maize$xlDateTime, lwd=3, col='light blue')
#used 650x360 for export

par(new=TRUE)
plot(maize.pair~format(maize$xlDateTime, "%j"), col='', axes="FALSE", ylab='', xlab='')
abline(v=(195-1)) #Hailstorm
abline(v=156)
abline(v=(181), lty=2) #dry spell start
abline(v=(190), lty=2) #dry spell end
abline(v=223)
legend(230, 23,legend=c("maize", "sorghum", "miscanthus"), col=c('orange','forest green', 'light blue'), lwd=2, cex=0.7)
text(149,-50,"Flood >")
text(185,18, "Heatwave")
text(205,-50, "< Hailstorm")
text(233, -50, "< Derecho")

plot(aggregate(sorg$Ta, by=list(doylist),FUN='mean', na.rm=TRUE), col='white', xlab="Day of Year", ylab="Daily avg. air temp (circles)")
par(new=T)
plot(maize$RAIN_CEN~format(maize$xlDateTime, "%j"),pch=2,axes=FALSE, ylab='', xlab='')
axis(side=4,at=seq(0,40, by=10));mtext("Precipitation (triangles)", side = 4, line = 3)
abline(v=(195-1)) #Hailstorm
abline(v=156)
abline(v=(181), lty=2) #dry spell start
abline(v=(190), lty=2) #dry spell end


# #Same plot but for ET####
# sorg.pair.et<-sorg$ET
# sorg.pair.et[is.na(maize$ET)]<-NA
# 
# maize.pair.et<-maize$ET
# maize.pair.et[is.na(sorg$ET)]<-NA
# 
# plot(maize.pair.et~maize$xlDateTime, col='peachpuff', xaxt='n',type='l',  lwd=0.5, ylab="Carbon Flux (umolm-2s-1)", xlab="Day of Year", ylim=c(0,0.6))
# lines(sorg.pair.et~maize$xlDateTime, col='darkseagreen1', lwd=0.5)
# #~format(maize$xlDateTime, "%j"
# 
# lines(rollmean(maize.pair.et, k=48*3, fill=NA, na.rm=TRUE)~maize$xlDateTime, lwd=3, col="orange")
# lines(rollmean(sorg.pair.et, k=48*3, na.rm=TRUE, fill=NA)~maize$xlDateTime, lwd=3, col='forest green')
# 
# plot(aggregate(sorg$Ta, by=list(doylist), FUN='mean', na.rm=TRUE), col='red', xlab="Day of Year", ylab="Daily average air temperature")
# par(new=T)
# plot(maize$RAIN_CEN~format(maize$xlDateTime, "%j"),pch=2,axes=FALSE, ylab='', xlab='')
# axis(side=4,at=seq(0,40, by=10))
# abline(v=(195-1)) #Hailstorm
# abline(v=156)
# abline(v=(181), lty=2) #dry spell start
# abline(v=(190), lty=2) #dry spell end

#####

#points(aggregate(sorg$Ta, by=list(doylist), FUN='mean', na.rm=TRUE), col='red')

#sorg.cum<-cumsum(sorg.pair[!is.na(sorg.pair)])
#maize.cum<-cumsum(maize.pair[!is.na(maize.pair)])

#plot(maize.cum, type='l', col='orange', lwd=3)
#lines(sorg.cum, type='l', col='forest green', lwd=3)

sorg.interp<-cumsum(na.approx(sorg.pair.u)[notmissed])/48
maize.interp<-cumsum(na.approx(maize.pair.u)[notmissed])/48
misc.interp<-cumsum(na.approx(misc.pair.u)[notmissed])/48

ts.force<-format(sorg$xlDateTime[notmissed],  "%j")
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
#Pull in 2018
##Maize
if(!exists('maize.raw.2018')){maize.raw.2018<-read_excel("MaizeNoBasalt_2017_to_2019_L6.xlsx", sheet=2,skip=2)}
if(!exists('misc.raw.2018')){misc.raw.2018<-read_excel("MiscanthusNoBasalt_2017_to_2019_L6.xlsx", sheet=2,skip=2)}

#Prepare maize data
maize.2018<-maize.raw.2018[17521:35040,]
maize.2018[maize.2018==-9999]<-NA
maize.2018.months<-maize.2018[6790:12475,]
maize.2018.months$Fc[which(is.na(maize$Fc))]<-NA

maize.2017<-maize.raw.2018[1:17520,]
maize.2017[maize.2017==-9999]<-NA
maize.2017.months<-maize.2017[6790:12475,]
maize.2017.months$Fc[which(is.na(maize$Fc))]<-NA

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
lines(rollmean(umolCO2.to.gC(maize$Fc), k=48, na.rm=TRUE, na.pad=TRUE)~maize$xlDateTime,col='gray', lwd=0.5)
abline(h=0, col='gray')
lines(rollmean(umolCO2.to.gC(maize.2018.months$Fc), k=48*7, fill=NA, na.rm=TRUE)~maize$xlDateTime, lwd=3, col="red")
lines(rollmean(umolCO2.to.gC(maize.2017.months$Fc), k=48*7, fill=NA, na.rm=TRUE)~maize$xlDateTime, lwd=3, col="blue")
lines(rollmean(umolCO2.to.gC(maize$Fc), k=48*7, na.rm=TRUE, fill=NA)~maize$xlDateTime, lwd=3, col='black')
par(new=T)
plot(1:10, axes=FALSE, col='', ylab='', xlab='')
legend(1,5, legend=c(2020, 2018, 2017),bty='n',cex=0.8, col=c('black', 'red','blue'), lwd=2)

maize.20.interp<-cumsum(na.approx(umolCO2.to.gC(maize$Fc)))/48
maize.18.interp<-cumsum(na.approx(umolCO2.to.gC(maize.2018.months$Fc)))/48
maize.17.interp<-cumsum(na.approx(umolCO2.to.gC(maize.2017.months$Fc)))/48

plot(maize.20.interp, type='l',ylab="Cumulative Carbon flux (gC m-2)", xaxt='n', xlab='', ylim=c(-40000/48,4000/48), lwd=2)
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

sorg.2018<-sorg.raw.2018[1:3853,]
sorg.2018[sorg.2018==-9999]<-NA
sorg.2018.months<-sorg.2018[1:3853,]
#sorg.2018.months$Fc[which(is.na(sorg$Fc))]<-NA

na.pad<-matrix(data=NA, nrow=1833, ncol=ncol(sorg.2018.months));colnames(na.pad)<-colnames(sorg.2018.months)
sorg.2018.months<-rbind(na.pad, sorg.2018.months)

sorg.2018.months$Fc[which(is.na(sorg$Fc))]<-NA

plot(rollmean(umolCO2.to.gC(sorg.2018.months$Fc), k=48, na.pad=TRUE, na.rm=TRUE)~sorg$xlDateTime, type='l', col='lightpink', main="Sorghum", lwd=0.5, ylab="Carbon Flux (gC d-1)", xlab='date', ylim=c(-20, 15))
lines(rollmean(umolCO2.to.gC(sorg$Fc), k=48, na.pad=TRUE, na.rm=TRUE)~sorg$xlDateTime,col='gray', lwd=0.5)

sorgroll<-rollapply(umolCO2.to.gC(sorg.2018.months$Fc), width=48*7, fill=NA, FUN='mean',na.rm=TRUE, partial=FALSE)
sorgroll[c(1600:1833,missed)]<-NA

sorgroll20<-rollmean(umolCO2.to.gC(sorg$Fc), k=48*7, na.rm=TRUE, fill=NA)
sorgroll20[missed]<-NA

lines(sorgroll~sorg$xlDateTime, lwd=3, col="red")
lines(sorgroll20~sorg$xlDateTime, lwd=3, col='black')

sorg.pairy.20<-umolCO2.to.gC(sorg$Fc)/48;sorg.pairy.20[is.na(sorg.2018.months$Fc)]<-NA
sorg.pairy.18<-umolCO2.to.gC(sorg.2018.months$Fc)/48;sorg.pairy.18[is.na(sorg.pairy.20)]<-NA

plot(cumsum(sorg.pairy.20[!is.na(sorg.pairy.20)])~sorg$xlDateTime[!is.na(sorg.pairy.20)], type='l', ylim=c(-26000/48,1000/48),ylab="Cumulative Carbon flux (gC m-2)",xlab='', lwd=2)
lines(cumsum(sorg.pairy.18[!is.na(sorg.pairy.18)])~sorg$xlDateTime[!is.na(sorg.pairy.18)], col='red', lwd=2)

#Windfilter?
# sorg.west<-sorg$Fc[which(sorg$Wd_SONIC>200 & sorg$Wd_SONIC<340)];sorg.east<-sorg$Fc[which(sorg$Wd_SONIC>20 & sorg$Wd_SONIC<160)]
# plot(cumsum(sorg.west[!is.na(sorg.west)]), type='l')
# lines(cumsum(sorg.east[!is.na(sorg.east)]), col='red')
# 
# sorg.west.nas<-sorg$Fc;sorg.west.nas[which(sorg$Wd_SONIC>20 & sorg$Wd_SONIC<160)]<-NA
# sorg.east.nas<-sorg$Fc;sorg.east.nas[which(sorg$Wd_SONIC>200 & sorg$Wd_SONIC<340)]<-NA
# 
# plot(sorg.east.nas)
# plot(sorg.west.nas, col='red')
# 

#compare maize, misc  2017/18

par(mfrow=c(1,3),mar=c(4,4,3,1))
maize.interp2<-cumsum(na.approx(umolCO2.to.gC(maize$Fc)))/48
misc.interp2<-cumsum(na.approx(umolCO2.to.gC(misc$Fc)))/48

maize.17.interp.match<-cumsum(na.approx(umolCO2.to.gC(maize.2017.months$Fc[match])))/48

misc.18.interp.all<-cumsum(na.approx(umolCO2.to.gC(misc.2018.months$Fc)))/48

plot(c(misc.17.interp/48, NA, NA)~misc.2017.months$xlDateTime, xlab='',type='l',main="2017",ylab="Cumulative Carbon flux (gC m-2)", ylim=c(-42000/48,5000/48), lwd=3, col='lightblue')
lines(c(maize.17.interp.match)~misc.2017.months$xlDateTime, col='orange',  lwd=3)

plot(c(misc.18.interp.all, NA)~misc.2018.months$xlDateTime, xlab='',type='l',main="2018",ylab="Cumulative Carbon flux (gC m-2)", ylim=c(-42000/48,5000/48), lwd=3, col='lightblue')
lines(maize.18.interp/48~misc.2018.months$xlDateTime, col='orange', lwd=3)

plot(maize.interp2~maize$xlDateTime, type='l', lwd=3,main="2020", col='orange', ylab="Cumulative C Flux (gC m-2)", xlab="Day of Year", ylim=c(-42000/48,5000/48))
lines(c(misc.interp2,NA)~misc$xlDateTime, col='light blue', lwd=3)
polygon(y=c(-25000, 0, 0, -25000), x=c(210,210,220,220), col='white', border='NA')


#Whats going on late 2018

plot(rollmean(maize.2018.months$Ta, na.rm=TRUE, k=48, na.pad=TRUE)~maize.2018.months$xlDateTime,col='red', type='l', ylab="Air Temperature")
lines(rollmean(maize.2017.months$Ta, na.rm=TRUE, k=48, na.pad=TRUE)~maize.2018.months$xlDateTime, col='blue', type='l')
lines(rollmean(maize$Ta, na.rm=TRUE, k=48, type='l', na.pad=TRUE)~maize.2018.months$xlDateTime)

plot(rollmean(maize.2018.months$Precip, na.rm=TRUE, k=48, na.pad=TRUE)~maize.2018.months$xlDateTime, col='red', ylab="Precipitation")
points(rollmean(maize.2017.months$Precip, na.rm=TRUE, k=48, na.pad=TRUE)~maize.2018.months$xlDateTime, col='blue') 
points(rollmean(maize$Precip, na.rm=TRUE, k=48, na.pad=TRUE)~maize.2018.months$xlDateTime, col='black') 



plot(rollmean(maize.2018.months$ER_SOLO, na.rm=TRUE, k=48, na.pad=TRUE)~maize.2018.months$xlDateTime,col='red', type='l', ylab="ER")
lines(rollmean(maize.2017.months$ER_SOLO, na.rm=TRUE, k=48, na.pad=TRUE)~maize.2018.months$xlDateTime, col='blue', type='l')
#lines(rollmean(maize$ER, na.rm=TRUE, k=48, type='l', na.pad=TRUE)~maize.2018.months$xlDateTime)

plot(rollmean(maize.2018.months$GPP_SOLO, na.rm=TRUE, k=48, na.pad=TRUE)~maize.2018.months$xlDateTime,col='red', type='l', ylab="Gross Primary Productivity")
lines(rollmean(maize.2017.months$GPP_SOLO, na.rm=TRUE, k=48, na.pad=TRUE)~maize.2018.months$xlDateTime, col='blue', type='l')
#lines(rollmean(maize$ER, na.rm=TRUE, k=48, type='l', na.pad=TRUE)~maize.2018.months$xlDateTime)

#Grab maize precip
plot(maize$Precip~maize$xlDateTime, ylab="Precip")

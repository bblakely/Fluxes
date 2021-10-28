#Data filtered at L2
#rm(list=setdiff(ls(), c("sorg", "sorg.raw")))

if(!exists("sorg")){source('ReadAll.R')}

setwd("D:/R/Fluxes/WindFilter/Data")
sorg.east.raw<-read_excel("Sorghum_2020_EAST_L6_EP.xls", sheet=2,skip=2)
sorg.east.raw[sorg.east.raw==-9999]<-NA
sorg.east<-sorg.east.raw

sorg.west.raw<-read_excel("Sorghum_2020_WEST_L6_EP.xls", sheet=2,skip=2)
sorg.west.raw[sorg.west.raw==-9999]<-NA
sorg.west<-sorg.west.raw

setwd("D:/R/Fluxes")

par(mfrow=c(1,2))
plot(sorg.west$ER_LT~sorg$xlDateTime, ylim=c(-40, 30))
points(sorg.west$ER~sorg$xlDateTime, col='red')

plot(sorg.east$ER_LT~sorg$xlDateTime, ylim=c(-40, 30))
points(sorg.east$ER~sorg$xlDateTime, col='blue')

library(zoo); library(bigleaf)

##closure check####
# turb<-sorg.west$Fh+sorg.west$Fe; rng<-sorg.west$Fn-sorg.west$Fg
# 
# test<-lm(turb~rng)
# abline(0,0.7, col='red')
# summary(test)
# test$coefficients
# smoothScatter(turb~rng)
# abline(test$coefficients, col='blue')
# 
# plot(sorg.raw$ER_LT)#, ylim=c(-5, 25))
# points(sorg.raw$ER, col='red')
#####
#smooth em out

par(mfrow=c(2,1))
plot(rollmean(umolCO2.to.gC(sorg.east$Fco2), k=48, na.pad=TRUE, na.rm=TRUE)~format(sorg$xlDateTime, "%j"), type='l', col='lightblue', main="Sorghum", lwd=0.5, ylab="Carbon Flux (gC d-1)", xlab='date', ylim=c(-20, 15))
lines(rollmean(umolCO2.to.gC(sorg.west$Fco2), k=48, na.pad=TRUE, na.rm=TRUE)~format(sorg$xlDateTime, "%j"),col='lightpink', lwd=0.5)

sorgroll.es<-rollapply(umolCO2.to.gC(sorg.east$Fco2), width=48*14, fill=NA, FUN='mean',na.rm=TRUE, partial=FALSE)
sorgroll.we<-rollmean(umolCO2.to.gC(sorg.west$Fco2), k=48*14, na.rm=TRUE, fill=NA)

lines(sorgroll.es~format(sorg$xlDateTime, "%j"), lwd=3, col="blue")
lines(sorgroll.we~format(sorg$xlDateTime, "%j"), lwd=3, col='red')

abline(v=156) #Flood
abline(v=164, lty=3) #Replant
# abline(v=(195-1)) #Hailstorm
# abline(v=223) #derecho
abline(v=282) #End of split series

#cumulative
sorg.es.cum<-cumsum(umolCO2.to.gC(sorg.east$Fco2))/48
sorg.ws.cum<-cumsum(umolCO2.to.gC(sorg.west$Fco2))/48

plot(sorg.es.cum, type='l', col='blue', ylim=c(-1000,100),ylab="Cumulative Carbon Flux (gC m-2)"); lines(sorg.ws.cum,col='red')



#GPP
plot(rollmean(umolCO2.to.gC(sorg.east$GPP_LT), k=48, na.pad=TRUE, na.rm=TRUE)~format(sorg$xlDateTime, "%j"), type='l', col='lightblue', main="Sorghum", lwd=0.5, ylab="Carbon Flux (gC d-1)", xlab='date', ylim=c(-10, 25))
lines(rollmean(umolCO2.to.gC(sorg.west$GPP_LT), k=48, na.pad=TRUE, na.rm=TRUE)~format(sorg$xlDateTime, "%j"),col='lightpink', lwd=0.5)

sorgroll.es<-rollapply(umolCO2.to.gC(sorg.east$GPP_LT), width=48*14, fill=NA, FUN='mean',na.rm=TRUE, partial=FALSE)
sorgroll.we<-rollmean(umolCO2.to.gC(sorg.west$GPP_LT), k=48*14, na.rm=TRUE, fill=NA)

lines(sorgroll.es~format(sorg$xlDateTime, "%j"), lwd=3, col="blue")
lines(sorgroll.we~format(sorg$xlDateTime, "%j"), lwd=3, col='red')

abline(v=156) #Flood
abline(v=164, lty=3) #Replant
# abline(v=(195-1)) #Hailstorm
# abline(v=223) #derecho
abline(v=282) #End of split series


#cumulative
sorg.es.cum<-cumsum(umolCO2.to.gC(sorg.east$GPP_LT))/48
sorg.ws.cum<-cumsum(umolCO2.to.gC(sorg.west$GPP_LT))/48

plot(sorg.es.cum, type='l', col='blue', ylim=c(0,2500),ylab="Cumulative Carbon Flux (gC m-2)"); lines(sorg.ws.cum,col='red')

#Respiration

plot(rollmean(umolCO2.to.gC(sorg.east$ER_LT), k=48, na.pad=TRUE, na.rm=TRUE)~format(sorg$xlDateTime, "%j"), type='l', col='lightblue', main="Sorghum", lwd=0.5, ylab="Carbon Flux (gC d-1)", xlab='date', ylim=c(-20, 20))
lines(rollmean(umolCO2.to.gC(sorg.west$ER_LT), k=48, na.pad=TRUE, na.rm=TRUE)~format(sorg$xlDateTime, "%j"),col='lightpink', lwd=0.5)

sorgroll.es<-rollapply(umolCO2.to.gC(sorg.east$ER_LT), width=48*14, fill=NA, FUN='mean',na.rm=TRUE, partial=FALSE)
sorgroll.we<-rollmean(umolCO2.to.gC(sorg.west$ER_LT), k=48*14, na.rm=TRUE, fill=NA)

lines(sorgroll.es~format(sorg$xlDateTime, "%j"), lwd=3, col="blue")
lines(sorgroll.we~format(sorg$xlDateTime, "%j"), lwd=3, col='red')

abline(v=156) #Flood
abline(v=164, lty=3) #Replant
# abline(v=(195-1)) #Hailstorm
# abline(v=223) #derecho
abline(v=282) #End of split series


#cumulative
sorg.es.cum<-cumsum(umolCO2.to.gC(sorg.east$ER_LT))/48
sorg.ws.cum<-cumsum(umolCO2.to.gC(sorg.west$ER_LT))/48

plot(sorg.es.cum, type='l', col='blue', ylim=c(0,2000),ylab="Cumulative Carbon Flux (gC m-2)"); lines(sorg.ws.cum,col='red')




# #ok what's going on here
# par(mfrow=c(1,1))
# H<-as.numeric(format(sorg$xlDateTime, "%H%M"))
# gs<-which(format(sorg.east$xlDateTime, "%j")%in%c(150:300))
# plot(sorg.east$Fco2[gs]~H[gs], ylab="Fco2 (Sorghum)", xlab="Time of day (DOY 150 - 300)")
# 
# col=rep("black", 17568); col[sorg.east$Fsd<10]<-"red"
# points(sorg.east$Fco2[gs]~H[gs], col=col[gs])


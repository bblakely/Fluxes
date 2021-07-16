#Data filtered at L2
#rm(list=setdiff(ls(), c("sorg", "sorg.raw")))
if(!exists("sorg")){source('ReadAll.R')}

sorg.east.raw<-read_excel("Sorghum_2020_L6_EAST.xls", sheet=2,skip=2)
sorg.east.raw[sorg.east.raw==-9999]<-NA
sorg.east<-sorg.east.raw

sorg.west.raw<-read_excel("Sorghum_2020_L6_WEST.xls", sheet=2,skip=2)
sorg.west.raw[sorg.west.raw==-9999]<-NA
sorg.west<-sorg.west.raw

par(mfrow=c(1,2))
plot(sorg.west$ER_LT~sorg$xlDateTime, ylim=c(-40, 25))
points(sorg.west$ER~sorgt$xlDateTime, col='blue')

plot(sorg.east$ER_LT~sorg$xlDateTime, ylim=c(-40, 25))
points(sorg.east$ER~sorg$xlDateTime, col='red')



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


plot(rollmean(umolCO2.to.gC(sorg.east$Fco2), k=48, na.pad=TRUE, na.rm=TRUE)~format(sorg$xlDateTime, "%j"), type='l', col='lightpink', main="Sorghum", lwd=0.5, ylab="Carbon Flux (gC d-1)", xlab='date', ylim=c(-20, 15))
lines(rollmean(umolCO2.to.gC(sorg.west$Fco2), k=48, na.pad=TRUE, na.rm=TRUE)~format(sorg$xlDateTime, "%j"),col='gray', lwd=0.5)

sorgroll.es<-rollapply(umolCO2.to.gC(sorg.east$Fco2), width=48*14, fill=NA, FUN='mean',na.rm=TRUE, partial=FALSE)
sorgroll.we<-rollmean(umolCO2.to.gC(sorg.west$Fco2), k=48*14, na.rm=TRUE, fill=NA)

lines(sorgroll.es~format(sorg$xlDateTime, "%j"), lwd=3, col="red")
lines(sorgroll.we~format(sorg$xlDateTime, "%j"), lwd=3, col='black')

#Respiration

plot(rollmean(umolCO2.to.gC(sorg.east$ER_LT), k=48, na.pad=TRUE, na.rm=TRUE)~format(sorg$xlDateTime, "%j"), type='l', col='lightpink', main="Sorghum", lwd=0.5, ylab="Carbon Flux (gC d-1)", xlab='date', ylim=c(-20, 20))
lines(rollmean(umolCO2.to.gC(sorg.west$ER_LT), k=48, na.pad=TRUE, na.rm=TRUE)~format(sorg$xlDateTime, "%j"),col='lightblue', lwd=0.5)

sorgroll.es<-rollapply(umolCO2.to.gC(sorg.east$ER_LT), width=48*14, fill=NA, FUN='mean',na.rm=TRUE, partial=FALSE)
sorgroll.we<-rollmean(umolCO2.to.gC(sorg.west$ER_LT), k=48*14, na.rm=TRUE, fill=NA)

lines(sorgroll.es~format(sorg$xlDateTime, "%j"), lwd=3, col="red")
lines(sorgroll.we~format(sorg$xlDateTime, "%j"), lwd=3, col='blue')

#ok what's going on here
par(mfrow=c(1,1))
H<-as.numeric(format(sorg$xlDateTime, "%H%M"))
gs<-which(format(sorg$xlDateTime, "%j")%in%c(150:300))
plot(sorg$Fco2[gs]~H[gs])

col=rep("black", 17568); col[sorg$Fsd<0]<-"red"
points(sorg$Fco2[gs]~H[gs], col=col[gs])

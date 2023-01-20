#Seasonal met/eb
library(zoo)
comparesites<-function(dat1, dat2, var, condition=NULL){

timestamp<-as.POSIXct(dat1$xlDateTime)
doy<-as.numeric(format(timestamp, "%j"))

varcol1<-which(colnames(dat1)==var);varcol2<-which(colnames(dat2)==var)
concol1<-which(colnames(dat1)==condition);concol2<-which(colnames(dat2)==condition)


plot(aggregate(dat1[varcol1], by=list(doy), FUN='mean'), main=var)
points(aggregate(dat2[varcol2], by=list(doy), FUN='mean'), col='orange')


diff<-(unlist(unname(dat1[,varcol1]))-unlist(unname(dat2[,varcol2])))
diff[which(is.na(dat1[,concol1])|is.na(dat2[,concol2]))]<-NA

agdiff<-aggregate(diff, by=list(doy), FUN='mean')[,2]; doydiff<-aggregate(diff, by=list(doy), FUN='mean')[,1]
lims<-quantile(agdiff, c(0.01,0.99), na.rm=TRUE)



plot(agdiff, ylim=lims)
#<-which(!is.na(agdiff))
#agdiff.sm<-agdiff[nona.ind]; doydiff.sm<-doydiff[nona.ind]

lines(rollapply(agdiff, width=10, FUN='mean', fill=NA)~doydiff, lwd=3)
abline(h=0, lty=2, col='red')
abline(v=c(296, 288), col=c('black', 'orange'))
#abline(v=254, lty=2)


}


par(mfrow=c(2,1), mar=c(3,3,1,2))
comparesites(sorg, maize, "Fsd")
comparesites(sorg, maize, "Fsu")
comparesites(sorg, maize, "Fpard")
comparesites(sorg, maize, "Fparu")
comparesites(sorg, maize, "Tc")
comparesites(sorg, maize, "Ta", condition="Ta_HMP_3m")

comparesites(sorg, maize, "Fn", condition="Fn_NR")
comparesites(sorg, maize, "Fh")
comparesites(sorg, maize, "Fe")
comparesites(sorg, maize, "Fg")

comparesites(sorg, maize, "RH")



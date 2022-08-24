
#EnergyFluxes

if (!exists('sorg.hvst.sum')){source("LongRecord.R")}

#Check the energy fluxes


#Albedo / shortwave####

albify<-function(dat, window=1){
  dat.alb<-dat$Fsu/dat$Fsd; dat.alb[dat.alb<0|dat.alb>1|dat$Fsd<10]<-NA
  dat.doy<-as.numeric(format(dat$xlDateTime, "%m"))
  dat.d.alb<-aggregate(dat.alb, by=list(dat.doy), FUN='mean', na.rm=TRUE)
  dat.alb.sm<-rollapply(dat.d.alb$x, width=window, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
  
  return(dat.alb.sm)
}

# sorg.alb.sm<-albify(sorghum)
# maize.alb.sm<-albify(maize);maize.c.alb.sm<-albify(maize.c)
# misc.alb.sm<-albify(miscanthus);misc.c.alb.sm<-albify(miscanthus.c)
# switch.alb.sm<-albify(switchgrass)

# sorg.alb.sm<-albify(sorghum1)
# maize.alb.sm<-albify(maize1);maize.c.alb.sm<-albify(maize.c1)
# misc.alb.sm<-albify(miscanthus1);misc.c.alb.sm<-albify(miscanthus.c1)
# switch.alb.sm<-albify(switchgrass)

sorg.alb.sm<-albify(sorg.merge)
maize.alb.sm<-albify(maize.merge);maize.c.alb.sm<-albify(maize.c.merge)
misc.alb.sm<-albify(misc.merge);misc.c.alb.sm<-albify(misc.c.merge)
switch.alb.sm<-albify(switchgrass)


par(mfrow=c(1,1))
plot(sorg.alb.sm, type='l', col='forest green', xlab='month', main="Albedo")
lines(maize.alb.sm, col='orange')
lines(maize.c.alb.sm, col='gold')
lines(misc.alb.sm, col='blue')
lines(misc.c.alb.sm, col='light blue')
lines(switch.alb.sm, col='pink')


#aggregate plant types

misc.alb<-(misc.alb.sm+misc.c.alb.sm)/2
maize.alb<-(maize.alb.sm+maize.c.alb.sm)/2

plot(sorg.alb.sm, type='l', col='forest green', xlab='month', ylab="albedo", lwd=2.5)
lines(maize.alb, col='orange', lwd=2.5)
lines(misc.alb, col='light blue', lwd=2.5)

legend(3, .32, cex=0.8, bty='n',legend=c("Miscanthus", "Maize", "Sorghum"), col=c('light blue', 'orange','forest green'), lwd=2, ncol=3)

#Radiative forcing

monthrad<-aggregate(maize$Fsd, by=list(as.numeric(format(maize$xlDateTime, "%m"))), FUN='mean', na.rm=TRUE)

par(mfrow=c(1,3))

albdiff<-maize.alb-misc.alb #"for conversion from corn to miscanthus"
rf<-monthrad$x*albdiff; rf.ann<-mean(rf)
plot(rf, type='l', main="maize->miscanthus", lwd=2, ylab='raditive forcing (Wm-2)', xlab='month', ylim=c(-18, 18)); abline(h=0)
text(9, 15, paste("yearly:", round(rf.ann, 2)))#text(9, 0.9*max(rf), paste("yearly:", round(rf.ann, 2)))

albdiff<-maize.alb-sorg.alb.sm #for conversion from corn to sorgum
rf<-monthrad$x*albdiff; rf.ann<-mean(rf)
plot(rf, type='l', main="maize->sorghum", lwd=2,ylab='raditive forcing (Wm-2)', xlab='month', ylim=c(-18, 18)); 
abline(h=0);  text(9, 15, paste("yearly:", round(rf.ann, 2)))#text(9, 0.9*max(rf), paste("yearly:", round(rf.ann, 2)))

albdiff<-maize.alb-switch.alb.sm #for conversion from corn to sorgum
rf<-monthrad$x*albdiff; rf.ann<-mean(rf)
plot(rf, type='l', main="maize->switchgrass",lwd=2,ylab='raditive forcing (Wm-2)', xlab='month', ylim=c(-18, 18)); 
abline(h=0); text(9, 15, paste("yearly:", round(rf.ann, 2)))#text(9, 0.9*max(rf), paste("yearly:", round(rf.ann, 2)))

#Variability
albvar<-function(dat, id){
  dat.alb<-dat$Fsu/dat$Fsd; dat.alb[dat.alb<0|dat.alb>1|dat$Fsd<10]<-NA
  dat.month<-as.numeric(format(dat$xlDateTime, "%m"))
  dat.yr<-as.numeric(format(dat$xlDateTime, "%Y"))
  dat.agg<-aggregate(dat.alb~dat.month+dat.yr, FUN='mean', na.rm=TRUE)
  dat.agg$id<-(id)

  return(dat.agg)
}


sorg.albvar<-albvar(sorg.merge, "sb")
misc.albvar<-albvar(misc.merge,"mgb")
misc.c.albvar<-albvar(misc.c.merge, "mgc")
maize.albvar<-albvar(maize.merge, "zmb")
maize.c.albvar<-albvar(maize.c.merge, "zmc")
switch.albvar<-albvar(switchgrass, "sw")



all.albvar<-rbind(sorg.albvar, misc.albvar, misc.c.albvar, maize.albvar, maize.c.albvar, switch.albvar)

all.albvar$dat.month<-as.factor(all.albvar$dat.month)

nosoy<-c(2008, 2009, 2011, 2012, 2014, 2015, 2017, 2018, 2020, 2021)

all.albvar.ns<-all.albvar[all.albvar$dat.yr%in%nosoy,] #removing soy years
all.albvar.sbtime<-all.albvar.ns[all.albvar.ns$dat.yr>2017,] #no soy and since sorghum existed, i.e. 2018+

library(ggplot2)

ggplot(all.albvar.ns, aes(x=dat.month, y=dat.alb, fill=id),) + 
    geom_boxplot(outlier.size=0.1)+
    theme_minimal()+
    scale_fill_manual(values=c("light blue", "blue", "forest green", "light pink", "orange", "yellow"))


#####


#Bowen ratio####

bowenize<-function(dat, window=1){
  dat.bow<-dat$Fh/dat$Fe; dat.bow[dat$Fh<0|dat$Fe<0|dat.bow>100]<-NA
  dat.doy<-as.numeric(format(dat$xlDateTime, "%m"))
  dat.d.bow<-aggregate(dat.bow, by=list(dat.doy), FUN='mean', na.rm=TRUE)
  dat.bow.sm<-rollapply(dat.d.bow$x, width=window, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
  
  return(dat.bow.sm)
  
}

# sorg.bow<-bownize(sorghum)
# maize.bow<-bownize(maize);maize.c.bow<-bownize(maize.c)
# misc.bow<-bownize(miscanthus);misc.c.bow<-bownize(miscanthus.c)
# switch.bow<-bownize(switchgrass)

# sorg.bow<-bownize(sorghum1)
# maize.bow<-bownize(maize1);maize.c.bow<-bownize(maize.c1)
# misc.bow<-bownize(miscanthus1);misc.c.bow<-bownize(miscanthus.c1)
# switch.bow<-bownize(switchgrass)

sorg.bow<-bowenize(sorg.merge)
maize.bow<-bowenize(maize.merge);maize.c.bow<-bowenize(maize.c.merge)
misc.bow<-bowenize(misc.merge);misc.c.bow<-bowenize(misc.c.merge)
switch.bow<-bowenize(switchgrass)

par(mfrow=c(1,1))
plot(sorg.bow, type='l', lwd=2, col='forest green', xlab='month', ylim=c(0.1, 5), ylab="bowen ratio")
lines(maize.bow, col='orange', lwd=2)
lines(maize.c.bow, col='gold', lwd=2)
lines(misc.bow, col='blue', lwd=2)
lines(misc.c.bow, col='light blue', lwd=2)
lines(switch.bow, col='pink', lwd=2)

legend(3, 5, legend=c('miscanthus', 'miscanthus (basalt)', 'maize', 'maize (basalt)', 'sorghum', 'switchgrass'), col=c('light blue', 'blue', 'gold', 'orange', 'forest green', 'pink'), 
       lwd=2, ncol=2, cex=0.6, bty='n')


#####

#Ts-Ta #####

#Some Tc columns are incomplete; more data is found in the Tc_3m column.
switchgrass$Tc<-switchgrass$Tc_3m
misc.merge$Tc[is.na(misc.merge$Tc)]<-misc.merge$Tc_3m[is.na(misc.merge$Tc)]
maize.merge$Tc[is.na(maize.merge$Tc)]<-maize.merge$Tc_3m[is.na(maize.merge$Tc)]


stemp<-function(dat, window=1){
  dat.st<-dat$Tc-dat$Ta;dat.st[dat.st<(-20)|dat.st>20]<-NA
  dat.doy<-as.numeric(format(dat$xlDateTime, "%m"))
  plot(dat.st~dat.doy)
  dat.d.st<-aggregate(dat.st, by=list(dat.doy), FUN='mean', na.rm=TRUE)
  dat.st.sm<-rollapply(dat.d.st$x, width=window, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
  
  return(dat.st.sm)
  
}

# sorg.st<-stemp(sorghum)
# maize.st<-stemp(maize);maize.c.st<-stemp(maize.c)
# misc.st<-stemp(miscanthus);misc.c.st<-stemp(miscanthus.c)
# switch.st<-stemp(switchgrass)

# sorg.st<-stemp(sorghum1)
# maize.st<-stemp(maize1);maize.c.st<-stemp(maize.c1)
# misc.st<-stemp(miscanthus1);misc.c.st<-stemp(miscanthus.c1)
# switch.st<-stemp(switchgrass)

sorg.st<-stemp(sorg.merge)
maize.st<-stemp(maize.merge);maize.c.st<-stemp(maize.c.merge)
misc.st<-stemp(misc.merge);misc.c.st<-stemp(misc.c.merge)
switch.st<-stemp(switchgrass)


par(mfrow=c(1,1))
plot(sorg.st, type='l', col='forest green', xlab='month', main="Surface - Air temperature differential")
lines(maize.st, col='orange')
lines(maize.c.st, col='gold')
lines(misc.st, col='blue')
lines(misc.c.st, col='light blue')
lines(switch.st, col='pink')
abline(h=0)



#Variability
stvar<-function(dat, id){
  dat.st<-dat$Tc-dat$Ta;dat.st[dat.st<(-20)|dat.st>20]<-NA
  dat.month<-as.numeric(format(dat$xlDateTime, "%m"))
  dat.yr<-as.numeric(format(dat$xlDateTime, "%Y"))
  dat.agg<-aggregate(dat.st~dat.month+dat.yr, FUN='mean', na.rm=TRUE)
  #dat.count<-aggregate(dat.st~dat.month+dat.yr, FUN=length)
  #dat.agg$dat.st[which(dat.count$dat.st<1340)]<-NA #NAN months with few data
  dat.agg$id<-(id)
  
  return(dat.agg)
}


sorg.stvar<-stvar(sorg.merge, "sb")
misc.stvar<-stvar(misc.merge,"mgb")
misc.c.stvar<-stvar(misc.c.merge, "mgc")
maize.stvar<-stvar(maize.merge, "zmb")
maize.c.stvar<-stvar(maize.c.merge, "zmc")
switch.stvar<-stvar(switchgrass, "sw")

all.stvar<-rbind(sorg.stvar, misc.stvar, misc.c.stvar, maize.stvar, maize.c.stvar, switch.stvar)
all.stvar$dat.month<-as.factor(all.stvar$dat.month)

nosoy<-c(2008, 2009, 2011, 2012, 2014, 2015, 2017, 2018, 2020, 2021)

all.stvar.ns<-all.stvar[all.stvar$dat.yr%in%nosoy,] #removing soy years
all.stvar.sbtime<-all.stvar.ns[all.stvar.ns$dat.yr>2017,] #no soy and since sorghum existed, i.e. 2018+
all.stvar.nosbtime<-all.stvar.ns[all.stvar.ns$dat.yr<2017,] #no soy and since sorghum existed, i.e. 2018+


set1<-c("light blue", "blue", "forest green", "light pink", "orange", "yellow")
set2<-c( "light blue",  "light pink", "orange")
set3<-c("light blue", "blue", "forest green", "orange", "yellow")
  
ggplot(all.stvar.sbtime, aes(x=dat.month, y=dat.st, fill=id),) + 
  geom_boxplot(outlier.size=0.1)+
  theme_minimal()+
  scale_fill_manual(values=set3)

#what's up with maize control?
s<-sort(sample(1:nrow(maize.c.merge), 15000))

zmc.test<-maize.c.merge[s,]

zmb.test<-maize.merge[as.numeric(format(maize.merge$xlDateTime, "%Y"))>2016,]
zmb.test<-zmb.test[s,]


width<-500

par(mfrow=c(1,1))
plot(rollapply(zmc.test$Tc-zmc.test$Ta, width=width, FUN='mean', na.rm=TRUE), type='l', col="yellow")
lines(rollapply(zmb.test$Tc-zmb.test$Ta, width=width, FUN='mean', na.rm=TRUE), col='orange')

plot(rollapply(zmc.test$Tc, width=width, FUN='mean', na.rm=TRUE), type='l', main="surf t", col="yellow")
lines(rollapply(zmb.test$Tc, width=width, FUN='mean', na.rm=TRUE), col='orange')

plot(rollapply(zmc.test$Ta, width=width, FUN='mean', na.rm=TRUE), type='l', main= "air t", col="yellow")
lines(rollapply(zmb.test$Ta, width=width, FUN='mean', na.rm=TRUE), col='orange')


plot(rollapply(zmc.test$Ta, width=width, FUN='mean', na.rm=TRUE)-rollapply(zmb.test$Ta, width=width, FUN='mean', na.rm=TRUE), type='l', ylim=c(-2,2), col='blue')
lines(rollapply(zmc.test$Tc, width=width, FUN='mean', na.rm=TRUE)-rollapply(zmb.test$Tc, width=width, FUN='mean', na.rm=TRUE), type='l', ylim=c(-2,2), col='green')

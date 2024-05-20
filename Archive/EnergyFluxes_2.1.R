
#EnergyFluxes

if (!exists("sorg.hvst.sum")){source("LongRecord_2.1.R")}

library(zoo)
#Cleanup; prevents objects from previous runs from showing up erroneously in the plots
#rm(list=setdiff(ls(), c("sorg.hvst.sum","cumulatives", "maize.merge", "maize.c.merge", "misc.merge", "misc.c.merge", "sorg.merge", "switchgrass", "sorg.yield","misc.yield","maize.yield","switch.yield","maize.c.yield", "misc.c.yield")))

#indices for years

allyr<-c(2008:2022) #all years
nosoy<-c(2008, 2009, 2011, 2012, 2014, 2015, 2017, 2018, 2020, 2021) #years where annuals were NOT in soy
sorgyr<-c(2018:2022)#years when the sorghum site was active
ctrlyr<-c(2017:2022) #years where the 'control' sites are active
ogyr<-c(2008:2016) #original 8-year span with mg, sw, np, and zm

set<-allyr#The set of data you will use for this whole script
#applies the time subset from "set" 
subtime<-function(dat, subset=set){
  
  dat.time<-as.numeric(format(dat$xlDateTime, "%Y"))
  ind<-which(dat.time%in%subset)
  
  dat.out<-dat[ind,]
  
  return(dat.out)
  
}

varplots<-FALSE

#Check the energy fluxes
#Albedo / shortwave####

albify<-function(dat, window=1){
  dat<-subtime(dat)
  dat.alb<-dat$Fsu/dat$Fsd; dat.alb[dat.alb<0|dat.alb>1|dat$Fsd<10]<-NA
  dat.doy<-as.numeric(format(dat$xlDateTime, "%m"))
  dat.d.alb<-aggregate(dat.alb, by=list(dat.doy), FUN='mean', na.rm=TRUE)
  dat.alb.sm<-rollapply(dat.d.alb$x, width=window, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
  
  return(dat.alb.sm)
}



sorg.alb.sm<-albify(sorg.merge)
maize.alb.sm<-albify(maize.merge);maize.c.alb.sm<-albify(maize.c.merge)
misc.alb.sm<-albify(misc.merge);misc.c.alb.sm<-albify(misc.c.merge)
switch.alb.sm<-albify(switchgrass)

#aggregate plant types
if(exists('maize.c.alb.sm')&exists('misc.c.alb.sm')){
misc.alb<-(misc.alb.sm+misc.c.alb.sm)/2
maize.alb<-(maize.alb.sm+maize.c.alb.sm)/2
}else{
  misc.alb<-misc.alb.sm
  maize.alb<-maize.alb.sm
  }

#Adjust for darker soil?
#plot(maize.alb-sorg.alb.sm) #maize is ~0.06 brighter when the ground is bare
sorg.alb.soil<-sorg.alb.sm+c(0,0,0.06, 0.06, 0.06, 0.06,0,0,0,0,0.06,0.06)

#Radiative forcing

monthrad<-aggregate(maize.merge$Fsd, by=list(as.numeric(format(maize.merge$xlDateTime, "%m"))), FUN='mean', na.rm=TRUE)

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

if(varplots==TRUE){

#Variability
albvar<-function(dat, id){
  #dat<-subtime(dat)
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

zm.albvar<-merge(maize.albvar, maize.c.albvar, all=TRUE); zm.albvar$id<-"zm"
mg.albvar<-merge(misc.albvar, misc.c.albvar, all=TRUE); mg.albvar$id<-"mg"


all.albvar<-rbind(sorg.albvar, misc.albvar, misc.c.albvar, maize.albvar, maize.c.albvar, switch.albvar)
all.albvar.av<-rbind(sorg.albvar, mg.albvar, zm.albvar, switch.albvar)
all.albvar.av<-aggregate(data=all.albvar.av, dat.alb~dat.month+dat.yr+id, FUN='mean')

all.albvar$dat.month<-as.factor(all.albvar$dat.month)
all.albvar.av$dat.month<-as.factor(all.albvar.av$dat.month)

nosoy<-c(2008, 2009, 2011, 2012, 2014, 2015, 2017, 2018, 2020, 2021)

all.albvar.ns<-all.albvar[all.albvar$dat.yr%in%nosoy,] #removing soy years
all.albvar.sbtime<-all.albvar.ns[all.albvar.ns$dat.yr>2017,] #no soy and since sorghum existed, i.e. 2018+

all.albvar.gm<-all.albvar
all.albvar.gm$id[which(all.albvar$id%in%c("zmc", "zmb", "sb", "zm")&!all.albvar$dat.yr%in%nosoy)]<-"gm"


all.albvar.av.gm<-all.albvar.av
all.albvar.av.gm$id[which(all.albvar.av$id%in%c("zmc", "zmb", "sb", "zm")&!all.albvar.av$dat.yr%in%nosoy)]<-"gm"


library(ggplot2)

ggplot(all.albvar.ns, aes(x=dat.month, y=dat.alb, fill=id),) + 
    geom_boxplot(outlier.size=0.1)+
    theme_minimal()+
    scale_fill_manual(values=c("light blue", "blue", "forest green", "light pink", "orange", "yellow"))


ggplot(all.albvar.av, aes(x=dat.month, y=dat.alb, fill=id),) + 
  geom_boxplot(outlier.size=0.1)+
  theme_minimal()+
  scale_fill_manual(values=c("blue",  "forest green", "light pink", "orange"))


ggplot(all.albvar.gm, aes(x=dat.month, y=dat.alb, fill=id),) + 
  geom_boxplot(outlier.size=0.1)+
  theme_minimal()+
  scale_fill_manual(values=c("light green", "light blue", "blue", "forest green", "light pink", "orange", "yellow"))

ggplot(all.albvar.av.gm, aes(x=dat.month, y=dat.alb, fill=id),) + 
  geom_boxplot(outlier.size=0.1)+
  theme_minimal()+
  scale_fill_manual(values=c("light green",  "blue", "forest green", "light pink", "orange"))


}
#####


#Bowen ratio####

bowenize<-function(dat, window=1){
  dat<-subtime(dat)
  dat.bow<-dat$Fh/dat$Fe; dat.bow[dat$Fh<0|dat$Fe<0|dat.bow>8]<-NA
  dat.doy<-as.numeric(format(dat$xlDateTime, "%m"))
  dat.d.bow<-aggregate(dat.bow, by=list(dat.doy), FUN='mean', na.rm=TRUE)
  dat.bow.sm<-rollapply(dat.d.bow$x, width=window, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
  
  return(dat.bow.sm)
  
}


sorg.bow<-bowenize(sorg.merge)
maize.bow<-bowenize(maize.merge);maize.c.bow<-bowenize(maize.c.merge)
misc.bow<-bowenize(misc.merge);misc.c.bow<-bowenize(misc.c.merge)
switch.bow<-bowenize(switchgrass)


#aggregate plant types

#aggregate plant types
if(exists('maize.c.bow')&exists('misc.c.bow')){
  misc.bow<-(misc.bow+misc.c.bow)/2
  maize.bow<-(maize.bow+maize.c.bow)/2
}else{
  misc.bow<-misc.bow
  maize.bow<-maize.bow
}

if(varplots==TRUE){
  
  #Variability
  brvar<-function(dat, id){
    #dat<-subtime(dat)
    dat.bow<-dat$Fh/dat$Fe; dat.bow[dat$Fh<0|dat$Fe<0|dat.bow>8]<-NA
    #dat.bow<-(1/(1+dat.bow)) #evaporative fraction
    dat.month<-as.numeric(format(dat$xlDateTime, "%m"))
    dat.yr<-as.numeric(format(dat$xlDateTime, "%Y"))
    dat.agg<-aggregate(dat.bow~dat.month+dat.yr, FUN='mean', na.rm=TRUE)
    #dat.count<-aggregate(dat.st~dat.month+dat.yr, FUN=length)
    #dat.agg$dat.st[which(dat.count$dat.st<1340)]<-NA #NAN months with few data
    dat.agg$id<-(id)
    
    return(dat.agg)
  }
  
  
  sorg.brvar<-brvar(sorg.merge, "sb")
  misc.brvar<-brvar(misc.merge,"mgb")
  misc.c.brvar<-brvar(misc.c.merge, "mgc")
  maize.brvar<-brvar(maize.merge, "zmb")
  maize.c.brvar<-brvar(maize.c.merge, "zmc")
  switch.brvar<-brvar(switchgrass, "sw")
  
  all.brvar<-rbind(sorg.brvar, misc.brvar, misc.c.brvar, maize.brvar, maize.c.brvar, switch.brvar)
  all.brvar$dat.month<-as.factor(all.brvar$dat.month)
  
  
  nosoy<-c(2008, 2009, 2011, 2012, 2014, 2015, 2017, 2018, 2020, 2021)
  
  all.brvar.ns<-all.brvar[all.brvar$dat.yr%in%nosoy,] #removing soy years
  
  all.brvar.soy<-all.brvar; all.brvar.soy$id[!all.brvar.soy$dat.yr%in%nosoy&all.brvar.soy$id%in%c("zmc", "zmb", "sb")]<-"gm" #identify soy years
  all.brvar.fs<-all.brvar.soy; all.brvar.fs$id[all.brvar.fs$id%in%c("zmc", "zmb")]<-"zm"; all.brvar.fs$id[all.brvar.fs$id%in%c("mgc", "mgb")]<-"mg" #group feedstocks
  all.brvar.rot<-all.brvar; all.brvar.rot$id[all.brvar.rot$id%in%c("zmc", "zmb")]<-"zm"; all.brvar.rot$id[all.brvar.rot$id%in%c("mgc", "mgb")]<-"mg"
  
  all.brvar.sbtime<-all.brvar.ns[all.brvar.ns$dat.yr>2017,] #no soy and since sorghum existed, i.e. 2018+
  all.brvar.nosbtime<-all.brvar.ns[all.brvar.ns$dat.yr<2017,] #no soy and since sorghum existed, i.e. 2018+
  
  
  set1<-c( "blue", "light blue","forest green", "light pink", "orange", "yellow")
  set2<-c( "light blue",  "light pink", "orange") #nosbtime
  set3<-c("blue", "light blue","forest green", "orange", "yellow") #sbtime
  set4<-c( "light green", "blue", "light blue","forest green", "light pink", "orange", "yellow")
  set5<-c("light green","light blue", "forest green", "light pink", "yellow") #merged feedstocks, soy out (all.brvar.fs)
  set6<-c("light blue", "forest green", "light pink", "yellow") #merged feedstocks, soy in (all.brvar.rot)
  
  
  
  dat<-all.brvar.rot; set<-set6
  
  ggplot(dat, aes(x=dat.month, y=dat.bow, fill=id),) + 
    geom_boxplot(outlier.size=0.1)+
    theme_minimal()+
    scale_fill_manual(values=set)
  
  
  dat$dat.month<-as.numeric(dat$dat.month) #month must be a number for line graphs
  
  ggplot(dat, aes(x=dat.month, y=dat.bow, color=id, fill=id),) + 
    geom_smooth()+
    theme_minimal()+
    scale_fill_manual(values=set)+
    scale_color_manual(values=set)
}


#####

#Ts-Ta #####

#Some Tc columns are incomplete; more data is found in the Tc_3m column.
switchgrass$Tc<-switchgrass$Tc_3m
misc.merge$Tc[is.na(misc.merge$Tc)]<-misc.merge$Tc_3m[is.na(misc.merge$Tc)]
maize.merge$Tc[is.na(maize.merge$Tc)]<-maize.merge$Tc_3m[is.na(maize.merge$Tc)]


stemp<-function(dat, window=1){
  dat<-subtime(dat)
  dat.st<-dat$Tc-dat$Ta;dat.st[dat.st<(-20)|dat.st>20]<-NA
  dat.doy<-as.numeric(format(dat$xlDateTime, "%m"))
  #plot(dat.st~dat.doy)
  dat.d.st<-aggregate(dat.st, by=list(dat.doy), FUN='mean', na.rm=TRUE)
  dat.st.sm<-rollapply(dat.d.st$x, width=window, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
  
  return(dat.st.sm)
  
}



sorg.st<-stemp(sorg.merge)
maize.st<-stemp(maize.merge);maize.c.st<-stemp(maize.c.merge)
misc.st<-stemp(misc.merge);misc.c.st<-stemp(misc.c.merge)
switch.st<-stemp(switchgrass)


if(varplots==TRUE){

#Variability
stvar<-function(dat, id){
  #dat<-subtime(dat)
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


set1<-c( "blue", "light blue","forest green", "light pink", "orange", "yellow")
set2<-c( "light blue",  "light pink", "orange")
set3<-c("blue", "light blue","forest green", "orange", "yellow")
  
ggplot(all.stvar, aes(x=dat.month, y=dat.st, fill=id),) + 
  geom_boxplot(outlier.size=0.1)+
  theme_minimal()+
  scale_fill_manual(values=set1)


# #what's up with maize control?
# s<-sort(sample(1:nrow(maize.c.merge), 15000))
# 
# zmc.test<-maize.c.merge[s,]
# 
# zmb.test<-maize.merge[as.numeric(format(maize.merge$xlDateTime, "%Y"))>2016,]
# zmb.test<-zmb.test[s,]
# 
# 
# width<-500
# 
# par(mfrow=c(1,1))
# plot(rollapply(zmc.test$Tc-zmc.test$Ta, width=width, FUN='mean', na.rm=TRUE), type='l', col="yellow")
# lines(rollapply(zmb.test$Tc-zmb.test$Ta, width=width, FUN='mean', na.rm=TRUE), col='orange')
# 
# plot(rollapply(zmc.test$Tc, width=width, FUN='mean', na.rm=TRUE), type='l', main="surf t", col="yellow")
# lines(rollapply(zmb.test$Tc, width=width, FUN='mean', na.rm=TRUE), col='orange')
# 
# plot(rollapply(zmc.test$Ta, width=width, FUN='mean', na.rm=TRUE), type='l', main= "air t", col="yellow")
# lines(rollapply(zmb.test$Ta, width=width, FUN='mean', na.rm=TRUE), col='orange')
# 
# 
# plot(rollapply(zmc.test$Ta, width=width, FUN='mean', na.rm=TRUE)-rollapply(zmb.test$Ta, width=width, FUN='mean', na.rm=TRUE), type='l', ylim=c(-2,2), col='blue')
# lines(rollapply(zmc.test$Tc, width=width, FUN='mean', na.rm=TRUE)-rollapply(zmb.test$Tc, width=width, FUN='mean', na.rm=TRUE), type='l', ylim=c(-2,2), col='green')
}

#####

# H #####


hflux<-function(dat, window=1){
  dat<-subtime(dat)
  dat.fh<-dat$Fh
  dat.doy<-as.numeric(format(dat$xlDateTime, "%m"))
  #plot(dat.fh~dat.doy)
  dat.d.fh<-aggregate(dat.fh, by=list(dat.doy), FUN='mean', na.rm=TRUE)
  dat.fh.sm<-rollapply(dat.d.fh$x, width=window, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
  
  return(dat.fh.sm)
  
}


sorg.fh.sm<-hflux(sorg.merge)
sorg.fh.sm<-hflux(sorg.merge)
maize.fh.sm<-hflux(maize.merge);maize.c.fh.sm<-hflux(maize.c.merge)
misc.fh.sm<-hflux(misc.merge);misc.c.fh.sm<-hflux(misc.c.merge)
switch.fh.sm<-hflux(switchgrass)


#aggregate plant types

#aggregate plant types
if(exists('maize.c.fh.sm')&exists('misc.c.fh.sm')){
  misc.fh<-(misc.fh.sm+misc.c.fh.sm)/2
  maize.fh<-(maize.fh.sm+maize.c.fh.sm)/2
}else{
  misc.fh<-misc.fh.sm
  maize.fh<-maize.fh.sm
}



if(varplots==TRUE){
  
  #Variability
  hvar<-function(dat, id){
    #dat<-subtime(dat)
    dat.h<-dat$Fh;#dat.le[dat.le<(-200)|dat.le>1000]<-NA
    dat.month<-as.numeric(format(dat$xlDateTime, "%m"))
    dat.yr<-as.numeric(format(dat$xlDateTime, "%Y"))
    dat.agg<-aggregate(dat.h~dat.month+dat.yr, FUN='mean', na.rm=TRUE)
    #dat.count<-aggregate(dat.st~dat.month+dat.yr, FUN=length)
    #dat.agg$dat.st[which(dat.count$dat.st<1340)]<-NA #NAN months with few data
    dat.agg$id<-(id)
    
    return(dat.agg)
  }
  
  
  sorg.hvar<-hvar(sorg.merge, "sb")
  misc.hvar<-hvar(misc.merge,"mgb")
  misc.c.hvar<-hvar(misc.c.merge, "mgc")
  maize.hvar<-hvar(maize.merge, "zmb")
  maize.c.hvar<-hvar(maize.c.merge, "zmc")
  switch.hvar<-hvar(switchgrass, "sw")
  
  all.hvar<-rbind(sorg.hvar, misc.hvar, misc.c.hvar, maize.hvar, maize.c.hvar, switch.hvar)
  all.hvar$dat.month<-as.factor(all.hvar$dat.month)
  
  
  nosoy<-c(2008, 2009, 2011, 2012, 2014, 2015, 2017, 2018, 2020, 2021)
  
  #Complete time period
  #Unmerged feedstocks, soy removed
  all.hvar.ns<-all.hvar[all.hvar$dat.yr%in%nosoy,] #removing soy years
  #Unmerged feedstocks, soy included and symbolized separately
  all.hvar.soy<-all.hvar; all.hvar.soy$id[!all.hvar.soy$dat.yr%in%nosoy&all.hvar.soy$id%in%c("zmc", "zmb", "sb")]<-"gm" #identify soy years
  #Merged feedstocks, soy symbolized separately
  all.hvar.fs<-all.hvar.soy; all.hvar.fs$id[all.hvar.fs$id%in%c("zmc", "zmb")]<-"zm"; all.hvar.fs$id[all.hvar.fs$id%in%c("mgc", "mgb")]<-"mg" #group feedstocks
  #Merged feedstocks, soy included in rotation
  all.hvar.rot<-all.hvar; all.hvar.rot$id[all.hvar.rot$id%in%c("zmc", "zmb")]<-"zm"; all.hvar.rot$id[all.hvar.rot$id%in%c("mgc", "mgb")]<-"mg"
  
  ## Separated time periods
  #Soy removed
  #Unmerged...
  hvar.post<-all.hvar.ns[all.hvar.ns$dat.yr>2017,] #no soy and since sorghum existed, i.e. 2018+
  hvar.pre<-all.hvar.ns[all.hvar.ns$dat.yr<2017,] #no soy and since sorghum existed
  #Merged...
  hvar.post.fs<-hvar.post;hvar.post.fs$id[hvar.post.fs$id%in%c("zmc", "zmb")]<-"zm"; hvar.post.fs$id[hvar.post.fs$id%in%c("mgc", "mgb")]<-"mg" #no soy, merged feedstocks

  #Soy included
  #Unmerged feedstocks, soy in rotation
  hvar.post<-all.hvar[all.hvar$dat.yr>2017,] #soy in rotation and since sorghum existed, i.e. 2018+
  hvar.pre<-all.hvar[all.hvar$dat.yr<2017,] #soy in rotation and since sorghum existed, i.e. 2018+
  #Merged feedstocks, soy in rotation
  hvar.post.fs<-hvar.post;hvar.post.fs$id[hvar.post.fs$id%in%c("zmc", "zmb")]<-"zm"; hvar.post.fs$id[hvar.post.fs$id%in%c("mgc", "mgb")]<-"mg" #group feedstocks
  hvar.pre.fs<-hvar.pre;hvar.pre.fs$id[hvar.pre.fs$id%in%c("zmc", "zmb")]<-"zm"; hvar.pre.fs$id[hvar.pre.fs$id%in%c("mgc", "mgb")]<-"mg" #group feedstocks
  #Merged feedstocsk, soy symbolized separately
  hvar.post.fs.soy<-hvar.post.fs;hvar.post.fs$id[!hvar.post.fs$dat.yr%in%nosoy&hvar.post.fs$id%in%c("zm", "sb")]<-"gm" #identify soy years
  hvar.pre.fs.soy<-hvar.pre.fs;hvar.pre.fs$id[!hvar.pre.fs$dat.yr%in%nosoy&hvar.pre.fs$id%in%c("zm")]<-"gm" #identify soy years
  
  #Old color setting system####
  # set1<-c( "blue", "light blue","forest green", "light pink", "orange", "yellow") #all years, no soy or soy in rotation
  # set2<-c( "light blue",  "light pink", "orange") #pre sorghum, no soy or soy in rotation
  # set3<-c("blue", "light blue","forest green", "orange", "yellow") #post sorghum, unmerged feedstocks, no soy or soy in rotation
  # set4<-c( "light green", "blue", "light blue","forest green", "light pink", "orange", "yellow") #all years, soy symbolized
  # set5<-c("light green","light blue", "forest green", "light pink", "yellow") #merged feedstocks, soy out
  # set6<-c("light blue", "forest green", "light pink", "yellow") #merged feedstocks, soy in 
  # set7<-c("light green", "light blue",  "light pink", "orange")# pre sorghum, soy symbolized
  # set8<-c("light green","blue", "light blue","forest green", "orange", "yellow")# post sorghum, unmerged feedstocks, soy symbolized
  # set9<-c("light green","light blue", "forest green", "yellow")# post sorghum, merged feedstocks, soy symbloized
  #####
  
  #Plots
  
  #Shared color palette
  pal <- c("zm" = "yellow","zmc" = "yellow", "zmb" = "orange", "mg" = "light blue","mgc" = "light blue", 
           "mgb" = "blue", "sw" = "light pink", "sb" = "forestgreen","gm" = "light green")
  
  #All years
  dat<-all.hvar.fs

  ggplot(dat, aes(x=dat.month, y=dat.h, fill=id),) + 
    geom_boxplot(outlier.size=0.1)+
    theme_minimal()+
    scale_fill_manual(values=pal)+
    ylim(min=-5, max=100)
  
  #Pre-2017
  dat<-hvar.pre.fs.soy
  
  ggplot(dat, aes(x=dat.month, y=dat.h, fill=id),) + 
    geom_boxplot(outlier.size=0.1)+
    theme_minimal()+
    scale_fill_manual(values=pal)+
    ylim(min=-5, max=100)
  
  #Post-2017
  dat<-hvar.post.fs.soy
  
  ggplot(dat, aes(x=dat.month, y=dat.h, fill=id),) + 
    geom_boxplot(outlier.size=0.1)+
    theme_minimal()+
    scale_fill_manual(values=pal)+
    ylim(min=-5, max=100)
  
}

#####

#LE#####

leflux<-function(dat, window=1){
  dat<-subtime(dat)
  dat.fe<-dat$Fe
  dat.doy<-as.numeric(format(dat$xlDateTime, "%m"))
  #plot(dat.fe~dat.doy)
  dat.d.fe<-aggregate(dat.fe, by=list(dat.doy), FUN='mean', na.rm=TRUE)
  dat.fe.sm<-rollapply(dat.d.fe$x, width=window, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
  
  return(dat.fe.sm)
  
}

sorg.fe.sm<-leflux(sorg.merge)
sorg.fe.sm<-leflux(sorg.merge)
maize.fe.sm<-leflux(maize.merge);maize.c.fe.sm<-leflux(maize.c.merge)
misc.fe.sm<-leflux(misc.merge);misc.c.fe.sm<-leflux(misc.c.merge)
switch.fe.sm<-leflux(switchgrass)

#aggregate plant types

#aggregate plant types
if(exists('maize.c.fe.sm')&exists('misc.c.fe.sm')){
  misc.fe<-(misc.fe.sm+misc.c.fe.sm)/2
  maize.fe<-(maize.fe.sm+maize.c.fe.sm)/2
}else{
  misc.fe<-misc.fe.sm
  maize.fe<-maize.fe.sm
}



if(varplots==TRUE){
  
  #Variability
  levar<-function(dat, id){
    #dat<-subtime(dat)
    dat.le<-dat$Fe;#dat.le[dat.le<(-200)|dat.le>1000]<-NA
    dat.month<-as.numeric(format(dat$xlDateTime, "%m"))
    dat.yr<-as.numeric(format(dat$xlDateTime, "%Y"))
    dat.agg<-aggregate(dat.le~dat.month+dat.yr, FUN='mean', na.rm=TRUE)
    #dat.count<-aggregate(dat.st~dat.month+dat.yr, FUN=length)
    #dat.agg$dat.st[which(dat.count$dat.st<1340)]<-NA #NAN months with few data
    dat.agg$id<-(id)
    
    return(dat.agg)
  }
  
  
  sorg.levar<-levar(sorg.merge, "sb")
  misc.levar<-levar(misc.merge,"mgb")
  misc.c.levar<-levar(misc.c.merge, "mgc")
  maize.levar<-levar(maize.merge, "zmb")
  maize.c.levar<-levar(maize.c.merge, "zmc")
  switch.levar<-levar(switchgrass, "sw")
  
  all.levar<-rbind(sorg.levar, misc.levar, misc.c.levar, maize.levar, maize.c.levar, switch.levar)
  all.levar$dat.month<-as.factor(all.levar$dat.month)
  
  
  nosoy<-c(2008, 2009, 2011, 2012, 2014, 2015, 2017, 2018, 2020, 2021)
  
  #Complete time period
  #Unmerged feedstocks, soy removed
  all.levar.ns<-all.levar[all.levar$dat.yr%in%nosoy,] #removing soy years
  #Unmerged feedstocks, soy included and symbolized separately
  all.levar.soy<-all.levar; all.levar.soy$id[!all.levar.soy$dat.yr%in%nosoy&all.levar.soy$id%in%c("zmc", "zmb", "sb")]<-"gm" #identify soy years
  #Merged feedstocks, soy symbolized separately
  all.levar.fs<-all.levar.soy; all.levar.fs$id[all.levar.fs$id%in%c("zmc", "zmb")]<-"zm"; all.levar.fs$id[all.levar.fs$id%in%c("mgc", "mgb")]<-"mg" #group feedstocks
  #Merged feedstocks, soy included in rotation
  all.levar.rot<-all.levar; all.levar.rot$id[all.levar.rot$id%in%c("zmc", "zmb")]<-"zm"; all.levar.rot$id[all.levar.rot$id%in%c("mgc", "mgb")]<-"mg"
  
  ## Separated time periods
  #Soy removed
  #Unmerged...
  levar.post<-all.levar.ns[all.levar.ns$dat.yr>2017,] #no soy and since sorghum existed, i.e. 2018+
  levar.pre<-all.levar.ns[all.levar.ns$dat.yr<2017,] #no soy and since sorghum existed
  #Merged...
  levar.post.fs<-levar.post;levar.post.fs$id[levar.post.fs$id%in%c("zmc", "zmb")]<-"zm"; levar.post.fs$id[levar.post.fs$id%in%c("mgc", "mgb")]<-"mg" #no soy, merged feedstocks
  
  #Soy included
  #Unmerged feedstocks, soy in rotation
  levar.post<-all.levar[all.levar$dat.yr>2017,] #soy in rotation and since sorghum existed, i.e. 2018+
  levar.pre<-all.levar[all.levar$dat.yr<2017,] #soy in rotation and since sorghum existed, i.e. 2018+
  #Merged feedstocks, soy in rotation
  levar.post.fs<-levar.post;levar.post.fs$id[levar.post.fs$id%in%c("zmc", "zmb")]<-"zm"; levar.post.fs$id[levar.post.fs$id%in%c("mgc", "mgb")]<-"mg" #group feedstocks
  levar.pre.fs<-levar.pre;levar.pre.fs$id[levar.pre.fs$id%in%c("zmc", "zmb")]<-"zm"; levar.pre.fs$id[levar.pre.fs$id%in%c("mgc", "mgb")]<-"mg" #group feedstocks
  #Merged feedstocsk, soy symbolized separately
  levar.post.fs.soy<-levar.post.fs;levar.post.fs$id[!levar.post.fs$dat.yr%in%nosoy&levar.post.fs$id%in%c("zm", "sb")]<-"gm" #identify soy years
  levar.pre.fs.soy<-levar.pre.fs;levar.pre.fs$id[!levar.pre.fs$dat.yr%in%nosoy&levar.pre.fs$id%in%c("zm")]<-"gm" #identify soy years
  

  #Plots
  
  #Shared color palette
  pal <- c("zm" = "yellow","zmc" = "yellow", "zmb" = "orange", "mg" = "light blue","mgc" = "light blue", 
           "mgb" = "blue", "sw" = "light pink", "sb" = "forestgreen","gm" = "light green")
  
  #All years
  dat<-all.levar.fs
  
  ggplot(dat, aes(x=dat.month, y=dat.le, fill=id),) + 
    geom_boxplot(outlier.size=0.1)+
    theme_minimal()+
    scale_fill_manual(values=pal)+
    ylim(min=-5, max=200)
  
  #Pre-2017
  dat<-levar.pre.fs.soy
  
  ggplot(dat, aes(x=dat.month, y=dat.le, fill=id),) + 
    geom_boxplot(outlier.size=0.1)+
    theme_minimal()+
    scale_fill_manual(values=pal)+
    ylim(min=-5, max=200)
  
  #Post-2017
  dat<-levar.post.fs.soy
  
  ggplot(dat, aes(x=dat.month, y=dat.le, fill=id),) + 
    geom_boxplot(outlier.size=0.1)+
    theme_minimal()+
    scale_fill_manual(values=pal)+
    ylim(min=-5, max=200)
   
}
##### 



#G#####

gflux<-function(dat, window=1){
  dat<-subtime(dat)
  dat.fg<-dat$Fg
  dat.doy<-as.numeric(format(dat$xlDateTime, "%m"))
  #plot(dat.fg~dat.doy)
  dat.d.fg<-aggregate(dat.fg, by=list(dat.doy), FUN='mean', na.rm=TRUE)
  dat.fg.sm<-rollapply(dat.d.fg$x, width=window, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
  
  return(dat.fg.sm)
  
}

sorg.fg.sm<-gflux(sorg.merge)
sorg.fg.sm<-gflux(sorg.merge)
maize.fg.sm<-gflux(maize.merge);maize.c.fg.sm<-gflux(maize.c.merge)
misc.fg.sm<-gflux(misc.merge);misc.c.fg.sm<-gflux(misc.c.merge)
switch.fg.sm<-gflux(switchgrass)

#aggregate plant types
if(exists('maize.c.fg.sm')&exists('misc.c.fg.sm')){
  misc.fg<-(misc.fg.sm+misc.c.fg.sm)/2
  maize.fg<-(maize.fg.sm+maize.c.fg.sm)/2
}else{
  misc.fg<-misc.fg.sm
  maize.fg<-maize.fg.sm
}



#####



###PLOTS#####


##Each site#####

par(mfrow=c(2,2), mar=c(4,4,1,1))

#Albedo
plot(maize.alb.sm, type='l', lwd=2, col='orange', xlab='month', main="albedo", ylim=c(0.1, 0.4))
lines(sorg.alb.sm,col='forest green' ,lwd=2)
lines(maize.c.alb.sm, col='gold' ,lwd=2)
lines(misc.alb.sm, col='blue' ,lwd=2)
lines(misc.c.alb.sm, col='light blue' ,lwd=2)
lines(switch.alb.sm, col='pink' ,lwd=2)

#Bowen Ratio
plot(maize.bow, type='l', lwd=2, col='orange', xlab='month', ylim=c(0.1, 5), ylab="bowen ratio", main="bowen ratio")
lines(sorg.bow, col='forest green', lwd=2)
lines(maize.c.bow, col='gold', lwd=2)
lines(misc.bow, col='blue', lwd=2)
lines(misc.c.bow, col='light blue', lwd=2)
lines(switch.bow, col='pink', lwd=2)

#Sensible Heat
plot(maize.fh.sm, type='l', lwd=2, col='orange', xlab='month', main="sensible heat", ylim=c(0,50))
lines(sorg.fh.sm, col='forest green' ,lwd=2)
lines(maize.c.fh.sm, col='gold' ,lwd=2)
lines(misc.fh.sm, col='blue' ,lwd=2)
lines(misc.c.fh.sm, col='light blue' ,lwd=2)
lines(switch.fh.sm, col='pink' ,lwd=2)

#Latent Heat
plot(maize.fe.sm, type='l', lwd=2, col='orange', xlab='month', main="latent heat", ylim=c(0,150))
lines(sorg.fe.sm, col='forest green' ,lwd=2)
lines(maize.c.fe.sm, col='gold' ,lwd=2)
lines(misc.fe.sm, col='blue' ,lwd=2)
lines(misc.c.fe.sm, col='light blue' ,lwd=2)
lines(switch.fe.sm, col='pink' ,lwd=2)

#Ground heat
plot(maize.fg.sm, type='l', lwd=2, col='orange', xlab='month', main="ground heat", ylim=c(-10,10))
lines(sorg.fg.sm, col='forest green' ,lwd=2)
lines(maize.c.fg.sm, col='gold' ,lwd=2)
lines(misc.fg.sm, col='blue' ,lwd=2)
lines(misc.c.fg.sm, col='light blue' ,lwd=2)
lines(switch.fg.sm, col='pink' ,lwd=2)



#Ts-Ta
par(mfrow=c(1,1))
plot(maize.st, type='l', lwd=2, col='orange', xlab='month', main="Surface - Air temperature differential", ylim=c(-1.5, 3))
lines(sorg.st, col='forest green' ,lwd=2)
lines(maize.c.st, col='gold' ,lwd=2)
lines(misc.st, col='blue' ,lwd=2)
lines(misc.c.st, col='light blue' ,lwd=2)
lines(switch.st, col='pink' ,lwd=2)
abline(h=0)
#####

##Plant types aggregated#####

par(mfrow=c(2,2), mar=c(4,4,1,1))

#Albedo
plot(maize.alb, type='l', col='orange', xlab='month', ylab="albedo", lwd=2.5, ylim=c(0.1, 0.4))
lines(sorg.alb.soil,col='forest green', lwd=2.5, lty=2 )
lines(sorg.alb.sm, col='forest green', lwd=2.5)
lines(misc.alb, col='light blue', lwd=2.5)
lines(switch.alb.sm, col='light pink', lwd=2.5)

legend(2, .4, cex=0.8, bty='n',legend=c("Miscanthus", "Maize", "Sorghum","Switchgrass"), col=c('light blue', 'orange','forest green', 'light pink'), lwd=2, ncol=2)


#Bowen ratio
plot(maize.bow, type='l', col='orange', xlab='month', ylab="bowen ratio", lwd=2.5, ylim=c(0,3))
lines(sorg.bow, col='forest green', lwd=2.5)
lines(misc.bow, col='light blue', lwd=2.5)
lines(switch.bow, col='light pink', lwd=2.5)

legend(2, 3, cex=0.8, bty='n',legend=c("Miscanthus", "Maize", "Sorghum","Switchgrass"), col=c('light blue', 'orange','forest green', 'light pink'), lwd=2, ncol=2)


#Sensible heat

plot(maize.fh, type='l', col='orange', xlab='month', ylab="sensible heat", lwd=2.5, ylim=c(0, 60))
lines(sorg.fh.sm, col='forest green', lwd=2.5)
lines(misc.fh, col='light blue', lwd=2.5)
lines(switch.fh.sm, col='light pink', lwd=2.5)

legend(2, 60, cex=0.8, bty='n',legend=c("Miscanthus", "Maize", "Sorghum","Switchgrass"), col=c('light blue', 'orange','forest green', 'light pink'), lwd=2, ncol=2)

#Latent heat

plot(maize.fe, type='l', col='orange', xlab='month', ylab="latent heat", lwd=2.5, ylim=c(0,150))
lines(sorg.fe.sm, col='forest green', lwd=2.5)
lines(misc.fe, col='light blue', lwd=2.5)
lines(switch.fe.sm, col='light pink', lwd=2.5)

legend(2, 150, cex=0.8, bty='n',legend=c("Miscanthus", "Maize", "Sorghum","Switchgrass"), col=c('light blue', 'orange','forest green', 'light pink'), lwd=2, ncol=2)


#Ground heat

plot(maize.fg, type='l', col='orange', xlab='month', ylab="ground heat", lwd=2.5, ylim=c(-10,10))
lines(sorg.fg.sm, col='forest green', lwd=2.5)
lines(misc.fg, col='light blue', lwd=2.5)
lines(switch.fg.sm, col='light pink', lwd=2.5)

legend(6, 10, cex=0.8, bty='n',legend=c("Miscanthus", "Maize", "Sorghum","Switchgrass"), col=c('light blue', 'orange','forest green', 'light pink'), lwd=2, ncol=2)



## Highlight contrast between annuals #####

par(mfrow=c(2,2), mar=c(4,4,1,1))

#Albedo
plot(maize.alb, type='l', col='orange', xlab='month', ylab="albedo", lwd=2.5, ylim=c(0.1, 0.4))
lines(sorg.alb.soil,col='forest green', lwd=2.5, lty=2 )
lines(sorg.alb.sm, col='forest green', lwd=2.5)

legend(2, .4, cex=0.8, bty='n',legend=c("Maize", "Sorghum"), col=c('orange','forest green'), lwd=2, ncol=2)

#Bowen ratio
plot(maize.bow, type='l', col='orange', xlab='month', ylab="bowen ratio", lwd=2.5, ylim=c(0,3))
lines(sorg.bow, col='forest green', lwd=2.5)

legend(2, 3, cex=0.8, bty='n',legend=c( "Maize", "Sorghum"), col=c( 'orange','forest green'), lwd=2, ncol=2)

#Sensible heat

plot(maize.fh, type='l', col='orange', xlab='month', ylab="sensible heat", lwd=2.5, ylim=c(0, 60))
lines(sorg.fh.sm, col='forest green', lwd=2.5)

legend(2, 60, cex=0.8, bty='n',legend=c("Maize", "Sorghum"), col=c( 'orange','forest green'), lwd=2, ncol=2)

#Latent heat

plot(maize.fe, type='l', col='orange', xlab='month', ylab="latent heat", lwd=2.5, ylim=c(0,150))
lines(sorg.fe.sm, col='forest green', lwd=2.5)

legend(2, 150, cex=0.8, bty='n',legend=c( "Maize", "Sorghum"), col=c('orange','forest green'), lwd=2, ncol=2)

#####

##Highlight contrast between perennials#####

par(mfrow=c(2,2), mar=c(4,4,1,1))

#Albedo
plot(misc.alb, type='l', col='light blue', xlab='month', ylab="albedo", lwd=2.5, ylim=c(0.1, 0.4))
lines(switch.alb.sm, col='light pink', lwd=2.5)

legend(2, .4, cex=0.8, bty='n',legend=c("Miscanthus","Switchgrass"), col=c('light blue','light pink'), lwd=2, ncol=2)


#Bowen ratio
plot(misc.bow, type='l', col='light blue', xlab='month', ylab="bowen ratio", lwd=2.5, ylim=c(0,3))
lines(switch.bow, col='light pink', lwd=2.5)

legend(2, 3, cex=0.8, bty='n',legend=c("Miscanthus", "Switchgrass"), col=c('light blue', 'light pink'), lwd=2, ncol=2)


#Sensible heat

plot(misc.fh, type='l', col='light blue', xlab='month', ylab="sensible heat", lwd=2.5, ylim=c(0, 60))
lines(switch.fh.sm, col='light pink', lwd=2.5)

legend(2, 60, cex=0.8, bty='n',legend=c("Miscanthus", "Switchgrass"), col=c('light blue', 'light pink'), lwd=2, ncol=2)

#Latent heat

plot(misc.fe, type='l', col='light blue', xlab='month', ylab="latent heat", lwd=2.5, ylim=c(0,150))
lines(switch.fe.sm, col='light pink', lwd=2.5)

legend(2, 150, cex=0.8, bty='n',legend=c("Miscanthus", "Switchgrass"), col=c('light blue', 'light pink'), lwd=2, ncol=2)


#####


##conversion from maize#####

par(mfrow=c(2,2), mar=c(4,4,1,1))

#Albedo conversion

plot(sorg.alb.sm-maize.alb, type='l', col='forest green', xlab='month', ylab="albedo change (convert from maize)", lwd=2.5, ylim=c(-0.2, 0.2))
lines(sorg.alb.soil-maize.alb,col='forest green', lwd=2.5, lty=2 )
lines(misc.alb-maize.alb, col='light blue', lwd=2.5)
lines(switch.alb.sm-maize.alb, col='light pink', lwd=2.5)
abline(h=0, lty=3)

legend(1, .2, cex=0.8, bty='n',legend=c("Miscanthus", "Switchgrass", "Sorghum"), col=c('light blue', 'light pink','forest green'), lwd=2, ncol=2)

#Bowen ratio conversion
plot(misc.bow-maize.bow, type='l', col='light blue', xlab='month', ylab="bowen ratio change (convert from maize)", lwd=2.5, ylim=c(-2,2))
lines(sorg.bow-maize.bow, col='forest green', lwd=2.5)
lines(switch.bow-maize.bow, col='light pink', lwd=2.5)
abline(h=0, lty=3)

legend(1, 2, cex=0.8, bty='n',legend=c("Miscanthus", "Switchgrass", "Sorghum"), col=c('light blue', 'light pink','forest green'), lwd=2, ncol=2)


#Sensible heat conversion

plot(misc.fh-maize.fh, type='l', col='light blue', xlab='month', ylab="sensible heat change (convert from maize)", lwd=2.5, ylim=c(-30, 30))
lines(sorg.fh.sm-maize.fh, col='forest green', lwd=2.5)
lines(switch.fh.sm-maize.fh, col='light pink', lwd=2.5)
abline(h=0, lty=3)

legend(1, 30, cex=0.8, bty='n',legend=c("Miscanthus", "Switchgrass", "Sorghum"), col=c('light blue', 'light pink','forest green'), lwd=2, ncol=2)


#Latent heat conversion

plot(misc.fe-maize.fe, type='l', col='light blue', xlab='month', ylab="latent heat change (convert from maize)", lwd=2.5, ylim=c(-50,50))
lines(sorg.fe.sm-maize.fe, col='forest green', lwd=2.5)
lines(switch.fe.sm-maize.fe, col='light pink', lwd=2.5)
abline(h=0, lty=3)

legend(1, 50, cex=0.8, bty='n',legend=c("Miscanthus", "Switchgrass", "Sorghum"), col=c('light blue', 'light pink','forest green'), lwd=2, ncol=2)

#Ground heat conversion

plot(misc.fg-maize.fg, type='l', col='light blue', xlab='month', ylab="ground heat change (convert from maize)", lwd=2.5, ylim=c(-5,5))
lines(sorg.fg.sm-maize.fg, col='forest green', lwd=2.5)
lines(switch.fg.sm-maize.fg, col='light pink', lwd=2.5)
abline(h=0, lty=3)

legend(1, 5, cex=0.8, bty='n',legend=c("Miscanthus", "Switchgrass", "Sorghum"), col=c('light blue', 'light pink','forest green'), lwd=2, ncol=2)

#####


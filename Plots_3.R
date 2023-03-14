#Figures for Manuscript
if(!exists("sorg.fg.sm")){source(EnergyFluxes_2.1)} 
#Most stuff from  energy fluxes has been ported over here
#Should figure out how to remove from pipeline eventually

#Carbon fluxes, incl. cumulative plot####

#squiggle plot (requires "long record" be run)
par(bty='n', mfrow=c(1,1))

decyr<-function(date){
  yr<-as.numeric(format(date, "%Y"))
  dec<-as.numeric(format(date, "%j"))/366
  decyr<-yr+dec
  return(decyr)
}


#Plotting
par(mar=c(3,5,1,1))
plot(maize.hvst.sum[samp]~decyr(maize.merge$xlDateTime[samp]), ylim=c(-45,40), col="white", pch='.', ylab="Cumulative NEE, MgC ha-1", xlab='', cex=2, yaxt="n", xaxt = "n", cex.lab=2); 
abline(h=c(-60, -40, -20, 0, 20, 40),v=seq(from=2008, to=2022, by=2), col='light gray')
axis(2, labels=c(-60, -40, -20, 0, 20, 40), at=c(-60,-40, -20, 0, 20, 40), col='white', cex.axis=1.6)
axis(1, labels=seq(from=2008, to=2022, by=2), at=seq(from=2008, to=2022, by=2), col='white', cex.axis=1.6)
abline(h=0, lwd=2, lty=2)

points(maize.hvst.sum[samp]~decyr(maize.merge$xlDateTime[samp]), col=col.zm[samp], pch='.', cex=3)

points(misc.hvst.sum[samp]~decyr(misc.merge$xlDateTime[samp]), col='blue', pch='.', cex=3)
points(switch.hvst.sum[samp]~decyr(switchgrass$xlDateTime[samp]), col='pink', pch='.', cex=3)
points(sorg.hvst.sum[samp]~decyr(sorg.merge$xlDateTime[samp]), col=col.sb[samp], pch='.', cex=3)
points(misc.c.hvst.sum[samp]~decyr(misc.c.merge$xlDateTime[samp]), col='light blue', pch='.', cex=3)
points(maize.c.hvst.sum[samp]~decyr(maize.c.merge$xlDateTime[samp]), col=col.zmc[samp], pch='.', cex=3)
points(np.hvst.sum[samp]~decyr(nativeprairie$xlDateTime[samp]), col="plum3" , pch='.', cex=3)

legend(as.numeric(min(decyr(maize$xlDateTime))), 40, legend=c("maize 1", "maize 2", "soybean", "miscanthus 1", "miscanthus 2", "native prairie", "switchgrass","sorghum"), col=c("orange","yellow","light green","blue", "light blue", "plum3", "pink", "forest green"), 
       lwd=2, bty='n', cex=1.1, ncol=2, text.font = 2, text.width=2, x.intersp = 0.3, y.intersp=0.7)


dev.copy(png,'D:/R/Fluxes/Writeout/Plots/fig2_cflux_over_time.png', width=900, height=500)
dev.off()




#Set up Boxplots
getcflux<-function(dat, yield, nodatyr, id){
  
  #Unit conversions to tC/ha
  dat.gpd<-dat$Fc*0.0792 #gc02/30min
  dat.gpd.tha<-dat.gpd*.0027 #tC/ha/30min
  
  #Put yields into cumulative time series
  years<-unique(as.numeric(format(dat$xlDateTime,"%Y")))
  Y<-(as.numeric(format(dat$xlDateTime, "%Y")))
  endofyear<-c(match(years,Y )-1, length(Y))[2:(length(years)-(nodatyr-1))]
  
  dat.hvst<-dat.gpd.tha
  dat.hvst.30<-dat.hvst
  dat.hvst.30[endofyear]<-dat.hvst.30[endofyear]+yield # pretend there's a giant flux of C from harvest on the 31st of each year
  dat.hvst.sum<-cumsum(dat.hvst.30)
  #plot(dat.hvst.sum~dat$xlDateTime)
  
  firstyear<-dat.hvst.sum[endofyear][1]
  
  yearseq<-c(firstyear, as.numeric(diff(dat.hvst.sum[endofyear]))) #net carbon storage after harvest removal each year
  year<-as.numeric(format(dat$xlDateTime[endofyear], "%Y")[1:(length(endofyear))])
  lab<-rep(id, length(year))
  
  return(cbind.data.frame(year, yearseq, lab, yield))
  
}

switch.cflux<-getcflux(switchgrass, switch.yield, nodatyr=0, id="switchgrass")
misc.cflux<-getcflux(misc.merge, misc.yield, nodatyr=1, id="mgb")
misc.c.cflux<-getcflux(misc.c.merge, misc.c.yield, nodatyr=0, id="mgc") #was 1 before 2021 removal
sorg.cflux<-getcflux(sorg.merge, sorg.yield, nodatyr=1, id="sorghum")
maize.cflux<-getcflux(maize.merge, maize.yield, nodatyr=1, id="zmb")
maize.c.cflux<-getcflux(maize.c.merge, maize.c.yield, nodatyr=1, id="zmc")
np.cflux<-getcflux(nativeprairie, np.yield, nodatyr=0, id="prairie")



fluxes<-rbind(switch.cflux,misc.cflux, sorg.cflux, maize.cflux, maize.c.cflux, misc.c.cflux, np.cflux)
fluxes.gm<-fluxes
fluxes.gm$lab[which(!fluxes.gm$year%in%nosoy&fluxes.gm$lab%in%c('zmc','zmb', 'zm', 'sorghum'))]<-'soy'

fluxes.gm$lab[fluxes.gm$lab=="zmb"]<-"maize 1"; fluxes.gm$lab[fluxes.gm$lab=="zmc"]<-"maize 2"
fluxes.gm$lab[fluxes.gm$lab=="mgb"]<-"misc. 1"; fluxes.gm$lab[fluxes.gm$lab=="mgc"]<-"misc. 2"



fluxes.merge<-fluxes
fluxes.merge$lab[fluxes.merge$lab%in%c('zmc','zmb')]<-"maize"
fluxes.merge$lab[fluxes.merge$lab%in%c('mgc','mgb')]<-"miscanthus"

fluxes.merge.gm<-fluxes.merge
fluxes.merge.gm$lab[which(!fluxes.merge.gm$year%in%nosoy&fluxes.merge.gm$lab%in%c('zmc','zmb', 'maize', 'sorghum'))]<-'soy'


#color sets for unmerged sites, soy not separate
colset.kitsin<-c("blue", "light blue", "plum3", "forest green", "light pink",  "orange", "yellow")
colset.og<-c("blue",  "light pink", "plum3", "orange")
colset.post<-c("blue", "light blue", "forest green",  "orange", "yellow")

#color sets for unmerged sites, soy separate
colset.kitsin.gm<-c("light green","blue", "light blue", "plum3","forest green", "light pink",  "orange", "yellow")
colset.og.gm<-c("orange","blue","plum3","light green", "light pink")
colset.post.gm<-c("light green","blue", "light blue", "forest green",  "orange", "yellow")
colset.post.gm.labs<-c("orange", "yellow", "blue", "light blue", "forest green", "light green")

#color sets for merged sites, soy not separate
colset.merge<-c("light blue", "forest green", "light pink",  "orange")
colset.merge.og<-colset.og
colset.merge.post<-c("light blue", "forest green",  "orange")


#color sets for merged sites, soy separate
colset.merge.gm<-c("orange","blue", "plum3","forest green", "light green","light pink")
colset.merge.og.gm<-colset.og.gm
colset.merge.post.gm<-c("orange","light blue", "forest green", "light green")

#####

##Carbon flux plots####

#the kitchen sink
library(ggplot2)

all<-ggplot(subset(fluxes.merge.gm, year%in%c(2008:2022))) +
  aes(x = lab, y = yearseq) +
  geom_boxplot(shape = "circle", fill=colset.merge.gm)+
  geom_hline(yintercept=0)+
  geom_label(label="2008 - 2022", x=5.8, y=4.5, size=10)+
  theme_minimal()+
  labs(y = "Mg C ha-1 y-1", x=NULL)+
  ylim(-5, 5)+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=30,face="bold"))

all

png('D:/R/FLuxes/Writeout/Plots/fig3alt_combinedyr.png', width=900, height=500)
all
dev.off()


#original four
og<-ggplot(subset(fluxes.merge.gm, year%in%c(2008:2016))) +
  aes(x = lab, y = yearseq) +
  geom_hline(yintercept=0)+
  geom_boxplot(shape = "circle", fill = colset.merge.og.gm)+
  theme_minimal()+
  labs(y = "Mg C ha-1 y-1", x=NULL)+
  ylim(-5, 5)+
  geom_label(label="2008 - 2016", x=4.8, y=4.5, size=10)+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=30,face="bold"))

  og
  
  png('D:/R/FLuxes/Writeout/Plots/fig3a_ogyr.png', width=700, height=500)
  og
  dev.off()


#Set post 2017
newyr<-ggplot(subset(fluxes.gm, year%in%c(2018:2022))) +
  aes(x = lab, y = yearseq) +
  geom_hline(yintercept=0)+
  geom_boxplot(shape = "circle", fill = colset.post.gm.labs)+
  theme_minimal()+
  labs(y = "Mg C ha-1 y-1", x=NULL)+
  ylim(-5, 5)+
  geom_label(label="2018 - 2022", x=5.8, y=4.5, size=10)+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=30,face="bold"))


newyr

png('D:/R/FLuxes/Writeout/Plots/fig3b_newyr.png', width=800, height=500)
newyr
dev.off()
#####  

#Yields####

yield<-ggplot(subset(fluxes.merge.gm, year%in%c(2008:2022))) +
  aes(x = lab, y = yield) +
  geom_hline(yintercept=0)+
  geom_boxplot(shape = "circle", color=colset.merge.gm, lwd=2)+
  theme_minimal()+
  labs(x = NULL, y = "Mg C ha-1 y-1")+
  ylim(0,8)+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=30,face="bold"))

yield


# png('D:/R/Fluxes/Writeout/Plots/fig3alt_combinedyield.png', width=900, height=500)
# yield
# dev.off()



#original years
ogyield<-ggplot(subset(fluxes.merge.gm, year%in%c(2008:2016))) +
  aes(x = lab, y = yield) +
  geom_hline(yintercept=0)+
  geom_boxplot(shape = "circle", color=colset.merge.og.gm, lwd=2)+
  theme_minimal()+
  labs(x = NULL, y = "Mg C ha-1 y-1")+
  ylim(0,8)+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=30,face="bold"))

ogyield

png('D:/R/Fluxes/Writeout/Plots/fig3c_ogyield.png', width=700, height=500)
ogyield
dev.off()



#later years
newyield<-ggplot(subset(fluxes.gm, year%in%c(2018:2022))) +
  aes(x = lab, y = yield) +
  geom_hline(yintercept=0)+
  geom_boxplot(shape = "circle", color=colset.post.gm.labs, lwd=2)+
  theme_minimal()+
  labs(x = NULL, y = "Mg C ha-1 y-1")+
  ylim(0,8)+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=30,face="bold"))

newyield


png('D:/R/Fluxes/Writeout/Plots/fig3d_newyield.png', width=800, height=500)
newyield
dev.off()

#####


###Albedo####

#Albedo Variability#
albvar<-function(dat, id, years=c(2008:2021)){
  #subset to years of interest
  yearvec<-as.numeric(format(dat$xlDateTime, "%Y"))
  subset<-which(yearvec%in%years)
  dat<-dat[subset,]
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
np.albvar<-albvar(nativeprairie, "np")



zm.albvar<-merge(maize.albvar, maize.c.albvar, all=TRUE); zm.albvar$id<-"zm"
mg.albvar<-merge(misc.albvar, misc.c.albvar, all=TRUE); mg.albvar$id<-"mg"


all.albvar<-rbind(sorg.albvar, misc.albvar, misc.c.albvar, maize.albvar, maize.c.albvar, switch.albvar, np.albvar)

all.albvar.p1<-rbind(misc.albvar, maize.albvar, switch.albvar, np.albvar)
all.albvar.p1<-aggregate(data=all.albvar.p1, dat.alb~dat.month+dat.yr+id, FUN='mean')
all.albvar.p1$dat.month<-as.factor(all.albvar.p1$dat.month)

all.albvar.p2<-rbind(rbind(sorg.albvar, misc.albvar, misc.c.albvar, maize.albvar, maize.c.albvar))
all.albvar.p2<-aggregate(data=all.albvar.p2, dat.alb~dat.month+dat.yr+id, FUN='mean')
all.albvar.p2$dat.month<-as.factor(all.albvar.p2$dat.month)

all.albvar.av<-rbind(sorg.albvar, mg.albvar, zm.albvar, switch.albvar, np.albvar)
all.albvar.av<-aggregate(data=all.albvar.av, dat.alb~dat.month+dat.yr+id, FUN='mean')

all.albvar$dat.month<-as.factor(all.albvar$dat.month)
all.albvar.av$dat.month<-as.factor(all.albvar.av$dat.month)
#####

###Albedo plots ####

pal.full <- c("zm" = "yellow","zmc" = "yellow", "zmb" = "orange", "mg" = "blue","mgc" = "light blue", 
         "mgb" = "blue", "sw" = "light pink", "sb" = "forestgreen","gm" = "light green", "np"="plum3")
pal<-c("zmc" = "yellow", "zmb" = "orange","mgc" = "light blue", "mgb" = "blue", "sw" = "light pink", "sb" = "forestgreen", "np"="plum3")

labs<-c("zmc"="maize 2", "zmb" = "maize 1", "mgb" = "misc. 1", "mgc" = "misc. 2", "sb"="sorghum", "np" = "praire", "sw" = "switchgrass")

ogalb<-ggplot(all.albvar.p1, aes(x=dat.month, y=dat.alb, fill=id),) + 
  geom_boxplot(outlier.size=0.1)+
  theme_minimal()+
  scale_fill_manual(values=pal, labels=labs)+#c("blue", "plum3", "forest green", "light pink", "orange"),labels=c('miscanthus',  'native prairie','sorghum', 'switchgrass','maize'))+
  ylab("albedo (unitless)")+
  xlab("month")+
  ylim(0.09, 0.6)+
  geom_label(label="2008 - 2016", x=11, y=0.55, size=11, show.legend = FALSE, inherit.aes = FALSE)+
  theme(axis.text=element_text(size=30),axis.title=element_text(size=32,face="bold"),legend.text=element_text(size=24), legend.title = element_blank())

ogalb

png('D:/R/Fluxes/Writeout/Plots/fig4a_ogalb.png', width=1400, height=700)
ogalb
dev.off()


newalb<-ggplot(all.albvar.p2, aes(x=dat.month, y=dat.alb, fill=id),) + 
  geom_boxplot(outlier.size=0.1)+
  theme_minimal()+
  scale_fill_manual(values=pal, labels=labs)+#c("blue", "plum3", "forest green", "light pink", "orange"),labels=c('miscanthus',  'native prairie','sorghum', 'switchgrass','maize'))+
  ylab("albedo (unitless)")+
  xlab("month")+
  ylim(0.09, 0.6)+
  geom_label(label="2018 - 2022", x=11, y=0.55, size=11, show.legend = FALSE, inherit.aes = FALSE)+
  theme(axis.text=element_text(size=30),axis.title=element_text(size=32,face="bold"),legend.text=element_text(size=24), legend.title = element_blank())
newalb

png('D:/R/Fluxes/Writeout/Plots/fig4b_newalb.png', width=1400, height=700)
newalb
dev.off()

#####

#Radiative forcing from Albedo####

#New radiative forcing, adjusted from from GWI; calcualtes TOA RF#

#1) daily albedo
albify<-function(dat, window=1, adj=FALSE, years=c(2008:2022)){
  dat<-subtime(dat, subset=years)
  dat.alb<-dat$Fsu/dat$Fsd; dat.alb[dat.alb<0|dat.alb>1|dat$Fsd<10]<-NA
  
  adj.ind<-which(as.numeric(format(sorg.merge$xlDateTime, "%m"))%in%c(3,4,5,6,11,12))
  dat.alb.adj<-dat.alb; dat.alb.adj[adj.ind]<-dat.alb[adj.ind]+0.08
  
  dat.doy<-as.numeric(format(dat$xlDateTime, "%j"))
  dat.d.alb<-aggregate(dat.alb, by=list(dat.doy), FUN='mean', na.rm=TRUE)
  
  if(adj==TRUE){
    dat.d.alb<-aggregate(dat.alb.adj, by=list(dat.doy), FUN='mean', na.rm=TRUE)
  }
  
  dat.alb.sm<-rollapply(dat.d.alb$x, width=window, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
  
  return(dat.alb.sm)
}


#Calculate ALL albedos
sorg.alb<-albify(sorg.merge, adj=TRUE) #adj=TRUE adjusts sorghum albedo by 8% during the bare soil season

maize.alb<-albify(maize.merge);maize.c.alb<-albify(maize.c.merge)
maize.og.alb<-albify(maize.merge, years=c(2008:2016)); maize.new.alb<-albify(maize.merge, years=c(2018:2022))

misc.alb<-albify(misc.merge);misc.c.alb<-albify(misc.c.merge)
misc.og.alb<-albify(misc.merge, years=c(2008:2016)); misc.new.alb<-albify(misc.merge, years=c(2018:2022))


switch.alb<-albify(switchgrass)
np.alb<-albify(nativeprairie)

#Get RF set up: TOA solar
toa.dat.raw<-read.csv("Us-UiB_HH_2008_2016.csv")
unpack.time.af<-function(dat){
  dat.ts<-dat$TIMESTAMP_START
  ts<-strptime(dat.ts, "%Y%m%d%H%M", tz="")
  YEAR<-as.numeric(format(ts, '%Y'))
  DECDOY<-as.numeric(format(ts, '%j'))+(as.numeric(format(ts, '%H'))/24)+(as.numeric(format(ts, '%M'))/1440)
  DOY<-as.numeric(format(ts, '%j'))
  MONTH<-as.numeric(format(ts, '%m'))
  DAY<-as.numeric(format(ts, '%d'))
  HOUR<-as.numeric(format(ts, '%H'))
  DECHR<-as.numeric(format(ts, '%H'))+as.numeric(format(ts, '%M'))/60
  DECYEAR<-YEAR+(DECDOY/366)
  ts.dat<-data.frame(cbind(YEAR,DECDOY,DECYEAR, MONTH, DAY, DOY,DECHR,HOUR))
  
  return(ts.dat)
}

toa.ts.raw<-unpack.time.af(toa.dat.raw)
toa<-aggregate(toa.dat.raw$SW_IN_POT, by=list(toa.ts.raw$DOY), FUN='mean')$x

#Get RF set up: transmittance / transmittance adjusted solar
rfparam<-function(dat, years=c(2008:2022)){
  dat<-subtime(dat, subset=years)
  dat.swin<-aggregate(dat$Fsd, by=list(format(dat$xlDateTime, "%j")), FUN='mean')$x
  dat.trans<-dat.swin/toa
  rfscl<-dat.swin*dat.trans
  
  rfdat<-data.frame(cbind(dat.swin,dat.trans, rfscl)); colnames(rfdat)<-c("swin", "trans", "scalar")
  
  return(rfscl) #change to rfdat if you want the pieces individually
  
}



#Calculate ALL RFs

#Merged
#maize to sorghum,all
sbrf<-rfparam(sorg.merge)*(maize.alb-sorg.alb); 
#maize to miscanthus, all
mgrf<-rfparam(misc.merge)*(maize.alb-misc.alb)
#maize to switchgrass, all
swrf<-rfparam(switchgrass)*(maize.alb-switch.alb)
#maize to prairie, all
nprf<-rfparam(nativeprairie)*(maize.alb-np.alb)

#Orignal years
#maize to miscanthus, og
mgrf.og<-rfparam(misc.merge, years=c(2008:2016))*(maize.og.alb-misc.og.alb)
#maize to switchgrass, og
swrf.og<-rfparam(switchgrass, years=c(2008:2016))*(maize.og.alb-switch.alb)
#maize to prairie, og
nprf.og<-rfparam(nativeprairie, years=c(2008:2016))*(maize.og.alb-np.alb)

#New years
#maize to other maize, new
zmrf.new<-rfparam(maize.c.merge, years=c(2018:2022))*(maize.new.alb-maize.c.alb)
#maize to misc 1, new
mgbrf.new<-rfparam(misc.merge, years=c(2018:2022))*(maize.new.alb-misc.new.alb)
#maize to misc 2, new
mgcrf.new<-rfparam(misc.c.merge, years=c(2018:2022))*(maize.new.alb-misc.c.alb)
#maize to sorghum, new
sbrf.new<-rfparam(sorg.merge, years=c(2018:2022))*(maize.new.alb-sorg.alb)
#####

#Radiative Forcing Plots#####

#Weirdly difficult setup to aggregate a DOY vector to months:#

#mode fucntion so that a DOY is assigned to the month it most commonly lands in (necessary b/c leap years change doy-month pairings slightly )
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#creates a doy-length vector of months (e.g. "1" 31 times for DOY 1-31) for use in aggregating RF's
doytomonth<-function (dat){
  dat.ts<-unpack.time(dat)
  key<-aggregate(dat.ts, by=list(dat.ts$DOY), FUN="mode")$MONTH
  return(key)
}


##Plots

#Merged
sbrf.month<-aggregate(sbrf, by=list(doytomonth(sorg.merge)), FUN='mean')$x
mgrf.month<-aggregate(mgrf, by=list(doytomonth(misc.merge)), FUN='mean')$x
swrf.month<-aggregate(swrf, by=list(doytomonth(switchgrass)), FUN='mean')$x
nprf.month<-aggregate(nprf, by=list(doytomonth(nativeprairie)), FUN='mean')$x


#png('C:/Users/Bethany/Desktop/albrf.png', width=680, height=340)    

par(mfrow=c(1,1), mar=c(4,4.2,2,1))

plot(mgrf.month, type='l', main="RF of Conversion from Maize", font.lab=2, lwd=4, ylab='raditive forcing (Wm-2)', xlab='month', ylim=c(-10, 10), col='blue', cex.lab=1.5, cex.axis=1.5, cex.main=2)
lines(sbrf.month, type='l',  lwd=4, col='forest green') 
lines(swrf.month,lwd=4,col='light pink')
lines(nprf.month, lwd=4, col='plum3')
abline(h=0, lwd=4, lty=3)

legend(7, 11, 
       legend=c(paste("Miscanthus:", round(mean(mgrf, na.rm=TRUE), 2), "Wm-2"), paste("Switchgrass:", round(mean(swrf, na.rm=TRUE), 2), "Wm-2"), paste("Native Prairie:", round(mean(nprf, na.rm=TRUE), 2), "Wm-2"), paste("Sorghum:", round(mean(sbrf, na.rm=TRUE), 2), "Wm-2")), 
       lwd=2, col=c("blue", "light pink","plum3", "forest green"), bty='n', cex=1, text.font=2)


#dev.off()


#Original years

#Merged
mgrf.og.month<-aggregate(mgrf.og, by=list(doytomonth(misc.merge)), FUN='mean')$x
swrf.og.month<-aggregate(swrf.og, by=list(doytomonth(switchgrass)), FUN='mean')$x
nprf.og.month<-aggregate(nprf.og, by=list(doytomonth(nativeprairie)), FUN='mean')$x

png('D:/R/Fluxes/Writeout/Plots/fig4a_pan_ogrf.png', width=500, height=350)   

plot(mgrf.og.month, type='l', main="RF of Conversion from Maize", font.lab=2, lwd=4, ylab='radiative forcing (Wm-2)', xlab='month', ylim=c(-10, 10), col='blue', cex.lab=1.5, cex.axis=1.5, cex.main=2)
lines(swrf.og.month,lwd=4,col='light pink')
lines(nprf.og.month, lwd=4, col='plum3')
abline(h=0, lwd=4, lty=3)

legend(7, 11, 
       legend=c(paste("Miscanthus:", round(mean(mgrf.og, na.rm=TRUE), 2), "Wm-2"), paste("Switchgrass:", round(mean(swrf.og, na.rm=TRUE), 2), "Wm-2"), paste("Native Prairie:", round(mean(nprf.og, na.rm=TRUE), 2), "Wm-2")), 
       lwd=2, col=c("blue", "light pink","plum3"), bty='n', cex=1, text.font=2)

dev.off()

#Newer years

zmrf.new.month<-aggregate(zmrf.new, by=list(doytomonth(maize.merge)), FUN='mean')$x
mgbrf.new.month<-aggregate(mgbrf.new, by=list(doytomonth(misc.merge)), FUN='mean')$x
mgcrf.new.month<-aggregate(mgcrf.new, by=list(doytomonth(misc.c.merge)), FUN='mean')$x
sbrf.new.month<-aggregate(sbrf.new, by=list(doytomonth(sorg.merge)), FUN='mean')$x

png('D:/R/Fluxes/Writeout/Plots/fig4b_pan_newrf.png', width=500, height=350)   

plot(mgbrf.new.month, type='l', main="RF of Conversion from Maize", font.lab=2, lwd=4, ylab='raditive forcing (Wm-2)', xlab='month', ylim=c(-10, 10), col='blue', cex.lab=1.5, cex.axis=1.5, cex.main=2)
lines(mgcrf.new.month,lwd=4,col='light blue')
lines(sbrf.new.month, lwd=4, col='forest green')
lines(zmrf.new.month, lwd=4, col='yellow', lty=2)
abline(h=0, lwd=4, lty=3)

#Consider using average of zm as baseline and plotting both

legend(7, 11, 
       legend=c(paste("Miscanthus 1:", round(mean(mgbrf.new, na.rm=TRUE), 2), "Wm-2"), paste("Miscanthus 2:", round(mean(mgcrf.new, na.rm=TRUE), 2), "Wm-2"), paste("Sorghum:", round(mean(sbrf.new, na.rm=TRUE), 2), "Wm-2"), paste("Maize 2:", round(mean(zmrf.new, na.rm=TRUE), 2), "Wm-2")), 
       lwd=2, col=c("blue", "light blue","forest green", "yellow"), bty='n', cex=1, text.font=2)

dev.off()
#####


###Turbulent Fluxes####

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
np.fh.sm<-hflux(nativeprairie)


#aggregate plant types

if(exists('maize.c.fh.sm')&exists('misc.c.fh.sm')){
  misc.fh<-(misc.fh.sm+misc.c.fh.sm)/2
  maize.fh<-(maize.fh.sm+maize.c.fh.sm)/2
}else{
  misc.fh<-misc.fh.sm
  maize.fh<-maize.fh.sm
}


  
#Variability in H flux

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
  np.hvar<-hvar(nativeprairie, "np")
  
  all.hvar<-rbind(sorg.hvar, misc.hvar, misc.c.hvar, maize.hvar, maize.c.hvar, switch.hvar, np.hvar)
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
  
  #Plots
  
  #Shared color palette and labels can be found in albedo section
  # pal <- c("zm" = "orange","zmc" = "yellow", "zmb" = "orange", "mg" = "blue","mgc" = "light blue", 
  #          "mgb" = "blue", "sw" = "light pink", "sb" = "forestgreen","gm" = "light green", "np"="plum3")
  # 
  #All years
  dat<-all.hvar.rot
  
  h<-ggplot(dat, aes(x=dat.month, y=dat.h, fill=id),) + 
    geom_boxplot(outlier.size=0.1)+
    theme_minimal()+
    scale_fill_manual(values=pal)+
    ylim(min=-5, max=100)+
    ylab("H (W m-1)")+
    xlab("month")+
    theme(axis.text=element_text(size=30),axis.title=element_text(size=32,face="bold"),legend.text=element_text(size=24), legend.title = element_blank())
  
  h
  
  png('C:/Users/Bethany/Desktop/hvar.png', width=1200, height=600)
  h
  dev.off()

  
  #Panel: means as line graph
  all.lines<-aggregate(all.hvar.rot$dat.h, by=list(all.hvar.rot$dat.month, all.hvar.rot$id), FUN='mean')
  
  
  png('C:/Users/Bethany/Desktop/htrace.png', width=500, height=400)  
  
  par(mfrow=c(1,1), mar=c(4,4.2,2,1))
  
  plot(all.lines$x[all.lines$Group.2=="mg"], type='l', col='blue', lwd=4, ylim=c(0, 65),
       lty=5, ylab="Sensible heat flux (W m-2)", xlab="Month",font.lab=2, cex.lab=1.5, cex.axis=2, cex.main=2)
  
  lines(all.lines$x[all.lines$Group.2=="zm"], col='orange', lwd=4, lty=1)
  lines(all.lines$x[all.lines$Group.2=="sw"], col='light pink', lwd=4, lty=5)
  lines(all.lines$x[all.lines$Group.2=="np"], col='plum3', lwd=4, lty=5)
  lines(all.lines$x[all.lines$Group.2=="sb"], col='forest green', lwd=4, lty=1)
  
  dev.off()
  

#####

#LE#####

leflux<-function(dat, window=1, years=c(2008:2022)){
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
np.fe.sm<-leflux(nativeprairie)

#aggregate plant types

if(exists('maize.c.fe.sm')&exists('misc.c.fe.sm')){
  misc.fe<-(misc.fe.sm+misc.c.fe.sm)/2
  maize.fe<-(maize.fe.sm+maize.c.fe.sm)/2
}else{
  misc.fe<-misc.fe.sm
  maize.fe<-maize.fe.sm
}


#Cumulative latent heat flux

leflux.cum<-function(dat){
  dat<-subtime(dat)
  dat.fe<-dat$Fe
  dat.doy<-as.numeric(format(dat$xlDateTime, "%j"))
  dat.yr<-as.numeric(format(dat$xlDateTime, "%Y"))
  #plot(dat.fe~dat.doy)
  dat.fe.yravg<-aggregate(dat.fe~dat.doy+dat.yr, FUN='sum')#sums for each day of each year
  dat.fe.cumyr<-aggregate(dat.fe.yravg$dat.fe, by=list(dat.fe.yravg$dat.doy), FUN='mean') #average these daily sums across years
  dat.cum<-cumsum(dat.fe.cumyr$x)
  
  return(dat.cum)
  
}

sorg.fe.cum<-leflux.cum(sorg.merge)
maize.fe.cum<-leflux.cum(maize.merge);maize.c.fe.cum<-leflux.cum(maize.c.merge)
misc.fe.cum<-leflux.cum(misc.merge);misc.c.fe.cum<-leflux.cum(misc.c.merge)
switch.fe.cum<-leflux.cum(switchgrass)
np.fe.cum<-leflux.cum(nativeprairie)

#aggregate plant types

if(exists('maize.c.fe.cum')&exists('misc.c.fe.cum')){
  misc.fe.ccum<-(misc.fe.cum+misc.c.fe.cum)/2
  maize.fe.ccum<-(maize.fe.cum+maize.c.fe.cum)/2
}else{
  misc.fe.ccum<-misc.fe.cum
  maize.fe.ccum<-maize.fe.cum
}


  
#Variability in latent heat flux

  levar<-function(dat, id, years=c(2008:2021)){
    #dat<-subtime(dat)
    yearvec<-as.numeric(format(dat$xlDateTime, "%Y"))
    subset<-which(yearvec%in%years)
    dat<-dat[subset,]
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
  np.levar<-levar(nativeprairie, "np")
  
  all.levar<-rbind(sorg.levar, misc.levar, misc.c.levar, maize.levar, maize.c.levar, switch.levar, np.levar)
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
  
  
###LE Plots#####
  
  #Shared color palette and labeling can be found in albedo section
  # pal <- c("zm" = "orange","zmc" = "yellow", "zmb" = "orange", "mg" = "blue","mgc" = "light blue", 
  #          "mgb" = "blue", "sw" = "light pink", "sb" = "forestgreen","gm" = "light green", "np"="plum3")
  # 
  
  
  #OG years
  dat<-levar.pre
  
  ogle<-ggplot(dat, aes(x=dat.month, y=dat.le, fill=id),) + 
    geom_boxplot(outlier.size=0.1)+
    theme_minimal()+
    scale_fill_manual(values=pal, labels=labs)+
    ylim(min=-5, max=200)+
    ylab("LE (W m-2)")+
    xlab("month")+
    theme(axis.text=element_text(size=30),axis.title=element_text(size=32,face="bold"),legend.text=element_text(size=24), legend.title = element_blank())
  
  ogle
  
  png('D:/R/FLuxes/Writeout/Plots/fig6a_le_ogyr.png', width=1400, height=700)
  ogle
  dev.off()
  
  #Panel: means as line graph
  all.lines<-aggregate(levar.pre$dat.le, by=list(levar.pre$dat.month, levar.pre$id), FUN='mean')
  
  
  png('D:/R/FLuxes/Writeout/Plots/fig6a_pan_le_ogyr.png', width=500, height=400)  
  
  par(mfrow=c(1,1), mar=c(4,4.2,2,1))
  
  plot(all.lines$x[all.lines$Group.2=="mgb"], type='l', col='blue', lwd=4, ylim=c(0, 130),
       lty=5, ylab="Latent heat flux (W m-2)", xlab="Month",font.lab=2, cex.lab=1.5, cex.axis=2, cex.main=2)
  
  lines(all.lines$x[all.lines$Group.2=="zmc"], col='yellow', lwd=4, lty=1)
  lines(all.lines$x[all.lines$Group.2=="zmb"], col='orange', lwd=4, lty=1)
  lines(all.lines$x[all.lines$Group.2=="mgc"], col='light blue', lwd=4, lty=5)
  lines(all.lines$x[all.lines$Group.2=="sw"], col='light pink', lwd=4, lty=5)
  lines(all.lines$x[all.lines$Group.2=="np"], col='plum3', lwd=4, lty=5)
  lines(all.lines$x[all.lines$Group.2=="sb"], col='forest green', lwd=4, lty=1)
  
  dev.off()
  
#New years
  
  dat<-levar.post
  
  newle<-ggplot(dat, aes(x=dat.month, y=dat.le, fill=id),) + 
    geom_boxplot(outlier.size=0.1)+
    theme_minimal()+
    scale_fill_manual(values=pal, labels=labs)+
    ylim(min=-5, max=200)+
    ylab("LE (W m-2)")+
    xlab("month")+
    theme(axis.text=element_text(size=30),axis.title=element_text(size=32,face="bold"),legend.text=element_text(size=24), legend.title = element_blank())
  
  newle
  
  png('D:/R/FLuxes/Writeout/Plots/fig6b_le_newyr.png', width=1400, height=700)
  newle
  dev.off()
  
  #Panel: means as line graph
  all.lines<-aggregate(levar.post$dat.le, by=list(levar.post$dat.month, levar.post$id), FUN='mean')
  
  
  png('D:/R/FLuxes/Writeout/Plots/fig6b_pan_le_newyr.png', width=500, height=400)  
  
  par(mfrow=c(1,1), mar=c(4,4.2,2,1))
  
  plot(all.lines$x[all.lines$Group.2=="mgb"], type='l', col='blue', lwd=4, ylim=c(0, 130),
       lty=5, ylab="Latent heat flux (W m-2)", xlab="Month",font.lab=2, cex.lab=1.5, cex.axis=2, cex.main=2)
  
  lines(all.lines$x[all.lines$Group.2=="zmc"], col='yellow', lwd=4, lty=1)
  lines(all.lines$x[all.lines$Group.2=="zmb"], col='orange', lwd=4, lty=1)
  lines(all.lines$x[all.lines$Group.2=="mgc"], col='light blue', lwd=4, lty=5)
  lines(all.lines$x[all.lines$Group.2=="sw"], col='light pink', lwd=4, lty=5)
  lines(all.lines$x[all.lines$Group.2=="np"], col='plum3', lwd=4, lty=5)
  lines(all.lines$x[all.lines$Group.2=="sb"], col='forest green', lwd=4, lty=1)
  
  dev.off()
  
  # #Pre-2017
  # dat<-levar.pre.fs.soy
  # 
  # ggplot(dat, aes(x=dat.month, y=dat.le, fill=id),) + 
  #   geom_boxplot(outlier.size=0.1)+
  #   theme_minimal()+
  #   scale_fill_manual(values=pal)+
  #   ylim(min=-5, max=200)
  # 
  # #Post-2017
  # dat<-levar.post.fs.soy
  # 
  # ggplot(dat, aes(x=dat.month, y=dat.le, fill=id),) + 
  #   geom_boxplot(outlier.size=0.1)+
  #   theme_minimal()+
  #   scale_fill_manual(values=pal)+
  #   ylim(min=-5, max=200)
 

  #Cumulative LE
  plot(maize.fe.ccum, type='l', col='orange', xlab='month', ylab="cumulative latent heat", lwd=2.5, ylim=c(0, 12E+05))
  lines(sorg.fe.cum, col='forest green', lwd=2.5)
  lines(misc.fe.ccum, col='light blue', lwd=2.5)
  lines(switch.fe.cum, col='light pink', lwd=2.5)
  lines(np.fe.cum, col='plum3', lwd=2.5)
  

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
    np.brvar<-brvar(nativeprairie, "np")
    
    all.brvar<-rbind(sorg.brvar, misc.brvar, misc.c.brvar, maize.brvar, maize.c.brvar, switch.brvar, np.brvar)
    all.brvar$dat.month<-as.factor(all.brvar$dat.month)
    
    
    nosoy<-c(2008, 2009, 2011, 2012, 2014, 2015, 2017, 2018, 2020, 2021)
    
    all.brvar.ns<-all.brvar[all.brvar$dat.yr%in%nosoy,] #removing soy years
    
    all.brvar.soy<-all.brvar; all.brvar.soy$id[!all.brvar.soy$dat.yr%in%nosoy&all.brvar.soy$id%in%c("zmc", "zmb", "sb")]<-"gm" #identify soy years
    all.brvar.fs<-all.brvar.soy; all.brvar.fs$id[all.brvar.fs$id%in%c("zmc", "zmb")]<-"zm"; all.brvar.fs$id[all.brvar.fs$id%in%c("mgc", "mgb")]<-"mg" #group feedstocks
    all.brvar.rot<-all.brvar; all.brvar.rot$id[all.brvar.rot$id%in%c("zmc", "zmb")]<-"zm"; all.brvar.rot$id[all.brvar.rot$id%in%c("mgc", "mgb")]<-"mg"
    
    all.brvar.sbtime<-all.brvar.ns[all.brvar.ns$dat.yr>2017,] #no soy and since sorghum existed, i.e. 2018+
    all.brvar.nosbtime<-all.brvar.ns[all.brvar.ns$dat.yr<2017,] #no soy and since sorghum existed, i.e. 2018+

    #Shared color palette
    pal <- c("zm" = "orange","zmc" = "yellow", "zmb" = "orange", "mg" = "blue","mgc" = "light blue", 
             "mgb" = "blue", "sw" = "light pink", "sb" = "forestgreen","gm" = "light green", "np"="plum3")
    
    #Line plot with uncertainty
    
    dat<-all.brvar.rot
    
    dat$dat.month<-as.numeric(dat$dat.month) #month must be a number for line graphs
    
    ggplot(dat, aes(x=dat.month, y=dat.bow, color=id, fill=id)) + 
      geom_smooth(fill="gray90", size=2)+
      theme_minimal()+
      scale_fill_manual()+
      scale_color_manual(values=pal)+
      ylim(min=0, max=3)+
      ylab("Bowen Ratio")+
      xlab("month")+
      theme(axis.text=element_text(size=30),axis.title=element_text(size=32,face="bold"),legend.text=element_text(size=24), legend.title = element_blank())
  
  
  #####

##Data availability#####

years<-c(2008:2022)
soyyr<-years[!years%in%nosoy]
p2<-c(2017:2022)
p1<-c(2008:2016)

colnames(mg)<-c("y", "col", "yr")

mgb<-data.frame(rep(3, length(years)), rep('light blue', length(years)), years, rep ("miscanthus 1", length(years)))
mgc<-data.frame(rep(2.5, length(years)), rep('blue', length(years)), years, rep ("miscanthus 2", length(years)))
zmb<-data.frame(rep(4, length(years)), rep('orange', length(years)), years, rep ("maize 1", length(years)))
zmc<-data.frame(rep(3.5, length(years)), rep('yellow', length(years)), years, rep ("maize 2", length(years)))
sw<-data.frame(rep(1.5, length(years)), rep('light pink', length(years)), years, rep ("switchgrass", length(years)))
sb<-data.frame(rep(2, length(years)), rep('forest green', length(years)), years, rep ("sorghum", length(years)))
np<-data.frame(rep(1, length(years)), rep('plum2', length(years)), years, rep ("native prairie", length(years)))


colnames(zmc)<-colnames(mgc)<-colnames(mgb)<-colnames(sw)<-colnames(sb)<-colnames(zmb)<-colnames(np)<-c("y", "col", "yr", "feedstock")

zmc$y[!zmc$yr%in%p2]<-NA; mgc$y[!mgc$yr%in%p2]<-NA;sb$y[!zmc$yr%in%p2]<-NA; sw$y[!zmc$yr%in%p1]<-NA;np$y[!zmc$yr%in%p1]<-NA

coverage<-rbind(mgb, mgc, zmb, zmc,sw, sb, np)


avail<-ggplot(coverage) +
  aes(x = yr, y = y, fill = feedstock) +
  geom_tile(size = 1.5)+
  scale_fill_manual(values=c("orange", "yellow","blue", "light blue", "plum3", "forest green", "light pink")) +
  theme_minimal()+
  scale_x_continuous(breaks = seq(from=2008, to=2022, by=2))+
  xlab("")+
  theme(legend.text=element_text(size=22),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.text=element_text(size=24),
        axis.title=element_text(size=26,face="bold"), axis.title.y = element_blank(),
        legend.title=element_blank(), panel.grid.minor.x = element_line(color = "black"),
        panel.grid.major.x = element_line(color = "black"))
  

avail

png("D:/R/Fluxes/Writeout/Plots/fig1_availability.png", width=1000, height=500)
avail
dev.off()


#####

##GWI####


#Plotting for GWI
gwi<-ggplot(dat.gwi[dat.gwi$types%in%c("albedo.gwi", "carbon"),]) +
  aes(x = names, fill = types, weight = dat) +
  geom_bar(color='black') +
  scale_fill_manual(values = c(carbon = "#BABEC0",albedo.gwi = "#8B96C2"), labels=c("carbon", "albedo"))+
  labs(x = "Feedstock",
       y = "Mg CO2-eq ha-1 y-1",
       fill = "forcing",
       title = "annual combined effect (100-yr time horizon)")+
  theme_minimal()+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=28,face="bold"),legend.text=element_text(size=28),
        legend.position = c(0.8, 0.2), legend.title = element_blank(),plot.title = element_text(size=28, face="bold"))

gwi

png('C:/Users/Bethany/Desktop/gwi.png', width=700, height=800)
gwi
dev.off()


pal <- c("maize" = "orange", "miscanthus" = "blue", "switchgrass" = "light pink", "sorghum" = "forestgreen", "prairie"="plum3")


#plotting for eesf

alb.eesf<-ggplot(dat.gwi[dat.gwi$types=="albedo.eesf",]) +
  aes(x = names, weight = dat, fill=names) +
  geom_bar(colour="black",show.legend = FALSE)+
  scale_fill_manual(values=pal)+  
  labs(
    x = "Feedstock",
    y = "Mg Co2-eq ha-1",
    title = "albedo-equivalent co2 (single pulse)"
  ) +
  theme_minimal()+
  ylim(c(-90, 20))+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=28,face="bold"),legend.text=element_text(size=28),
        plot.title = element_text(size=32, face="bold") )

png('C:/Users/Bethany/Desktop/alb.eesf.png', width=700, height=500)
alb.eesf
dev.off()


carb.eesf<-ggplot(dat.gwi[dat.gwi$types=="carbon",]) +
  aes(x = names, weight = dat, fill=names) +
  geom_bar(colour="black",show.legend = FALSE)+
  scale_fill_manual(values=pal)+
  labs(
    x = "Feedstock",
    y = "Mg Co2-eq ha-1 y-1",
    title = "actual co2 (annual)"
  ) +
  theme_minimal()+
  ylim(c(-90, 20))+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=28,face="bold"),legend.text=element_text(size=28),
        plot.title = element_text(size=32, face="bold") )

png('C:/Users/Bethany/Desktop/carb.eesf.png', width=700, height=500)
carb.eesf
dev.off()




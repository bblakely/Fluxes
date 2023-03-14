#Figures for ESA poster.
#Run energy fluxes (for now)

#Part 1: carbon fluxes
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
misc.c.cflux<-getcflux(misc.c.merge, misc.c.yield, nodatyr=1, id="mgc")
sorg.cflux<-getcflux(sorg.merge, sorg.yield, nodatyr=1, id="sorghum")
maize.cflux<-getcflux(maize.merge, maize.yield, nodatyr=1, id="zmb")
maize.c.cflux<-getcflux(maize.c.merge, maize.c.yield, nodatyr=1, id="zmc")
np.cflux<-getcflux(nativeprairie, np.yield, nodatyr=0, id="prairie")



fluxes<-rbind(switch.cflux,misc.cflux, sorg.cflux, maize.cflux, maize.c.cflux, misc.c.cflux, np.cflux)
fluxes.gm<-fluxes
fluxes.gm$lab[which(!fluxes.gm$year%in%nosoy&fluxes.gm$lab%in%c('zmc','zmb', 'zm', 'sorghum'))]<-'gm'

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
colset.og.gm<-c("orange","light blue","plum3","light green", "light pink")
colset.post.gm<-c("light green","blue", "light blue", "forest green",  "orange", "yellow")

#color sets for merged sites, soy not separate
colset.merge<-c("light blue", "forest green", "light pink",  "orange")
colset.merge.og<-colset.og
colset.merge.post<-c("light blue", "forest green",  "orange")


#color sets for merged sites, soy separate
colset.merge.gm<-c("orange","light blue", "plum3","forest green", "light green","light pink")
colset.merge.og.gm<-colset.og.gm
colset.merge.post.gm<-c("orange","light blue", "forest green", "light green")





# ##Collection 1: no differentiation for soy####
# 
# #the kitchen sink
# ggplot(subset(fluxes, year%in%c(2008:2020))) +
#   aes(x = lab, y = yearseq) +
#   geom_boxplot(shape = "circle", fill = colset.kitsin) +
#   theme_minimal()+
#   labs(x = "Feedstock",
#        y = "Mg C ha-1 y-1")+
#   ylim(-5, 12)
# 
# 
# #original three
# ggplot(subset(fluxes, year%in%c(2008:2016))) +
#   aes(x = lab, y = yearseq) +
#   geom_boxplot(shape = "circle", fill = colset.og) +
#   theme_minimal()+
#   labs(x = "Feedstock",
#        y = "Mg C ha-1 y-1")+
#   ylim(-5, 12)
# 
# 
# #original three minus burp years
# ggplot(subset(fluxes, year%in%c(2008:2012, 2015:2016))) +
#   aes(x = lab, y = yearseq) +
#   geom_boxplot(shape = "circle",  fill = colset.og) +
#   theme_minimal()+
#   labs(x = "Feedstock",
#        y = "Mg C ha-1 y-1")+
#   ylim(-5, 12)
# 
# 
# #sorghum years
# ggplot(subset(fluxes, year%in%c(2018:2022))) +
#   aes(x = lab, y = yearseq) +
#   geom_boxplot(shape = "circle", fill = colset.post) +
#   theme_minimal()+
#   labs(x = "Feedstock",
#        y = "Mg C ha-1 y-1")+
#   ylim(-5, 12)
# #####
# 
# ##Collection 2: soy separate####
# 
# #the kitchen sink
# ggplot(subset(fluxes.gm, year%in%c(2008:2020))) +
#   aes(x = lab, y = yearseq) +
#   geom_boxplot(shape = "circle", fill=colset.kitsin.gm)+
#   theme_minimal()+
#   labs(x = "Feedstock",
#        y = "Mg C ha-1 y-1")+
#   ylim(-5, 12)
# 
# 
# #original three
# ggplot(subset(fluxes.gm, year%in%c(2008:2016))) +
#   aes(x = lab, y = yearseq) +
#   geom_boxplot(shape = "circle", fill = colset.og.gm)+
#   theme_minimal()+
#   labs(x = "Feedstock",
#        y = "Mg C ha-1 y-1")+
#   ylim(-5, 12)
# 
# #original three, no burp
# ggplot(subset(fluxes.gm, year%in%c(2008:2012, 2015:2016))) +
#   aes(x = lab, y = yearseq) +
#   geom_boxplot(shape = "circle", fill = colset.og.gm) +
#   theme_minimal()+
#   labs(x = "Feedstock",
#        y = "Mg C ha-1 y-1")+
#   ylim(-5, 12)
# 
# 
# #sorghum years
# ggplot(subset(fluxes.gm, year%in%c(2018:2022))) +
#   aes(x = lab, y = yearseq) +
#   geom_boxplot(shape = "circle", fill = colset.post.gm) +
#   theme_minimal()+
#   labs(x = "Feedstock",
#        y = "Mg C ha-1 y-1")+
#   ylim(-5, 12)
# #####
# 
# ##Collection 3: merged sites, no differentiation for soy####
# 
# #the kitchen sink
# ggplot(subset(fluxes.merge, year%in%c(2008:2020))) +
#   aes(x = lab, y = yearseq) +
#   geom_boxplot(shape = "circle", fill=colset.merge) +
#   theme_minimal()+
#   labs(x = "Feedstock",
#        y = "Mg C ha-1 y-1")+
#   ylim(-5, 12)
# 
# 
# #original three
# ggplot(subset(fluxes.merge, year%in%c(2008:2016))) +
#   aes(x = lab, y = yearseq) +
#   geom_boxplot(shape = "circle", fill = colset.merge.og) +
#   theme_minimal()+
#   labs(x = "Feedstock",
#        y = "Mg C ha-1 y-1")+
#   ylim(-5, 12)
# 
# 
# #original three minus burp years
# ggplot(subset(fluxes.merge, year%in%c(2008:2012, 2015:2016))) +
#   aes(x = lab, y = yearseq) +
#   geom_boxplot(shape = "circle",  fill = colset.merge.og) +
#   theme_minimal()+
#   labs(x = "Feedstock",
#        y = "Mg C ha-1 y-1")+
#   ylim(-5, 12)
# 
# 
# #sorghum years
# ggplot(subset(fluxes.merge, year%in%c(2018:2022))) +
#   aes(x = lab, y = yearseq) +
#   geom_boxplot(shape = "circle", fill = colset.merge.post) +
#   theme_minimal()+
#   labs(x = "Feedstock",
#        y = "Mg C ha-1 y-1")+
#   ylim(-5, 12)
# #####

##Collection 4: merged sites, soy separate####

#the kitchen sink


all<-ggplot(subset(fluxes.merge.gm, year%in%c(2008:2022))) +
  aes(x = lab, y = yearseq) +
  geom_boxplot(shape = "circle", fill=colset.merge.gm)+
  theme_minimal()+
  labs(x = "Feedstock",
       y = "Mg C ha-1 y-1")+
  ylim(-5, 5)+
  theme(axis.text=element_text(size=30),axis.title=element_text(size=32,face="bold"))

all

png('C:/Users/Bethany/Desktop/allcarb.png', width=900, height=630)
all
dev.off()


#original three
og<-ggplot(subset(fluxes.merge.gm, year%in%c(2008:2016))) +
  aes(x = lab, y = yearseq) +
  geom_boxplot(shape = "circle", fill = colset.merge.og.gm)+
  theme_minimal()+
  labs(x = "Feedstock",
       y = "Mg C ha-1 y-1")+
  ylim(-5, 5)+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=24,face="bold"))

  og
  
  png('C:/Users/Bethany/Desktop/ogyr.png', width=600, height=300)
  og
  dev.off()
  

# 
# #original three, no burp
# ggplot(subset(fluxes.merge.gm, year%in%c(2008:2012, 2015:2016))) +
#   aes(x = lab, y = yearseq) +
#   geom_boxplot(shape = "circle", fill = colset.merge.og.gm) +
#   theme_minimal()+
#   labs(x = "Feedstock",
#        y = "Mg C ha-1 y-1")+
#   ylim(-5, 5)


#sorghum years
sorgyr<-ggplot(subset(fluxes.merge.gm, year%in%c(2018:2022))) +
  aes(x = lab, y = yearseq) +
  geom_boxplot(shape = "circle", fill = colset.merge.post.gm) +
  theme_minimal()+
  labs(x = "Feedstock",
       y = "Mg C ha-1 y-1")+
  ylim(-5, 5)+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=24,face="bold"))


sorgyr

png('C:/Users/Bethany/Desktop/sorgyr.png', width=600, height=300)
sorgyr
dev.off()

#Yields

yield<-ggplot(subset(fluxes.merge.gm, year%in%c(2008:2022))) +
  aes(x = lab, y = yield) +
  geom_boxplot(shape = "circle", fill=colset.merge.gm)+
  theme_minimal()+
  labs(x = "Feedstock",
       y = "Mg C ha-1 y-1")+
  theme(axis.text=element_text(size=30),axis.title=element_text(size=32,face="bold"))

yield



#####

###Albedo###


#Variability####
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


library(ggplot2)


alb<-ggplot(all.albvar.av, aes(x=dat.month, y=dat.alb, fill=id),) + 
  geom_boxplot(outlier.size=0.1)+
  theme_minimal()+
  scale_fill_manual(values=c("blue",  "forest green", "light pink", "orange"),labels=c('miscanthus', 'sorghum', 'switchgrass','maize'))+
  ylab("albedo (unitless)")+
  xlab("month")+
  ylim(0.09, 0.6)+
  theme(axis.text=element_text(size=30),axis.title=element_text(size=32,face="bold"),legend.text=element_text(size=24), legend.title = element_blank())

alb

png('C:/Users/Bethany/Desktop/albvar.png', width=1400, height=700)
alb
dev.off()

#####

#Radiative forcing####
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


#Radiative forcing (modified from energyfluxes)

monthrad<-aggregate(maize.merge$Fsd, by=list(as.numeric(format(maize.merge$xlDateTime, "%m"))), FUN='mean', na.rm=TRUE)

albdiff.mg<-maize.alb-misc.alb #"for conversion from corn to miscanthus"
rf.mg<-monthrad$x*albdiff.mg; rf.ann.mg<-mean(rf.mg)

albdiff.sb<-maize.alb-sorg.alb.sm #for conversion from corn to sorgum
rf.sb<-monthrad$x*albdiff.sb; rf.ann.sb<-mean(rf.sb)

albdiff.sw<-maize.alb-switch.alb.sm #for conversion from corn to sorgum
rf.sw<-monthrad$x*albdiff.sw; rf.ann.sw<-mean(rf.sw)
  

png('C:/Users/Bethany/Desktop/albrf.png', width=680, height=340)    

par(mfrow=c(1,1), mar=c(4,4.2,2,1))

plot(rf.mg, type='l', main="RF of Conversion from Maize", font.lab=2, lwd=4, ylab='raditive forcing (Wm-2)', xlab='month', ylim=c(-18, 18), col='blue', cex.lab=1.5, cex.axis=1.5, cex.main=2)
lines(rf.sb, type='l',  lwd=4, col='forest green') 
lines(rf.sw,lwd=4,col='light pink')
abline(h=0, lwd=4, lty=3)

legend(7, 19, 
       legend=c(paste("Miscanthus:", round(rf.ann.mg, 2), "Wm-2"), paste("Switchgrass:", round(rf.ann.sw,2), "Wm-2"), paste("Sorghum:", round(rf.ann.sb, 2), "Wm-2")), 
       lwd=2, col=c("light blue", "light pink", "forest green"), bty='n', cex=1, text.font=2)


dev.off()

#####

###Turbulent Fluxes###


#Energy balance bits
ebpartition<-function(dat.fe, dat.fh, dat.fg, dat.all, id){
  dat.combo<-dat.fe+dat.fh+dat.fg
  dat.nr<-aggregate(dat.all$Fn, by=list(as.numeric(format(dat.all$xlDateTime, "%m"))), FUN='mean')$x
  
  val<-c(dat.fe,dat.fh,dat.fg); lab<-c(rep('LE', 12), rep('H', 12), rep('G', 12));month<-rep(c(1:12), 3)
  part<-data.frame(val, lab, month, dat.nr)
  part[5]<-id
  colnames(part)[4:5]<-c("NR", "id")
  
  return(part)
  
}

misc.eb<-ebpartition(misc.fe, misc.fh, misc.fg, misc.merge, "mg")
maize.eb<-ebpartition(maize.fe, maize.fh, maize.fg, maize.merge, "zm")
sorg.eb<-ebpartition(sorg.fe.sm, sorg.fh.sm, sorg.fg.sm, sorg.merge, "sb")
switch.eb<-ebpartition(switch.fe.sm, switch.fh.sm, switch.fg.sm, switchgrass, "sw")
  
all.eb<-rbind(misc.eb, maize.eb, sorg.eb, switch.eb)  
  
eb.fluxes.ann<-aggregate(all.eb$val~all.eb$lab+all.eb$id, FUN='mean'); colnames(eb.fluxes.ann)<-c("Flux", "Feedstock", "watts")
nr.ann<-aggregate(all.eb$NR~all.eb$lab+all.eb$id, FUN='mean'); colnames(nr.ann)<-c("Flux","Feedstock", "NR")
all.eb.ann<-merge(eb.fluxes.ann, nr.ann)

aggnum<-aggregate(all.eb.ann$watts, by=list(all.eb.ann$Flux), FUN='mean')

#Separate plots, gridded
library(gridExtra)

p.mg<-ggplot(misc.eb, aes(fill=lab, y=val, x=month)) + 
  geom_bar(position="stack", stat="identity")+
  theme_minimal()+
  geom_point(aes(y=NR, group=lab), show.legend = FALSE)+
  scale_fill_brewer(palette="Blues")+
  theme(legend.text=element_text(size=22),axis.text=element_text(size=18),plot.title=element_text(size=26),axis.title=element_text(size=26,face="bold"),legend.position=c(0.9,0.75), axis.title.y = element_blank(),legend.title=element_blank())+
  ylim(-10, 170)+
  ggtitle("miscanthus")



p.zm<-ggplot(maize.eb, aes(fill=lab, y=val, x=month)) + 
  geom_bar(position="stack", stat="identity")+
  theme_minimal()+
  geom_point(aes(y=NR, group=lab), show.legend = FALSE)+
  scale_fill_brewer(palette="Oranges")+
  theme(legend.text=element_text(size=22),axis.text=element_text(size=18),plot.title=element_text(size=26),axis.title=element_text(size=26,face="bold"),legend.position=c(0.9,0.75),axis.title.y = element_blank(),legend.title=element_blank())+
  ylim(-10, 170)+
  ggtitle("maize")

p.sb<-ggplot(sorg.eb, aes(fill=lab, y=val, x=month)) + 
  geom_bar(position="stack", stat="identity")+
  theme_minimal()+
  geom_point(aes(y=NR, group=lab), show.legend = FALSE)+
  scale_fill_brewer(palette="Greens")+
  theme(legend.text=element_text(size=22),axis.text=element_text(size=18),plot.title=element_text(size=26),axis.title=element_text(size=26,face="bold"),legend.position=c(0.9,0.75), axis.title.y = element_blank(),legend.title=element_blank())+
  ylim(-10, 170)+
  ggtitle("sorghum")



p.sw<-ggplot(switch.eb, aes(fill=lab, y=val, x=month)) + 
  geom_bar(position="stack", stat="identity")+
  theme_minimal()+
  geom_point(aes(y=NR, group=lab), show.legend = FALSE)+
  scale_fill_brewer(palette="RdPu")+
  theme(legend.text=element_text(size=22),axis.text=element_text(size=18),plot.title=element_text(size=26),axis.title=element_text(size=26,face="bold"),legend.position=c(0.9,0.75), axis.title.y = element_blank(),legend.title=element_blank())+
  ylim(-10, 170)+
  ggtitle("switchgrass")

#p.sw



turb<-grid.arrange(p.zm,  p.mg, p.sb, p.sw, ncol=4)
turb

ggsave(filename='C:/Users/Bethany/Desktop/turbulent.png', device = 'png', units="in", width=16, height=7.5, plot=turb)


# grid.arrange(p.zm, p.mg,p.sb, p.sw)
# 
# ggsave(filename='C:/Users/Bethany/Desktop/turbulent.png', device = 'png', units="in", width=16, height=7, plot=turb)
# 
# ggplot(all.eb, aes(fill=lab, y=val, x=month)) +
#   geom_bar(position="stack", stat="identity")+
#   theme_minimal()+
#   facet_grid(~id)+
#   geom_point(aes(y=NR, group=lab), show.legend = FALSE)+
#   scale_fill_manual(values=c("dark gray", "dark red","blue4"))
# 
# 
# ggplot(all.eb, aes(fill=lab, y=val, x=id)) +
#   geom_bar(position="stack", stat="identity")+
#   theme_minimal()+
#   facet_grid(~month)+
#   geom_point(aes(y=NR, group=lab), show.legend = FALSE)+
#   scale_fill_manual(values=c("dark gray", "dark red","blue4"))

##Data availability

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

test<-rbind(mgb, mgc, zmb, zmc,sw, sb, np)


avail<-ggplot(test) +
  aes(x = yr, y = y, fill = feedstock) +
  geom_tile(size = 1.5)+
  scale_fill_manual(values=c("orange", "bisque","blue", "light blue", "plum3", "forest green", "light pink")) +
  theme_minimal()+
  scale_x_continuous(breaks = seq(from=2008, to=2022, by=2))+
  xlab("year")+
  theme(legend.text=element_text(size=18),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.text=element_text(size=30),
        axis.title=element_text(size=26,face="bold"), axis.title.y = element_blank(),
        legend.title=element_blank(), panel.grid.minor.x = element_line(color = "black"),
        panel.grid.major.x = element_line(color = "black"))
  

avail

png('C:/Users/Bethany/Desktop/avail.png', width=1500, height=500)
avail
dev.off()

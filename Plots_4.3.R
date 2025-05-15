#Figures for Manuscript
#if(!exists("sorg.fg.sm")){source("EnergyFluxes_2.1.R")} 
#Most stuff from  energy fluxes has been ported over here
#Should figure out how to remove from pipeline eventually

printnum=TRUE #set to true if you want numerical values to print out

#Carbon fluxes, incl. cumulative plot####


#Cumulative plot #####
#points version of squiggle plot (requires "long record" be run)
par(bty='n', mfrow=c(1,1))

decyr<-function(date){
  yr<-as.numeric(format(date, "%Y"))
  dec<-as.numeric(format(date, "%j"))/366
  decyr<-yr+dec
  return(decyr)
}

ts<-unpack.time(maize.merge)
yearstart<-function(dat){
  ind<- which(format(dat$xlDateTime, "%j")=="001" & format(dat$xlDateTime, "%H")=="00" & format(dat$xlDateTime, "%M")=="30")
  ind<-c(ind, nrow(dat))
   return(ind)
}

#as currently coded, a pretty inefficient way to subset to Jan 1s

par(mar= c(3,5.5,1,1))

plot(maize.hvst.sum[yearstart(maize.merge)]~decyr(maize.merge$xlDateTime[yearstart(maize.merge)]), xlim=c(2009, 2023), col=alpha(col.zm[yearstart(maize.merge)], 0), ylim=c(-45, 40), ylab=bquote("Cumulative NECB, Mg C"~ha^-1), xlab='', yaxt="n", xaxt = "n", cex.lab=2, cex=2.8, pch=16)
abline(h=c(-60, -40, -20, 0, 20, 40),v=seq(from=2009, to=2023, by=2), col='light gray')
axis(2, labels=c(-60, -40, -20, 0, 20, 40), at=c(-60,-40, -20, 0, 20, 40), col='white', cex.axis=1.6)
axis(1, labels=seq(from=2009, to=2023, by=2), at=seq(from=2009, to=2023, by=2), col='white', cex.axis=1.6)
abline(h=0, lwd=2, lty=2)

#show transition years
xbound<-c(2016,2016,2018,2018)#c(rep(yearstart(maize.merge)[years==2016],2), rep(yearstart(maize.merge)[years==2017], 2))
ybound<-c(-50,50,50,-50)
polygon(xbound, ybound, angle=45, density=20, col=adjustcolor("dark red", alpha.f = 0.50), border="NA" )

#error brackets(here so that points overlap)
polygonize<-function(err.df, dat, dat.hvst){
  dates<-decyr(dat$xlDateTime[yearstart(dat)])
  xax<-c(dates, rev(dates))
  
  ctr<-dat.hvst[yearstart(dat)]
  err<-c(0, err.df$cumerr_pad)
  
  val<-c(ctr-err, rev(ctr+err))
  
  polygon(x=xax, y=val, col =adjustcolor("gray", alpha.f = 0.8), border=NA)
  
}
polygonize(err.df=allerr$maize.merge, dat=maize.merge,dat.hvst=maize.hvst.sum)
polygonize(err.df=allerr$maize.c.merge, dat=maize.c.merge,dat.hvst=maize.c.hvst.sum)
polygonize(err.df=allerr$misc.c.merge, dat=misc.c.merge,dat.hvst=misc.c.hvst.sum)
polygonize(err.df=allerr$misc.merge, dat=misc.merge,dat.hvst=misc.hvst.sum)
polygonize(err.df=allerr$switchgrass, dat=switchgrass,dat.hvst=switch.hvst.sum)
polygonize(err.df=allerr$nativeprairie, dat=nativeprairie,dat.hvst=np.hvst.sum)
polygonize(err.df=allerr$sorg.merge, dat=sorg.merge,dat.hvst=sorg.hvst.sum)


points(misc.hvst.sum[yearstart(misc.merge)]~decyr(misc.merge$xlDateTime[yearstart(misc.merge)]), col=alpha('blue',0.8), pch=17, cex=2.2)
points(switch.hvst.sum[yearstart(switchgrass)]~decyr(switchgrass$xlDateTime[yearstart(switchgrass)]), col="pink", bg=alpha('pink', 0.8), pch=25, cex=2.2)
points(sorg.hvst.sum[yearstart(sorg.merge)]~decyr(sorg.merge$xlDateTime[yearstart(sorg.merge)]), col=alpha(col.sb[yearstart(sorg.merge)], 0.8), pch=15, cex=2.2)
points(misc.c.hvst.sum[yearstart(misc.c.merge)]~decyr(misc.c.merge$xlDateTime[yearstart(misc.c.merge)]), col=alpha('light blue', 0.8), pch=17, cex=2.2)
points(maize.c.hvst.sum[yearstart(maize.c.merge)]~decyr(maize.c.merge$xlDateTime[yearstart(maize.c.merge)]), col=alpha(col.zmc[yearstart(maize.c.merge)], 0.8), cex=2.2, pch=16)
points(np.hvst.sum[yearstart(nativeprairie)]~decyr(nativeprairie$xlDateTime[yearstart(nativeprairie)]), col="plum3", bg=alpha("plum3",0.8), pch=23, cex=2.2)
points(maize.hvst.sum[yearstart(maize.merge)]~decyr(maize.merge$xlDateTime[yearstart(maize.merge)]), col=alpha(col.zm[yearstart(maize.merge)],0.8), bg=alpha(col.zm[yearstart(maize.merge)],0.8), pch=16, cex=2.2)



legend(as.numeric(min(decyr(maize$xlDateTime))), 40, legend=c("maize-soy 1", "maize-soy 2", "miscanthus 1", "miscanthus 2", "native prairie", "switchgrass","sorghum-soy"), col=c("orange","yellow","blue", "light blue", "plum3", "pink", "forest green"), 
       pch=c(16,16,17,17,23,25,15), pt.bg=c(rep(NA, 4),"plum3","light pink", NA), bty='n', pt.cex=2.2, cex=1.1, ncol=2, text.font = 2, text.width=2, x.intersp = 0.4, y.intersp=.8)


legend(as.numeric(min(decyr(maize$xlDateTime)))-0.2, 40.3, legend="", pch=16, col="light green", bty="n",pt.cex=2.2)
legend(as.numeric(min(decyr(maize$xlDateTime)))-0.2, 36, legend="", pch=16, col="light green", bty="n",pt.cex=2.2)
legend(as.numeric(min(decyr(maize$xlDateTime)))+2.5, 32.5, legend="", pch=15, col="light green", bty="n",pt.cex=2.2)



dev.copy(png,'D:/R/Fluxes/Writeout/Plots/fig2_cflux_over_time.png', width=900, height=500)
dev.off()


# #Original squiggle plot
# par(mar=c(3,5,1,1))
# plot(maize.hvst.sum[samp]~decyr(maize.merge$xlDateTime[samp]), ylim=c(-45,40), col="white", pch='.', ylab="Cumulative NECB, MgC ha-1", xlab='', cex=2, yaxt="n", xaxt = "n", cex.lab=2); 
# abline(h=c(-60, -40, -20, 0, 20, 40),v=seq(from=2009, to=2023, by=2), col='light gray')
# axis(2, labels=c(-60, -40, -20, 0, 20, 40), at=c(-60,-40, -20, 0, 20, 40), col='white', cex.axis=1.6)
# axis(1, labels=seq(from=2009, to=2023, by=2), at=seq(from=2009, to=2023, by=2), col='white', cex.axis=1.6)
# abline(h=0, lwd=2, lty=2)
# 
# points(maize.hvst.sum[samp]~decyr(maize.merge$xlDateTime[samp]), col=col.zm[samp], pch='.', cex=3)
# 
# points(misc.hvst.sum[samp]~decyr(misc.merge$xlDateTime[samp]), col='blue', pch='.', cex=3)
# points(switch.hvst.sum[samp]~decyr(switchgrass$xlDateTime[samp]), col='pink', pch='.', cex=3)
# points(sorg.hvst.sum[samp]~decyr(sorg.merge$xlDateTime[samp]), col=col.sb[samp], pch='.', cex=3)
# points(misc.c.hvst.sum[samp]~decyr(misc.c.merge$xlDateTime[samp]), col='light blue', pch='.', cex=3)
# points(maize.c.hvst.sum[samp]~decyr(maize.c.merge$xlDateTime[samp]), col=col.zmc[samp], pch='.', cex=3)
# points(np.hvst.sum[samp]~decyr(nativeprairie$xlDateTime[samp]), col="plum3" , pch='.', cex=3)
# 
# legend(as.numeric(min(decyr(maize$xlDateTime))), 40, legend=c("maize-soy 1", "maize-soy 2", "soybean", "miscanthus 1", "miscanthus 2", "native prairie", "switchgrass","sorghum-soy" ), col=c("orange","yellow","light green","blue", "light blue", "plum3", "pink", "forest green"), 
#        lwd=2, bty='n', cex=1.1, ncol=2, text.font = 2, text.width=2, x.intersp = 0.3, y.intersp=0.7)
# 
# 
# dev.copy(png,'D:/R/Fluxes/Writeout/Plots/fig2_cflux_over_time_full.png', width=900, height=500)
# dev.off()

#Cumulative numbers
if(printnum=TRUE){
#og
print(paste("switcgrass 2009-2015:", tail(switch.hvst.sum[format(switchgrass$xlDateTime, "%Y")%in%c(2009:2015)],n=1)))
print(paste("prairie 2009-2015:", tail(np.hvst.sum[format(nativeprairie$xlDateTime, "%Y")%in%c(2009:2015)],n=1)))
print(paste("miscanthus 2009-2015:", tail(misc.hvst.sum[format(misc.merge$xlDateTime, "%Y")%in%c(2009:2015)],n=1)))
print(paste("maize-soy 2009-2015:", tail(maize.hvst.sum[format(maize.merge$xlDateTime, "%Y")%in%c(2009:2015)],n=1)))

#full end

print(paste("maize-soy 2009-2022:", tail(maize.hvst.sum[format(maize.merge$xlDateTime, "%Y")%in%c(2009:2022)],n=1)))
print(paste("miscanthus 2009-2022:", tail(misc.hvst.sum[format(misc.merge$xlDateTime, "%Y")%in%c(2009:2022)],n=1)))

#shorties
print(paste("sorghum 2019-2022:", tail(sorg.hvst.sum[format(sorg.merge$xlDateTime, "%Y")%in%c(2019:2022)],n=1)))
print(paste("maize-soy 2 2018-2022:", tail(maize.c.hvst.sum[format(maize.c.merge$xlDateTime, "%Y")%in%c(2018:2022)],n=1)))
print(paste("miscanthus 2 2018-2022:", tail(misc.c.hvst.sum[format(misc.c.merge$xlDateTime, "%Y")%in%c(2018:2022)],n=1)))

#maize 1 a little tricky to subset; need to subtract off cumulative emissions up until 2018:
start<-which(format(maize.merge$xlDateTime, "%Y")==2017)
pre<-tail(maize.hvst.sum[format(maize.merge$xlDateTime, "%Y")==2017], n=1)#value on the 1st of 2018
print(paste("maize-soy 1 2018-2022:", tail(maize.hvst.sum[format(maize.merge$xlDateTime, "%Y")%in%c(2018:2022)],n=1)-pre))
#misc 1 also a little tricky to subset; need to subtract off cumulative emissions up until 2018:
start<-which(format(misc.merge$xlDateTime, "%Y")==2017)
pre<-tail(misc.hvst.sum[format(misc.merge$xlDateTime, "%Y")==2017], n=1)#value on the 1st of 2018
print(paste("miscanthus 1 2018-2022:", tail(misc.hvst.sum[format(misc.merge$xlDateTime, "%Y")%in%c(2018:2022)],n=1)-pre))



#Errors
print("P1:")
for (i in (1:length(allerr))){
p1.ind<-which(allerr[[i]]$year==2015)
print(paste(names(allerr)[[i]],":", allerr[[i]]$cumerr_pad[p1.ind]))
}

print("full")
for (i in (1:length(allerr))){
  print(paste(names(allerr)[[i]],":",tail(allerr[[i]]$cumerr_pad, 1)))
}

print("p2:")

for (i in (1:length(allerr))){
  
  err.syn<-allerr[[i]]
  pad<-which(is.na(allerr[[i]]$year))
  
  err.syn$toterr[pad]<-mean(diff(err.syn$cumerr_pad[pad]))
  
  ind<-c(which(allerr[[i]]$year%in%c(2018:2022)), pad)
  print(paste(names(allerr)[i],":",sum(err.syn$toterr[ind])))
  
  
}

}

#####

#Set up Boxplots
getcflux<-function(dat, yield, nodatyr=0, id){
  
  #Unit conversions to tC/ha
  dat.gpd<-dat$Fc*0.0792 #gc02/30min
  dat.gpd.tha<-dat.gpd*.0027 #tC/ha/30min
  
  #Put yields into cumulative time series
  years<-unique(as.numeric(format(dat$xlDateTime,"%Y")))
  Y<-(as.numeric(format(dat$xlDateTime, "%Y")))
  endofyear<-c(match(years,Y )-1, length(Y))#[2:(length(years)-(nodatyr-1))]
  
  dat.hvst<-dat.gpd.tha
  dat.hvst.30<-dat.hvst
  dat.hvst.30[endofyear]<-dat.hvst.30[endofyear]+yield$yield # pretend there's a giant flux of C from harvest on the 31st of each year
  dat.hvst.sum<-cumsum(dat.hvst.30)
  #plot(dat.hvst.sum~dat$xlDateTime)
  
  firstyear<-dat.hvst.sum[endofyear][1]#firstyear is the value at the end of the first year, i.e. year ond fluxes plus harvest
  
  yearseq<-c(firstyear, as.numeric(diff(dat.hvst.sum[endofyear]))) #net carbon storage after harvest removal each year
  year<-as.numeric(format(dat$xlDateTime[endofyear], "%Y"))#[1:(length(endofyear))])
  lab<-rep(id, length(year))
  
  yield<-yield$yield
  
  return(cbind.data.frame(year, yearseq, lab, yield))
  
}

switch.cflux<-getcflux(switchgrass, switch.yield, nodatyr=0, id="switchgrass")
misc.cflux<-getcflux(misc.merge, misc.yield, id="mgb") #was 1
misc.c.cflux<-getcflux(misc.c.merge, misc.c.yield, id="mgc")
sorg.cflux<-getcflux(sorg.merge, sorg.yield, id="sorghum-soy" ) #was 1
maize.cflux<-getcflux(maize.merge, maize.yield, id="zmb") #was 1?
maize.c.cflux<-getcflux(maize.c.merge, maize.c.yield, id="zmc")
np.cflux<-getcflux(nativeprairie, np.yield, nodatyr=0, id="prairie")

nosoy<-c(2008, 2009, 2011, 2012, 2014, 2015, 2017, 2018, 2020, 2021)

fluxes.raw<-rbind(switch.cflux,misc.cflux, sorg.cflux, maize.cflux, maize.c.cflux, misc.c.cflux, np.cflux)
fluxes.gm<-fluxes.raw
fluxes.gm$lab[which(!fluxes.gm$year%in%nosoy&fluxes.gm$lab%in%c('zmc','zmb', 'zm', 'sorghum'))]<-'soy'

fluxes.gm$lab[fluxes.gm$lab=="zmb"]<-"maize-soy 1"; fluxes.gm$lab[fluxes.gm$lab=="zmc"]<-"maize-soy 2"
fluxes.gm$lab[fluxes.gm$lab=="mgb"]<-"mxg 1"; fluxes.gm$lab[fluxes.gm$lab=="mgc"]<-"mxg 2"

fluxes<-fluxes.raw
fluxes$lab[fluxes$lab=="zmb"]<-"maize-soy 1"; fluxes$lab[fluxes$lab=="zmc"]<-"maize-soy 2"
fluxes$lab[fluxes$lab=="mgb"]<-"mxg 1"; fluxes$lab[fluxes$lab=="mgc"]<-"mxg 2"


fluxes.merge<-fluxes.raw
fluxes.merge$lab[fluxes.merge$lab%in%c('zmc','zmb')]<-"maize"
fluxes.merge$lab[fluxes.merge$lab%in%c('mgc','mgb')]<-"miscanthus"


fluxes.merge.gm<-fluxes.merge
fluxes.merge.gm$lab[which(!fluxes.merge.gm$year%in%nosoy&fluxes.merge.gm$lab%in%c('zmc','zmb', 'maize', 'sorghum'))]<-'soy'


#color sets for unmerged sites, soy not separate
colset.kitsin<-c("blue", "light blue", "plum3", "forest green", "light pink",  "orange", "yellow")
colset.og<-c("orange","blue", "plum3","light pink")
colset.post<-c("orange", "yellow", "blue", "light blue", "forest green")

#color sets for unmerged sites, soy separate
colset.kitsin.gm<-c("light green","blue", "light blue", "plum3","forest green", "light pink",  "orange", "yellow")
colset.og.gm<-c("orange","blue","plum3","light green", "light pink")
colset.post.gm<-c("light green","blue", "light blue", "forest green",  "orange", "yellow")
colset.post.gm.labs<-c("orange", "yellow", "blue", "light blue", "forest green", "light green")

#color sets for merged sites, soy not separate
colset.merge<-c( "orange", "blue", "plum3", "forest green", "light pink")
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

all<-ggplot(subset(fluxes.merge.gm, year%in%c(2009:2022))) +
  aes(x = lab, y = yearseq) +
  geom_hline(yintercept=0)+
  geom_boxplot(shape = "circle", fill=colset.merge.gm)+
  geom_label(label="2009 - 2022", x=4.8, y=9.5, size=10)+
  theme_minimal()+
  labs(y = "NECB (Mg C ha-1 y-1)", x=NULL)+
  ylim(-10, 10)+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=30,face="bold"))

all

png('D:/R/Fluxes/Writeout/Plots/alt/fig3alt_combinedyr.png', width=900, height=500)
all
dev.off()


#original four
og<-ggplot(subset(fluxes.merge, year%in%c(2009:2015))) +
  aes(x = lab, y = yearseq) +
  geom_hline(yintercept=0)+
  geom_boxplot(shape = "circle", fill = colset.og)+
  theme_minimal()+
  labs(y = bquote("NECB (Mg C"~ha^-1~y^-1*")"), x=NULL)+
  ylim(-10, 10)+
  geom_label(label="2009 - 2015", x=4, y=9.5, size=10)+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=30,face="bold"))

  og
  
  png('D:/R/FLuxes/Writeout/Plots/fig3a_ogyr.png', width=700, height=500)
  og
  dev.off()
  
  if(printnum==TRUE){
    
    print("og year C averages:")
    print(aggregate(cbind(yearseq, yield)~lab, data=subset(fluxes.merge,year%in%c(2009:2015)), FUN="mean", na.rm=TRUE))
    
    print("og year sd:")
    print(aggregate(cbind(yearseq, yield)~lab, data=subset(fluxes.merge,year%in%c(2009:2015)), FUN="sd", na.rm=TRUE))
    
    
    #exceptions
    print("yields without establishment")
    print(aggregate(yield~lab, data=subset(fluxes.merge,year%in%c(2010:2015)), FUN="mean", na.rm=TRUE))
    
    
    print("fluxes without maize burp")
    print(aggregate(yearseq~lab, data=subset(fluxes.merge,year%in%c(2009:2013, 2015)), FUN="mean", na.rm=TRUE))
    print(aggregate(yearseq~lab, data=subset(fluxes.merge,year%in%c(2009:2013, 2015)), FUN="sd", na.rm=TRUE))
    
    }


#Set post 2017
newyr<-ggplot(subset(fluxes, year%in%c(2018:2022))) +
  aes(x = lab, y = yearseq) +
  geom_hline(yintercept=0)+
  geom_boxplot(shape = "circle", fill=colset.post)+
  theme_minimal()+
  labs(y = bquote("NECB (Mg C"~ha^-1~y^-1*")"), x=NULL)+
  ylim(-10, 10)+
  geom_label(label="2018 - 2022", x=5, y=9.5, size=10)+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=30,face="bold"))


newyr

png('D:/R/FLuxes/Writeout/Plots/fig3b_newyr.png', width=800, height=500)
newyr
dev.off()

#numbers 

if(printnum==TRUE){
  
print("new year C averages:")
print(aggregate(cbind(yearseq, yield)~lab, data=subset(fluxes,year%in%c(2018:2022)), FUN="mean", na.rm=TRUE))

print("new year sd:")
print(aggregate(cbind(yearseq, yield)~lab, data=subset(fluxes,year%in%c(20:2022)), FUN="sd", na.rm=TRUE))



#miscanthus sinkage really skewed by burn year; 
sub<-fluxes[fluxes$lab=="mxg 1" & fluxes$year%in%c(2018:2020),]
mean(sub$yearseq); sd(sub$yearseq)

#yield plots, experimental
#plot(yield~year, data=subset(fluxes, lab=="mxg 1"), col='blue')
#summary(lm(yield~year, data=subset(fluxes, lab=="mxg 1" & year%in%c(2010:2022))))
}








#####  

#Yields####

yield<-ggplot(subset(fluxes.merge.gm, year%in%c(2009:2022))) +
  aes(x = lab, y = yield) +
  geom_hline(yintercept=0)+
  geom_boxplot(shape = "circle", fill=colset.merge.gm)+
  theme_minimal()+
  labs(x = NULL, y = bquote("Harvested C, Mg C"~ha^-1~y^-1))+
  ylim(0,9)+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=30,face="bold"))

yield


# png('D:/R/Fluxes/Writeout/Plots/fig3alt_combinedyield.png', width=900, height=500)
# yield
# dev.off()



#original years
ogyield<-ggplot(subset(fluxes.merge, year%in%c(2009:2015))) +
  aes(x = lab, y = yield) +
  geom_hline(yintercept=0)+
  geom_boxplot(shape = "circle", fill=colset.og)+
  theme_minimal()+
  labs(x = NULL, y = bquote("Harvested C (Mg C"~ha^-1~y^-1*")"))+
  ylim(0,9)+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=30,face="bold"))

ogyield

png('D:/R/Fluxes/Writeout/Plots/fig3c_ogyield.png', width=700, height=500)
ogyield
dev.off()



#later years
newyield<-ggplot(subset(fluxes, year%in%c(2018:2022))) +
  aes(x = lab, y = yield) +
  geom_hline(yintercept=0)+
  geom_boxplot(shape = "circle", fill=colset.post)+
  theme_minimal()+
  labs(x = NULL, y = bquote("Harvested C (Mg C"~ha^-1~y^-1*")"))+
  ylim(0,9)+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=30,face="bold"))

newyield


png('D:/R/Fluxes/Writeout/Plots/fig3d_newyield.png', width=800, height=500)
newyield
dev.off()

if(printnum==TRUE){
  
print("Miscanthus yield range:")
aggregate(yield~lab, data=fluxes, FUN="mean", na.rm=TRUE)
print("Without establishment years")
aggregate(yield~lab, data=subset(fluxes, year>2010), FUN="mean", na.rm=TRUE)
print("Phase 2 only:")
aggregate(yield~lab, data=subset(fluxes, year%in%c(2019:2022)), FUN="mean", na.rm=TRUE)
  
}


#####


###Albedo####

#Albedo Variability#
albvar<-function(dat, id, years=c(2009:2022),adj=FALSE){
  #subset to years of interest
  yearvec<-as.numeric(format(dat$xlDateTime, "%Y"))
  subset<-which(yearvec%in%years)
  dat<-dat[subset,]
  dat.alb<-dat$Fsu/dat$Fsd; dat.alb[dat.alb<0|dat.alb>1|dat$Fsd<10]<-NA
  dat.month<-as.numeric(format(dat$xlDateTime, "%m"))
  dat.yr<-as.numeric(format(dat$xlDateTime, "%Y"))
  if(adj==TRUE){
  ind<-which(dat.month%in%c(3:6, 11:12))
  dat.alb[ind]<-dat.alb[ind]+0.08
  }
  
  dat.agg<-aggregate(dat.alb~dat.month+dat.yr, FUN='mean', na.rm=TRUE)
  dat.agg$id<-(id)

  
  return(dat.agg)
}


sorg.albvar<-albvar(sorg.merge, "sb", adj=TRUE)#adj adjusts for soil color
sorg.albvar.alt<-albvar(sorg.merge, "sb.a")
misc.albvar<-albvar(misc.merge,"mgb")
misc.c.albvar<-albvar(misc.c.merge, "mgc")
maize.albvar<-albvar(maize.merge, "zmb")
maize.c.albvar<-albvar(maize.c.merge, "zmc")
switch.albvar<-albvar(switchgrass, "sw")
np.albvar<-albvar(nativeprairie, "np")



zm.albvar<-merge(maize.albvar, maize.c.albvar, all=TRUE); zm.albvar$id<-"zm"
mg.albvar<-merge(misc.albvar, misc.c.albvar, all=TRUE); mg.albvar$id<-"mg"


all.albvar<-rbind(sorg.albvar, misc.albvar, misc.c.albvar, maize.albvar, maize.c.albvar, switch.albvar, np.albvar) #corrected sorghum
all.albvar.merge<-rbind(sorg.albvar, mg.albvar, zm.albvar, switch.albvar, np.albvar) #corrected sorghum
all.albvar.opt<-rbind(sorg.albvar, sorg.albvar.alt, misc.albvar, misc.c.albvar, maize.albvar, maize.c.albvar, switch.albvar, np.albvar) #both sorghum included
all.albvar.alt<-rbind(sorg.albvar.alt, misc.albvar, misc.c.albvar, maize.albvar, maize.c.albvar, switch.albvar, np.albvar) #uncorrected sorghum


all.albvar.p1<-rbind(misc.albvar, maize.albvar, switch.albvar, np.albvar)
all.albvar.p1<-aggregate(data=all.albvar.p1, dat.alb~dat.month+dat.yr+id, FUN='mean')
all.albvar.p1$dat.month<-as.factor(all.albvar.p1$dat.month)

#uncorrected sorghum
all.albvar.p2<-rbind(rbind(sorg.albvar, misc.albvar, misc.c.albvar, maize.albvar, maize.c.albvar))
all.albvar.p2<-aggregate(data=all.albvar.p2, dat.alb~dat.month+dat.yr+id, FUN='mean')
all.albvar.p2$dat.month<-as.factor(all.albvar.p2$dat.month)

#corrected sorghum
all.albvar.p2.alt<-rbind(rbind(sorg.albvar.alt, misc.albvar, misc.c.albvar, maize.albvar, maize.c.albvar))
all.albvar.p2.alt<-aggregate(data=all.albvar.p2.alt, dat.alb~dat.month+dat.yr+id, FUN='mean')
all.albvar.p2.alt$dat.month<-as.factor(all.albvar.p2.alt$dat.month)

#both sorghum included
all.albvar.p2.opt<-rbind(rbind(sorg.albvar, sorg.albvar.alt, misc.albvar, misc.c.albvar, maize.albvar, maize.c.albvar))
all.albvar.p2.opt<-aggregate(data=all.albvar.p2.opt, dat.alb~dat.month+dat.yr+id, FUN='mean')
all.albvar.p2.opt$dat.month<-as.factor(all.albvar.p2.opt$dat.month)

#For merged plots (unsure why necessary, but doesn't plot right without it)
all.albvar.av.opt<-rbind(sorg.albvar, sorg.albvar.alt, mg.albvar, zm.albvar, switch.albvar, np.albvar)
all.albvar.av.opt<-aggregate(data=all.albvar.av.opt, dat.alb~dat.month+dat.yr+id, FUN='mean')


#Make months factors
all.albvar$dat.month<-as.factor(all.albvar$dat.month)
all.albvar.merge$dat.month<-as.factor(all.albvar.merge$dat.month)
all.albvar.alt$dat.month<-as.factor(all.albvar.alt$dat.month)
all.albvar.opt$dat.month<-as.factor(all.albvar.opt$dat.month)
all.albvar.av.opt$dat.month<-as.factor(all.albvar.av.opt$dat.month)



#Get some numbers
if(printnum==TRUE){
  
  alb.yr.p1<-aggregate(dat.alb~dat.yr+id, data=subset(all.albvar.p1, dat.yr%in%c(2009:2015)), FUN='mean')
  alb.p1<-aggregate(dat.alb~id, data=alb.yr.p1, FUN="mean")
  alb.p1$diff<-alb.p1$dat.alb-alb.p1$dat.alb[alb.p1$id=="zmb"]
  
  alb.p1
  
  alb.yr.p2<-aggregate(dat.alb~dat.yr+id, data=subset(all.albvar.p2.opt, dat.yr%in%c(2018:2022)), FUN='mean')
  alb.p2<-aggregate(dat.alb~id, data=alb.yr.p2, FUN="mean")
  alb.p2$diff<-alb.p2$dat.alb-((alb.p2$dat.alb[alb.p2$id=="zmb"]+ alb.p2$dat.alb[alb.p2$id=="zmc"])/2)
  
  #sorghum both ways (sb.a is UNadjusted)
  alb.p2
  
  
  
}


#####

###Albedo plots ####

pal.full <- c("zm" = "yellow","zmc" = "yellow", "zmb" = "orange", "mg" = "blue","mgc" = "light blue", 
         "mgb" = "blue", "sw" = "light pink", "sb" = "forestgreen","sb.a" = "honeydew2","gm" = "light green", "np"="plum3")

#Separate time periods
pal<-c("zmc" = "yellow", "zmb" = "orange","mgc" = "light blue", "mgb" = "blue", "sw" = "light pink", "sb" = "forestgreen", "np"="plum3")
pal.opt<-c("zmc" = "yellow", "zmb" = "orange","mgc" = "light blue", "mgb" = "blue", "sw" = "light pink", "sb" = "forestgreen", "sb.a" = "honeydew2","np"="plum3")
labs<-c("zmc"="maize-soy 2", "zmb" = "maize-soy 1", "mgb" = "mxg 1", "mgc" = "mxg 2", "sb"="sorghum-soy", "np" = "praire", "sw" = "switchgrass")
labs.opt<-c("zmc"="maize-soy 2", "zmb" = "maize-soy 1", "mgb" = "mxg 1", "mgc" = "mxg 2", "sb"="sorghum-soy" , "sb.a" = "sorg. unadj.", "np" = "praire", "sw" = "switchgrass")

#merged time periods
pal.merge<-c("zm" = "orange", "mg" = "blue", "sw" = "light pink", "sb" = "forestgreen","np"="plum3")
pal.merge.opt<-c("zm" = "orange", "mg" = "blue", "sw" = "light pink", "sb" = "forestgreen", "sb.a" = "honeydew2","np"="plum3")
labs.merge<-c("zm"="maize", "mg" = "miscanthus", "sb"="sorghum-soy" , "np" = "praire", "sw" = "switchgrass")
labs.merge.opt<-c("zm"="maize", "mg" = "miscanthus", "sb"="sorghum-soy" ,"sb.a"="sorg. unadj", "np" = "praire", "sw" = "switchgrass")

#Original
ogalb<-ggplot(subset(all.albvar.p1, dat.yr%in%c(2009:2015)), aes(x=dat.month, y=dat.alb, fill=id),) + 
  geom_boxplot(outlier.size=0.3)+
  theme_minimal()+
  scale_fill_manual(values=pal, labels=labs)+#c("blue", "plum3", "forest green", "light pink", "orange"),labels=c('miscanthus',  'native prairie','sorghum', 'switchgrass','maize'))+
  ylab("albedo (unitless)")+
  xlab("month")+
  ylim(0.09, 0.6)+
  geom_label(label="2009 - 2015", x=11, y=0.75, size=11, show.legend = FALSE, inherit.aes = FALSE)+
  theme(axis.text=element_text(size=30),axis.title=element_text(size=32,face="bold"),legend.text=element_text(size=24), legend.title = element_blank())

ogalb

png('D:/R/Fluxes/Writeout/Plots/fig4a_ogalb.png', width=1300, height=700)
ogalb
dev.off()

#New
newalb<-ggplot(subset(all.albvar.p2, dat.yr%in%c(2018:2022)), aes(x=dat.month, y=dat.alb, fill=id),) + 
  geom_boxplot(outlier.size=0.3)+
  theme_minimal()+
  scale_fill_manual(values=pal, labels=labs)+#c("blue", "plum3", "forest green", "light pink", "orange"),labels=c('miscanthus',  'native prairie','sorghum', 'switchgrass','maize'))+
  ylab("albedo (unitless)")+
  xlab("month")+
  ylim(0.09, 0.6)+
  geom_label(label="2018 - 2022", x=11, y=0.75, size=11, show.legend = FALSE, inherit.aes = FALSE)+
  theme(axis.text=element_text(size=30),axis.title=element_text(size=32,face="bold"),legend.text=element_text(size=24), legend.title = element_blank())
newalb

png('D:/R/Fluxes/Writeout/Plots/alt/fig4b_newalb.png', width=1300, height=700)
newalb
dev.off()


#New, uncorrected sorghum included
newalb.opt<-ggplot(all.albvar.p2.opt, aes(x=dat.month, y=dat.alb, fill=id),) + 
  geom_boxplot(outlier.size=0.3)+
  theme_minimal()+
  scale_fill_manual(values=pal.opt, labels=labs.opt)+#c("blue", "plum3", "forest green", "light pink", "orange"),labels=c('miscanthus',  'native prairie','sorghum', 'switchgrass','maize'))+
  ylab("albedo (unitless)")+
  xlab("month")+
  ylim(0.09, 0.6)+
  geom_label(label="2018 - 2022", x=11, y=0.55, size=11, show.legend = FALSE, inherit.aes = FALSE)+
  theme(axis.text=element_text(size=30),axis.title=element_text(size=32,face="bold"),legend.text=element_text(size=24), legend.title = element_blank())
newalb.opt

png('D:/R/Fluxes/Writeout/Plots/fig4b_newalb_opt.png', width=1300, height=700)
newalb.opt
dev.off()

#Merged time periods, for posters ####

mergealb<-ggplot(all.albvar.merge, aes(x=dat.month, y=dat.alb, fill=id),) + 
  geom_boxplot(outlier.size=0.3)+
  theme_minimal()+
  scale_fill_manual(values=pal.merge, labels=labs.merge)+#c("blue", "plum3", "forest green", "light pink", "orange"),labels=c('miscanthus',  'native prairie','sorghum', 'switchgrass','maize'))+
  ylab("albedo (unitless)")+
  xlab("month")+
  ylim(0.09, 0.6)+
  geom_label(label="2008 - 2022", x=11, y=0.55, size=11, show.legend = FALSE, inherit.aes = FALSE)+
  theme(axis.text=element_text(size=30),axis.title=element_text(size=32,face="bold"),legend.text=element_text(size=24), legend.title = element_blank())
mergealb

png('D:/R/Fluxes/Writeout/Plots/alt/fig4_mergealb.png', width=1300, height=700)
mergealb
dev.off()



#Merged time periods, uncorrected sorghum included
mergealb.opt<-ggplot(all.albvar.av.opt, aes(x=dat.month, y=dat.alb, fill=id),) + 
  geom_boxplot(outlier.size=0.3)+
  theme_minimal()+
  scale_fill_manual(values=pal.merge.opt, labels=labs.merge.opt)+#c("blue", "plum3", "forest green", "light pink", "orange"),labels=c('miscanthus',  'native prairie','sorghum', 'switchgrass','maize'))+
  ylab("albedo (unitless)")+
  xlab("month")+
  ylim(0.09, 0.6)+
  geom_label(label="2008 - 2022", x=11, y=0.55, size=11, show.legend = FALSE, inherit.aes = FALSE)+
  theme(axis.text=element_text(size=30),axis.title=element_text(size=32,face="bold"),legend.text=element_text(size=24), legend.title = element_blank())
mergealb.opt

png('D:/R/Fluxes/Writeout/Plots/alt/fig4_mergealb_opt.png', width=1300, height=700)
mergealb.opt
dev.off()

#####

#####

#Radiative forcing from Albedo####

#New radiative forcing, adjusted from from GWI; calcualtes TOA RF#


#preliminaries

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


#1) daily albedo
albify<-function(dat, window=1, adj=FALSE, years=c(2009:2022)){
  dat<-subtime(dat, subset=years)
  dat.alb<-dat$Fsu/dat$Fsd; dat.alb[dat.alb<0|dat.alb>1|dat$Fsd<10]<-NA
  
  adj.ind<-which(as.numeric(format(dat$xlDateTime, "%m"))%in%c(3,4,5,6,11,12))
  dat.alb.adj<-dat.alb; dat.alb.adj[adj.ind]<-dat.alb[adj.ind]+0.08
  
  dat.doy<-as.numeric(format(dat$xlDateTime, "%j"))
  dat.d.alb<-aggregate(dat.alb, by=list(dat.doy), FUN='mean', na.rm=TRUE)
  
  if(adj==TRUE){
    dat.d.alb<-aggregate(dat.alb.adj, by=list(dat.doy), FUN='mean', na.rm=TRUE)
  }
  
  dat.alb.sm<-rollapply(dat.d.alb$x, width=window, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
  
  return(dat.alb.sm)
}

#Version to calculate daily albedo without aggregating it across years, for finding interannual variability.
albify_yr<-function(dat, window=1, adj=FALSE, years=c(2009:2022)){
  dat<-subtime(dat, subset=years)
  dat.alb<-dat$Fsu/dat$Fsd; dat.alb[dat.alb<0|dat.alb>1|dat$Fsd<10]<-NA
  
  adj.ind<-which(as.numeric(format(dat$xlDateTime, "%m"))%in%c(3,4,5,6,11,12))
  dat.alb.adj<-dat.alb; dat.alb.adj[adj.ind]<-dat.alb[adj.ind]+0.08
  
  dat.doy<-as.numeric(format(dat$xlDateTime, "%j"))
  dat.yr<-as.numeric(format(dat$xlDateTime, "%Y"))
  
  dat.agg<-cbind(dat.alb,dat.alb.adj, dat.doy, dat.yr); colnames(dat.agg)<-c("alb","alb.adj", "DOY", "YEAR")
  
  dat.d.alb<-aggregate(alb~DOY+YEAR, dat=dat.agg, FUN='mean', na.rm=TRUE)
  
  if(adj==TRUE){
    
    dat.d.alb<-aggregate(alb.adj~DOY+YEAR, dat=dat.agg, FUN='mean', na.rm=TRUE)
    
  }
  
  
  #if some days have been dropped because all alb values were na, this fills the days back in.
  if(nrow(dat.d.alb)!=nrow(toa.yrdat[toa.yrdat$YEAR%in%years,])){
    
    account<-merge(toa.yrdat[toa.yrdat$YEAR%in%years,], dat.d.alb, all.x=TRUE, sort=FALSE)
    dat.d.alb<-account[ ,c(1:2,4)]
    dat.d.alb<- dat.d.alb[order(dat.d.alb$YEAR, dat.d.alb$DOY),]
    
  }
  
  
  dat.alb.sm<-rollapply(dat.d.alb$alb, width=window, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
  
  return(dat.alb.sm)
}



#Calculate ALL albedos
sorg.alb<-albify(sorg.merge, adj=TRUE) #adj=TRUE adjusts sorghum albedo by 8% during the bare soil season
sorg.alb.alt<-albify(sorg.merge, adj=FALSE)
maize.alb<-albify(maize.merge);maize.c.alb<-albify(maize.c.merge)
maize.og.alb<-albify(maize.merge, years=c(2009:2015)); maize.new.alb<-albify(maize.merge, years=c(2018:2022))

misc.alb<-albify(misc.merge);misc.c.alb<-albify(misc.c.merge)
misc.og.alb<-albify(misc.merge, years=c(2009:2015)); misc.new.alb<-albify(misc.merge, years=c(2018:2022))


switch.alb<-albify(switchgrass)
np.alb<-albify(nativeprairie)

#Calculate non-year-merged albedos
if(printnum=TRUE){
  
  #Necessary precursor: full timeseries of theoretical TOA
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
  
  lpyr<-seq(from=2008,to=2022, by=4)
  lpyr.ind<-which(toa.ts.raw$YEAR%in%lpyr);nmyr.ind<-which(!toa.ts.raw$YEAR%in%lpyr)
  toa.yr<-aggregate(toa.dat.raw$SW_IN_POT[nmyr.ind], by=list(toa.ts.raw$DOY[nmyr.ind]), FUN='mean')$x
  toa.lpyr<-aggregate(toa.dat.raw$SW_IN_POT[lpyr.ind], by=list(toa.ts.raw$DOY[lpyr.ind]), FUN='mean')$x
  #lol ameriflux just tacks on a DOY 366
  #Hard coded as 2009 - 2022
  toa.syn<-c(rep(toa.yr, 3),toa.lpyr,  rep(toa.yr, 3),toa.lpyr,  rep(toa.yr, 3),toa.lpyr,  rep(toa.yr, 2) )
  ts<-unpack.time(maize.merge)
  ts<-ts[ts$YEAR>2008,];toa.ts<-aggregate(DECDOY~DOY+YEAR, data=ts, FUN="mean", na.rm=TRUE)
  toa.yrdat<-cbind(toa.ts[1:2], toa.syn)
  
ogyr<-c(2009:2015); newyr<-c(2018:2022)


maize.alb.iav<-albify_yr(maize.merge);maize.c.alb.iav<-albify_yr(maize.c.merge)

maize.og.alb.iav<-albify_yr(maize.merge, years=c(2009:2015)); maize.new.alb.iav<-albify_yr(maize.merge, years=c(2018:2022))
maize.c.new.alb.iav<-albify_yr(maize.c.merge, years=newyr)

maize.new.alb.iav.ref<-rowMeans(cbind(maize.new.alb.iav, maize.c.new.alb.iav))


misc.alb.iav<-albify_yr(misc.merge);misc.c.alb.iav<-albify_yr(misc.c.merge)
misc.og.alb.iav<-albify_yr(misc.merge, years=c(2009:2015)); misc.new.alb.iav<-albify_yr(misc.merge, years=c(2018:2022))
misc.c.new.alb.iav<-albify_yr(misc.c.merge, years=newyr)


switch.alb.iav<-albify_yr(switchgrass); switch.og.alb.iav<-albify_yr(switchgrass, years=ogyr)
np.alb.iav<-albify_yr(nativeprairie); np.og.alb.iav<-albify_yr(nativeprairie, years=ogyr)

sorg.new.alb.iav<-albify_yr(sorg.merge, adj=TRUE, years=c(2019:2022)) #adj=TRUE adjusts sorghum albedo by 8% during the bare soil season
sorg.new.alb.alt.iav<-albify_yr(sorg.merge, adj=FALSE, years=c(2019:2022))

maize.new.alb.iav.sorg<-albify_yr(maize.merge, years=c(2019:2022))
maize.c.new.alb.iav.sorg<-albify_yr(maize.c.merge, years=c(2019:2022))
maize.new.alb.iav.sorg.ref<-rowMeans(cbind(maize.new.alb.iav.sorg,maize.c.new.alb.iav.sorg))


}

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
rfparam<-function(dat, years=c(2009:2022)){
  dat<-subtime(dat, subset=years)
  dat.swin<-aggregate(dat$Fsd, by=list(format(dat$xlDateTime, "%j")), FUN='mean')$x
  dat.trans<-dat.swin/toa
  rfscl<-dat.swin*dat.trans
  
  rfdat<-data.frame(cbind(dat.swin,dat.trans, rfscl)); colnames(rfdat)<-c("swin", "trans", "scalar")
  
  return(rfscl) #change to rfdat if you want the pieces individually
  
}

rfparam_yr<-function(dat, years=c(2009:2022)){
  
  dat<-subtime(dat, subset=years)
  dat.ts<-unpack.time(dat)
  
  dat.agg<-cbind(dat.ts,dat$Fsd); colnames(dat.agg)[ncol(dat.agg)]<-"Fsd" #make aggregatable df
  dat.fsd<-aggregate(Fsd~DOY+YEAR, data=dat.agg, FUN="mean", na.rm=TRUE)
  dat.rfcalc<-merge(toa.yrdat, dat.fsd, sort=FALSE)
  
  dat.swin<-dat.rfcalc$Fsd
  toa<-dat.rfcalc$toa.syn
  
  
  dat.trans<-dat.swin/toa
  rfscl<-dat.swin*dat.trans
  
  rfdat<-data.frame(cbind(dat.swin,dat.trans, rfscl)); colnames(rfdat)<-c("swin", "trans", "scalar")
  
  return(rfscl) #change to rfdat if you want the pieces individually
}



#Calculate ALL RFs ####

maize.alb.ref<-rowMeans(cbind(maize.alb ,maize.c.alb))
maize.new.alb.ref<-rowMeans(cbind(maize.new.alb,maize.c.alb))

#Merged
#maize to sorghum,all
sbrf<-rfparam(sorg.merge)*(maize.alb.ref-sorg.alb); 
sbrf.a<-rfparam(sorg.merge)*(maize.alb.ref-sorg.alb.alt)
#maize to miscanthus, all
mgrf<-rfparam(misc.merge)*(maize.alb.ref-misc.alb)
#maize to switchgrass, all
swrf<-rfparam(switchgrass)*(maize.alb.ref-switch.alb)
#maize to prairie, all
nprf<-rfparam(nativeprairie)*(maize.alb.ref-np.alb)

#Orignal years
#maize to miscanthus, og
mgrf.og<-rfparam(misc.merge, years=c(2009:2015))*(maize.og.alb-misc.og.alb)
#maize to switchgrass, og
swrf.og<-rfparam(switchgrass, years=c(2009:2015))*(maize.og.alb-switch.alb)
#maize to prairie, og
nprf.og<-rfparam(nativeprairie, years=c(2009:2015))*(maize.og.alb-np.alb)

#New years
#maize to other maize, new
#zmrf.new<-rfparam(maize.c.merge, years=c(2018:2022))*(maize.new.alb.ref-maize.c.alb)
#maize to misc 1, new
mgbrf.new<-rfparam(misc.merge, years=c(2018:2022))*(maize.new.alb.ref-misc.new.alb)
#maize to misc 2, new
mgcrf.new<-rfparam(misc.c.merge, years=c(2018:2022))*(maize.new.alb.ref-misc.c.alb)
#maize to sorghum, new
sbrf.new<-rfparam(sorg.merge, years=c(2018:2022))*(maize.new.alb.ref-sorg.alb)
sbrf.new.a<-rfparam(sorg.merge, years=c(2018:2022))*(maize.new.alb.ref-sorg.alb.alt)
#####

#Uncompressed, for finding variability
#if(printnum==TRUE){
  

#Orignal years
#maize to miscanthus, og
mgrf.iav.og<-rfparam_yr(misc.merge, years=c(2009:2015))*(maize.og.alb.iav-misc.og.alb.iav)
#maize to switchgrass, og
swrf.iav.og<-rfparam_yr(switchgrass, years=c(2009:2015))*(maize.og.alb.iav-switch.og.alb.iav)
#maize to prairie, og
nprf.iav.og<-rfparam_yr(nativeprairie, years=c(2009:2015))*(maize.og.alb.iav-np.og.alb.iav)

#New years
#maize to other maize, new
#zmrf.iav.new<-rfparam_yr(maize.c.merge, years=c(2018:2022))*(maize.new.alb.iav.ref-maize.c.new.alb.iav)
#maize to misc 1, new
mgbrf.iav.new<-rfparam_yr(misc.merge, years=c(2018:2022))*(maize.new.alb.iav.ref-misc.new.alb.iav)
#maize to misc 2, new
mgcrf.iav.new<-rfparam_yr(misc.c.merge, years=c(2018:2022))*(maize.new.alb.iav.ref-misc.c.new.alb.iav)
#maize to sorghum, new
sbrf.iav.new<-rfparam_yr(sorg.merge, years=c(2019:2022))*(maize.new.alb.iav.sorg.ref-sorg.new.alb.iav)
sbrf.iav.new.a<-rfparam_yr(sorg.merge, years=c(2019:2022))*(maize.new.alb.iav.sorg.ref-sorg.new.alb.alt.iav)

#}


#####

#Radiative Forcing Plots#####

#Weirdly difficult setup to aggregate a DOY vector to months: ####

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
#####

##Plots


sbrf.month<-aggregate(sbrf, by=list(doytomonth(sorg.merge)), FUN='mean')$x
sbrf.month.a<-aggregate(sbrf.a, by=list(doytomonth(sorg.merge)), FUN='mean')$x
mgrf.month<-aggregate(mgrf, by=list(doytomonth(misc.merge)), FUN='mean')$x
swrf.month<-aggregate(swrf, by=list(doytomonth(switchgrass)), FUN='mean')$x
nprf.month<-aggregate(nprf, by=list(doytomonth(nativeprairie)), FUN='mean')$x



#Merged plots, for posters ####

#With corrected sorghum
png('D:/R/Fluxes/Writeout/Plots/alt/Fig4_pan_albrf_merge.png', width=680, height=340)    

par(mfrow=c(1,1), mar=c(4,4.2,2,1))

plot(mgrf.month, type='l', main="RF of Conversion from Maize", font.lab=2, lwd=4, ylab=bquote('radiative forcing'~(W~m^-2)), xlab='month', ylim=c(-10, 10), col='blue', cex.lab=1.5, cex.axis=1.5, cex.main=2)
lines(swrf.month,lwd=4,col='light pink')
lines(nprf.month, lwd=4, col='plum3')
lines(sbrf.month, type='l',  lwd=4, col='forest green') 
abline(h=0, lwd=4, lty=3)

legend(7, 11, 
       legend=c(paste("Miscanthus:", round(mean(mgrf, na.rm=TRUE), 2), "Wm-2"), 
                paste("Switchgrass:", round(mean(swrf, na.rm=TRUE), 2), "Wm-2"), 
                paste("Native Prairie:", round(mean(nprf, na.rm=TRUE), 2), "Wm-2"), 
                paste("Sorghum:", round(mean(sbrf, na.rm=TRUE), 2), "Wm-2")), 
       lwd=2, col=c("blue", "light pink","plum3", "forest green", "honeydew2"), bty='n', cex=1, text.font=2)


dev.off()

#Merged, uncorrected sorghum included
png('D:/R/Fluxes/Writeout/Plots/alt/Fig4_pan_albrf_merge.opt.png', width=680, height=340)    

par(mfrow=c(1,1), mar=c(4,4.2,2,1))

plot(mgrf.month, type='l', main="RF of conversion from maize-soy", font.lab=2, lwd=4, ylab=bquote('radiative forcing'~(W~m^-2)), xlab='month', ylim=c(-10, 10), col='blue', cex.lab=1.5, cex.axis=1.5, cex.main=2)
lines(swrf.month,lwd=4,col='light pink')
lines(nprf.month, lwd=4, col='plum3')
lines(sbrf.month.a, type='l',  lwd=4, col='honeydew2')
lines(sbrf.month, type='l',  lwd=4, col='forest green') 
abline(h=0, lwd=4, lty=3)

legend(7, 11, 
       legend=c(paste("miscanthus:", round(mean(mgrf, na.rm=TRUE), 2), "Wm-2"), paste("switchgrass:", round(mean(swrf, na.rm=TRUE), 2), "Wm-2"), paste("native prairie:", round(mean(nprf, na.rm=TRUE), 2), "Wm-2"), paste("sorghum-soy:", round(mean(sbrf, na.rm=TRUE), 2), "Wm-2"), paste("Sorg. unadj.:", round(mean(sbrf.a, na.rm=TRUE), 2), "Wm-2")), 
       lwd=2, col=c("blue", "light pink","plum3", "forest green", "honeydew2"), bty='n', cex=1, text.font=2)


dev.off()
#####

#Original years

mgrf.og.month<-aggregate(mgrf.og, by=list(doytomonth(misc.merge)), FUN='mean')$x
swrf.og.month<-aggregate(swrf.og, by=list(doytomonth(switchgrass)), FUN='mean')$x
nprf.og.month<-aggregate(nprf.og, by=list(doytomonth(nativeprairie)), FUN='mean')$x

png('D:/R/Fluxes/Writeout/Plots/fig4a_pan_ogrf.png', width=500, height=350) 

par(mfrow=c(1,1), mar=c(4,5,2,1))

plot(mgrf.og.month, type='l', main="RF of conversion from maize-soy", font.lab=2, lwd=4, ylab=bquote('radiative forcing'~(W~m^-2)), xlab='month', ylim=c(-10, 10), col='blue', cex.lab=1.5, cex.axis=1.5, cex.main=2)
lines(swrf.og.month,lwd=4,col='light pink')
lines(nprf.og.month, lwd=4, col='plum3')
abline(h=0, lwd=4, lty=3)

legend(7, 11, 
       legend=c(paste("miscanthus:", round(mean(mgrf.og, na.rm=TRUE), 2), "Wm-2"), paste("switchgrass:", round(mean(swrf.og, na.rm=TRUE), 2), "Wm-2"), paste("native prairie:", round(mean(nprf.og, na.rm=TRUE), 2), "Wm-2")), 
       lwd=2, col=c("blue", "light pink","plum3"), bty='n', cex=1, text.font=2)

dev.off()

#Newer years
#Consider using average of zm as baseline and plotting both

#zmrf.new.month<-aggregate(zmrf.new, by=list(doytomonth(maize.merge)), FUN='mean')$x
mgbrf.new.month<-aggregate(mgbrf.new, by=list(doytomonth(misc.merge)), FUN='mean')$x
mgcrf.new.month<-aggregate(mgcrf.new, by=list(doytomonth(misc.c.merge)), FUN='mean')$x
sbrf.new.month<-aggregate(sbrf.new, by=list(doytomonth(sorg.merge)), FUN='mean')$x
sbrf.new.month.a<-aggregate(sbrf.new.a, by=list(doytomonth(sorg.merge)), FUN='mean')$x

png('D:/R/Fluxes/Writeout/Plots/alt/fig4b_pan_newrf.png', width=500, height=350)   

par(mfrow=c(1,1), mar=c(4,5,2,1))

plot(mgbrf.new.month, type='l', main="RF of conversion from maize", font.lab=2, lwd=4, ylab=bquote('radiative forcing'~(W~m^-2)), xlab='month', ylim=c(-10, 10), col='blue', cex.lab=1.5, cex.axis=1.5, cex.main=2)
lines(mgcrf.new.month,lwd=4,col='light blue')
lines(sbrf.new.month, lwd=4, col='forest green')
#lines(zmrf.new.month, lwd=4, col='yellow', lty=2)

abline(h=0, lwd=4, lty=3)

legend(7, 11, 
       legend=c(paste("miscanthus 1:", round(mean(mgbrf.new, na.rm=TRUE), 2), "Wm-2"), paste("miscanthus 2:", round(mean(mgcrf.new, na.rm=TRUE), 2), "Wm-2"), paste("sorghum-soy:", round(mean(sbrf.new, na.rm=TRUE), 2), "Wm-2")), 
       lwd=2, col=c("blue", "light blue","forest green"), bty='n', cex=1, text.font=2)

dev.off()

#With uncorrected sorghum

png('D:/R/Fluxes/Writeout/Plots/fig4b_pan_newrf_opt.png', width=500, height=350)   

par(mfrow=c(1,1), mar=c(4,5,2,1))   

plot(mgbrf.new.month, type='l', main="RF of Conversion from Maize", font.lab=2, lwd=4, ylab=bquote('radiative forcing'~(W~m^-2)), xlab='month', ylim=c(-10, 10), col='blue', cex.lab=1.5, cex.axis=1.5, cex.main=2)
lines(mgcrf.new.month,lwd=4,col='light blue')
lines(sbrf.new.month.a, lwd=4, col='honeydew2')
lines(sbrf.new.month, lwd=4, col='forest green')
#lines(zmrf.new.month, lwd=4, col='yellow', lty=2)

abline(h=0, lwd=4, lty=3)

legend(7, 11, 
       legend=c(paste("miscanthus 1:", round(mean(mgbrf.new, na.rm=TRUE), 2), "Wm-2"), paste("miscanthus 2:", round(mean(mgcrf.new, na.rm=TRUE), 2), "Wm-2"), paste("sorg. unadj.:", round(mean(sbrf.new.a, na.rm=TRUE), 2), "Wm-2"),paste("sorghum:", round(mean(sbrf.new, na.rm=TRUE), 2), "Wm-2")), 
       lwd=2, col=c("blue", "light blue", "honeydew2", "forest green"), bty='n', cex=1, text.font=2)

dev.off()

if(printnum==TRUE){
  print("Average radiative forcings, original years")
  print("native prairie:"); print(mean(nprf.og.month))
  print("switchgrass:"); print(mean(swrf.og.month))
  print("misc 1:"); print(mean(mgrf.og.month))
  
  print("Average radiative forcings, new years")
  #print("maize 2"); print(mean(zmbrf.new.month))
  print("misc 2:"); print(mean(mgcrf.new.month))
  print("misc 1:"); print(mean(mgbrf.new.month))
  print("sorghum:"); print(mean(sbrf.new.month))
  
  
  #Get SDs
  yrvec.og<-toa.yrdat$YEAR[toa.yrdat$YEAR%in%c(2009:2015)]
  yrvec.new<-toa.yrdat$YEAR[toa.yrdat$YEAR%in%c(2018:2022)]
  yrvec.sorg<-toa.yrdat$YEAR[toa.yrdat$YEAR%in%c(2019:2022)]
 
  print("SD of annual RFs, original years")
  print("native prairie:"); print(sd(aggregate(nprf.iav.og, by=list(yrvec.og), FUN="mean")$x))
  print("switchgrass:"); print(sd(aggregate(swrf.iav.og, by=list(yrvec.og), FUN="mean", na.rm=TRUE)$x))
  print("misc 1"); print(sd(aggregate(mgrf.iav.og, by=list(yrvec.og), FUN="mean")$x))
  
  print("SD of annual RFs, new years")
  #print("maize 2"); print(sd(aggregate(zmrf.iav.new, by=list(yrvec.new), FUN="mean")$x))
  print("misc 1"); print(sd(aggregate(mgbrf.iav.new, by=list(yrvec.new), FUN="mean")$x))
  print("misc 2"); print(sd(aggregate(mgcrf.iav.new, by=list(yrvec.new), FUN="mean")$x))
  print("sorghum"); print(sd(aggregate(sbrf.iav.new, by=list(yrvec.sorg), FUN="mean", na.rm=TRUE)$x))
  print("sorghum"); print(sd(aggregate(sbrf.iav.new.a, by=list(yrvec.sorg), FUN="mean", na.rm=TRUE)$x))
  
}


#####



#Calculate GWI ####

#For GWI, radiative forcing and albedo vectors are from the albedo section, which must be run before this section

#First, need to make yields referenced to years, manually for now
switch.yield.stamp<-data.frame(cbind(switch.yield$yield, c(2009:2015)));np.yield.stamp<-data.frame(cbind(np.yield$yield, c(2009:2015)))
misc.yield.stamp<-data.frame(cbind(misc.yield$yield, c(2009:2022))); maize.yield.stamp<-data.frame(cbind(maize.yield$yield, c(2009:2022)))
misc.c.yield.stamp<-data.frame(cbind(misc.c.yield$yield, c(2018:2022))); maize.c.yield.stamp<-data.frame(cbind(maize.c.yield$yield, c(2017:2022))) 
sorg.yield.stamp<-data.frame(cbind(sorg.yield$yield, c(2019:2022)))
colnames(switch.yield.stamp)<-colnames(np.yield.stamp)<-colnames(misc.yield.stamp)<-colnames(maize.yield.stamp)<-colnames(misc.c.yield.stamp)<-colnames(maize.c.yield.stamp)<-colnames(sorg.yield.stamp)<-c("yield", "yr")

#Function calculating GWI

#Show my work calculating maize emissions
maize.necb.all<-rbind(maize.cflux, maize.c.cflux)#all maize annual fluxes together
maize.necb.co2<-mean(maize.necb.all$yearseq)*3.66

calcgwi.static<-function(dat, yield.in, rf, years=c(2009:2022), th=100, nodatyr=0){
  
  dat<-subtime(dat, subset=years)
  yield<-yield.in$yield[yield.in$yr%in%years]
  ##Albedo GWI
  
  RFtoa<-mean(rf) #annual mean radiative forcing
  
  #Constants####
  yco2<-48; if(th==20){yco2<-69} #airborne co2, estimating 45% here, from wikipedia (can probably do better)
  #52.4
  A<-1 #perturbed area, m2
  Ae<-5.1E14 #earth's surface area, m2
  
  Mco2<-44.01 #molec. wt. carbon, g mol-1
  mair<-5.148E15 #mass of atmosphere, Mg
  mair.kg<-5.148E18 #mass of atmosphere, kg
  co2.ref<-389 #reference co2 concentration, ppm
  
  df2x<-3.7 #radiative forcing for doubling co2, W m-2
  Mair<-28.95 #molec. wt. air, g mol-1 
  
  TH<-th #time horizon, yr
  #####
  
  dat.gwi.alb<-(RFtoa/yco2)*(A/Ae)*((log(2)*Mco2*mair*co2.ref)/(df2x*Mair))*(1/TH) #in MgCo2/ha
  
  #albedo EESF equation
  
  aco2<-5.35*log((co2.ref+1)/co2.ref) #radiative efficiency co2, from Bright 2020, Wm-2ppm-1
  
  kco2<-(aco2*Mair*10^6)/(Mco2*mair.kg) #global mean radiative efficiency, W m-2 kg-1
  
  #dat.eesf.alb<- RFtoa/(kco2*Ae*0.5) #kg CO2-eq m-2, native units from Bright 2020
  af.eesf<-yco2/100 #airborne fraction
  dat.eesf.alb<- (RFtoa/(kco2*Ae*af.eesf))*10 #Mg CO2-eq ha-1, for comparison with gwi
  
  
  ##Carbon GWI####
  
  
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
  
  yearseq<-diff(dat.hvst.sum[endofyear]) #net carbon storage after harvest removal each year
  #####
  
  #dat.gwi.co2<-mean(yearseq)*0.37 #in kgCo2/m2; conversion:tC/ha -> 44.01tCO2 / 12.1tC * 1000kgCo2 / tCO2 * 1 ha / 10000m2 -> kGCO2/m2
  #dat.gwi.co2<-mean(yearseq)*3.66 #in MgCo2/ha == tCO2/ha; conversion: 44.01tCO2 / 12.1tC
  dat.gwi.co2<-(mean(yearseq)*3.66)-5.47 #in MgCo2/ha == tCO2/ha; to represent relative to maize; originally 8.4 using just zmb
  #dat.gwi.co2<-(mean(yearseq)*0.37)-0.547 #in kgCo2/m2; to represent relative to maize
  
  #return(data.frame(rbind(dat.eesf.alb, dat.gwi.co2)))# for individual plots?
  #return(data.frame(rbind(dat.gwi.alb, dat.gwi.co2)))#for combined plot
  return(data.frame(rbind(dat.eesf.alb, dat.gwi.alb, dat.gwi.co2))) #both
}


calcgwi<-function(dat, yield.in, rf, years=c(2009:2022), th=100, nodatyr=0){
  
  dat<-subtime(dat, subset=years)
  yield<-yield.in$yield[yield.in$yr%in%years]
  
  
  ##Albedo GWI
  
  #Components of albedo gwi equation
  
  RFtoa<-mean(rf) #annual mean radiative forcing
  
  #Constants####
  yco2<-48; if(th==20){yco2<-69} #airborne co2, estimating 45% here, from (can probably do better)
  
  print(yco2)
  
  A<-1 #perturbed area, m2
  Ae<-5.1E14 #earth's surface area, m2
  
  Mco2<-44.01 #molec. wt. carbon, g mol-1
  mair<-5.148E15 #mass of atmosphere, Mg
  mair.kg<-5.148E18 #mass of atmosphere, kg
  #co2.ref<-389 #reference co2 concentration, ppm
  co2.ref<-c(385, 388, 390, 392, 394, 397, 399, 401, 404, 407, 409, 412, 414, 416) #reference co2 concentration, ppm
  
  df2x<-3.7 #radiative forcing for doubling co2, W m-2
  Mair<-28.95 #molec. wt. air, g mol-1 
  
  TH<-th #time horizon, yr
  #####
  
  dat.gwi.alb<-mean((RFtoa/yco2)*(A/Ae)*((log(2)*Mco2*mair*co2.ref)/(df2x*Mair))*(1/TH)) #in MgCo2/ha
  
  #albedo EESF equation
  
  aco2<-5.35*log((co2.ref+1)/co2.ref) #radiative efficiency co2, from Bright 2020, Wm-2ppm-1
  
  kco2<-(aco2*Mair*10^6)/(Mco2*mair.kg) #global mean radiative efficiency, W m-2 kg-1
  
  af.eesf<-yco2/100 #airborne fraction
  #dat.eesf.alb<- RFtoa/(kco2*Ae*0.5) #kg CO2-eq m-2, native units from Bright 2020
  dat.eesf.alb<-mean((RFtoa/(kco2*Ae*af.eesf))*10) #Mg CO2-eq ha-1, for comparison with gwi
  
  
  aco2<-5.35*log((co2.ref+1)/co2.ref) #radiative efficiency co2, from Bright 2020, Wm-2ppm-1
  
  kco2<-(aco2*Mair*10^6)/(Mco2*mair.kg) #global mean radiative efficiency, W m-2 kg-1
  
  #dat.eesf.alb<- RFtoa/(kco2*Ae*0.5) #kg CO2-eq m-2, native units from Bright 2020
  af.eesf<-yco2/100 #airborne fraction
  dat.eesf.alb<-mean((RFtoa/(kco2*Ae*af.eesf))*10) #Mg CO2-eq ha-1, for comparison with gwi
  
  
  ##Carbon GWI
  
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
  
  yearseq<-diff(dat.hvst.sum[endofyear]) #net carbon storage after harvest removal each year
  
  #dat.gwi.co2<-mean(yearseq)*0.37 #in kgCo2/m2; conversion:tC/ha -> 44.01tCO2 / 12.1tC * 1000kgCo2 / tCO2 * 1 ha / 10000m2 -> kGCO2/m2
  #dat.gwi.co2<-mean(yearseq)*3.66 #in MgCo2/ha == tCO2/ha; conversion: 44.01tCO2 / 12.1tC
  dat.gwi.co2<-(mean(yearseq)*3.66)-5.47 #in MgCo2/ha == tCO2/ha; to represent relative to maize. 7.4 comes from avg maize-soy 1 fluxes = 2.02tC/ha y * 3.66 tCO2 / tC
  #dat.gwi.co2<-(mean(yearseq)*0.37)-0.547 #in kgCo2/m2; to represent relative to maize
  
  #return(data.frame(rbind(dat.eesf.alb, dat.gwi.co2)))# for individual plots?
  #return(data.frame(rbind(dat.gwi.alb, dat.gwi.co2)))#for combined plot
  eq<-rbind(dat.eesf.alb, dat.gwi.alb, dat.gwi.co2)
  return(data.frame(eq)) #both
}


#ogyears

#at th=100 (default)
misc.gwi.og<-calcgwi(dat=misc.merge, yield.in=misc.yield.stamp, rf<-mgrf.og, years<-c(2009:2015))
switch.gwi.og<-calcgwi(dat=switchgrass, yield.in=switch.yield.stamp, rf<-swrf.og, years<-c(2009:2015))
np.gwi.og<-calcgwi(dat=nativeprairie, yield.in=np.yield.stamp, rf<-nprf.og, years<-c(2009:2015))

names<-rep(c("switchgrass", "miscanthus", "prairie"), each=3)
numbers<-rbind(switch.gwi.og, misc.gwi.og, np.gwi.og)
types<-rep(c("albedo.eesf","albedo.gwi", "carbon"), 3)
dat.gwi.og<-cbind(names, numbers, types); colnames(dat.gwi.og)[2]<-"dat"


#at th=20:
misc.gwi.og.20<-calcgwi(dat=misc.merge, yield.in=misc.yield.stamp, rf<-mgrf.og, th=20, years<-c(2009:2015))
switch.gwi.og.20<-calcgwi(dat=switchgrass, yield.in=switch.yield.stamp, rf<-swrf.og, th=20, years<-c(2009:2015))
np.gwi.og.20<-calcgwi(dat=nativeprairie, yield.in=np.yield.stamp, rf<-nprf.og, th=20, years<-c(2009:2015))

names<-rep(c("switchgrass", "miscanthus", "prairie"), each=3)
numbers<-rbind(switch.gwi.og.20, misc.gwi.og.20, np.gwi.og.20)
types<-rep(c("albedo.eesf","albedo.gwi", "carbon"), 3)
dat.gwi.og.20<-cbind(names, numbers, types); colnames(dat.gwi.og.20)[2]<-"dat"


##New years
#th=100
mgb.gwi.new<-calcgwi(dat=misc.merge, yield.in=misc.yield.stamp, rf<-mgbrf.new, years<-c(2018:2022))
mgc.gwi.new<-calcgwi(dat=misc.c.merge, yield.in=misc.c.yield.stamp, rf<-mgcrf.new, years<-c(2018:2022))
#zmc.gwi.new<-calcgwi(dat=maize.c.merge, yield.in=maize.c.yield.stamp, rf<-zmrf.new, years<-c(2018:2022))
sb.gwi.new<-calcgwi(dat=sorg.merge, yield.in=sorg.yield.stamp, rf<-sbrf.new, years<-c(2018:2022))
sb.gwi.new.a<-calcgwi(dat=sorg.merge, yield.in=sorg.yield.stamp, rf<-sbrf.new.a, years<-c(2018:2022))
#zmb.gwi.new<-calcgwi(dat=maize.merge, yield.in=maize.yield.stamp, rf<-0, years<-c(2018:2022))

names<-rep(c("mxg 1", "mxg 2", "sorghum-soy"), each=3) #, "maize-soy 2", "maize.soy 1"
numbers<-rbind(mgb.gwi.new, mgc.gwi.new, sb.gwi.new) # zmc.gwi.new, zmb.gwi.new
types<-rep(c("albedo.eesf","albedo.gwi", "carbon"), 3)
dat.gwi.new<-cbind(names, numbers, types); colnames(dat.gwi.new)[2]<-"dat"

#With corrected sorghum (for Carl)
names<-rep(c("mxg 1", "mxg 2", "sorghum-soy"), each=3)
numbers<-rbind(mgb.gwi.new, mgc.gwi.new, sb.gwi.new.a)
types<-rep(c("albedo.eesf","albedo.gwi", "carbon"), 3)
dat.gwi.new.a<-cbind(names, numbers, types); colnames(dat.gwi.new.a)[2]<-"dat"


#at th=20

mgb.gwi.new.20<-calcgwi(dat=misc.merge, yield.in=misc.yield.stamp, rf<-mgbrf.new, th=20, years<-c(2018:2022))
mgc.gwi.new.20<-calcgwi(dat=misc.c.merge, yield.in=misc.c.yield.stamp, rf<-mgcrf.new, th=20, years<-c(2018:2021))
zmc.gwi.new.20<-calcgwi(dat=maize.c.merge, yield.in=maize.c.yield.stamp, rf<-zmrf.new, th=20, years<-c(2018:2021))
sb.gwi.new.20<-calcgwi(dat=sorg.merge, yield.in=sorg.yield.stamp, rf<-sbrf.new, th=20, years<-c(2018:2022))
zmb.gwi.new.20<-calcgwi(dat=maize.c.merge, yield.in=maize.c.yield.stamp, rf<-zmrf.new, th=20, years<-c(2018:2021))

names<-rep(c("mxg 1", "mxg 2", "sorghum-soy"), each=3)
numbers<-rbind(mgb.gwi.new.20, mgc.gwi.new.20, sb.gwi.new.20)
types<-rep(c("albedo.eesf","albedo.gwi", "carbon"), 3)
dat.gwi.new.20<-cbind(names, numbers, types); colnames(dat.gwi.new.20)[2]<-"dat"


##Merged

dat.gwi.merge<-rbind(dat.gwi.og, dat.gwi.new.a)

#combine miscanthus by site-year weighted average:
#mgb og: 9 years
#mgb new: 5 years
#mgc new: 5 years 

mnames<-c("miscanthus", "mxg 1", "mxg 2")
dat.gwi.misc<-subset(dat.gwi.merge, dat.gwi.merge$names%in%mnames)
miscval<-aggregate(x=dat.gwi.misc$dat, by=list(dat.gwi.misc$types), FUN= function(x) weighted.mean(x, w=c(9,5,5)))

miscsub<-data.frame(cbind(rep("miscanthus", 3), as.numeric(miscval$x), miscval$Group.1))
colnames(miscsub)<-colnames(dat.gwi.merge)

#Combine new miscanthus numbers with the rest
dat.gwi.merge.pre<-subset(dat.gwi.merge, !dat.gwi.merge$names%in%mnames)
dat.gwi.merge<-rbind(dat.gwi.merge.pre, miscsub); dat.gwi.merge$dat<-as.numeric(dat.gwi.merge$dat)

#####

#Plotting GWI####

#og years, 100-year th
gwi.og<-ggplot(dat.gwi.og[dat.gwi.og$types%in%c("albedo.gwi", "carbon"),]) +
  aes(x = names, fill = types, weight = dat) +
  geom_bar(color='black') +
  scale_fill_manual(values = c(carbon = "#BABEC0",albedo.gwi = "#8B96C2"), labels=c("carbon", "albedo"))+
  ylim(-45, 10)+
  geom_label(label="2009 - 2015", x=3, y=5, size=8, show.legend = FALSE, inherit.aes = FALSE)+
  labs(x = "",
       y =  bquote("Mg"~CO[2]-eq~ha^-1~y^-1),
       fill = "forcing",
       title = "100-yr time horizon")+
  theme_minimal()+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=28,face="bold"),legend.text=element_text(size=28),
        legend.position = c(0.8, 0.2), legend.title = element_blank(),plot.title = element_text(size=28, face="bold"))

gwi.og

png('D:/R/Fluxes/Writeout/Plots/fig5a_oggwi.png', width=650, height=450)
gwi.og
dev.off()

if(printnum==TRUE){
library(reshape2)

gwi.og.summ<-dcast(dat.gwi.og, names~types,value.var = "dat")
gwi.og.summ$pctot<-gwi.og.summ$albedo.gwi/(gwi.og.summ$carbon+gwi.og.summ$albedo.gwi)
gwi.og.summ$yreq<-gwi.og.summ$albedo.eesf/gwi.og.summ$carbon
gwi.og.summ

}

#og years, 20-year th
gwi.og.20<-ggplot(dat.gwi.og.20[dat.gwi.og.20$types%in%c("albedo.gwi", "carbon"),]) +
  aes(x = names, fill = types, weight = dat) +
  geom_bar(color='black') +
  scale_fill_manual(values = c(carbon = "#BABEC0",albedo.gwi = "#8B96C2"), labels=c("carbon", "albedo"))+
  ylim(-45, 10)+
  geom_label(label="2009 - 2015", x=3, y=5, size=8, show.legend = FALSE, inherit.aes = FALSE)+
  labs(x = "",
       y =  bquote("Mg"~CO[2]-eq~ha^-1~y^-1),
       fill = "forcing",
       title = "20-yr time horizon")+
  theme_minimal()+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=28,face="bold"),legend.text=element_text(size=28),
        legend.position = c(0.8, 0.2), legend.title = element_blank(),plot.title = element_text(size=28, face="bold"))

gwi.og.20

png('D:/R/Fluxes/Writeout/Plots/fig5b_oggwi_20.png', width=650, height=450)
gwi.og.20
dev.off()

if(printnum==TRUE){
  
  gwi.og.20.summ<-dcast(dat.gwi.og.20, names~types,value.var = "dat")
  gwi.og.20.summ$pctot<-gwi.og.20.summ$albedo.gwi/(gwi.og.20.summ$carbon+gwi.og.20.summ$albedo.gwi)
  gwi.og.20.summ$yreq<-gwi.og.20.summ$albedo.eesf/gwi.og.20.summ$carbon
  gwi.og.20.summ
  
}

#new years, th=100
gwi.new<-ggplot(dat.gwi.new[dat.gwi.new$types%in%c("albedo.gwi", "carbon"),]) +
  aes(x = names, fill = types, weight = dat) +
  geom_bar(color='black') +
  scale_fill_manual(values = c(carbon = "#BABEC0",albedo.gwi = "#8B96C2"), labels=c("carbon", "albedo"))+
  ylim(-45, 10)+
  geom_label(label="2018 - 2022", x=3, y=5, size=8, show.legend = FALSE, inherit.aes = FALSE)+
  labs(x = "",
       y =  bquote("Mg"~CO[2]-eq~ha^-1~y^-1),
       fill = "forcing",
       title = "100-yr time horizon")+
  theme_minimal()+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=28,face="bold"),legend.text=element_text(size=28),
        legend.position = c(0.8, 0.2), legend.title = element_blank(),plot.title = element_text(size=28, face="bold"))

gwi.new


png('D:/R/Fluxes/Writeout/Plots/fig5c_newgwi.png', width=650, height=450)
gwi.new
dev.off()


if(printnum==TRUE){
  
  gwi.new.summ<-dcast(dat.gwi.new, names~types,value.var = "dat")
  gwi.new.summ$pctot<-gwi.new.summ$albedo.gwi/(gwi.new.summ$carbon+gwi.new.summ$albedo.gwi)
  gwi.new.summ$yreq<-gwi.new.summ$albedo.eesf/gwi.new.summ$carbon
  gwi.new.summ
  
}


#new years, th=20
gwi.new.20<-ggplot(dat.gwi.new.20[dat.gwi.new.20$types%in%c("albedo.gwi", "carbon"),]) +
  aes(x = names, fill = types, weight = dat) +
  geom_bar(color='black') +
  scale_fill_manual(values = c(carbon = "#BABEC0",albedo.gwi = "#8B96C2"), labels=c("carbon", "albedo"))+
  ylim(-45, 10)+
  geom_label(label="2018 - 2022", x=3, y=5, size=8, show.legend = FALSE, inherit.aes = FALSE)+
  labs(x = "",
       y =  bquote("Mg"~CO[2]-eq~ha^-1~y^-1),
       fill = "forcing",
       title = "20-yr time horizon")+
  theme_minimal()+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=28,face="bold"),legend.text=element_text(size=28),
        legend.position = c(0.8, 0.2), legend.title = element_blank(),plot.title = element_text(size=28, face="bold"))

gwi.new.20

png('D:/R/Fluxes/Writeout/Plots/fig5d_newgwi_20.png', width=650, height=450)
gwi.new.20
dev.off()


#merged, th=100
gwi.merge<-ggplot(dat.gwi.merge[dat.gwi.merge$types%in%c("albedo.gwi", "carbon"),]) +
  aes(x = names, fill = types, weight = dat) +
  geom_bar(color='black') +
  scale_fill_manual(values = c(carbon = "#BABEC0",albedo.gwi = "#8B96C2"), labels=c("carbon", "albedo"))+
  ylim(-45, 10)+
  geom_label(label="2008 - 2022", x=5, y=5, size=8, show.legend = FALSE, inherit.aes = FALSE)+
  labs(x = "",
       y =  bquote("Mg"~CO[2]-eq~ha^-1~y^-1),
       fill = "forcing",
       title = "100-yr time horizon")+
  theme_minimal()+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=28,face="bold"),legend.text=element_text(size=28),
        legend.position = c(0.8, 0.2), legend.title = element_blank(),plot.title = element_text(size=28, face="bold"))

gwi.merge

png('D:/R/Fluxes/Writeout/Plots/alt/fig5_newgwi_merge.png', width=750, height=450)
gwi.merge
dev.off()



if(printnum==TRUE){
  
  gwi.new.20.summ<-dcast(dat.gwi.new.20, names~types,value.var = "dat")
  #gwi.new.20.summ$pct<-gwi.new.20.summ$albedo.gwi/gwi.new.20.summ$carbon
  gwi.new.20.summ$pctot<-gwi.new.20.summ$albedo.gwi/(gwi.new.20.summ$carbon+gwi.new.20.summ$albedo.gwi)
  gwi.new.20.summ$yreq<-gwi.new.20.summ$albedo.eesf/gwi.new.20.summ$carbon
  gwi.new.20.summ
  
}


##Data availability#####

years<-c(2009:2022)
soyyr<-years[!years%in%nosoy]
p2<-c(2018:2022)
p1<-c(2009:2015)

colnames(mg)<-c("y", "col", "yr")

mgb<-data.frame(rep(3, length(years)), rep('light blue', length(years)), years, rep ("miscanthus 1", length(years)))
mgc<-data.frame(rep(2.5, length(years)), rep('blue', length(years)), years, rep ("miscanthus 2", length(years)))
zmb<-data.frame(rep(4, length(years)), rep('orange', length(years)), years, rep ("maize-soy 1", length(years)))
zmc<-data.frame(rep(3.5, length(years)), rep('yellow', length(years)), years, rep ("maize-soy 2", length(years)))
sw<-data.frame(rep(1.5, length(years)), rep('light pink', length(years)), years, rep ("switchgrass", length(years)))
sb<-data.frame(rep(2, length(years)), rep('forest green', length(years)), years, rep ("sorghum-soy" , length(years)))
np<-data.frame(rep(1, length(years)), rep('plum2', length(years)), years, rep ("native prairie", length(years)))


colnames(zmc)<-colnames(mgc)<-colnames(mgb)<-colnames(sw)<-colnames(sb)<-colnames(zmb)<-colnames(np)<-c("y", "col", "yr", "feedstock")

zmc$y[!zmc$yr%in%p2]<-NA; mgc$y[!mgc$yr%in%p2]<-NA;sb$y[!zmc$yr%in%p2]<-NA; sw$y[!zmc$yr%in%p1]<-NA;np$y[!zmc$yr%in%p1]<-NA

sb$y[10]<-NA #NAN 2018, which sorghum does not have a complete year of

coverage<-rbind(mgb, mgc, zmb, zmc,sw, sb, np)

coverage$yr<-coverage$yr+0.5

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

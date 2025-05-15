#cumulative plot graphical abstract

#Cumulative plot #####
#points version of squiggle plot (requires "long record" be run)
par(bty='n', mfrow=c(1,1))

#Set up alternating colors for maize-soy and sorghum-soy rotations
Y<-(as.numeric(format(maize.merge$xlDateTime, "%Y")))
col.zm=rep('orange', length(Y));col.zm[which(Y%in%c(2010, 2013, 2016, 2019, 2022))]<-"light green"; col.zm[length(col.zm)]<-"orange"
Y<-(as.numeric(format(sorg.merge$xlDateTime, "%Y")))
col.sb<-rep('forest green', length(Y));col.sb[which(Y%in%c(2010, 2013, 2016, 2019, 2022))]<-"light green"; col.sb[length(col.sb)]<-"forest green"
Y<-(as.numeric(format(maize.c.merge$xlDateTime, "%Y")))
col.zmc=rep('gold', length(Y));col.zmc[which(Y%in%c(2010, 2013, 2016, 2019, 2022))]<-"light green"; col.zmc[length(col.zmc)]<-"gold"


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

plot(maize.hvst.sum[yearstart(maize.merge)]~decyr(maize.merge$xlDateTime[yearstart(maize.merge)]), xlim=c(2009, 2023), col=alpha(col.zm[yearstart(maize.merge)], 0), ylim=c(-60, 45), ylab=bquote("Cumulative NECB, Mg C"~ha^-1), xlab='', yaxt="n", xaxt = "n", cex.lab=2, cex=2.8, pch=16)
abline(h=c(-60, -40, -20, 0, 20, 40),v=seq(from=2009, to=2023, by=2), col='light gray')
axis(2, labels=c(-60, -40, -20, 0, 20, 40), at=c(-60,-40, -20, 0, 20, 40), col='white', cex.axis=1.6)
axis(1, labels=seq(from=2009, to=2023, by=2), at=seq(from=2009, to=2023, by=2), col='white', cex.axis=1.6)
abline(h=0, lwd=2, lty=2)

#show transition years
#xbound<-c(2016,2016,2018,2018)#c(rep(yearstart(maize.merge)[years==2016],2), rep(yearstart(maize.merge)[years==2017], 2))
#ybound<-c(-60,50,50,-60)
#polygon(xbound, ybound, angle=45, density=20, col=adjustcolor("dark red", alpha.f = 0.50), border="NA" )

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


legend(as.numeric(min(decyr(maize$xlDateTime)))-0.2, 40, legend="", pch=16, col="light green", bty="n",pt.cex=2.2)
legend(as.numeric(min(decyr(maize$xlDateTime)))-0.2, 35.1, legend="", pch=16, col="light green", bty="n",pt.cex=2.2)
legend(as.numeric(min(decyr(maize$xlDateTime)))+2.5, 29.8, legend="", pch=15, col="light green", bty="n",pt.cex=2.2)



#dev.copy(png,'D:/R/Fluxes/Writeout/Plots/fig2_cflux_over_time.png', width=900, height=500)
#dev.off()

dev.copy(png,'D:/R/Fluxes/Writeout/Plots/fig2_cflux_over_time_highres_graphicalabs.png', width=900*res.scl, height=550*res.scl, res=res.dpi)
dev.off()

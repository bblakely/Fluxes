if(!exists("sorg")){source("ReadAll.R")}
   
   
  par(mfrow=c(2,1), mar=c(2,4,2,1))
  DOY<-format(sorg$xlDateTime, "%j")
  lim<-c(min(which(DOY==150)), max(which(DOY==290)))
  subset<-lim[1]:lim[2]
  plot(sorg$Sws[subset]~sorg$xlDateTime[subset], type='l', lwd=2, ylab="VWC (cm2 cm-2)")
  abline(h=c(0.366,0.327), col='blue', lty=c(1,2))
  abline(h=c(0.270,0.233), col='orange4', lty=c(1,2))
  legend(quantile(sorg$xlDateTime[subset], 0.70), 0.58, bty='n', col=c('blue', 'orange4','blue', 'orange4'), 
         legend=c("field capacity, Rawls", "wilting point, Rawls", "field capacity, Rosetta", "wilting point, Rosetta"), cex=0.8, lty=c(1,1,2,2))

  text(quantile(sorg$xlDateTime[subset], 0.20), 0.5, "Soil water content at 10cm (EFarm)")
  
  abline(v=sorg$xlDateTime[subset][3292], lty=3)
  
  smooth.er<-rollapply(sorg$GPP_LT, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
   plot(smooth.er[subset]~sorg$xlDateTime[subset], type='l', lwd=2,ylab="GPP(umol m-2 s-1)")
   text(quantile(sorg$xlDateTime[subset], 0.10), 15, "GPP (EFarm)")
  
   abline(v=sorg$xlDateTime[subset][c(3292)], lty=3)
   
   
   
   
   #Now use GPP
   par(mfrow=c(2,1), mar=c(2,3,2,1))
   DOY<-format(sorg$xlDateTime, "%j")
   lim<-c(min(which(DOY==150)), max(which(DOY==290)))
   subset<-lim[1]:lim[2]
   plot(sorg$Sws[subset]~sorg$xlDateTime[subset], type='l')
   abline(h=c(0.366,0.327), col='blue', lty=c(1,2))
   abline(h=c(0.270,0.233), col='orange4', lty=c(1,2))
   legend(quantile(sorg$xlDateTime[subset], 0.70), 0.58, bty='n', col=c('blue', 'orange4','blue', 'orange4'), 
          legend=c("field capacity, Rawls", "wilting point, Rawls", "field capacity, Rosetta", "wilting point, Rosetta"), cex=0.8, lty=c(1,1,2,2))
   
   text(quantile(sorg$xlDateTime[subset], 0.20), 0.5, "Soil water content at 10cm (EFarm)")
   
   abline(v=sorg$xlDateTime[subset][3292], lty=3)
   
   smooth.er<-rollapply(sorg$GPP_LT, width=48*7, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
   plot(smooth.er[subset]~sorg$xlDateTime[subset], type='l')
   text(quantile(sorg$xlDateTime[subset], 0.10), 15, "GPP (EFarm)")
   
   abline(v=sorg$xlDateTime[subset][c(3292)], lty=3)
   
   
   (which(sorg$Sws[subset]==0.270))
   
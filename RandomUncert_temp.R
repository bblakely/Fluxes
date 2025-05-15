dat<-maize.merge

err.orig<-dat$Fc_Rerr/abs(dat$Fc)

err<-err.orig
err[err.orig > quantile(err.orig, 0.99, na.rm=TRUE)]<-NA

years<-c(2009:2019)
par(mfrow=c(2,2))

for(i in 1:length(years)){

dat2<-dat[format(dat$xlDateTime, "%Y")==years[i],]  

rerr.orig<-dat2$Fc_Rerr

rerr<-rerr.orig
#rerr[rerr.orig>quantile(rerr.orig, 0.99, na.rm=TRUE)]<-NA

#(1/0.52)*sqrt(sum(rerr, na.rm=TRUE))*.01

plot(rerr~abs(dat2$Fc), ylim=c(0,10),main=years[i])
#smoothScatter(rerr~abs(dat2$Fc), ylim=c(0,10))
errind<-which(!is.na(dat2$Fc_Rerr))
print(paste(years[i],":", sqrt(sum(rerr[errind]^2))/abs(sum(dat2$Fc[errind]))))
}



dat2<-dat[format(dat$xlDateTime, "%Y")==2017,] 

plot(dat2$Fc_Rerr~dat2$xlDateTime)

rerr.fill<-dat2$Fc_Rerr; rerr.fill[is.na(rerr.fill)]<-0

max<-cumsum(dat2$Fc+rerr.fill)
min<-cumsum(dat2$Fc-rerr.fill)

plot(max, ylim=c(-35000,10000)); points(min);points(dat2$fc, col="red")


length(which(is.na(err)))/nrow(maize.merge)




plot(err)
hist(err)

sum(err, na.rm=TRUE)/sum(abs(dat$Fc), na.rm=TRUE)


mean(dat$Fc_Rerr, na.rm=TRUE)
hist(dat$Fc_Rerr, na.rm=TRUE)
median(dat$Fc_Rerr, na.rm=TRUE)




#Random uncertainty

#let's do a little simulation
years<-c(2009:2019)
nsims<-100
conv<-0.0792*0.0027 #conversion factors for umol/m2/s to MgC/ha
outyr<-rep(NA, length(years))


for(y in 1:length(years)){
print(paste("random error sim for year", years[y]))
  
dat2<-dat[format(dat$xlDateTime, "%Y")==years[y],]  
iter<-rep(NA, 100)

for(j in 1:nsims){
if(j%%(nsims/10)==0){print(j)}
simval<-rep(NA,nrow(dat2))
for(i in 1:nrow(dat2)){

  datum<-dat2$Fc_Rerr[i]
  if(!is.na(datum)){
  err<-runif(min=-1*datum, max=datum, n=1)
  simval[i]<-(dat2$Fc[i]+err)*conv
  }else{simval[i]<-NA}

}
iter[j]<-sum(simval,na.rm=TRUE)

}

hist(iter)
outyr[y]<-2*sd(iter)
print(sd(iter))
}


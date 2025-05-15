#Uncertainty analysis


#Update at revision time: maize, misc, and sorghum have all had custom filtered Fc made.
#create a fake custom filtered Fc, that is just the PFP-generated filtered FC, for prairie and switchgrass:
switchgrass$Fc_filtered_cust<-switchgrass$Fc_filtered
nativeprairie$Fc_filtered_cust<-nativeprairie$Fc_filtered


#Begin algorithm
allsites<-list(maize.merge, maize.c.merge, misc.merge, misc.c.merge, switchgrass, nativeprairie, sorg.merge)
sitenames<-c("maize.merge", "maize.c.merge", "misc.merge", "misc.c.merge", "switchgrass", "nativeprairie", "sorg.merge")
names(allsites)<-sitenames


allerr<-list()

for(s in 1:length(allsites)){
  
print(paste("starting site", names(allsites)[s]))

dat<-allsites[[s]]
dat.ts<-unpack.time(dat)
years<-unique(dat.ts$YEAR); #years<-years[years<2020]


#let's do a little simulation
nsims<-100 #number rand error simulations
conv<-0.0792*0.0027 #conversion factors for umol/m2/s to MgC/ha

#outputs
rerr.yr<-rep(NA, length(years)) #random error by year,absolute (tc/ha)
record<-rep(NA, length(years)) #model error by year, as proportioin of total sum
total<-rep(NA, length(years)) #regular cumulative sum no error
moderr.yr<-rep(NA, length(years))


for(y in 1:length(years)){

print(paste("starting year", years[y]))
  
print(paste("model error sim for year", years[y]))
  
year<-years[y]
dat.yr<-dat[dat.ts$YEAR==year,]

#Model error section ###

dat.yr$Fc_filtered<-dat.yr$Fc_filtered_cust*conv; dat.yr$Fc<-dat.yr$Fc*conv

#dat.yr$Fc_filtered<-dat.yr$Fc_filtered*conv; dat.yr$Fc<-dat.yr$Fc*conv #old version with no custom filtered Fc
#plot(dat.yr$Fc_filtered~dat.yr$xlDateTime, ylim=c(-0.01,0.01))

result<-rep(NA, 1000)


for(i in 1:1000){
  
if(i%%100==0){print(i)} #readout to follow loop

#find 1000 records that have obs
goodind<-which(!is.na(dat.yr$Fc_filtered))

if(length(goodind)>=1000){
sub.ind<-sample(goodind, 1000)
}else{sub.ind<-sample(goodind, 500)}

sub<-dat.yr[sub.ind,]

#plot(sub$Fc~sub$xlDateTime)


#remove x% of them and fill with model data where x is prop data missing
#miss<-length(!goodind)/(nrow(dat.yr))
miss<-length(which(is.na(dat.yr$Fc_filtered)))/nrow(dat.yr)
#in this case 33%

if(length(goodind)>=1000){kill.ind<-sample(1:1000, round(miss*1000))}else{kill.ind<-sample(1:500, round(miss*500))}
  
#fc.dat<-sub$Fc_filtered
fc.dat<-sub$Fcbk #fcbk is actual measured Fc
#fc.mod<-sub$Fc_filtered;
fc.mod<-sub$Fcbk; fc.mod[kill.ind]<-((sub$NEE_SOLO[kill.ind]*conv)+(sub$NEE_LT[kill.ind]*conv))/2#sub$Fc_SOLO[kill.ind]*conv


#sum the differences between obs and model:
sumdiff<-cumsum(fc.dat-fc.mod)

#Express as proportion obs sum (using last item in vector for final sum)
final<-tail(sumdiff, n=1)/tail(cumsum(fc.dat), n=1)


result[i]<-final

}

yearerr<-2*sd(result)

#print(paste(year,":",round(yearerr, 4)))

record[y]<-yearerr

total[y]<-sum(dat.yr$Fc, na.rm=TRUE)

moderr.yr[y]<-record[y]*total[y]
####

#Random uncertainty section####

  print(paste("random error sim for year", years[y]))
  
  dat2<-dat.yr
  
  iter<-rep(NA, 100) #space for each simulation
  
  for(j in 1:nsims){
    
    if(j%%(nsims/10)==0){print(j)}
    simval<-rep(NA,nrow(dat2))
    for(i in 1:nrow(dat2)){
      
      datum<-dat2$Fc_Rerr[i]*conv
      
      if(!is.na(datum)){
        err<-runif(min=-1*datum, max=datum, n=1)
        simval[i]<-(dat2$Fc[i]+err)
      }else{simval[i]<-NA}
      
    } 
    
   
    
    iter[j]<-sum(simval,na.rm=TRUE)-sum(dat2$Fc[!is.na(dat2$Fc_Rerr)])
    
  }
  
  #hist(iter)
  #hist(iter/sum(dat2$Fc))
  
  rerr.yr[y]<-sum(dat2$Fc)*(2*sd(iter))
  #print(sd(iter))
  
  
}#closes year loop

toterr<-sqrt(moderr.yr^2+rerr.yr^2); toterr[toterr>5]<-mean(toterr[toterr<5], na.rm=TRUE)
rec.dat<-data.frame(cbind(toterr,total,years))

#fancy quadrature sums
toterr.cum<-rep(NA,length(toterr))
for(i in 1:length(toterr)){
  ind<-c(1:i)
  toterr.cum[i]<-sqrt(sum(toterr[ind]^2))
}


plot(cumsum(rec.dat$total)~rec.dat$years, ylim=c(-110, 50),main=names(allsites)[s], xlab="Year",ylab="NEE")
points(cumsum(rec.dat$total+rec.dat$toterr)~rec.dat$years, col='blue')
points(cumsum(rec.dat$total-rec.dat$toterr)~rec.dat$years, col='red')

points(cumsum(rec.dat$total+toterr.cum)~rec.dat$years, col='light blue')
points(cumsum(rec.dat$total-toterr.cum)~rec.dat$years, col='pink')



out<-data.frame(cbind(rec.dat,toterr.cum))

# #pad with 2020+years
# if(max(years)==2019){
# slope<-lm(toterr.cum~c(1:length(years)))$coefficients[2]
# addvec<-slope*c(1,2,3)
# toterr.cum.pad<-c(toterr.cum,tail(toterr.cum,1)+addvec)
# out[(nrow(out)+1):length(toterr.cum.pad),]<-NA
# }else{toterr.cum.pad<-toterr.cum}

#because padding no longer necessary...
toterr.cum.pad<-toterr.cum

out$toterr.pad<-toterr.cum.pad
colnames(out)<-c("toterr", "cum_nee", "year","cumerr", "cumerr_pad")

allerr[[s]]<-out


}#closes site loop

names(allerr)<-sitenames

#Make fake sorghum error until I can do this process for 2020+
#assume conservatively that it has 2019 error every year
#allerr$sorg.merge$cumerr_pad[2:4]<-c(0.884,1.33,1.78)



rm(allsites)




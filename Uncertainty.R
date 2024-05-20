#Uncertainty analysis?

#do by year




dat<-misc.merge
dat.ts<-unpack.time(dat)
years<-c(2009:2019)


record<-rep(NA, length(years))
total<-rep(NA, length(years))

for(y in 1:length(years)){

print(paste("starting year", years[y]))
  
year<-years[y]

#start with 2018 maybe
dat.yr<-dat[dat.ts$YEAR==year,]
dat.yr$Fc_filtered<-dat.yr$Fc_filtered*0.0792*0.0027; dat.yr$Fc<-dat.yr$Fc*0.0792*0.0027
plot(dat.yr$Fc_filtered~dat.yr$xlDateTime, ylim=c(-0.01,0.01))

result<-rep(NA, 1000)
for(i in 1:1000){
  
if(i%%100==0){print(i)} #readout to follow loop

#find 1000 records that have obs
goodind<-which(!is.na(dat.yr$Fc_filtered))

sub.ind<-sample(goodind, 1000)

sub<-dat.yr[sub.ind,]

#plot(sub$Fc~sub$xlDateTime)


#remove x% of them and fill with model data where x is prop data missing
miss<-length(!goodind)/(nrow(dat.yr)) 
#in this case 33%

kill.ind<-sample(1:1000, round(miss*1000))
fc.dat<-sub$Fc_filtered
fc.mod<-sub$Fc_filtered; fc.mod[kill.ind]<-sub$Fc_SOLO[kill.ind]*0.0792*0.0027


#sum the differences between obs and model:
sumdiff<-cumsum(fc.dat-fc.mod)

#Express as proportion obs sum (using last item in vector for final sum)
final<-tail(sumdiff, n=1)/tail(cumsum(fc.dat), n=1)


result[i]<-final

}

yearerr<-2*sd(result)
print(paste(year,":",round(yearerr, 4)))
record[y]<-yearerr
total[y]<-sum(dat.yr$Fc, na.rm=TRUE)

}

rec.dat<-data.frame(cbind(years, record, total))
rec.dat<-rec.dat[rec.dat$years!=2012,]




abs<-abs(rec.dat$total*rec.dat$record)

#does not include harvest losses or harvest uncertainty
sum(abs)
sum(dat$Fc[dat.ts$YEAR%in%c(2009:2011,2013:2019)])

plot(cumsum(rec.dat$total)~rec.dat$years, ylim=c(-100, 0))
points(cumsum(rec.dat$total*(1+rec.dat$record))~rec.dat$years, col='blue')
points(cumsum(rec.dat$total*(1-rec.dat$record))~rec.dat$years, col='red')

points(cumsum(rec.dat$total*(1+cumsum(rec.dat$record)))~rec.dat$years, col='light blue')
points(cumsum(rec.dat$total*(1-cumsum(rec.dat$record)))~rec.dat$years, col='pink')





#seems way too small
sqrt(sum(abs, na.rm=TRUE))/
sum(dat$Fc[dat.ts$YEAR%in%years])

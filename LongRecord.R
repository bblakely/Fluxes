#Long term fluxes

library(readxl)
library(bigleaf)

###Read in data####

#Read in Maize Basalt
maize.raw<-read_excel("Maize_2008_to_2019_L6.xlsx", sheet=2,skip=2) #Maize basalt all the way through; no basalt before 2017
maize.raw.2020<-read_excel("Maize_2020_to_2021_L6.xls", sheet=2, skip=2)#read_excel("MaizeCon_2020_L6.xls", sheet=2, skip=2) #Maize control 2020
maize.raw[maize.raw==-9999]<-NA; maize.raw.2020[maize.raw.2020==-9999]<-NA
maize<-maize.raw;maize.2020<-maize.raw.2020
#Merge 2020 with rest; need to rename co2 flux
maize.merge.mod<-maize.2020; colnames(maize.merge.mod)[colnames(maize.merge.mod)=="Fco2"]<-"Fc"
maize.merge<-merge(maize, maize.merge.mod, all=TRUE)
rm(maize.merge.mod)

maize.mgmt<-read_excel("Illinois Energy Farm Flux Towers Management Record.xlsx", sheet=1)
maize.mgmt<-maize.mgmt[, c(1:17)]

#Read in Maize Control
maize.c.raw<-read_excel("MaizeNoBasalt_2017_to_2019_L6.xlsx", sheet=2,skip=2) 
maize.c.raw.2020<-read_excel("MaizeNoBasalt_2020_to_2021_L6.xls", sheet=2, skip=2)#maize.raw.2020 #Maize control 2020
maize.c.raw[maize.c.raw==-9999]<-NA; maize.c.raw.2020[maize.c.raw.2020==-9999]<-NA
maize.c<-maize.c.raw;maize.c.2020<-maize.c.raw.2020

maize.c.merge.mod<-maize.c.2020; colnames(maize.c.merge.mod)[colnames(maize.c.merge.mod)=="Fco2"]<-"Fc"
maize.c.merge<-merge(maize.c, maize.c.merge.mod, all=TRUE)
rm(maize.c.merge.mod)

maize.c.mgmt<-read_excel("Illinois Energy Farm Flux Towers Management Record.xlsx", sheet=3)
maize.c.mgmt<-maize.c.mgmt[, c(1:8)]

#Read in Miscanthus Basalt
miscanthus.raw<-read_excel("Miscanthus_2008_to_2019_L6.xlsx", sheet=2,skip=2) #Basalt
miscanthus.raw[miscanthus.raw==-9999]<-NA
miscanthus.raw.2020<-read_excel("Miscanthus_2020_to_2021_L6.xls", sheet=2, skip=2)#read_excel("MiscanthusNoBasalt_2020_L6.xls", sheet=2, skip=2) #2020 control (fix)
miscanthus.raw[miscanthus.raw==-9999]<-NA; miscanthus.raw.2020[miscanthus.raw.2020==-9999]<-NA
miscanthus<-miscanthus.raw; miscanthus.2020<-miscanthus.raw.2020

misc.merge.mod<-miscanthus.2020; colnames(misc.merge.mod)[colnames(misc.merge.mod)=="Fco2"]<-"Fc"
misc.merge<-merge(miscanthus, misc.merge.mod, all=TRUE)
rm(misc.merge.mod)

miscanthus.mgmt<-read_excel("Illinois Energy Farm Flux Towers Management Record.xlsx", sheet=2)
miscanthus.mgmt<-miscanthus.mgmt[, c(1:17)]

#Read in Miscanthus Control
miscanthus.c.raw<-read_excel("MiscanthusNoBasalt_2017_to_2019_L6.xlsx", sheet=2,skip=2) 
miscanthus.c.raw.2020<-miscanthus.raw.2020# temp solution, uses MGB 2020-2021 read_excel("MiscanthusNoBasalt_2020_to_2021_L6.xls", sheet=2, skip=2)
miscanthus.c.raw[miscanthus.c.raw==-9999]<-NA; miscanthus.c.raw.2020[miscanthus.c.raw.2020==-9999]<-NA
miscanthus.c<-miscanthus.c.raw;miscanthus.c.2020<-miscanthus.raw.2020

misc.c.merge.mod<-miscanthus.c.2020; colnames(misc.c.merge.mod)[colnames(misc.c.merge.mod)=="Fco2"]<-"Fc"
misc.c.merge<-merge(miscanthus.c, misc.c.merge.mod, all=TRUE)
rm(misc.c.merge.mod)

miscanthus.c.mgmt<-read_excel("Illinois Energy Farm Flux Towers Management Record.xlsx", sheet=5)
miscanthus.c.mgmt<-miscanthus.c.mgmt[, c(1:8)]


#Read in sorghum

sorghum.raw<-read_excel("sorghum_2018_to_2019_L6.xlsx", sheet=2,skip=2)
sorghum.raw.2020<-read_excel("Sorghum_2020_to_2021_L6.xls", sheet=2, skip=2)
sorghum.raw[sorghum.raw==-9999]<-NA; sorghum.raw.2020[sorghum.raw.2020==-9999]<-NA
sorghum<-sorghum.raw; sorghum.2020<-sorghum.raw.2020

sorg.merge.mod<-sorghum.2020; colnames(sorg.merge.mod)[colnames(sorg.merge.mod)=="Fco2"]<-"Fc"
sorg.merge<-merge(sorghum, sorg.merge.mod, all=TRUE)
rm(sorg.merge.mod)

sorghum.mgmt<-read_excel("Illinois Energy Farm Flux Towers Management Record.xlsx", sheet=4)
sorghum.mgmt<-sorghum.mgmt[, c(1:7)]

#Read in switchgrass
switchgrass.raw<-read_excel("Switchgrass_2008_to_2016_L6.xlsx", sheet=2,skip=2)
switchgrass.raw[switchgrass.raw==-9999]<-NA
switchgrass<-switchgrass.raw

switchgrass.mgmt<-read_excel("Illinois Energy Farm Flux Towers Management Record.xlsx", sheet=6)
switchgrass.mgmt<-switchgrass.mgmt[, c(1:13)]
#####

###Timestamp unpacker####
unpack.time<-function(dat){
  dat.ts<-dat$xlDateTime
  ts<-strptime(dat.ts, "%Y-%m-%d %H:%M", tz="")
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

#####

#get shared-year datasets####

subyr<-function(dat, year=2017){
  
  dat.sh<-dat[which(as.numeric(format(dat$xlDateTime, "%Y"))>year),]
  return(dat.sh)
  
}

sorghum1<-subyr(sorg.merge)
maize1<-subyr(maize.merge); maize.c1<-subyr(maize.c.merge)
miscanthus1<-subyr(misc.merge); miscanthus.c1<-subyr(misc.c.merge)
#no shared years of switchgrass

#####


#Get yields in correct units
maize.yield.buac<-as.numeric(maize.mgmt[3,4:15]); maize.yield<-maize.yield.buac*0.028 #Conversion: bu corn/ac -> .025t corn/bu corn * 2.47ac/ha * 0.45tC/t corn -> tC/ha
maize.c.yield.buac<-as.numeric(maize.c.mgmt[3,4:8]); maize.c.yield<-maize.c.yield.buac*0.028 #Conversion: bu corn/ac -> .025t corn/bu corn * 2.47ac/ha * 0.45tC/t corn -> tC/ha

misc.yield.ustac<-as.numeric(miscanthus.mgmt[4,4:15]); misc.yield<-misc.yield.ustac*1.08 #conversion: us tons mxg / ac -> .91 metric tons / us ton * 2.47ac/ha * .48tC / t miscanthus -> tC/ha
misc.c.yield.ustac<-as.numeric(miscanthus.c.mgmt[4,4:8]); misc.c.yield<-misc.c.yield.ustac*1.08 #conversion: us tons mxg / ac -> .91 metric tons / us ton * 2.47ac/ha * .48tC / t miscanthus -> tC/ha

switch.yield.ustac<-as.numeric(switchgrass.mgmt[4,4:12]); switch.yield<-switch.yield.ustac*1.08 #conversion: us tons mxg / ac -> .91 metric tons / us ton * 2.47ac/ha * .48tC / t miscanthus -> tC/ha

#sorg.yield.ustac<-as.numeric(sorghum.mgmt[4,4:5]); sorg.yield<-sorg.yield.ustac[1]*1.08 #conversion: us tons mxg / ac -> .91 metric tons / us ton * 2.47ac/ha * .48tC / t sorghum -> tC/ha

sorg.yield.ustac<-as.numeric(sorghum.mgmt[4,4:7]); sorg.yield.ustac[2]<-(-9999) #placeholder for soy year
sorg.yield<-sorg.yield.ustac*1.08 #conversion: us tons mxg / ac -> .91 metric tons / us ton * 2.47ac/ha * .48tC / t sorghum -> tC/ha

sorg.yield[2]<- as.numeric(sorghum.mgmt[3,5]) #bu/ac soy
sorg.yield[2]<-sorg.yield[2]*0.028 #bushel conversion for soy

#Get them into the timeseries
years<-unique(as.numeric(format(maize$xlDateTime, "%Y")))
Y<-(as.numeric(format(maize$xlDateTime, "%Y")))
#location of last record of each year
#match finds the first record of each year so by subtracting one you get the last record of the previous year
#and by appending the length ( == final index) you get the last value for the last year
endofyear<-c(match(years,Y )-1, length(Y))[2:13]
# #####
# maize.sum<-supersum.zm*.0027 #cumulative c flux in t/ha
# 
# maize.hvst<-maize.nep #regular C flux in gC02/m2/d
# maize.hvst.30<-maize.nep/48 #now in g/m2/30min
# maize.hvst.30[endofyear]<-maize.hvst.30[endofyear]+maize.yield*(1/0.0027)# pretend there's a giant flux of C from harvest on the 31st of each year
# maize.hvst.sum<-cumsum(maize.hvst.30)*.0027
# plot(maize.hvst.sum~maize$xlDateTime)
# 
# plot(maize.hvst.sum~maize$xlDateTime); lines(maize.sum~maize$xlDateTime, col='blue')
#####
#Put this aside for a bit; replicate Zeri

#plot(-(cumsum(misc.nep[Y==2011]/48))*.0027~miscanthus$xlDateTime[Y==2011])
#plot(-(cumsum(maize.nep[Y==2009]/48))*.0027~maize$xlDateTime[Y==2015])

library(zoo)
#plot(rollapply(maize.nep[Y==2018], width=48*7, FUN='mean'), type='l', lwd=4)
#####

#Back it up further: convert yourself

maize.gpd<-c(maize$Fc, maize.2020$Fco2)*0.0792 #gc02/pd. Add this directly for cum.
plot(cumsum(maize.gpd[Y==2010])); #lines(cumsum(maize.nep[Y==2008]/48), col='red')
maize.gpd.tha<-maize.gpd*.0027 #tC/ha/30min
plot(-cumsum(maize.gpd.tha[Y==2011]))


maize.hvst<-maize.gpd.tha
maize.hvst.30<-maize.hvst
maize.hvst.30[endofyear]<-maize.hvst.30[endofyear]+maize.yield# pretend there's a giant flux of C from harvest on the 31st of each year
maize.hvst.sum<-cumsum(maize.hvst.30)
plot(maize.hvst.sum~c(maize$xlDateTime, maize.2020$xlDateTime))

#Miscanthus
misc.gpd<-c(miscanthus$Fc, miscanthus.2020$Fco2)*0.0792 #gc02/pd. Add this directly for cum.

plot(cumsum(misc.gpd[Y==2011])); #lines(cumsum(misc.nep[Y==2011]/48), col='red')

misc.gpd.tha<-misc.gpd*.0027 #tC/ha/30min
plot(-cumsum(misc.gpd.tha[Y==2011]))

#Get them into the timeseries
years<-unique(as.numeric(format(miscanthus$xlDateTime, "%Y")))
Y<-(as.numeric(format(miscanthus$xlDateTime, "%Y")))
endofyear<-c(match(years,Y )-1, length(Y))[2:13]


misc.hvst<-misc.gpd.tha
misc.hvst.30<-misc.hvst
misc.yield[1]<-0.0001
misc.hvst.30[endofyear]<-misc.hvst.30[endofyear]+misc.yield# pretend there's a giant flux of C from harvest on the 31st of each year
misc.hvst.sum<-cumsum(misc.hvst.30)
plot(misc.hvst.sum~c(miscanthus$xlDateTime, miscanthus.2020$xlDateTime))

#Switchgrass

switch.gpd<-c(switchgrass$Fc)*0.0792 #gc02/pd. Add this directly for cum.

plot(cumsum(switch.gpd[Y==2011])); #lines(cumsum(misc.nep[Y==2011]/48), col='red')

switch.gpd.tha<-switch.gpd*.0027 #tC/ha/30min
plot(-cumsum(switch.gpd.tha[Y==2011]))

#Get them into the timeseries
years<-unique(as.numeric(format(switchgrass$xlDateTime, "%Y")))
Y<-(as.numeric(format(switchgrass$xlDateTime, "%Y")))
endofyear<-c(match(years,Y )-1, length(Y))[2:10]


switch.hvst<-switch.gpd.tha
switch.hvst.30<-switch.hvst
switch.yield[1]<-0.0001
switch.hvst.30[endofyear]<-switch.hvst.30[endofyear]+switch.yield# pretend there's a giant flux of C from harvest on the 31st of each year
switch.hvst.sum<-cumsum(switch.hvst.30)
plot(switch.hvst.sum~c(switchgrass$xlDateTime))


#Sorghum?
sorg.gpd<-c(sorghum$Fc, sorghum.2020$Fco2)*0.0792 #gc02/pd. Add this directly for cum.

Y<-(as.numeric(format(sorghum$xlDateTime, "%Y")))

plot(cumsum(sorg.gpd[Y==2019])); lines(cumsum(sorg.nep[Y==2019]/48), col='red')

sorg.gpd.tha<-sorg.gpd*.0027 #tC/ha/30min
plot(-cumsum(sorg.gpd.tha[Y==2019]))

#Get them into the timeseries
years<-unique(as.numeric(format(sorghum$xlDateTime, "%Y")))
Y<-(as.numeric(format(sorghum$xlDateTime, "%Y")))
#location of last record of each year
#match finds the first record of each year so by subtracting one you get the last record of the previous year
#and by appending the length ( == final index) you get the last value for the last year
endofyear<-c(match(years,Y )-1, length(Y))[2:3]


sorg.hvst<-sorg.gpd.tha
sorg.hvst.30<-sorg.hvst
sorg.hvst.30[endofyear]<-sorg.hvst.30[endofyear]+sorg.yield# pretend there's a giant flux of C from harvest on the 31st of each year
sorg.hvst.sum<-cumsum(sorg.hvst.30)
plot(sorg.hvst.sum~c(sorghum$xlDateTime, sorghum.2020$xlDateTime))



samp<-seq(from=1, to=236333, by=10)

par(mfrow=c(1,1))

Y<-(as.numeric(format(c(maize$xlDateTime, maize.2020$xlDateTime), "%Y")))
col.zm=rep('orange', length(Y));col.zm[which(Y%in%c(2010, 2013, 2016, 2019))]<-"light green"
Y<-(as.numeric(format(c(sorghum$xlDateTime, sorghum.2020$xlDateTime), "%Y")))
col.sb<-rep('forest green', length(Y));col.sb[which(Y%in%c(2010, 2013, 2016, 2019))]<-"light green"


plot(maize.hvst.sum[samp]~c(maize$xlDateTime, maize.2020$xlDateTime)[samp], ylim=c(-35,35), col=col.zm[samp], pch='.', ylab="Cumulative NEE, tC ha-1", xlab='', cex=2); 
points(misc.hvst.sum[samp]~c(miscanthus$xlDateTime, miscanthus.2020$xlDateTime)[samp], col='light blue', pch='.', cex=2)
points(switch.hvst.sum[samp]~c(switchgrass$xlDateTime)[samp], col='pink', pch='.', cex=2)
points(sorg.hvst.sum[samp]~c(sorghum$xlDateTime,sorghum.2020$xlDateTime)[samp], col=col.sb[samp], pch='.', cex=2)
abline(h=0)
legend(as.numeric(min(maize$xlDateTime)), 35, legend=c("maize", "soybean", "miscanthus", "switchgrass","sorghum"), col=c("orange","light green", "light blue", "pink", "forest green"), lwd=2, bty='n', cex=0.8)

#Next steps: cleanup, adjusting yield for soy


#Things moved to other scripts####

# #Quick look at whether 2020 was bad?
# 
# years<-unique(as.numeric(format(maize$xlDateTime, "%Y")))
# Y<-(as.numeric(format(maize$xlDateTime, "%Y")))
# #read in maize
# maize.raw.2020<-read_excel("MaizeCon_2020_L6.xls", sheet=2,skip=2)
# maize.raw.2020[maize.raw.2020==-9999]<-NA
# maize.2020<-maize.raw.2020#[1:(nrow(maize.raw)-1),]
# 
# plot(cumsum(maize$Fc[which(Y==2011)])~as.numeric(format(maize$xlDateTime[Y==2011], "%j")), col='white', ylim=c(-30000, 15000))
# for (i in c(1:6, 9:length(years))){
#   yearsum<-cumsum(maize$Fc[which(Y==years[i])])
#   
#   lines(yearsum~as.numeric(format(maize$xlDateTime[Y==years[i]], "%j")), col=i)
#   
#   
# }
# 
# lines(cumsum(maize.2020$Fco2)~as.numeric(format(maize.2020$xlDateTime, "%j")), col='black', lwd=4, type='l')
# lines(cumsum(maize$Fc[Y==2017])~as.numeric(format(maize$xlDateTime[Y==2017], "%j")), col='blue', lwd=4, type='l')
# lines(cumsum(maize$Fc[Y==2018])~as.numeric(format(maize$xlDateTime[Y==2018], "%j")), col='red', lwd=4, type='l')
# 
# #Nope, corn was just fine being knocked over apparently


# #Check the energy fluxes
# 
# 
# #Albedo / shortwave
# 
# albify<-function(dat, window=1){
#   dat.alb<-dat$Fsu/dat$Fsd; dat.alb[dat.alb<0|dat.alb>1|dat$Fsd<10]<-NA
#   dat.doy<-as.numeric(format(dat$xlDateTime, "%m"))
#   dat.d.alb<-aggregate(dat.alb, by=list(dat.doy), FUN='mean', na.rm=TRUE)
#   dat.alb.sm<-rollapply(dat.d.alb$x, width=window, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
#   
#   return(dat.alb.sm)
# }
# 
# # sorg.alb.sm<-albify(sorghum)
# # maize.alb.sm<-albify(maize);maize.c.alb.sm<-albify(maize.c)
# # misc.alb.sm<-albify(miscanthus);misc.c.alb.sm<-albify(miscanthus.c)
# # switch.alb.sm<-albify(switchgrass)
# 
# # sorg.alb.sm<-albify(sorghum1)
# # maize.alb.sm<-albify(maize1);maize.c.alb.sm<-albify(maize.c1)
# # misc.alb.sm<-albify(miscanthus1);misc.c.alb.sm<-albify(miscanthus.c1)
# # switch.alb.sm<-albify(switchgrass)
# 
# sorg.alb.sm<-albify(sorg.merge)
# maize.alb.sm<-albify(maize.merge);maize.c.alb.sm<-albify(maize.c.merge)
# misc.alb.sm<-albify(misc.merge);misc.c.alb.sm<-albify(misc.c.merge)
# switch.alb.sm<-albify(switchgrass)
# 
# 
# par(mfrow=c(1,1))
# plot(sorg.alb.sm, type='l', col='forest green', xlab='month', main="Albedo")
# lines(maize.alb.sm, col='orange')
# lines(maize.c.alb.sm, col='gold')
# lines(misc.alb.sm, col='blue')
# lines(misc.c.alb.sm, col='light blue')
# lines(switch.alb.sm, col='pink')
# 
# #aggregate plant types
# 
# misc.alb<-(misc.alb.sm+misc.c.alb.sm)/2
# maize.alb<-(maize.alb.sm+maize.c.alb.sm)/2
# 
# plot(sorg.alb.sm, type='l', col='forest green', xlab='month')
# lines(maize.alb, col='orange')
# lines(misc.alb, col='light blue')
# 
# 
# #Radiative forcing
# 
# monthrad<-aggregate(maize$Fsd, by=list(as.numeric(format(maize$xlDateTime, "%m"))), FUN='mean', na.rm=TRUE)
# 
# par(mfrow=c(1,3))
# 
# albdiff<-maize.alb-misc.alb #"for conversion from corn to miscanthus"
# rf<-monthrad$x*albdiff; rf.ann<-mean(rf)
# plot(rf, type='l', main="maize->misc"); abline(h=0)
# text(9, 0.9*max(rf), paste("yearly:", round(rf.ann, 2)))
# 
# albdiff<-maize.alb-sorg.alb.sm #for conversion from corn to sorgum
# rf<-monthrad$x*albdiff; rf.ann<-mean(rf)
# plot(rf, type='l', main="maize->sorg"); 
# abline(h=0); text(9, 0.9*max(rf), paste("yearly:", round(rf.ann, 2)))
# 
# albdiff<-maize.alb-switch.alb.sm #for conversion from corn to sorgum
# rf<-monthrad$x*albdiff; rf.ann<-mean(rf)
# plot(rf, type='l', main="maize->switchgrass"); 
# abline(h=0); text(9, 0.9*max(rf), paste("yearly:", round(rf.ann, 2)))
# 
# 
# #Bowen ratio##
# 
# bowenize<-function(dat, window=1){
#   dat.bow<-dat$Fh/dat$Fe; dat.bow[dat$Fh<0|dat$Fe<0|dat.bow>100]<-NA
#   dat.doy<-as.numeric(format(dat$xlDateTime, "%m"))
#   dat.d.bow<-aggregate(dat.bow, by=list(dat.doy), FUN='mean', na.rm=TRUE)
#   dat.bow.sm<-rollapply(dat.d.bow$x, width=window, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
#   
#   return(dat.bow.sm)
#   
# }
# 
# # sorg.bow<-bownize(sorghum)
# # maize.bow<-bownize(maize);maize.c.bow<-bownize(maize.c)
# # misc.bow<-bownize(miscanthus);misc.c.bow<-bownize(miscanthus.c)
# # switch.bow<-bownize(switchgrass)
# 
# # sorg.bow<-bownize(sorghum1)
# # maize.bow<-bownize(maize1);maize.c.bow<-bownize(maize.c1)
# # misc.bow<-bownize(miscanthus1);misc.c.bow<-bownize(miscanthus.c1)
# # switch.bow<-bownize(switchgrass)
# 
# sorg.bow<-bowenize(sorg.merge)
# maize.bow<-bowenize(maize.merge);maize.c.bow<-bowenize(maize.c.merge)
# misc.bow<-bowenize(misc.merge);misc.c.bow<-bowenize(misc.c.merge)
# switch.bow<-bowenize(switchgrass)
# 
# par(mfrow=c(1,1))
# plot(sorg.bow, type='l', col='forest green', xlab='month', ylim=c(0.1, 4), main="Bowen Ratio")
# lines(maize.bow, col='orange')
# lines(maize.c.bow, col='gold')
# lines(misc.bow, col='blue')
# lines(misc.c.bow, col='light blue')
# lines(switch.bow, col='pink')
# ##
# 
# #Ts-Ta ##
# #Make a just tc column for switchgrass
# 
# switchgrass$Tc<-switchgrass$Tc_3m
# 
# stemp<-function(dat, window=1){
#   dat.st<-dat$Tc-dat$Ta;dat.st[dat.st<(-20)|dat.st>20]<-NA
#   dat.doy<-as.numeric(format(dat$xlDateTime, "%m"))
#   dat.d.st<-aggregate(dat.st, by=list(dat.doy), FUN='mean', na.rm=TRUE)
#   dat.st.sm<-rollapply(dat.d.st$x, width=window, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
#   
#   return(dat.st.sm)
#   
# }
# 
# # sorg.st<-stemp(sorghum)
# # maize.st<-stemp(maize);maize.c.st<-stemp(maize.c)
# # misc.st<-stemp(miscanthus);misc.c.st<-stemp(miscanthus.c)
# # switch.st<-stemp(switchgrass)
# 
# # sorg.st<-stemp(sorghum1)
# # maize.st<-stemp(maize1);maize.c.st<-stemp(maize.c1)
# # misc.st<-stemp(miscanthus1);misc.c.st<-stemp(miscanthus.c1)
# # switch.st<-stemp(switchgrass)
# 
# sorg.st<-stemp(sorg.merge)
# maize.st<-stemp(maize.merge);maize.c.st<-stemp(maize.c.merge)
# misc.st<-stemp(misc.merge);misc.c.st<-stemp(miscanthus.c.merge)
# switch.st<-stemp(switchgrass)
# 
# 
# par(mfrow=c(1,1))
# plot(sorg.st, type='l', col='forest green', xlab='month', main="Surface - Air temperature differential")
# lines(maize.st, col='orange')
# lines(maize.c.st, col='gold')
# lines(misc.st, col='blue')
# lines(misc.c.st, col='light blue')
# lines(switch.st, col='pink')
##
# 
# 
# #Soil temp at planting time
# sbdates<-unpack.time(maize.merge)
# subset<-which(sbdates$MONTH==5)
# 
# #thing<-aggregate(sorg.merge$Ts_5cm[subset], by=list(sbdates$DOY[subset]), FUN='mean', na.rm=TRUE)
# 
# #plot(thing, ylab='temp deg.c', xlab="day of year (May)");(abline(v=135))
# 
# interan<-function(dat, sub=c(2008:2021)){
#   ts<-unpack.time(dat)
#   ind<-which(ts$YEAR%in%sub)
#   nee<-aggregate(dat$ER_LT[ind]*0.0792*0.0027, by=list(ts$YEAR[ind]), FUN='sum', na.rm=TRUE)
# }
# 
# nee.zm<-interan(maize.merge); nee.zm$id<-"ZM"
# nee.mg<-interan(misc.merge);nee.mg$id<-"MG"
# nee.sw<-interan(switchgrass); nee.sw$id<-"SW"
# nee.sb<-interan(sorg.merge); nee.sb$id<-"SB"
# 
# nee.zmc<-interan(maize.c.merge); nee.zmc$id<-"ZMC"
# nee.mgc<-interan(misc.c.merge); nee.mgc$id<-"MGC"
# 
# nee.comp<-rbind(nee.zm,nee.mg, nee.sb, nee.sw, nee.zmc, nee.mgc)
# 
# 
# short.nosoy<- c(2020, 2021)
# early.nosoy<-c(2008, 2009, 2011, 2012,  2014, 2015)
# long.nosoy<-c(2008, 2009, 2011, 2012, 2014, 2015, 2017, 2018, 2020, 2021)
# soy.yr<-c(2010, 2013, 2016, 2019); soy.yr.early<-c(2010, 2013, 2016)
# 
# 
# boxplot(x~id, dat=nee.comp[nee.comp$Group.1%in%short.nosoy,], ylab="ER", main="2019 to 2021 (sorghum, maize, miscanthus)")
# boxplot(x~id, dat=nee.comp[nee.comp$Group.1%in%early.nosoy,], ylab="ER", main="2008 to 2016 (switchgrass, maize, miscanthus")
# boxplot(x~id, dat=nee.comp[which(nee.comp$id%in%c("ZM", "MG")& nee.comp$Group.1%in%long.nosoy),], ylab="ER", main="2008 to 2021 (maize, miscanthus)")
# 
# boxplot(x~id, dat=nee.comp[which(nee.comp$id%in%c("ZM", "MG")& nee.comp$Group.1%in%soy.yr),], ylab="ER", main="2010/13/16/19 (soy years)")
# boxplot(x~id, dat=nee.comp[which(nee.comp$Group.1%in%soy.yr.early),], ylab="ER", main="2010/13/16 (soy years w/switchgrass)")
# 
# 
# library(vioplot)
# 
# 
# vioplot(x~id, dat=nee.comp[nee.comp$Group.1%in%short.nosoy,], ylab="ER", main="2019 to 2021 (sorghum, maize, miscanthus)")
# vioplot(x~id, dat=nee.comp[nee.comp$Group.1%in%early.nosoy,], ylab="ER", main="2008 to 2016 (switchgrass, maize, miscanthus")
# vioplot(x~id, dat=nee.comp[which(nee.comp$id%in%c("ZM", "MG")& nee.comp$Group.1%in%long.nosoy),], ylab="ER", main="2008 to 2021 (maize, miscanthus)")
# 
# 
# # #extract some stuff for matt
# # 
# # 
# # windind<-function(dat, year, month, day){
# # dat.ts<-unpack.time(dat); 
# # dat.ind<-which(dat.ts$YEAR==year&dat.ts$MONTH==month&dat.ts$DAY==day)
# # return(dat.ind)
# # }
# # 
# # datevec<-data.frame(matrix(nrow=3, ncol=0))
# # datevec$years<-c(2018, 2019, 2020); datevec$months<-c(8,8,9); datevec$days<-c(31, 28, 19)
# # datenames<-paste(datevec$years, datevec$months,  datevec$days, sep ="-")
# # 
# # summary.stat<-data.frame(matrix(nrow=nrow(datevec), ncol=3)); colnames(summary.stat)<-c("avg", "max", "min")
# # avgwind<-data.frame(matrix(nrow=48, ncol=nrow(datevec)+1)); colnames(avgwind)<-c("hour",datenames)
# # 
# # 
# # for(i in 1:nrow(datevec)){
# # 
# # 
# # sb.ind.1<-windind(dat=sorg.merge, datevec[i,1], datevec[i,2], datevec[i,3])
# # mgc.ind.1<-windind(dat=misc.c.merge, datevec[i,1], datevec[i,2], datevec[i,3])
# # mgb.ind.1<-windind(dat=misc.merge, datevec[i,1], datevec[i,2], datevec[i,3])
# # zmc.ind.1<-windind(dat=maize.c.merge, datevec[i,1], datevec[i,2], datevec[i,3])
# # zmb.ind.1<-windind(dat=maize.merge, datevec[i,1], datevec[i,2], datevec[i,3])
# # 
# # wind.dat<-cbind(sorg.merge$Ws[sb.ind.1],
# #                 misc.merge$Ws[mgb.ind.1], misc.c.merge$Ws[mgc.ind.1],
# #                 maize.merge$Ws[zmb.ind.1], maize.c.merge$Ws[zmc.ind.1])
# # 
# # cols<-c("forest green", "blue", "light blue", "orange", "yellow")
# # 
# # plot(wind.dat[,i], col='white', main=datevec[i,], ylim=c(0, max(wind.dat[,i])*1.3))
# # for (p in 1: ncol(wind.dat)){
# #   points(wind.dat[,p], col=cols[p])
# # }
# # 
# # avg.wind<-rowMeans(wind.dat)
# # avgwind[,i+1]<-avg.wind
# # 
# # 
# # #lines(avg.wind)
# # 
# # avg.all<-mean(avg.wind); max.all<-max(avg.wind); min.all<-min(avg.wind)
# # 
# # summary.stat[i,]<-c(avg.all, max.all, min.all)
# # }
# # 
# # time.ex<-unpack.time(sorg.merge)[sb.ind.1,]
# # avgwind[,1]<-c(time.ex$DECHR)
# # 
# # 
# # write.csv(avgwind, "Windspeed(ms-2)_dates_requested_by_MS.csv", row.names = FALSE)
#####
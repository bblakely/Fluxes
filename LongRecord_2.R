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

nativeprairie.raw<-read_excel("Prairie_2008_to_2016_L6.xlsx", sheet=2,skip=2)
nativeprairie.raw[nativeprairie.raw==-9999]<-NA
nativeprairie<-nativeprairie.raw

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


#Unit conversions for yields####
maize.yield.buac<-as.numeric(maize.mgmt[3,4:17]); maize.yield<-maize.yield.buac*0.028 #Conversion: bu corn/ac -> .025t corn/bu corn * 2.47ac/ha * 0.45tC/t corn -> tC/ha
maize.c.yield.buac<-as.numeric(maize.c.mgmt[3,4:8]); maize.c.yield<-maize.c.yield.buac*0.028 #Conversion: bu corn/ac -> .025t corn/bu corn * 2.47ac/ha * 0.45tC/t corn -> tC/ha

misc.yield.ustac<-as.numeric(miscanthus.mgmt[4,4:17]); misc.yield<-misc.yield.ustac*1.08 #conversion: us tons mxg / ac -> .91 metric tons / us ton * 2.47ac/ha * .48tC / t miscanthus -> tC/ha
misc.c.yield.ustac<-as.numeric(miscanthus.c.mgmt[4,4:8]); misc.c.yield<-misc.c.yield.ustac*1.08 #conversion: us tons mxg / ac -> .91 metric tons / us ton * 2.47ac/ha * .48tC / t miscanthus -> tC/ha

switch.yield.ustac<-as.numeric(switchgrass.mgmt[4,4:12]); switch.yield<-switch.yield.ustac*1.08 #conversion: us tons mxg / ac -> .91 metric tons / us ton * 2.47ac/ha * .48tC / t miscanthus -> tC/ha

np.yield.ustac<-c(0, 1.02, 2.73,1.51, 1.26, 2.49, 2.4, 2.19, 0); np.yield<-np.yield.ustac*1.08 

sorg.yield.ustac<-as.numeric(sorghum.mgmt[4,4:7]); sorg.yield.ustac[2]<-(-9999) #placeholder for soy year
sorg.yield<-sorg.yield.ustac*1.08 #conversion: us tons mxg / ac -> .91 metric tons / us ton * 2.47ac/ha * .48tC / t sorghum -> tC/ha

sorg.yield[2]<- as.numeric(sorghum.mgmt[3,5]) #bu/ac soy
sorg.yield[2]<-sorg.yield[2]*0.028 #bushel conversion for soy


#####

#Prepare harvest-adjusted carbon fluxes####

par(mfrow=c(1,1))

#Maize-basalt
years<-unique(as.numeric(format(maize.merge$xlDateTime, "%Y")))
Y<-(as.numeric(format(maize.merge$xlDateTime, "%Y")))
#endofyear is the location of last record of each year
#match finds the first record of each year so by subtracting one you get the last record of the previous year
#and by appending the length ( == final index) you get the last value for the last year
endofyear<-c(match(years,Y )-1, length(Y))[2:15]


maize.gpd<-maize.merge$Fc*0.0792 #gc02/pd. Add this directly for cum.
#plot(cumsum(maize.gpd[Y==2010])); #lines(cumsum(maize.nep[Y==2008]/48), col='red')
maize.gpd.tha<-maize.gpd*.0027 #tC/ha/30min
#plot(-cumsum(maize.merge.gpd.tha[Y==2011]))


maize.hvst<-maize.gpd.tha
maize.hvst.30<-maize.hvst
maize.hvst.30[endofyear]<-maize.hvst.30[endofyear]+maize.yield# pretend there's a giant flux of C from harvest on the 31st of each year
maize.hvst.sum<-cumsum(maize.hvst.30)
plot(maize.hvst.sum~maize.merge$xlDateTime)


#Maize-control

#Set up Timeseries
years<-unique(as.numeric(format(maize.c.merge$xlDateTime, "%Y")))
Y<-(as.numeric(format(maize.c.merge$xlDateTime, "%Y")))
endofyear<-c(match(years,Y )-1, length(Y))[2:6]

#Unit conversions for carbon
maize.c.gpd<-maize.c.merge$Fc*0.0792 #gc02/pd. Add this directly for cum.
maize.c.gpd.tha<-maize.c.gpd*.0027 #tC/ha/30min

#Deduct harvest from Carbon Flux
maize.c.hvst<-maize.c.gpd.tha
maize.c.hvst.30<-maize.c.hvst
maize.c.hvst.30[endofyear]<-maize.c.hvst.30[endofyear]+maize.c.yield# pretend there's a giant flux of C from harvest on the 31st of each year
maize.c.hvst.sum<-cumsum(maize.c.hvst.30)
plot(maize.c.hvst.sum~maize.c.merge$xlDateTime)


#Miscanthus-basalt

#Set up timeseries
years<-unique(as.numeric(format(misc.merge$xlDateTime, "%Y")))
Y<-(as.numeric(format(misc.merge$xlDateTime, "%Y")))
endofyear<-c(match(years,Y )-1, length(Y))[2:15]

#Unit conversions for carbon
misc.gpd<-misc.merge$Fc*0.0792 #gc02/pd. Add this directly for cum.
misc.gpd.tha<-misc.gpd*.0027 #tC/ha/30min

#Deduct harvest from cumulative carbon flux
misc.hvst<-misc.gpd.tha
misc.hvst.30<-misc.hvst
misc.yield[1]<-0.0001
misc.hvst.30[endofyear]<-misc.hvst.30[endofyear]+misc.yield# pretend there's a giant flux of C from harvest on the 31st of each year
misc.hvst.sum<-cumsum(misc.hvst.30)
plot(misc.hvst.sum~misc.merge$xlDateTime)

#Miscanthus-control

#Set up timeseries
years<-unique(as.numeric(format(misc.c.merge$xlDateTime, "%Y")))
Y<-(as.numeric(format(misc.c.merge$xlDateTime, "%Y")))
endofyear<-c(match(years,Y )-1, length(Y))[2:6]

#Unit conversions for carbon
misc.c.gpd<-misc.c.merge$Fc*0.0792 #gc02/pd. Add this directly for cum.
misc.c.gpd.tha<-misc.c.gpd*.0027 #tC/ha/30min

#Deduct harvest from cumulative carbon flux
misc.c.hvst<-misc.c.gpd.tha
misc.c.hvst.30<-misc.c.hvst
misc.c.yield[1]<-0.0001
misc.c.hvst.30[endofyear]<-misc.c.hvst.30[endofyear]+misc.c.yield# pretend there's a giant flux of C from harvest on the 31st of each year
misc.c.hvst.sum<-cumsum(misc.c.hvst.30)
plot(misc.c.hvst.sum~misc.c.merge$xlDateTime)


#Switchgrass

#Set up timeseries
years<-unique(as.numeric(format(switchgrass$xlDateTime, "%Y")))
Y<-(as.numeric(format(switchgrass$xlDateTime, "%Y")))
endofyear<-c(match(years,Y )-1, length(Y))[2:10]

#Unit conversions for carbon
switch.gpd<-c(switchgrass$Fc)*0.0792 #gc02/pd. Add this directly for cum.
switch.gpd.tha<-switch.gpd*.0027 #tC/ha/30min

switch.hvst<-switch.gpd.tha
switch.hvst.30<-switch.hvst
switch.yield[1]<-0.0001
switch.hvst.30[endofyear]<-switch.hvst.30[endofyear]+switch.yield# pretend there's a giant flux of C from harvest on the 31st of each year
switch.hvst.sum<-cumsum(switch.hvst.30)
plot(switch.hvst.sum~c(switchgrass$xlDateTime))



#Native Priairie

#Set up timeseries
years<-unique(as.numeric(format(nativeprairie$xlDateTime, "%Y")))
Y<-(as.numeric(format(nativeprairie$xlDateTime, "%Y")))
endofyear<-c(match(years,Y )-1, length(Y))[2:10]

#Unit conversions for carbon
np.gpd<-c(nativeprairie$Fc)*0.0792 #gc02/pd. Add this directly for cum.
np.gpd.tha<-np.gpd*.0027 #tC/ha/30min

np.hvst<-np.gpd.tha
np.hvst.30<-np.hvst
np.yield[1]<-0.0001
np.hvst.30[endofyear]<-np.hvst.30[endofyear]+np.yield# pretend there's a giant flux of C from harvest on the 31st of each year
np.hvst.sum<-cumsum(np.hvst.30)
plot(np.hvst.sum~c(nativeprairie$xlDateTime))

#Sorghum

#Set up timeseries
years<-unique(as.numeric(format(sorg.merge$xlDateTime, "%Y")))
Y<-(as.numeric(format(sorg.merge$xlDateTime, "%Y")))
endofyear<-c(match(years,Y )-1, length(Y))[2:5]

#Unit conversions for carbon
sorg.gpd<-sorg.merge$Fc*0.0792 #gc02/pd. Add this directly for cum.
sorg.gpd.tha<-sorg.gpd*.0027 #tC/ha/30min

#Deduct harvest from carbon flux
sorg.hvst<-sorg.gpd.tha
sorg.hvst.30<-sorg.hvst
sorg.hvst.30[endofyear]<-sorg.hvst.30[endofyear]+sorg.yield# pretend there's a giant flux of C from harvest on the 31st of each year
sorg.hvst.sum<-cumsum(sorg.hvst.30)
plot(sorg.hvst.sum~sorg.merge$xlDateTime)



#Mega plot

samp<-seq(from=1, to=236333, by=10)
par(mfrow=c(1,1))

#Set up alternating colors for maize-soy and sorghum-soy rotations
Y<-(as.numeric(format(maize.merge$xlDateTime, "%Y")))
col.zm=rep('orange', length(Y));col.zm[which(Y%in%c(2010, 2013, 2016, 2019))]<-"light green"
Y<-(as.numeric(format(sorg.merge$xlDateTime, "%Y")))
col.sb<-rep('forest green', length(Y));col.sb[which(Y%in%c(2010, 2013, 2016, 2019))]<-"light green"
Y<-(as.numeric(format(maize.c.merge$xlDateTime, "%Y")))
col.zmc=rep('yellow', length(Y));col.zmc[which(Y%in%c(2010, 2013, 2016, 2019))]<-"light green"

par(bty='n')
#Plotting
plot(maize.hvst.sum[samp]~maize.merge$xlDateTime[samp], ylim=c(-45,35), col=col.zm[samp], pch='.', ylab="Cumulative NEE, tC ha-1", xlab='', cex=2); 
points(misc.hvst.sum[samp]~misc.merge$xlDateTime[samp], col='blue', pch='.', cex=2)
points(switch.hvst.sum[samp]~c(switchgrass$xlDateTime)[samp], col='pink', pch='.', cex=2)
points(sorg.hvst.sum[samp]~sorg.merge$xlDateTime[samp], col=col.sb[samp], pch='.', cex=2)
points(misc.c.hvst.sum[samp]~misc.c.merge$xlDateTime[samp], col='light blue', pch='.', cex=2)
points(maize.c.hvst.sum[samp]~maize.c.merge$xlDateTime[samp], col=col.zmc[samp], pch='.', cex=2)
points(np.hvst.sum[samp]~nativeprairie$xlDateTime[samp], col="plum3" , pch='.', cex=2)

zmc<-data.frame(cbind(unpack.time(maize.c.merge),maize.c.hvst.sum))
zmb<-data.frame(cbind(unpack.time(maize.merge),maize.hvst.sum))
mgc<-data.frame(cbind(unpack.time(misc.c.merge),misc.c.hvst.sum))
mgb<-data.frame(cbind(unpack.time(misc.merge),misc.hvst.sum))
sb<-data.frame(cbind(unpack.time(sorg.merge),sorg.hvst.sum))
sw<-data.frame(cbind(unpack.time(switchgrass),switch.hvst.sum))
np<-data.frame(cbind(unpack.time(nativeprairie),np.hvst.sum))

colnames(zmc)[9]<-colnames(zmb)[9]<-colnames(mgc)[9]<-colnames(mgb)[9]<-colnames(sb)[9]<-colnames(sw)[9]<-"cflux"

cumulatives<-list(zmc, zmb, mgc, mgb, sb, sw)
names(cumulatives)<-c('zmc', 'zmb', 'mgc', 'mgb', 'sb', 'sw')


abline(h=0)
legend(as.numeric(min(maize$xlDateTime)), 35, legend=c("maize 1", "maize 2", "soybean", "miscanthus 1", "miscanthus 2", "native prairie", "switchgrass","sorghum"), col=c("orange","yellow","light green","blue", "light blue", "plum3", "pink", "forest green"), lwd=2, bty='n', cex=0.8, ncol=2)

#harvest plot?


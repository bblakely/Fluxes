#Long term fluxes

library(readxl)
library(scales)
library(ggplot2)
library(zoo)
#library(bigleaf)

harvest.version<-"endofyear" #set to "endofyear' for harvest out at the end of the year, "harvestdate" for harvest out on the harvest date.
fullyears<-"TRUE" #set to true to only keep full years of data (no partial years)
###Read in data####

dir<-getwd()
setwd("Fluxdata")

#Read in Maize Basalt
maize.raw<-read_excel("Maize_2008_to_2019_L6.xlsx", sheet=2,skip=2) #Maize basalt all the way through; no basalt before 2017
maize.raw.2<-read_excel("MaizeBasalt_2020_2022_L6.xls", sheet=2, skip=2)#Add data from after 2020 (separate file due to pyfluxpro update)
maize.raw[maize.raw==-9999]<-NA; maize.raw.2[maize.raw.2==-9999]<-NA
maize<-maize.raw;maize.2<-maize.raw.2
#Merge 2020+ with rest; need to rename co2 flux
maize.merge.mod<-maize.2; colnames(maize.merge.mod)[colnames(maize.merge.mod)=="Fco2"]<-"Fc"
maize.merge<-merge(maize, maize.merge.mod, all=TRUE)
rm(maize.merge.mod)

maize.mgmt<-read_excel("Illinois Energy Farm Flux Towers Management Record.xlsx", sheet=1)
maize.mgmt<-maize.mgmt[, c(1:18)]

#Read in Maize Control
maize.c.raw<-read_excel("MaizeNoBasalt_2017_to_2019_L6.xlsx", sheet=2,skip=2) 
maize.c.raw.2<-read_excel("MaizeNoBasalt_2020_to_2022_L6.xls", sheet=2, skip=2)
maize.c.raw[maize.c.raw==-9999]<-NA; maize.c.raw.2[maize.c.raw.2==-9999]<-NA
maize.c<-maize.c.raw;maize.c.2<-maize.c.raw.2

maize.c.merge.mod<-maize.c.2; colnames(maize.c.merge.mod)[colnames(maize.c.merge.mod)=="Fco2"]<-"Fc"
maize.c.merge<-merge(maize.c, maize.c.merge.mod, all=TRUE)
rm(maize.c.merge.mod)

maize.c.mgmt<-read_excel("Illinois Energy Farm Flux Towers Management Record.xlsx", sheet=3)
maize.c.mgmt<-maize.c.mgmt[, c(1:9)]

#Read in Miscanthus Basalt
miscanthus.raw<-read_excel("Miscanthus_2008_to_2019_L6.xlsx", sheet=2,skip=2) #Basalt
miscanthus.raw[miscanthus.raw==-9999]<-NA
miscanthus.raw.2<-read_excel("MiscanthusBasalt_2020_2022_L6.xls", sheet=2, skip=2)#read_excel("MiscanthusNoBasalt_2020_L6.xls", sheet=2, skip=2) #2020 control (fix)
miscanthus.raw[miscanthus.raw==-9999]<-NA; miscanthus.raw.2[miscanthus.raw.2==-9999]<-NA
miscanthus<-miscanthus.raw; miscanthus.2<-miscanthus.raw.2

misc.merge.mod<-miscanthus.2; colnames(misc.merge.mod)[colnames(misc.merge.mod)=="Fco2"]<-"Fc"
misc.merge<-merge(miscanthus, misc.merge.mod, all=TRUE)
rm(misc.merge.mod)

miscanthus.mgmt<-read_excel("Illinois Energy Farm Flux Towers Management Record.xlsx", sheet=2)
miscanthus.mgmt<-miscanthus.mgmt[, c(1:18)]

#Read in Miscanthus Control
miscanthus.c.raw<-read_excel("MiscanthusNoBasalt_2017_to_2019_L6.xlsx", sheet=2,skip=2) 
miscanthus.c.raw.2<-read_excel("MiscanthusNoBasalt_2020_2022_L6.xls", sheet=2, skip=2)#miscanthus.raw.2# temp solution, uses MGB 2020-2021 read_excel("MiscanthusNoBasalt_2020_to_2021_L6.xls", sheet=2, skip=2)
miscanthus.c.raw[miscanthus.c.raw==-9999]<-NA; miscanthus.c.raw.2[miscanthus.c.raw.2==-9999]<-NA
miscanthus.c<-miscanthus.c.raw;miscanthus.c.2<-miscanthus.c.raw.2

misc.c.merge.mod<-miscanthus.c.2; colnames(misc.c.merge.mod)[colnames(misc.c.merge.mod)=="Fco2"]<-"Fc"
misc.c.merge<-merge(miscanthus.c, misc.c.merge.mod, all=TRUE)
rm(misc.c.merge.mod)

miscanthus.c.mgmt<-read_excel("Illinois Energy Farm Flux Towers Management Record.xlsx", sheet=5)
miscanthus.c.mgmt<-miscanthus.c.mgmt[, c(1:9)]


#Read in sorghum

sorghum.raw<-read_excel("sorghum_2018_to_2019_L6.xlsx", sheet=2,skip=2)
sorghum.raw.2<-read_excel("Sorghum_2020_2022_L6.xls", sheet=2, skip=2)
sorghum.raw[sorghum.raw==-9999]<-NA; sorghum.raw.2[sorghum.raw.2==-9999]<-NA
sorghum<-sorghum.raw; sorghum.2<-sorghum.raw.2

sorg.merge.mod<-sorghum.2; colnames(sorg.merge.mod)[colnames(sorg.merge.mod)=="Fco2"]<-"Fc"
sorg.merge<-merge(sorghum, sorg.merge.mod, all=TRUE)
rm(sorg.merge.mod)

sorghum.mgmt<-read_excel("Illinois Energy Farm Flux Towers Management Record.xlsx", sheet=4)
sorghum.mgmt<-sorghum.mgmt[, c(1:8)]

#Read in switchgrass
switchgrass.raw<-read_excel("Switchgrass_2008_to_2016_L6.xlsx", sheet=2,skip=2)
switchgrass.raw[switchgrass.raw==-9999]<-NA
switchgrass<-switchgrass.raw

switchgrass.mgmt<-read_excel("Illinois Energy Farm Flux Towers Management Record.xlsx", sheet=6)
switchgrass.mgmt<-switchgrass.mgmt[, c(1:12)]

#Read in prairie
nativeprairie.raw<-read_excel("Prairie_2008_to_2016_L6.xlsx", sheet=2,skip=2)
nativeprairie.raw[nativeprairie.raw==-9999]<-NA
nativeprairie<-nativeprairie.raw

setwd(dir)


#####

#Yield management: convert units, match to years in data####

maize.yield.years<-as.numeric(colnames(maize.mgmt[4:18]))
maize.yield.buac<-as.numeric(maize.mgmt[3,4:18]); maize.yield<-maize.yield.buac*0.028 #Conversion: bu corn/ac -> .025t corn/bu corn * 2.47ac/ha * 0.45tC/t corn -> tC/ha
maize.yield<-data.frame(cbind(maize.yield.years, maize.yield)); colnames(maize.yield)<-c("year", "yield")

maize.c.yield.years<-as.numeric(colnames(maize.c.mgmt[4:9]))
maize.c.yield.buac<-as.numeric(maize.c.mgmt[3,4:9]); maize.c.yield<-maize.c.yield.buac*0.028 #Conversion: bu corn/ac -> .025t corn/bu corn * 2.47ac/ha * 0.45tC/t corn -> tC/ha
maize.c.yield<-data.frame(cbind(maize.c.yield.years, maize.c.yield)); colnames(maize.c.yield)<-c("year", "yield")


misc.yield.years<-as.numeric(colnames(miscanthus.mgmt[4:18]))
misc.yield.ustac<-as.numeric(miscanthus.mgmt[4,4:18]); misc.yield<-misc.yield.ustac*1.08 #conversion: us tons mxg / ac -> .91 metric tons / us ton * 2.47ac/ha * .48tC / t miscanthus -> tC/ha
misc.yield<-data.frame(cbind(misc.yield.years, misc.yield)); colnames(misc.yield)<-c("year", "yield")

misc.c.yield.years<-as.numeric(colnames(miscanthus.c.mgmt[4:9]))
misc.c.yield.ustac<-as.numeric(miscanthus.c.mgmt[4,4:9]); misc.c.yield<-misc.c.yield.ustac*1.08 #conversion: us tons mxg / ac -> .91 metric tons / us ton * 2.47ac/ha * .48tC / t miscanthus -> tC/ha
misc.c.yield<-data.frame(cbind(misc.c.yield.years, misc.c.yield)); colnames(misc.c.yield)<-c("year", "yield")

switch.yield.years<-as.numeric(colnames(switchgrass.mgmt[4:12]))
switch.yield.ustac<-as.numeric(switchgrass.mgmt[4,4:12]); switch.yield<-switch.yield.ustac*1.08 #conversion: us tons mxg / ac -> .91 metric tons / us ton * 2.47ac/ha * .48tC / t miscanthus -> tC/ha
switch.yield<-data.frame(cbind(switch.yield.years, switch.yield));colnames(switch.yield)<-c("year", "yield")

np.yield.years<-c(2008:2016)
np.yield.ustac<-c(0, 1.02, 2.73,1.51, 1.26, 2.49, 2.4, 2.19, 0); np.yield<-np.yield.ustac*1.08 
np.yield<-data.frame(cbind(np.yield.years, np.yield));colnames(np.yield)<-c("year", "yield")

sorg.yield.years<-as.numeric(colnames(sorghum.mgmt[4:8]))
sorg.yield.ustac<-as.numeric(sorghum.mgmt[4,4:8]); sorg.yield.ustac[c(2, 5)]<-(-9999) #placeholder for soy years
sorg.yield<-sorg.yield.ustac*1.08 #conversion: us tons mxg / ac -> .91 metric tons / us ton * 2.47ac/ha * .48tC / t sorghum -> tC/ha
#sub in correct values for soy years
sorg.yield[c(2,5)]<- as.numeric(sorghum.mgmt[3,c(5,8)]) #bu/ac soy
sorg.yield[c(2,5)]<-sorg.yield[c(2,5)]*0.028 #bushel conversion for soy
#combine, as with others
sorg.yield<-data.frame(cbind(sorg.yield.years, sorg.yield));colnames(sorg.yield)<-c("year", "yield")

#####


#Subset data and yields to full years (if desired)####

##Timestamp unpacker
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

##Function to find full years of data

clipyears<-function(dat){
  
  yearvec<-as.numeric(format(dat$xlDateTime, "%Y"))
  allyears<-unique(yearvec)
  
  #get counts of measurements per year; get a list of which years have a whole year of data
  yeartab<-as.data.frame(table(yearvec)); colnames(yeartab)<-c("year", "numpts")
  fullyears<-as.numeric(yeartab$year[yeartab$numpts>=17500])
  years<-allyears[fullyears]
  
  return(years)
  
}
  
##Apply subsetting (if desired)

if (fullyears==TRUE){
  
  #data subsetting function
  clipdat<-function(dat){
    yearvec<-as.numeric(format(dat$xlDateTime, "%Y"))
    dat2<-dat[yearvec%in%clipyears(dat),]
    return(dat2)
  }
  
  #apply data subsetting function
  maize.merge<-clipdat(maize.merge)
  maize.c.merge<-clipdat(maize.c.merge)
  misc.merge<-clipdat(misc.merge)
  misc.c.merge<-clipdat(misc.c.merge)
  sorg.merge<-clipdat(sorg.merge)
  switchgrass<-clipdat(switchgrass)
  nativeprairie<-clipdat(nativeprairie)
  
  #yield subsetting function
  clipyield<-function(yield, dat){
    yield2<-yield[yield$year%in%clipyears(dat),]
    return(yield2)
  }
  
  #apply yield subsetting function
  maize.yield<-clipyield(maize.yield, maize.merge)
  maize.c.yield<-clipyield(maize.c.yield, maize.c.merge)
  misc.yield<-clipyield(misc.yield, misc.merge)
  misc.c.yield<-clipyield(misc.c.yield, misc.c.merge)
  sorg.yield<-clipyield(sorg.yield,sorg.merge)
  switch.yield<-clipyield(switch.yield,switchgrass)
  np.yield<-clipyield(np.yield, nativeprairie)
  
  
}


#####


#Prepare harvest-adjusted carbon fluxes####

par(mfrow=c(1,1))

#Maize-basalt
if(harvest.version=="endofyear"){
years<-unique(as.numeric(format(maize.merge$xlDateTime, "%Y")))
Y<-(as.numeric(format(maize.merge$xlDateTime, "%Y")))
#endofyear is the location of last record of each year
#match finds the first record of each year so by subtracting one you get the last record of the previous year
#and by appending the length ( == final index) you get the last value for the last year
endofyear<-c(match(years,Y )-1, length(Y))#[2:15]
}

#New version: harvest comes out on harvest date
if(harvest.version=="harvestdate"){
harvest.ts <- as.POSIXct(as.numeric(maize.mgmt[6,4:17]) * (60*60*24), origin="1899-12-30", tz="GMT")
harvest.date.na<-as.numeric(format(harvest.ts, '%j'));harvest.date<-harvest.date.na
harvest.date[is.na(harvest.date.na)]<-round(mean(harvest.date.na[!is.na(harvest.date.na)]),0)
harvest.year<-round(as.numeric(names(maize.mgmt[6,4:17]), 0))

ts.dat<-unpack.time(maize.merge)
ts.harvest<-data.frame(cbind(harvest.year, harvest.date))

endofyear<-rep(NA, nrow(ts.harvest))
for(i in 1:nrow(ts.harvest)){
  endofyear[i]<-max(which(ts.dat$YEAR==ts.harvest$harvest.year[i]& ts.dat$DOY==ts.harvest$harvest.date[i]))
}
}



maize.gpd<-maize.merge$Fc*0.0792 #gc02/pd. Add this directly for cum.
maize.gpd.tha<-maize.gpd*.0027 #tC/ha/30min


maize.hvst<-maize.gpd.tha
maize.hvst.30<-maize.hvst
maize.hvst.30[endofyear]<-maize.hvst.30[endofyear]+maize.yield$yield# pretend there's a giant flux of C from harvest on the 31st of each year
maize.hvst.sum<-cumsum(maize.hvst.30)
plot(maize.hvst.sum~maize.merge$xlDateTime);points(maize.hvst.sum[endofyear]~maize.merge$xlDateTime[endofyear], col='red')


#Maize-control

#Set up Timeseries
if(harvest.version=="endofyear"){
years<-unique(as.numeric(format(maize.c.merge$xlDateTime, "%Y")))
Y<-(as.numeric(format(maize.c.merge$xlDateTime, "%Y")))
endofyear<-c(match(years,Y )-1, length(Y))#[2:6]
}

if(harvest.version=="harvestdate"){
  harvest.ts <- as.POSIXct(as.numeric(maize.c.mgmt[6,4:ncol(maize.c.mgmt)]) * (60*60*24), origin="1899-12-30", tz="GMT")
  harvest.date.na<-as.numeric(format(harvest.ts, '%j'));harvest.date<-harvest.date.na
  harvest.date[is.na(harvest.date.na)]<-round(mean(harvest.date.na[!is.na(harvest.date.na)]),0)
  harvest.year<-round(as.numeric(names(maize.c.mgmt[6,4:ncol(maize.c.mgmt)]), 0))
  
  ts.dat<-unpack.time(maize.c.merge)
  ts.harvest<-data.frame(cbind(harvest.year, harvest.date))
  
  endofyear<-rep(NA, nrow(ts.harvest))
  for(i in 1:nrow(ts.harvest)){
    endofyear[i]<-max(which(ts.dat$YEAR==ts.harvest$harvest.year[i]& ts.dat$DOY==ts.harvest$harvest.date[i]))
  }
}

#Unit conversions for carbon
maize.c.gpd<-maize.c.merge$Fc*0.0792 #gc02/pd. Add this directly for cum.
maize.c.gpd.tha<-maize.c.gpd*.0027 #tC/ha/30min

#Deduct harvest from Carbon Flux
maize.c.hvst<-maize.c.gpd.tha
maize.c.hvst.30<-maize.c.hvst
maize.c.hvst.30[endofyear]<-maize.c.hvst.30[endofyear]+maize.c.yield$yield# pretend there's a giant flux of C from harvest on the 31st of each year
maize.c.hvst.sum<-cumsum(maize.c.hvst.30)
plot(maize.c.hvst.sum~maize.c.merge$xlDateTime);points(maize.c.hvst.sum[endofyear]~maize.c.merge$xlDateTime[endofyear], col='red')



#Miscanthus-basalt

#Set up timeseries

if(harvest.version=="endofyear"){
years<-unique(as.numeric(format(misc.merge$xlDateTime, "%Y")))
Y<-(as.numeric(format(misc.merge$xlDateTime, "%Y")))
endofyear<-c(match(years,Y )-1, length(Y))#[2:15]
}

if(harvest.version=="harvestdate"){
  harvest.ts <- as.POSIXct(as.numeric(miscanthus.mgmt[6,4:ncol(miscanthus.mgmt)]) * (60*60*24), origin="1899-12-30", tz="GMT")
  harvest.date.na<-as.numeric(format(harvest.ts, '%j'));harvest.date<-harvest.date.na
  harvest.date[is.na(harvest.date.na)]<-round(mean(harvest.date.na[!is.na(harvest.date.na)]),0)
  harvest.year<-round(as.numeric(names(miscanthus.mgmt[6,4:ncol(miscanthus.mgmt)]), 0))+1

  ts.dat<-unpack.time(misc.merge)
  ts.harvest<-data.frame(cbind(harvest.year, harvest.date))

  endofyear<-rep(NA, nrow(ts.harvest))
  for(i in 1:nrow(ts.harvest)){
    endofyear[i]<-max(which(ts.dat$YEAR==ts.harvest$harvest.year[i]& ts.dat$DOY==ts.harvest$harvest.date[i]))
  }
}

#Unit conversions for carbon
misc.gpd<-misc.merge$Fc*0.0792 #gc02/pd. Add this directly for cum.
misc.gpd.tha<-misc.gpd*.0027 #tC/ha/30min

#Deduct harvest from cumulative carbon flux
misc.hvst<-misc.gpd.tha
misc.hvst.30<-misc.hvst
misc.yield[1]<-0.0001
misc.hvst.30[endofyear]<-misc.hvst.30[endofyear]+misc.yield$yield# pretend there's a giant flux of C from harvest on the 31st of each year
misc.hvst.sum<-cumsum(misc.hvst.30)
plot(misc.hvst.sum~misc.merge$xlDateTime);points(misc.hvst.sum[endofyear]~misc.merge$xlDateTime[endofyear], col='red')


#Miscanthus-control

#Set up timeseries
if(harvest.version=="endofyear"){
years<-unique(as.numeric(format(misc.c.merge$xlDateTime, "%Y")))
Y<-(as.numeric(format(misc.c.merge$xlDateTime, "%Y")))
endofyear<-c(match(years,Y )-1, length(Y))#[2:6]
}

if(harvest.version=="harvestdate"){
  harvest.ts <- as.POSIXct(as.numeric(miscanthus.c.mgmt[6,4:ncol(miscanthus.c.mgmt)]) * (60*60*24), origin="1899-12-30")
  harvest.date.na<-as.numeric(format(harvest.ts, '%j'));harvest.date<-harvest.date.na
  harvest.date[is.na(harvest.date.na)]<-round(mean(harvest.date.na[!is.na(harvest.date.na)]),0)
  harvest.year<-round(as.numeric(names(miscanthus.c.mgmt[6,4:ncol(miscanthus.c.mgmt)]), 0))+1
  
  ts.dat<-unpack.time(misc.c.merge)
  ts.harvest<-data.frame(cbind(harvest.year, harvest.date))
  
  endofyear<-rep(NA, nrow(ts.harvest))
  for(i in 1:nrow(ts.harvest)){
    endofyear[i]<-max(which(ts.dat$YEAR==ts.harvest$harvest.year[i]& ts.dat$DOY==ts.harvest$harvest.date[i]))
  }
}

#Unit conversions for carbon
misc.c.gpd<-misc.c.merge$Fc*0.0792 #gc02/pd. Add this directly for cum.
misc.c.gpd.tha<-misc.c.gpd*.0027 #tC/ha/30min

#Deduct harvest from cumulative carbon flux
misc.c.hvst<-misc.c.gpd.tha
misc.c.hvst.30<-misc.c.hvst
#misc.c.yield[1]<-0.0001
misc.c.hvst.30[endofyear]<-misc.c.hvst.30[endofyear]+misc.c.yield$yield
misc.c.hvst.sum<-cumsum(misc.c.hvst.30)
plot(misc.c.hvst.sum~misc.c.merge$xlDateTime);points(misc.c.hvst.sum[endofyear]~misc.c.merge$xlDateTime[endofyear], col='red')


#Switchgrass

#Set up timeseries

if(harvest.version=="endofyear"){
years<-unique(as.numeric(format(switchgrass$xlDateTime, "%Y")))
Y<-(as.numeric(format(switchgrass$xlDateTime, "%Y")))
endofyear<-c(match(years,Y )-1, length(Y))#[2:10]
}


if(harvest.version=="harvestdate"){
  harvest.ts <- as.POSIXct(as.numeric(switchgrass.mgmt[6,4:11]) * (60*60*24), origin="1899-12-30")
  harvest.date.na<-as.numeric(format(harvest.ts, '%j'));harvest.date<-harvest.date.na
  harvest.date[is.na(harvest.date.na)]<-round(mean(harvest.date.na[!is.na(harvest.date.na)]),0)
  harvest.year<-round(as.numeric(names(switchgrass.mgmt[6,4:11]), 0))
  
  ts.dat<-unpack.time(switchgrass)
  ts.harvest<-data.frame(cbind(harvest.year, harvest.date))
  
  endofyear<-rep(NA, nrow(ts.harvest))
  for(i in 1:nrow(ts.harvest)){
    endofyear[i]<-max(which(ts.dat$YEAR==ts.harvest$harvest.year[i]& ts.dat$DOY==ts.harvest$harvest.date[i]))
  }
}

#Unit conversions for carbon
switch.gpd<-c(switchgrass$Fc)*0.0792 #gc02/pd. Add this directly for cum.
switch.gpd.tha<-switch.gpd*.0027 #tC/ha/30min

switch.hvst<-switch.gpd.tha
switch.hvst.30<-switch.hvst
#switch.yield[1]<-0.0001
switch.hvst.30[endofyear]<-switch.hvst.30[endofyear]+switch.yield$yield
switch.hvst.sum<-cumsum(switch.hvst.30)
plot(switch.hvst.sum~c(switchgrass$xlDateTime));points(switch.hvst.sum[endofyear]~switchgrass$xlDateTime[endofyear], col='red')




#Native Priairie

#Set up timeseries
if(harvest.version=="endofyear"){
years<-unique(as.numeric(format(nativeprairie$xlDateTime, "%Y")))
Y<-(as.numeric(format(nativeprairie$xlDateTime, "%Y")))
endofyear<-c(match(years,Y )-1, length(Y))#[2:10]
}

if(harvest.version=="harvestdate"){
  
  harvest.year<-c(2008,2010,2010,2011, 2012, 2013, 2014,2015)
  harvest.date.na<-c(NA, 74,323,315, NA,330,318, NA); harvest.date<-harvest.date.na
  harvest.date[is.na(harvest.date.na)]<-round(mean(harvest.date.na[!is.na(harvest.date.na)&harvest.date.na>300]),0)
  
  ts.dat<-unpack.time(nativeprairie)
  ts.harvest<-data.frame(cbind(harvest.year, harvest.date))
  endofyear<-rep(NA, nrow(ts.harvest))
  for(i in 1:nrow(ts.harvest)){
    endofyear[i]<-max(which(ts.dat$YEAR==ts.harvest$harvest.year[i]& ts.dat$DOY==ts.harvest$harvest.date[i]))
  }
}

#Unit conversions for carbon
np.gpd<-c(nativeprairie$Fc)*0.0792 #gc02/pd. Add this directly for cum.
np.gpd.tha<-np.gpd*.0027 #tC/ha/30min

np.hvst<-np.gpd.tha
np.hvst.30<-np.hvst
#np.yield[1]<-0.0001
np.hvst.30[endofyear]<-np.hvst.30[endofyear]+np.yield$yield
np.hvst.sum<-cumsum(np.hvst.30)
plot(np.hvst.sum~c(nativeprairie$xlDateTime));points(np.hvst.sum[endofyear]~nativeprairie$xlDateTime[endofyear], col='red')


#Sorghum

#Set up timeseries
if(harvest.version=="endofyear"){
years<-unique(as.numeric(format(sorg.merge$xlDateTime, "%Y")))
Y<-(as.numeric(format(sorg.merge$xlDateTime, "%Y")))
endofyear<-c(match(years,Y )-1, length(Y))#[2:5]
}

if(harvest.version=="harvestdate"){
  harvest.ts <- as.POSIXct(as.numeric(sorghum.mgmt[6,4:ncol(sorghum.mgmt)]) * (60*60*24), origin="1899-12-30", tz="GMT")
  harvest.date.na<-as.numeric(format(harvest.ts, '%j'));harvest.date<-harvest.date.na
  harvest.date[is.na(harvest.date.na)]<-round(mean(harvest.date.na[!is.na(harvest.date.na)]),0)
  harvest.year<-round(as.numeric(names(sorghum.mgmt[6,4:ncol(sorghum.mgmt)]), 0))
  
  ts.dat<-unpack.time(sorg.merge)
  ts.harvest<-data.frame(cbind(harvest.year, harvest.date))
  
  endofyear<-rep(NA, nrow(ts.harvest))
  for(i in 1:nrow(ts.harvest)){
    endofyear[i]<-max(which(ts.dat$YEAR==ts.harvest$harvest.year[i]& ts.dat$DOY==ts.harvest$harvest.date[i]))
  }
}


#Unit conversions for carbon
sorg.gpd<-sorg.merge$Fc*0.0792 #gc02/pd. Add this directly for cum.
sorg.gpd.tha<-sorg.gpd*.0027 #tC/ha/30min

#Deduct harvest from carbon flux
sorg.hvst<-sorg.gpd.tha
sorg.hvst.30<-sorg.hvst
sorg.hvst.30[endofyear]<-sorg.hvst.30[endofyear]+sorg.yield$yield# pretend there's a giant flux of C from harvest on the 31st of each year
sorg.hvst.sum<-cumsum(sorg.hvst.30)
plot(sorg.hvst.sum~sorg.merge$xlDateTime);points(sorg.hvst.sum[endofyear]~sorg.merge$xlDateTime[endofyear], col='red')




#Mega plot


samp<-seq(from=1, to=254948, by=10)
par(mfrow=c(1,1))

#Set up alternating colors for maize-soy and sorghum-soy rotations
Y<-(as.numeric(format(maize.merge$xlDateTime, "%Y")))
col.zm=rep('orange', length(Y));col.zm[which(Y%in%c(2010, 2013, 2016, 2019, 2022))]<-"light green"; col.zm[length(col.zm)]<-"orange" 
Y<-(as.numeric(format(sorg.merge$xlDateTime, "%Y")))
col.sb<-rep('forest green', length(Y));col.sb[which(Y%in%c(2010, 2013, 2016, 2019, 2022))]<-"light green"; col.sb[length(col.sb)]<-"forest green"
Y<-(as.numeric(format(maize.c.merge$xlDateTime, "%Y")))
col.zmc=rep('gold', length(Y));col.zmc[which(Y%in%c(2010, 2013, 2016, 2019, 2022))]<-"light green"; col.zmc[length(col.zmc)]<-"gold"

par(bty='n')
#Plotting
plot(maize.hvst.sum[samp]~maize.merge$xlDateTime[samp], ylim=c(-45,50), col=col.zm[samp], pch='.', ylab="Cumulative NEE, tC ha-1", xlab='', cex=2); 
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




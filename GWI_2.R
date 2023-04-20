#GWIs

#Steps:
#1) get albedo daily
#2) calcualte RF
#3) plug into Abraha formula
#4) get GWI for carbon

#Prelminaries: functions and libraries, data read####

if (!exists("sorg.hvst.sum")){source("LongRecord_2.R")}

library(zoo)
#Cleanup; prevents objects from previous runs from showing up erroneously in the plots
#rm(list=setdiff(ls(), c("sorg.hvst.sum","cumulatives", "maize.merge", "maize.c.merge", "misc.merge", "misc.c.merge", "sorg.merge", "switchgrass", "sorg.yield","misc.yield","maize.yield","switch.yield","maize.c.yield", "misc.c.yield")))

#indices for years

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

#####


#1) daily albedo ####
albify<-function(dat, window=1, adj=FALSE){
  dat<-subtime(dat)
  dat.alb<-dat$Fsu/dat$Fsd; dat.alb[dat.alb<0|dat.alb>1|dat$Fsd<10]<-NA
  
  adj.ind<-which(as.numeric(format(sorg.merge$xlDateTime, "%m"))%in%c(3,4,5,6,11,12))
  dat.alb.adj<-dat.alb; dat.alb.adj[adj.ind]<-dat.alb[adj.ind]+0.08
  
  dat.doy<-as.numeric(format(dat$xlDateTime, "%j"))
  dat.d.alb<-aggregate(dat.alb, by=list(dat.doy), FUN='mean', na.rm=TRUE)
  
  if(adj==TRUE){
    dat.d.alb<-aggregate(dat.alb.adj, by=list(dat.doy), FUN='mean', na.rm=TRUE)
  }
  
  dat.alb.sm<-rollapply(dat.d.alb$x, width=window, fill=NA, na.rm=TRUE, FUN='mean', partial=TRUE)
  
  return(dat.alb.sm)
}

sorg.alb<-albify(sorg.merge, adj=TRUE) #Adjusts sorghum albedo by 8% (derived with commented lines below) during the bare soil season
# jd<-as.numeric(format(sorg.merge$xlDateTime, "%j"))
# month<-as.numeric(format(sorg.merge$xlDateTime, "%m"))
# ind<-sort(unique(jd[month%in%c(3:5, 11:12)]))
# sorg.alb<-albify(sorg.merge);maize.alb<-albify(maize.merge)
# mean(maize.alb[ind]-sorg.alb[ind])


maize.alb<-albify(maize.merge);maize.c.alb<-albify(maize.c.merge)
misc.alb<-albify(misc.merge);misc.c.alb<-albify(misc.c.merge)
switch.alb<-albify(switchgrass)
np.alb<-albify(nativeprairie)
#####


#2) radiative forcing####

#get TOA SW
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

#Get swin, transmittance for RF calculation
rfparam<-function(dat){

  dat.swin<-aggregate(dat$Fsd, by=list(format(dat$xlDateTime, "%j")), FUN='mean')$x
  dat.trans<-dat.swin/toa
  rfscl<-dat.swin*dat.trans
  
  rfdat<-data.frame(cbind(dat.swin,dat.trans, rfscl)); colnames(rfdat)<-c("swin", "trans", "scalar")
  
  return(rfscl) #change to rfdat if you want the pieces individually

}

#RF calculation:

#maize to sorghum
sbrf<-rfparam(sorg.merge)*(maize.alb-sorg.alb)

#maize to miscanthus
mgrf<-rfparam(misc.merge)*(maize.alb-misc.alb)

#maize to switchgrass
swrf<-rfparam(switchgrass)*(maize.alb-switch.alb)

#maize to prairie
nprf<-rfparam(nativeprairie)*(maize.alb-np.alb)

#####



#3) GWI formula from Abraha 2021

calcgwi<-function(dat, yield, rf, nodatyr=3){

  
##Albedo GWI
  
#Components of albedo gwi equation
  
RFtoa<-mean(rf) #annual mean radiative forcing

#Constants####
yco2<-45 #airborne co2, estimating 45% here, from (can probably do better)

A<-1 #perturbed area, m2
Ae<-5.1E14 #earth's surface area, m2

Mco2<-44.01 #molec. wt. carbon, g mol-1
mair<-5.148E15 #mass of atmosphere, Mg
mair.kg<-5.148E18 #mass of atmosphere, kg
co2.ref<-389 #reference co2 concentration, ppm

df2x<-3.7 #radiative forcing for doubling co2, W m-2
Mair<-28.95 #molec. wt. air, g mol-1 

TH<-100 #time horizon, yr
#####

dat.gwi.alb<-(RFtoa/yco2)*(A/Ae)*((log(2)*Mco2*mair*co2.ref)/(df2x*Mair))*(1/TH) #in MgCo2/ha

#albedo EESF equation

aco2<-5.35*log((co2.ref+1)/co2.ref) #radiative efficiency co2, from Bright 2020, Wm-2ppm-1

kco2<-(aco2*Mair*10^6)/(Mco2*mair.kg) #global mean radiative efficiency, W m-2 kg-1

#dat.eesf.alb<- RFtoa/(kco2*Ae*0.5) #kg CO2-eq m-2, native units from Bright 2020
dat.eesf.alb<- (RFtoa/(kco2*Ae*0.5))*10 #Mg CO2-eq ha-1, for comparison with gwi


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
dat.gwi.co2<-(mean(yearseq)*3.66)-8.4 #in MgCo2/ha == tCO2/ha; to represent relative to maize
#dat.gwi.co2<-(mean(yearseq)*0.37)-0.84 #in kgCo2/m2; to represent relative to maize

#return(data.frame(rbind(dat.eesf.alb, dat.gwi.co2)))# for individual plots?
#return(data.frame(rbind(dat.gwi.alb, dat.gwi.co2)))#for combined plot
return(data.frame(rbind(dat.eesf.alb, dat.gwi.alb, dat.gwi.co2))) #both
}







switch.gwi<-calcgwi(switchgrass, switch.yield, swrf, nodatyr=0)
misc.gwi<-calcgwi(misc.merge, misc.yield, mgrf, nodatyr=1)#originally 3...
sorg.gwi<-calcgwi(sorg.merge, sorg.yield, sbrf, nodatyr=1)#originally 2
maize.gwi<-calcgwi(maize.merge, maize.yield, rep(0, 366), nodatyr=1) #originally 2
np.gwi<-calcgwi(nativeprairie, np.yield, nprf, nodatyr=0)

names<-rep(c("switchgrass", "miscanthus", "sorghum", "maize", "prairie"), each=3)
numbers<-rbind(switch.gwi, misc.gwi, sorg.gwi, maize.gwi, np.gwi)
types<-rep(c("albedo.eesf","albedo.gwi", "carbon"), 5)
dat.gwi<-cbind(names, numbers, types); colnames(dat.gwi)[2]<-"dat"

library(ggplot2)


#Plotting for GWI
ggplot(dat.gwi[dat.gwi$types%in%c("albedo.gwi", "carbon"),]) +
  aes(x = names, fill = types, weight = dat) +
  geom_bar(color='black') +
  scale_fill_manual(values = c(carbon = "#BABEC0",albedo.gwi = "#8B96C2"))+
  labs(x = "Feedstock",
      y = "Mg CO2-eq ha-1 y-1",
    fill = "forcing") +
  theme_minimal()


pal <- c("maize" = "orange", "miscanthus" = "blue", "switchgrass" = "light pink", "sorghum" = "forestgreen", "prairie"="plum3")


#plotting for eesf

ggplot(dat.gwi[dat.gwi$types=="albedo.eesf",]) +
  aes(x = names, weight = dat, fill=names) +
  geom_bar(colour="black",show.legend = FALSE)+
  scale_fill_manual(values=pal)+  
  labs(
    x = "Feedstock",
    y = "Mg Co2-eq ha-1",
    title = "albedo-equivalent co2 (single pulse)"
  ) +
  theme_minimal()+
  ylim(c(-90, 20))


ggplot(dat.gwi[dat.gwi$types=="carbon",]) +
  aes(x = names, weight = dat, fill=names) +
  geom_bar(colour="black",show.legend = FALSE)+
  scale_fill_manual(values=pal)+
  labs(
    x = "Feedstock",
    y = "Mg Co2-eq ha-1",
    title = "actual co2 (annual)"
  ) +
  theme_minimal()+
  ylim(c(-90, 20))


ratios<-dat.gwi$dat[dat.gwi$types=="albedo.gwi"]/dat.gwi$dat[dat.gwi$types=="carbon"]

ratios.sum<-dat.gwi$dat[dat.gwi$types=="albedo"]/
 aggregate(dat.gwi$dat, by=list(dat.gwi$names), FUN='sum')$x[c(5,2,4,1,3)]

#Second function to pull carbon fluxes by year (for variability)

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
  
  yearseq<-as.numeric(diff(dat.hvst.sum[endofyear])) #net carbon storage after harvest removal each year
  year<-as.numeric(format(dat$xlDateTime[endofyear], "%Y")[1:(length(endofyear)-1)])
  lab<-rep(id, length(year))
  
  return(cbind.data.frame(year, yearseq, lab))

}

switch.cflux<-getcflux(switchgrass, switch.yield, nodatyr=0, id="sw")
misc.cflux<-getcflux(misc.merge, misc.yield, nodatyr=3, id="mgb")
sorg.cflux<-getcflux(sorg.merge, sorg.yield, nodatyr=2, id="sb")
maize.cflux<-getcflux(maize.merge, maize.yield, nodatyr=2, id="zm")

fluxes<-rbind(switch.cflux,misc.cflux, sorg.cflux, maize.cflux)

ggplot(subset(fluxes, year%in%c(2008:2016))) +
  aes(x = lab, y = yearseq) +
  geom_boxplot(shape = "circle", fill = "#112446") +
  theme_minimal()+
  labs(x = "Feedstock",
       y = "Mg C ha-1 y-1")


fluxsd<-aggregate(fluxes$yearseq, by=list(fluxes$lab), FUN="sd")

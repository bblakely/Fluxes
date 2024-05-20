#reading in L6 files:
#Long term fluxes

library(readxl)
library(ggplot2)

###Read in data####

dir<-getwd()
setwd("Fluxdata")

#Read in Maize Basalt
maize.raw<-read_excel("Maize_2008_to_2019_L6.xlsx", sheet=2,skip=2) #Maize basalt all the way through; no basalt before 2017
maize.raw.2020<-read_excel("MaizeBasalt_2020_2022_L6.xls", sheet=2, skip=2)
maize.raw[maize.raw==-9999]<-NA; maize.raw.2020[maize.raw.2020==-9999]<-NA
maize<-maize.raw;maize.2020<-maize.raw.2020
#Merge 2020-2022 with the rest; rename co2 flux for PFP renaming
maize.merge.mod<-maize.2020; colnames(maize.merge.mod)[colnames(maize.merge.mod)=="Fco2"]<-"Fc"
maize.merge<-merge(maize, maize.merge.mod, all=TRUE)
rm(maize.merge.mod)


#Read in Maize Control
maize.c.raw<-read_excel("MaizeNoBasalt_2017_to_2019_L6.xlsx", sheet=2,skip=2) 
maize.c.raw.2020<-read_excel("MaizeNoBasalt_2020_to_2021_L6.xls", sheet=2, skip=2)
maize.c.raw[maize.c.raw==-9999]<-NA; maize.c.raw.2020[maize.c.raw.2020==-9999]<-NA
maize.c<-maize.c.raw;maize.c.2020<-maize.c.raw.2020
#Merge 2020-2022 with the rest; rename co2 flux for PFP renaming
maize.c.merge.mod<-maize.c.2020; colnames(maize.c.merge.mod)[colnames(maize.c.merge.mod)=="Fco2"]<-"Fc"
maize.c.merge<-merge(maize.c, maize.c.merge.mod, all=TRUE)
rm(maize.c.merge.mod)


#Read in Miscanthus Basalt
miscanthus.raw<-read_excel("Miscanthus_2008_to_2019_L6.xlsx", sheet=2,skip=2) #Basalt
miscanthus.raw[miscanthus.raw==-9999]<-NA
miscanthus.raw.2020<-read_excel("MiscanthusBasalt_2020_2022_L6.xls", sheet=2, skip=2)#read_excel("MiscanthusNoBasalt_2020_L6.xls", sheet=2, skip=2) #2020 control (fix)
miscanthus.raw[miscanthus.raw==-9999]<-NA; miscanthus.raw.2020[miscanthus.raw.2020==-9999]<-NA
miscanthus<-miscanthus.raw; miscanthus.2020<-miscanthus.raw.2020
#Merge 2020-2022 with the rest; rename co2 flux for PFP renaming
misc.merge.mod<-miscanthus.2020; colnames(misc.merge.mod)[colnames(misc.merge.mod)=="Fco2"]<-"Fc"
misc.merge<-merge(miscanthus, misc.merge.mod, all=TRUE)
rm(misc.merge.mod)


#Read in Miscanthus Control
miscanthus.c.raw<-read_excel("MiscanthusNoBasalt_2017_to_2019_L6.xlsx", sheet=2,skip=2) 
miscanthus.c.raw.2020<-read_excel("MiscanthusNoBasalt_2020_2022_L6.xls", sheet=2, skip=2)#miscanthus.raw.2020# temp solution, uses MGB 2020-2021 read_excel("MiscanthusNoBasalt_2020_to_2021_L6.xls", sheet=2, skip=2)
miscanthus.c.raw[miscanthus.c.raw==-9999]<-NA; miscanthus.c.raw.2020[miscanthus.c.raw.2020==-9999]<-NA
miscanthus.c<-miscanthus.c.raw;miscanthus.c.2020<-miscanthus.c.raw.2020
#Merge 2020-2022 with the rest; rename co2 flux for PFP renaming
misc.c.merge.mod<-miscanthus.c.2020; colnames(misc.c.merge.mod)[colnames(misc.c.merge.mod)=="Fco2"]<-"Fc"
misc.c.merge<-merge(miscanthus.c, misc.c.merge.mod, all=TRUE)
rm(misc.c.merge.mod)

#Read in sorghum

sorghum.raw<-read_excel("sorghum_2018_to_2019_L6.xlsx", sheet=2,skip=2)
sorghum.raw.2020<-read_excel("Sorghum_2020_2022_L6.xls", sheet=2, skip=2)
sorghum.raw[sorghum.raw==-9999]<-NA; sorghum.raw.2020[sorghum.raw.2020==-9999]<-NA
sorghum<-sorghum.raw; sorghum.2020<-sorghum.raw.2020
#Merge 2020-2022 with the rest; rename co2 flux for PFP renaming
sorg.merge.mod<-sorghum.2020; colnames(sorg.merge.mod)[colnames(sorg.merge.mod)=="Fco2"]<-"Fc"
sorg.merge<-merge(sorghum, sorg.merge.mod, all=TRUE)
rm(sorg.merge.mod)

#Read in switchgrass
switchgrass.raw<-read_excel("Switchgrass_2008_to_2016_L6.xlsx", sheet=2,skip=2)
switchgrass.raw[switchgrass.raw==-9999]<-NA
switchgrass<-switchgrass.raw

#Read in prairie
nativeprairie.raw<-read_excel("Prairie_2008_to_2016_L6.xlsx", sheet=2,skip=2)
nativeprairie.raw[nativeprairie.raw==-9999]<-NA
nativeprairie<-nativeprairie.raw

setwd(dir)

#cleanup
rm("maize.2020", "maize.c.2020", "maize.raw", "maize.raw.2020","maize.c.raw.2020","maize.c.raw",
   "miscanthus.2020", "miscanthus.c.2020", "miscanthus.raw", "miscanthus.raw.2020","miscanthus.c.raw", "miscanthus.c.raw.2020",
   "nativeprairie.raw", "switchgrass.raw", "sorghum.2020", "sorghum.raw", "sorghum.raw.2020")

#####

###Timestamp unpacker function####
#Breaks down excel timestamp into easily manipulatable numeric columns
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


####Main function####

#Creates dataset of average cumulative carbon fluxes for each half-hourly period throughout the year
#Output includes the mean, standard deviation, mean + 1sd, mean - 1sd, and the number of years available for each half-hourly period (varies due to partial years of fluxes)


cumul.var<-function(dat, label="feedstock"){

ts.all<-unpack.time(dat)  
  
#make counter for sites loop; only applicable to soy, which has 3 sites per (soy) year
if(!is.null(dat$site)){sites<-unique(dat$site)}else{sites<-label; dat$site<-label}

#Empty space to be populated in the for-loop
longdat<-data.frame(matrix(data=NA, ncol=3, nrow=nrow(dat))); names(longdat)<-c("ccum", "doy", "site")

#main loop creating a dataset of consecutive years of C flux, with the cumulative flux starting at 0 at the beginning of each year
for(s in 1:length(sites)){
  
  ts<-unpack.time(dat[dat$site==sites[s],]) #break down excel timestamp into useful vectors like DOY, hour, etc. 
  years<-unique(ts$YEAR) #list of years included in dat for this site
  
  for(i in 1:length(years)){

    yearsub<-which(ts.all$YEAR==years[i] & dat$site==sites[s])

      if(length(yearsub)>=17500){ #avoids years with almost no measurements; often jan 1 of the year following the last year
      cyear<-(cumsum(dat$Fc[yearsub]*0.0792*0.0027)) # 0.792 converts from  <umol_co2 / m2 s> to <g_co2 / m2 30min> ; 0.0027 converts from <g_co2 / m2 30min> to <t_c / ha 30min>
      add<-cbind(cyear, ts.all$DECDOY[yearsub], rep(sites[s], length(cyear))) #fluxes from this year and site, to be added to the full record in longdat
      longdat[yearsub,]<-add 
    }
  }
}

longdat$ccum<-as.numeric(longdat$ccum); longdat$doy<-as.numeric(longdat$doy) #force these to be numericif they aren't already


#get the mean and standard deviation across years of each half-hourly period
mean.val<-aggregate(ccum~doy, data=longdat, FUN='mean', na.rm=TRUE);names(mean.val)[2]<-"ccum"
sd.val<-aggregate(ccum~doy, data=longdat, FUN='sd', na.rm=TRUE); names(sd.val)[2]<-"sd"
count.val<-aggregate(longdat$doy, by=list(longdat$doy), FUN = function(x) sum(!is.na(x))); names(count.val)[2]<-"num.years"

#stick it all together in ggplot-friendly format with descriptive names
gplot<-cbind(mean.val, sd.val$sd, mean.val$ccum+sd.val$sd, mean.val$ccum-sd.val$sd, count.val$num.years); names(gplot)[3:6]<-c("sd", "mean+1sd", "mean-1sd", "num.years.included")
return(gplot)

}

#####



#Function to plot the result

plot.cumul<-function(dat, col, title){
  dat<-dat[!is.na(rowMeans(dat[,1:5])),]
  
  ggplot(dat, aes(x=doy,y=ccum))+
    geom_line(size=2, color=col) + 
    geom_ribbon(aes(ymin=min, ymax=max), alpha=0.2)+
    ylab("Cumulative carbon flux, tC ha-1")+
    xlab("Day of Year")+
    ylim(-12, 4)+
    geom_hline(yintercept=0)+
    ggtitle(title)+
    theme_minimal()+
    theme(axis.text=element_text(size=16), axis.title=element_text(size=18), title=element_text(size=18))
}


#Function to remove soy years;apply it to the dat input of cumul.var to get plots that do not include soy years

soy.yr<-c(2010, 2013, 2016, 2019, 2022)
soysub<-function(dat){
  
  dat.ts<-unpack.time(dat)

  sub<-which(!dat.ts$YEAR%in%soy.yr)

  dat.soysub<-dat[sub,]
  
  return(dat.soysub)
  
}


###Call functions###

sorg<-cumul.var(dat=soysub(sorg.merge), label="sorghum")
plot.cumul(sorg, title="sorghum", col="forest green")
#clean it up for carl
sorg.print<-sorg[sorg$doy<366,]#rename and clip off leap day, num years
colnames(sorg.print)[1:6]<-c("DOY", "mean_cumulative_c_flux_tc_ha-1","standard_deviation", "mean+1sd", "mean-1sd", "num_site_years")
write.csv(sorg.print, "Writeout/efarm_sorghum_nosoy.csv")


maize1<-cumul.var(dat=soysub(maize.merge), label="maize1")
plot.cumul(maize1, title="maize 1", col="orange")

maize2<-cumul.var(dat=soysub(maize.c.merge), label="maize2")
plot.cumul(maize2, title="maize 2", col="yellow")


misc1<-cumul.var(dat=misc.merge,  label="miscanthus1")
plot.cumul(misc1, title="miscanthus 1", col="blue")

misc2<-cumul.var(dat=misc.c.merge, label="miscanthus2")
plot.cumul(misc2, title="miscanthus 2", col="light blue")

switch<-cumul.var(dat=switchgrass,label="switchgrass")
plot.cumul(switch, title="switchgrass", col="light pink")
#clean it up for carl
switch.print<-switch[switch$doy<366,]#rename and clip off leap day, num years
colnames(switch.print)[1:5]<-c("DOY", "mean_cumulative_c_flux_tc_ha-1","standard_deviation", "mean+1sd", "mean-1sd", "num_site_years")
write.csv(switch.print, "Writeout/efarm_switchgrass.csv", row.names=FALSE)


prairie<-cumul.var(dat=nativeprairie, label="prairie")
plot.cumul(prairie, title="prairie", col="plum3")
#clean it up for carl
prairie.print<-prairie[prairie$doy<366,]#rename and clip off leap day, num years
colnames(prairie.print)[1:6]<-c("DOY", "mean_cumulative_c_flux_tc_ha-1","standard_deviation", "mean+1sd", "mean-1sd", "num_site_years")
write.csv(prairie.print, "Writeout/efarm_nativeprairie.csv", row.names=FALSE)

#combined ecosystems
#this merge takes forever
maize1.combo<-maize.merge;maize1.combo$site<-"zmb"
maize2.combo<-maize.c.merge;maize2.combo$site<-"zmc"
maize.combined<-merge(maize1.combo, maize2.combo, all=TRUE)
maize.combo<-cumul.var(dat=soysub(maize.combined), label="maize (sites combined)")
plot.cumul(maize.combo, title="maize combined", col="orange")
#clean it up for carl
maize.print<-maize.combo[maize.combo$doy<366,]#rename and clip off leap day
colnames(maize.print)[1:6]<-c("DOY", "mean_cumulative_c_flux_tc_ha-1","standard_deviation", "mean+1sd", "mean-1sd", "num_site_years")
write.csv(maize.print, "efarm_maize_combined_nosoy.csv")


misc1.combo<-misc.merge;misc1.combo$site<-"mgb"
misc2.combo<-misc.c.merge;misc2.combo$site<-"mgc"
miscanthus.combined<-merge(misc1.combo, misc2.combo, all=TRUE)
misc.combo<-cumul.var(dat=miscanthus.combined, label="miscanthus (sites combined)")
plot.cumul(misc.combo, title="miscanthus combined", col="blue")
#clean it up for carl
misc.print<-misc.combo[misc.combo$doy<366,]#rename and clip off leap day
colnames(misc.print)[1:5]<-c("DOY", "mean_cumulative_c_flux_tc_ha-1","standard_deviation", "mean+1sd", "mean-1sd", "num_site_years")
write.csv(misc.print, "efarm_miscanthus_combined.csv")


#Special case for soy

soy.maize<-maize.merge[unpack.time(maize.merge)$YEAR%in%soy.yr,]; soy.maize$site<-"zmb"
soy.maize.c<-maize.c.merge[unpack.time(maize.c.merge)$YEAR%in%soy.yr,]; soy.maize.c$site<-"zmc"
soy.sorghum<-sorg.merge[unpack.time(sorg.merge)$YEAR%in%soy.yr,]; soy.sorghum$site<-"sb"

#these merges take a while
soy.combined1<-merge(soy.maize, soy.maize.c, all = TRUE)
soy.combined<-merge(soy.combined1, soy.sorghum, all=TRUE)
rm(soy.combined1)

soy<-cumul.var(dat=soy.combined, label="soy (sites combined)")
plot.cumul(soy, title="soy", col="light green")

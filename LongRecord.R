#Long term fluxes

library(readxl)
library(bigleaf)

#Read in Maize
maize.raw<-read_excel("Maize_2008_to_2019_L6.xlsx", sheet=2,skip=2)
maize.raw.2020<-read_excel("MaizeCon_2020_L6.xls", sheet=2, skip=2)
maize.raw[maize.raw==-9999]<-NA; maize.raw.2020[maize.raw.2020==-9999]<-NA
maize<-maize.raw;maize.2020<-maize.raw.2020

maize.mgmt<-read_excel("Illinois Energy Farm Flux Towers Management Record.xlsx", sheet=1)
maize.mgmt<-maize.mgmt[, c(1:15)]

#maize.nep<-umolCO2.to.gC(maize$Fc)
# plot(maize.nep~maize$xlDateTime, type='l', col='goldenrod')
# supersum.zm<-cumsum(maize.nep/48)
# plot(supersum.zm, type='l', col='goldenrod')

#Read in Miscanthus
miscanthus.raw<-read_excel("Miscanthus_2008_to_2019_L6.xlsx", sheet=2,skip=2)
miscanthus.raw[miscanthus.raw==-9999]<-NA
miscanthus.raw.2020<-read_excel("MiscanthusNoBasalt_2020_L6.xls", sheet=2, skip=2)
miscanthus.raw[miscanthus.raw==-9999]<-NA; miscanthus.raw.2020[miscanthus.raw.2020==-9999]<-NA
miscanthus<-miscanthus.raw; miscanthus.2020<-miscanthus.raw.2020

miscanthus.mgmt<-read_excel("Illinois Energy Farm Flux Towers Management Record.xlsx", sheet=2)
miscanthus.mgmt<-miscanthus.mgmt[, c(1:15)]

#misc.nep<-umolCO2.to.gC(miscanthus$Fc)
#plot(misc.nep~miscanthus$xlDateTime, type='l')

#supersum.mxg<-cumsum(misc.nep/48)
#plot(supersum.mxg, type='l', col='light blue')

#Read in sorghum

sorghum.raw<-read_excel("sorghum_2018_to_2019_L6.xlsx", sheet=2,skip=2)
sorghum.raw.2020<-read_excel("Sorghum_2020_L6.xls", sheet=2, skip=2)
sorghum.raw[sorghum.raw==-9999]<-NA; sorghum.raw.2020[sorghum.raw.2020==-9999]<-NA
sorghum<-sorghum.raw; sorghum.2020<-sorghum.raw.2020

sorghum.mgmt<-read_excel("Illinois Energy Farm Flux Towers Management Record.xlsx", sheet=4)
sorghum.mgmt<-sorghum.mgmt[, c(1:5)]

#Read in switchgrass?

#####
#Get yields in correct units
maize.yield.buac<-as.numeric(maize.mgmt[3,4:15]); maize.yield<-maize.yield.buac*0.028 #Conversion: bu corn/ac -> .025t corn/bu corn * 2.47ac/ha * 0.45tC/t corn -> tC/ha
misc.yield.ustac<-as.numeric(miscanthus.mgmt[4,4:15]); misc.yield<-misc.yield.ustac*1.08 #conversion: us tons mxg / ac -> .91 metric tons / us ton * 2.47ac/ha * .48tC / t miscanthus -> tC/ha
sorg.yield.ustac<-as.numeric(sorghum.mgmt[4,4:5]); sorg.yield<-sorg.yield.ustac[1]*1.08 #conversion: us tons mxg / ac -> .91 metric tons / us ton * 2.47ac/ha * .48tC / t sorghum -> tC/ha
sorg.yield[2]<-sorg.yield.ustac[2]*0.028 #bushel conversion for soy

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
plot(rollapply(maize.nep[Y==2018], width=48*7, FUN='mean'), type='l', lwd=4)
#####

#Back it up further: convert yourself

maize.gpd<-c(maize$Fc, maize.2020$Fco2)*0.0792 #gc02/pd. Add this directly for cum.
plot(cumsum(maize.gpd[Y==2008])); lines(cumsum(maize.nep[Y==2008]/48), col='red')
maize.gpd.tha<-maize.gpd*.0027 #tC/ha/30min
plot(-cumsum(maize.gpd.tha[Y==2011]))


maize.hvst<-maize.gpd.tha
maize.hvst.30<-maize.hvst
maize.hvst.30[endofyear]<-maize.hvst.30[endofyear]+maize.yield# pretend there's a giant flux of C from harvest on the 31st of each year
maize.hvst.sum<-cumsum(maize.hvst.30)
plot(maize.hvst.sum~c(maize$xlDateTime, maize.2020$xlDateTime))

#Miscanthus
misc.gpd<-c(miscanthus$Fc, miscanthus.2020$Fco2)*0.0792 #gc02/pd. Add this directly for cum.

plot(cumsum(misc.gpd[Y==2011])); lines(cumsum(misc.nep[Y==2011]/48), col='red')

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



samp<-seq(from=1, to=219907, by=10)

par(mfrow=c(1,1))

Y<-(as.numeric(format(c(maize$xlDateTime, maize.2020$xlDateTime), "%Y")))
col.zm=rep('orange', length(Y));col.zm[which(Y%in%c(2010, 2013, 2016, 2019))]<-"light green"
Y<-(as.numeric(format(c(sorghum$xlDateTime, sorghum.2020$xlDateTime), "%Y")))
col.sb<-rep('forest green', length(Y));col.sb[which(Y%in%c(2010, 2013, 2016, 2019))]<-"light green"

plot(maize.hvst.sum[samp]~c(maize$xlDateTime, maize.2020$xlDateTime)[samp], ylim=c(-35,35), col=col.zm[samp], pch='.', ylab="Cumulative NEE, tC ha-1", xlab='', cex=2); points(misc.hvst.sum[samp]~c(miscanthus$xlDateTime, miscanthus.2020$xlDateTime)[samp], col='light blue', pch='.', cex=2)
points(sorg.hvst.sum[samp]~c(sorghum$xlDateTime,sorghum.2020$xlDateTime)[samp], col=col.sb[samp], pch='.', cex=2)
abline(h=0)
legend(as.numeric(min(maize$xlDateTime)), 35, legend=c("maize", "soybean", "miscanthus", "sorghum"), col=c("orange","light green", "light blue", "forest green"), lwd=2, bty='n', cex=0.8)

#Next steps: cleanup, adjusting yield for soy


#Quick look at whether 2020 was bad?####

years<-unique(as.numeric(format(maize$xlDateTime, "%Y")))
Y<-(as.numeric(format(maize$xlDateTime, "%Y")))
#read in maize
maize.raw.2020<-read_excel("MaizeCon_2020_L6.xls", sheet=2,skip=2)
maize.raw.2020[maize.raw.2020==-9999]<-NA
maize.2020<-maize.raw.2020#[1:(nrow(maize.raw)-1),]

plot(cumsum(maize$Fc[which(Y==2011)])~as.numeric(format(maize$xlDateTime[Y==2011], "%j")), col='white', ylim=c(-30000, 15000))
for (i in c(1:6, 9:length(years))){
  yearsum<-cumsum(maize$Fc[which(Y==years[i])])
  
  lines(yearsum~as.numeric(format(maize$xlDateTime[Y==years[i]], "%j")), col=i)
  
  
}

lines(cumsum(maize.2020$Fco2)~as.numeric(format(maize.2020$xlDateTime, "%j")), col='black', lwd=4, type='l')
lines(cumsum(maize$Fc[Y==2017])~as.numeric(format(maize$xlDateTime[Y==2017], "%j")), col='blue', lwd=4, type='l')
lines(cumsum(maize$Fc[Y==2018])~as.numeric(format(maize$xlDateTime[Y==2018], "%j")), col='red', lwd=4, type='l')

#Nope, corn was just fine being knocked over apparently

#Script to call just to read stufff in
#Original code from "FluxComparison2020")

library(readxl)

maize.raw<-read_excel("MaizeCon_2020_L6.xls", sheet=2,skip=2)
maize.raw[maize.raw==-9999]<-NA
maize<-maize.raw#[1:(nrow(maize.raw)-1),]

sorg.raw<-read_excel("Sorghum_2018_2020_L6.xls", sheet=2,skip=2)
sorg.raw[sorg.raw==-9999]<-NA
sorg<-sorg.raw[as.numeric(format(sorg.raw$xlDateTime, "%Y"))==2020,]

misc.raw<-read_excel("MiscanthusNoBasalt_2020_L6.xls", sheet=2,skip=2)
misc<-misc.raw#[1030:6715,]
misc[misc==-9999]<-NA


#Pull in other years
if(!exists('maize.raw.2018')){maize.raw.2018<-read_excel("MaizeNoBasalt_2017_to_2019_L6.xlsx", sheet=2,skip=2)}
if(!exists('misc.raw.2018')){misc.raw.2018<-read_excel("MiscanthusNoBasalt_2017_to_2019_L6.xlsx", sheet=2,skip=2)}

maize.years<-format(maize.raw.2018$xlDateTime, "%Y")
misc.years<-format(misc.raw.2018$xlDateTime, "%Y")

#Prepare maize data
maize.2018<-maize.raw.2018[maize.years=="2018",]
maize.2018[maize.2018==-9999]<-NA
#maize.2018.months<-maize.2018[1:14640,]

maize.2017<-maize.raw.2018[maize.years=="2017",]
maize.2017[maize.2017==-9999]<-NA
#maize.2017.months<-maize.2017[1:14640,]

maize.2019<-maize.raw.2018[maize.years=="2019",]
maize.2019[maize.2019==-9999]<-NA
#maize.2019.months<-maize.2019[1:14640,]



if(!exists('sorg.raw.2018')){sorg.raw.2018<-read_excel("Sorghum_2018_to_2019_L6.xlsx", sheet=2,skip=2)}

sorg.2018.yearmark<-format(sorg.raw.2018$xlDateTime, '%Y')
sorg.2018<-sorg.raw.2018[which(sorg.2018.yearmark=="2018"),]
sorg.2018[sorg.2018==-9999]<-NA
#sorg.2018.months<-sorg.2018[1:6018,]
#sorg.2018.months$Fc[which(is.na(sorg$Fc))]<-NA

na.pad<-matrix(data=NA, nrow=(17520-nrow(sorg.2018)), ncol=ncol(sorg.2018));colnames(na.pad)<-colnames(sorg.2018)
sorg.2018<-rbind(na.pad, sorg.2018)
#sorg.2018$xlDateTime[which(is.na(sorg.2018$xlDateTime))]<-sorg$xlDateTime[which(is.na(sorg.2018$xlDateTime))]
sorg.2018$xlDateTime<-maize.2018$xlDateTime




library("Evapotranspiration")

data(climatedata)

dat.raw<-maize.merge
ts.raw<-unpack.time(maize.merge)

sub<-which(ts$YEAR%in%c(2011:2017))

dat<-test[sub,]; ts<-ts[sub,]

dat<-cbind(ts, dat)


#Prepare climatic data
Tmax<-aggregate(dat$Ta, by=list(ts$DOY, ts$YEAR), FUN=max)$x
Tmin<-aggregate(dat$Ta, by=list(ts$DOY, ts$YEAR), FUN=min)$x

RHmax<-aggregate(dat$RH, by=list(ts$DOY, ts$YEAR), FUN=max)$x
RHmin<-aggregate(dat$RH, by=list(ts$DOY, ts$YEAR), FUN=min)$x

rs.jm2.unagg<-dat$Fsd*1800/1000000
Rs<-aggregate(rs.jm2.unagg, by=list(ts$DOY, ts$YEAR), FUN=sum)$x

Year<-aggregate(dat$YEAR,by=list(ts$DOY, ts$YEAR), FUN=mean)$x
Month<-aggregate(dat$MONTH,by=list(ts$DOY, ts$YEAR), FUN=mean)$x
Day<-aggregate(dat$DAY,by=list(ts$DOY, ts$YEAR), FUN=mean)$x

dat.input<-list(Year, Month, Day, Tmax, Tmin, RHmax, RHmin, Rs); names(dat.input)<-c("Year", "Month", "Day", "Tmax", "Tmin", "RHmax", "RHmin", "Rs")


#Prepare constants
data(constants)#load defaults

#edit for the energy farm
constants$Elev<-222
constants$lat<-40.11
constants$lat_rad<-0.70


test<-ET.PriestleyTaylor(data=dat.input, constants=constants, solar="data", ts="daily")

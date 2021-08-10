library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting


raw<-nc_open('Sorghum_2020_L3_WEST.nc', write=TRUE)#('Sorghum_2020_L3_EAST.nc', write=TRUE)
print(raw)
carb<-ncvar_get(raw,"Fco2")
head(carb)

if(!exists(subtime)){
library(readxl)
subtime<-read_excel("Sorghum_2020_L6.xls", sheet=2,skip=2)

subtime.t<-subtime$xlDateTime
}

jd<-format(subtime.t, "%j")
H<-as.numeric(format(subtime.t, "%H"))
light<-subtime$Fsd

plot(carb[jd%in%c(150:300)]~H[jd%in%c(150:300)])

points(carb[which(jd%in%c(150:300)&light<100&carb<(-10))]~H[which(jd%in%c(150:300)&light<100&carb<(-10))], col='red')


carb.filt<-carb; carb.filt[which(jd%in%c(150:300)&light<100&carb<(-10))]<-0

plot(carb.filt[jd%in%c(150:300)]~H[jd%in%c(150:300)])


ncvar_put(raw, "Fco2", carb.filt)


newcarb<-ncvar_get(raw, "Fco2")

plot(newcarb[jd%in%c(150:300)]~H[jd%in%c(150:300)])

nc_sync(raw)

nc_close(raw)

new<-nc_open('Sorghum_2020_L3_WEST.nc', write=TRUE)

carbnew<-ncvar_get(new, "Fco2")
plot(carbnew[jd%in%c(150:300)]~H[jd%in%c(150:300)])



names(new$var)

nc_close(new)

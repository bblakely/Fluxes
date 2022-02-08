library(readxl)

cls<-c("character", rep("numeric", 37))
ep.input.raw<-read.csv("Sorghum_2020_Met_Data_for_EddyPro.csv", colClasses = cls, stringsAsFactors = FALSE, skip=2, header=FALSE)

for(i in 2:37){
  ep.input.raw[,i]<-as.numeric(ep.input.raw[,i])
}

ep.input.names<-read.csv("Sorghum_2020_Met_Data_for_EddyPro.csv", as.is=TRUE)[1,]

colnames(ep.input.raw)<-as.character(colnames(ep.input.names))

ep.input<-ep.input.raw; ep.input[ep.input==-9999]<-NA

ep.timestamp<-as.POSIXct(ep.input$TIMESTAMP,format="%Y/%m/%d %H:%M" )

for(i in 2:20){
plot(ep.input[,i]~ep.timestamp, main=colnames(ep.input)[i])
}
  

for(i in 2:18){
  plot(ep.input[,i]~format(ep.timestamp, "%H"), main=colnames(ep.input)[i])
}
#make flag datasets, get custom-filtered fc

#run NEE switching funcion first (?)

get_flagdat<-function(dat, filename1, filename2=NA, clip=TRUE){

  #Read and merge the files
  flag.raw<-read_excel(paste("Fluxdata/",filename1, sep=""), sheet=3,skip=2)
  
  #if there's a second file
  if(!is.na(filename2)){ 
  flag.raw.2<-read_excel(paste("Fluxdata/",filename2, sep=""), sheet=3, skip=2)
  colnames(flag.raw.2)[1]<-"xlDateTime"
  flag.raw[flag.raw==-9999]<-NA; flag.raw.2[flag.raw.2==-9999]<-NA
  flag<-flag.raw; flag.2<-flag.raw.2
  flag.merge.mod<-flag.2; colnames(flag.merge.mod)[colnames(flag.merge.mod)=="Fco2"]<-"Fc"
  flag.merge<-merge(flag, flag.merge.mod, all=TRUE)
  }else{flag.raw[flag.raw==-9999]<-NA; flag.merge<-flag.raw}

  #clip to full years
  
  if(clip==TRUE){
  
  yearvec<-as.numeric(format(flag.merge$xlDateTime, "%Y"))
  allyears<-unique(yearvec)
  #get counts of measurements per year; get a list of which years have a whole year of data
  yeartab<-as.data.frame(table(yearvec)); colnames(yeartab)<-c("year", "numpts")
  fullyears<-as.numeric(yeartab$year[yeartab$numpts>=17500])
  years<-allyears[fullyears]
  
  
  yearvec<-as.numeric(format(flag.merge$xlDateTime, "%Y"))
  flag2<-flag.merge[yearvec%in%years,]
  }else{flag2<-flag.merge}

  
  return(flag2)
  
}



unfill_fc<-function(dat, flag){
  
  fc.unf<-dat$Fc
  fc.unf[flag$Fc!=0 & flag$Fc!=50]<-NA
  
  dat$Fc_filtered_cust<-fc.unf
  
  return(dat)
  
}


#generate flag datasets. No need to do praire or switchgrass since they have Fc_filtered. 
maize.flag<-get_flagdat(filename1="Maize_2008_to_2019_L6.xlsx", filename2="MaizeBasalt_2020_2022_L6.xls");unique(maize.merge$Fc_filtered[maize.flag$Fc!=50 & maize.flag$Fc!=0])
maize.c.flag<-get_flagdat(filename1="MaizeNoBasalt_2017_to_2019_L6.xlsx", filename2="MaizeNoBasalt_2020_to_2022_L6.xls");unique(maize.c.merge$Fc_filtered[maize.c.flag$Fc!=50 & maize.c.flag$Fc!=0])

misc.flag<-get_flagdat(filename1="Miscanthus_2008_to_2019_L6.xlsx", filename2="MiscanthusBasalt_2020_2022_L6.xls");unique(misc.merge$Fc_filtered[misc.flag$Fc!=50 & misc.flag$Fc!=0])
misc.c.flag<-get_flagdat(filename1="MiscanthusNoBasalt_2017_to_2019_L6.xlsx", filename2="MiscanthusNoBasalt_2020_2022_L6.xls");unique(misc.c.merge$Fc_filtered[misc.c.flag$Fc!=50 & misc.c.flag$Fc!=0])

sorg.flag<-get_flagdat(filename1="sorghum_2018_to_2019_L6.xlsx", filename2<-"Sorghum_2020_2022_L6.xls");unique(sorg.merge$Fc_filtered[sorg.flag$Fc!=50 & sorg.flag$Fc!=0])



#apply flag datasets to get custom fc filtered
maize.merge<-unfill_fc(maize.merge,maize.flag)
maize.c.merge<-unfill_fc(maize.c.merge,maize.c.flag)

misc.merge<-unfill_fc(misc.merge,misc.flag)
misc.c.merge<-unfill_fc(misc.c.merge,misc.c.flag)

sorg.merge<-unfill_fc(sorg.merge,sorg.flag)

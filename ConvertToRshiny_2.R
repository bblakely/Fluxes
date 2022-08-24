library(readxl)


Rshinyize<-function(infilename, met_out, flux_out, year, sheet=2, skip=2, pfp.version=3){

  
  #Reading in and formatting data
  
  data.raw.orig<-read_excel(infilename, sheet=sheet, skip=skip)
  data.raw.orig[data.raw.orig==9999|data.raw.orig==-9999]<-NA
  timestamp.orig<-as.character(data.raw.orig$xlDateTime)
  
  data.raw<-data.frame(data.raw.orig)[substr(timestamp.orig, 1,4)==year,]
  if(nrow(data.raw)==0){print(paste("The dataset does not contain year", year))}
  
  timestamp<-as.character(data.raw$xlDateTime)
  
  #List variables needing conversion (both flux and met)
  conversion.vars<-c("Ta", "Ts", "VPD", "ps","Ts_10cma", "Ts_10cmb")
  
  
#Function to reassign variables  
  
Rshinyvar<-function(conversion.vars, rs.varnames, rs.units, pfp.varnames, pfp.units){
  
rshiny.dat<-data.frame(matrix(nrow=nrow(data.raw), ncol=length(rs.varnames), data=NA))
  
if(length(pfp.varnames)==length(rs.varnames)){
  
  for(v in 1:length(pfp.varnames)){
    
    if(pfp.varnames[v]%in%colnames(data.raw)){ #If pfp output  has this variable
      
      if(pfp.varnames[v]%in%conversion.vars){ #If this variable is known (a priori) as one that needs to be convertesd
        
        print(paste("Variable", pfp.varnames[v], "needs to be converted"))
        if(pfp.varnames[v]%in%c("VPD", "ps")){rshiny.dat[,v]<-data.raw[,which(colnames(data.raw)==pfp.varnames[v])]*1000;print(paste(pfp.varnames[v], "converted to pa and used for", rs.varnames[v])) }
        if(pfp.varnames[v]%in%c("Ts", "Ta","Ts_10cma", "Ts_10cmb")){rshiny.dat[,v]<-data.raw[,which(colnames(data.raw)==pfp.varnames[v])]+273.15;print(paste(pfp.varnames[v], "converted to K and used for", rs.varnames[v])) }
        colnames(rshiny.dat)[v]<-rs.varnames[v]
        
        }else{ #The usual situation; just sub in the pfp data
        
    rshiny.dat[,v]<-data.raw[,which(colnames(data.raw)==pfp.varnames[v])]
    colnames(rshiny.dat)[v]<-rs.varnames[v]
    print(paste("used", pfp.varnames[v], "for", rs.varnames[v] ))
    
    }
    
    }else{
      
      print(paste("data file does not appear to have variable",v,",",rs.varnames[v],"/", pfp.varnames[v],"; check varguide if you expect pfp to have this variable"))
      colnames(rshiny.dat)[v]<-rs.varnames[v]
      }
  
  print("#######")}
  
}
  return(rshiny.dat)
}



#Apply to Fluxes
flux.varguide<-read_excel("Fluxes_varnames_rshiny.xlsx")[1:4,]
flux.rs.varnames<-as.character(colnames(flux.varguide))
flux.rs.units<-flux.varguide[1,]
flux.pfp.varnames<-as.character(flux.varguide[4,]) 
flux.pfp.units<-flux.varguide[3,]


if(pfp.version==2){
  flux.pfp.varnames[flux.pfp.varnames=="Fco2"]<-"Fc"
  flux.pfp.varnames[flux.pfp.varnames=="Fco2_EPFlag"]<-"Fc_EPFlag"
}


#Apply the function

flux.rshiny.dat<-Rshinyvar(conversion.vars, rs.varnames=flux.rs.varnames, rs.units=flux.rs.units, pfp.varnames=flux.pfp.varnames, pfp.units = flux.pfp.units)

#Format and write
flux.rshiny.dat$DateTime<-timestamp
flux.rshiny.write<-rbind(flux.rs.units, flux.rshiny.dat)
write.csv(flux.rshiny.write, file=flux_out, row.names=FALSE, na="NAN", quote=FALSE)


#Apply to Met
met.varguide<-read_excel("Biomet_varnames_rshiny.xlsx")[1:4,]
met.rs.varnames<-as.character(colnames(met.varguide))
met.rs.units<-met.varguide[1,]
met.pfp.varnames<-as.character(met.varguide[4,]) 
met.pfp.units<-met.varguide[3,]

met.pfp.varnames[2]<-"Albedo"


#Apply the function
met.rshiny.dat<-Rshinyvar(conversion.vars, rs.varnames=met.rs.varnames, rs.units=met.rs.units, pfp.varnames=met.pfp.varnames, pfp.units = met.pfp.units)

#Format and write
met.rshiny.dat$DateTime<-timestamp
met.rshiny.write<-rbind(met.rs.units, met.rshiny.dat)
write.csv(met.rshiny.write, file=met_out, row.names=FALSE, quote=FALSE, na="NAN")

}


#Set your inputs

infilename<-file.choose()
year<-"2021"
crop<-"Sorghum"

#Add soy naming in soy years
if(year%in%c("2022", "2019", "2016", "2013", "2010")&crop%in%c("Maize", "Sorghum")){
  crop<-paste(crop,"Soy", sep='')
  }

met_out<-paste("Writeout/Efarm_",crop,"_",year,"_biomet.csv", sep='')
flux_out<-paste("Writeout/Efarm_",crop,"_",year,"_fluxes.csv", sep='')


sheet=2; skip=2
pfp.version=3

Rshinyize(infilename=infilename,met_out=met_out,flux_out=flux_out, 
            year=year, sheet=2, skip=2, pfp.version=3)


rm(infilename)

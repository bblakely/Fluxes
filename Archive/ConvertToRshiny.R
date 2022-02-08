library(readxl)

maize.raw.orig<-read_excel("MiscanthusNoBasalt_2020_L6.xls", sheet=2,skip=2)
#maize.raw.orig<-read.csv("MaizeCon_2020_L6.xls", sheet=2,skip=2)
maize.raw.orig[maize.raw.orig==-9999]<-NA

timestamp.orig<-as.character(maize.raw.orig$xlDateTime)


maize.raw<-data.frame(maize.raw.orig)#[substr(timestamp.orig, 1,4)=="2019",])
timestamp<-timestamp<-as.character(maize.raw$xlDateTime)

#Fluxes
varguide<-read_excel("Fluxes_varnames_rshiny.xlsx")[1:4,]
rs.varnames<-as.character(colnames(varguide))
rs.units<-varguide[1,]
pfp.varnames<-as.character(varguide[4,]) 
pfp.units<-varguide[3,]

#pfp.varnames[pfp.varnames=="Fco2"]<-"Fc"
#pfp.varnames[pfp.varnames=="Fco2_EPFlag"]<-"Fc_EPFlag"



conversion.vars<-c("Ta", "Ts", "VPD", "ps","Ts_10cma", "Ts_10cmb")

rshiny.dat<-data.frame(matrix(nrow=nrow(maize.raw), ncol=length(rs.varnames), data=NA))

if(length(pfp.varnames)==length(rs.varnames)){
  
  for(v in 1:length(pfp.varnames)){
    
    if(pfp.varnames[v]%in%colnames(maize.raw)){ #If pfp output  has this variable
      
      if(pfp.varnames[v]%in%conversion.vars){ #If this variable is known (a priori) as one that needs to be convertesd
        
        print(paste("Variable", pfp.varnames[v], "needs to be converted"))
        if(pfp.varnames[v]%in%c("VPD", "ps")){rshiny.dat[,v]<-maize.raw[,which(colnames(maize.raw)==pfp.varnames[v])]*1000;print(paste(pfp.varnames[v], "converted to pa and used for", rs.varnames[v])) }
        if(pfp.varnames[v]%in%c("Ts", "Ta","Ts_10cma", "Ts_10cmb")){rshiny.dat[,v]<-maize.raw[,which(colnames(maize.raw)==pfp.varnames[v])]+273.15;print(paste(pfp.varnames[v], "converted to K and used for", rs.varnames[v])) }
        colnames(rshiny.dat)[v]<-rs.varnames[v]
        
        }else{ #The usual situation; just sub in the pfp data
        
    rshiny.dat[,v]<-maize.raw[,which(colnames(maize.raw)==pfp.varnames[v])]
    colnames(rshiny.dat)[v]<-rs.varnames[v]
    print(paste("used", pfp.varnames[v], "for", rs.varnames[v] ))
    
    }
    
    }else{
      
      print(paste("data file does not appear to have variable",v,",",rs.varnames[v],"/", pfp.varnames[v],"; check varguide if you expect pfp to have this variable"))
      colnames(rshiny.dat)[v]<-rs.varnames[v]
      }
  
  print("#######")}
  
}

rshiny.dat$DateTime<-timestamp
rshiny.write<-rbind(rs.units, rshiny.dat)
write.csv(rshiny.write, "Miscanthus_2020_Fluxes.csv", row.names=FALSE, na="NAN", quote=FALSE)


###FUNCTIONALIZE THIS IT'S LITERALLY THE SAME CODE WITH DIFFERENT INPUTS#
#Met
varguide<-read_excel("Biomet_varnames_rshiny.xlsx")[1:4,]
rs.varnames<-as.character(colnames(varguide))
rs.units<-varguide[1,]
pfp.varnames<-as.character(varguide[4,]) 
pfp.units<-varguide[3,]

pfp.varnames[2]<-"Albedo"

rshiny.dat<-data.frame(matrix(nrow=nrow(maize.raw), ncol=length(rs.varnames), data=NA))


if(length(pfp.varnames)==length(rs.varnames)){
  
  for(v in 1:length(pfp.varnames)){
    
    if(pfp.varnames[v]%in%colnames(maize.raw)){ #If pfp output  has this variable
      
      if(pfp.varnames[v]%in%conversion.vars){ #If this variable is known (a priori) as one that needs to be convertesd
        
        print(paste("Variable", pfp.varnames[v], "needs to be converted"))
         if(pfp.varnames[v]%in%c("Ts", "Ta","Ts_10cma", "Ts_10cmb")){rshiny.dat[,v]<-maize.raw[,which(colnames(maize.raw)==pfp.varnames[v])]+273.15;print(paste(pfp.varnames[v], "converted to K and used for", rs.varnames[v])) }
        colnames(rshiny.dat)[v]<-rs.varnames[v]
        
      }else{ #The usual situation; just sub in the pfp data
        
        rshiny.dat[,v]<-maize.raw[,which(colnames(maize.raw)==pfp.varnames[v])]
        colnames(rshiny.dat)[v]<-rs.varnames[v]
        print(paste("used", pfp.varnames[v], "for", rs.varnames[v] ))
        
      }
      
    }else{
      
      print(paste("data file does not appear to have variable",v,",",rs.varnames[v],"/", pfp.varnames[v],"; check varguide if you expect pfp to have this variable"))
      colnames(rshiny.dat)[v]<-rs.varnames[v]
    }
    
    print("#######")}
  
}

rshiny.dat$DateTime<-timestamp

rshiny.write<-rbind(rs.units, rshiny.dat)
write.csv(rshiny.write, "Miscanthus_2020_biomet.csv", row.names=FALSE, quote=FALSE, na="NAN")



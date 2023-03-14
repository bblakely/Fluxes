#noodle about with random forest

#RandomForest package
library(randomForest); library(readxl); library(zoo)

#Data prep
dir<-getwd()
dat.all<-read_excel("Sorghum_2018_2020_L6_Summary.xls", skip=1)
setwd("WindFilter/data")
dat.east<-read_excel("Sorghum_2020_EAST_L6_Summary.xls", skip=1)
dat.west<-read_excel("Sorghum_2020_WEST_L6_Summary.xls", skip=1)
setwd(dir)


#Main function
applyrf<-function(dat.day, indvar="Fco2",year="2020", title="Sorghum", rankplots="FALSE", nmonths=c(1:12)){

#Prepare dataframe
var.select<-c(indvar,"Fsd", "VP", "Sws", "Ta", "Ts")
dat.year<-dat.day[format(dat.day$Days,"%Y")%in%year,]
dat.select<-dat.year[,colnames(dat.day)%in%var.select]

#Make data holders
superlist<-list()
imps<-data.frame(matrix(nrow=12,ncol=length(var.select)-1)); imps.rel<-data.frame(matrix(nrow=12,ncol=length(var.select)-1))
month.ts<-as.numeric(format(dat.year$Days, "%m"))

#Monthly loop

for(i in 1:12){
  dat<-dat.select[month.ts==i,]
  formula<-eval(parse(text = paste(indvar, "~ .")))
  rf<-randomForest(formula,data=dat,importance=TRUE)#importance=TRUE
  superlist[[i]]<-rf
  imps[i,]<-as.numeric(importance(superlist[[i]], type=1, scale=FALSE))#type=1,scale=FALSE
  imps.rel[i,]<-as.numeric(var.share(rf,rownames(importance(superlist[[i]]))))

}

colnames(imps)<-colnames(imps.rel)<-rownames(importance(superlist[[1]]))

#Plot yearly series
plot(as.numeric(imps[,1]), col='white', ylim=c(0, max(imps)+1), xlim=c(min(nmonths), max(nmonths)), main=paste(title, indvar), xlab="month", ylab="importance")
for(i in 1:(length(var.select)-1)){
 points(imps[,i], col=i, pch=9)
}
legend(min(nmonths),max(imps),colnames(imps), col=c(1:(length(var.select)-1)), lwd=1, cex=0.6, bty='n')

if(rankplots==TRUE){
#Plot rank
for(i in nmonths){
varImpPlot(superlist[[i]], main=paste(title, month.name[i], indvar))
}
}

}

#Gaps not removed#####
par(mfrow=c(2,1), mar=c(3,3,2,1))
applyrf(dat.west,title="WEST", indvar="Fco2")
applyrf(dat.east, title="EAST", indvar="Fco2")

applyrf(dat.west,title="WEST", indvar="GPP_LT")
applyrf(dat.east, title="EAST", indvar="GPP_LT")

applyrf(dat.west,title="WEST", indvar="ER_LT")
applyrf(dat.east, title="EAST", indvar="ER_LT")

#
applyrf(dat.west,title="WEST", rankplots=TRUE)
applyrf(dat.east,title="EAST", rankplots=TRUE)
#####

applyrf(dat.all)

#Thinking about population of days (not random forest)
par(mfrow=c(1,1))
plot(dat.east$Fco2~dat.east$Days, col='blue')
points(dat.west$Fco2~dat.west$Days, col='red')

year=2020
dat.year<-dat.all[format(dat.all$Days,"%Y")%in%year,]
month.ts<-as.numeric(format(dat.year$Days, "%m"))

plot(rollapply(dat.east$GPP_LT, width=14, fill=NA, FUN='mean',na.rm=TRUE, partial=FALSE)~dat.east$Days, type='line', col='blue', ylim=c(0,max(c(dat.east$GPP_LT, dat.west$GPP_LT))))
lines(rollapply(dat.west$GPP_LT, width=14, fill=NA, FUN='mean',na.rm=TRUE, partial=FALSE)~dat.west$Days, type='line', col='red')
abline(v=dat.east$Days[296])

#Remove days were measurements were not being taken and all values are gapfill
dat.east.nogap<-dat.east;dat.east.nogap[c(211:219, 261:279),]<-NA
dat.west.nogap<-dat.west;dat.west.nogap[c(211:219, 261:279),]<-NA

#Plot importances with large gaps excluded
par(mfrow=c(2,1), mar=c(3,3,2,1))
applyrf(dat.west.nogap,title="WEST", indvar="Fco2", nmonths=c(6:10))
applyrf(dat.east.nogap, title="EAST", indvar="Fco2", nmonths=c(6:10))

applyrf(dat.west.nogap,title="WEST", indvar="GPP_LT", nmonths=c(6:10))
applyrf(dat.east.nogap, title="EAST", indvar="GPP_LT", nmonths=c(6:10))

applyrf(dat.west.nogap,title="WEST", indvar="ER_LT", nmonths=c(7:9))
applyrf(dat.east.nogap, title="EAST", indvar="ER_LT", nmonths=c(7:9))

#
applyrf(dat.west.nogap,title="WEST", rankplots=TRUE, indvar="GPP_LT", nmonths=c(7:9))
applyrf(dat.east.nogap,title="EAST", rankplots=TRUE, indvar="GPP_LT", nmonths=c(7:9))

#Calculate remaining gappage
sorg.raw.flag<-read_excel("Sorghum_2018_2020_L6.xls", sheet=3,skip=2)
sorg.raw.flag[sorg.raw.flag==-9999]<-NA
sorg.flag<-sorg.raw.flag[as.numeric(format(sorg.raw.flag$...1, "%Y"))==2020,]

#Get just months of interest, July - Sepetmber
sorg.flag.months<-sorg.flag[as.numeric(format(sorg.flag$...1, "%m"))%in%c(7:9),]

length(which(sorg.flag.months$Fco2==510))/nrow(sorg.flag.months)

#28 days * 48 periods are removed, =1344 records
(length(which(sorg.flag.months$Fco2==510))-1344)/(nrow(sorg.flag.months)-1344)

#Find general % filled after removal of gap days (65% data based)
sorg.flag.months.bin<-sorg.flag.months; sorg.flag.months.bin[sorg.flag.months.bin==510]<-1;sorg.flag.months.bin[sorg.flag.months.bin==50]<-0 
pctfill<-aggregate(sorg.flag.months.bin$Fco2, by=list(format(sorg.flag.months.bin$...1, "%j")), FUN="sum")
plot(pctfill$x/48~list$Group.1, ylab="prop. gapflilled", xlab="doy")
mean((pctfill$x/48)[pctfill$x!=48])

#Show that filled times are almost all at night (>90% data based)
ind<-which(!format(sorg.flag.months$...1, "%j")%in%(c(211:219, 261:279)))
diel<-aggregate(sorg.flag.months.bin$Fco2[ind], by=list(format(sorg.flag.months.bin$...1, "%H")[ind]), FUN="mean")
plot(diel, ylab="prop. gapflilled", xlab="hour")

#Give significance test results
for(i in 1:12){
  sub<-which(month.ts==i)
  print(paste(month.name[i], ":"))
  print(t.test(dat.east.nogap$GPP_LT[sub], dat.west.nogap$GPP_LT[sub])$p.value)
}


gdat.east<-dat.east; gdat.east$group<-"E"
gdat.west<-dat.west; gdat.west$group<-"W"

gdat<-rbind(gdat.east, gdat.west)
cdays<-format(gdat$Days, "%m")
gdat$months<-cdays

library(ggplot2)
ggplot(gdat, aes(x=months, y=GPP_LT, fill=group)) + 
  geom_boxplot()

ggplot(gdat, aes(x=months, y=ER_LT, fill=group)) + 
  geom_boxplot()

library(esquisse)

ggplot(gdat) +
  aes(x = Days, y = GPP_LT, colour = group) +
  scale_color_hue(direction = 1) +
  theme_minimal()+
  stat_smooth(span=0.18)+
  scale_color_manual(values=c("blue", "red"))

ggplot(gdat) +
  aes(x = Days, y = ER_LT, colour = group) +
  scale_color_hue(direction = 1) +
  theme_minimal()+
  stat_smooth(span=0.15)+
  scale_color_manual(values=c("blue", "red"))

#Some numbers

pcts<-dat.west$Fco2/dat.east$Fco2

mean(pcts[month.ts%in%c(5:9)])


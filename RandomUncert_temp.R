dat<-maize.merge

err.orig<-dat$Fc_Rerr/abs(dat$Fc)

err<-err.orig
err[err.orig > quantile(err.orig, 0.99, na.rm=TRUE)]<-NA


rerr.orig<-dat$Fc_Rerr

rerr<-rerr.orig
rerr<-rerr[rerr.orig<quantile(rerr.orig, 0.99, na.rm=TRUE)]

(1/0.52)*sqrt(sum(rerr, na.rm=TRUE))*.01



length(which(is.na(err)))/nrow(maize.merge)



plot(err)
hist(err)

sum(err, na.rm=TRUE)/sum(abs(dat$Fc), na.rm=TRUE)


mean(dat$Fc_Rerr, na.rm=TRUE)
hist(dat$Fc_Rerr, na.rm=TRUE)
median(dat$Fc_Rerr, na.rm=TRUE)


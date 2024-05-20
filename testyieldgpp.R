test<-fluxes.gm

test$year<-as.factor(test$year)

dat.y<-test[,c(1, 3,4)]; dat.y$type<-"yield"; colnames(dat.y)[3]<-"flux"; dat.y$flux<-dat.y$flux*-1
dat.f<-test[,c(1,3,2)]; dat.f$type<-"gpp"; colnames(dat.f)[3]<-"flux"

new<-rbind(dat.y, dat.f)
esquisser()

ggplot(new) +
  aes(x = lab, fill = type, weight = flux) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(year))


ggplot(new) +
  aes(x = year, fill = type, weight = flux) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  ylab("Cumulative C")+
  theme_minimal() +
  facet_wrap(vars(lab))



#Computing airborne fraction from Joo et al, using multi model mean params

yearvec<-c(1:100)

a0<-0.2173

a1<-0.2240

a2<-0.2824

a3<-0.2763

t1<-394.4

t2<-36.54

t3<-4.304

out<-rep(NA, 20)

for(i in yearvec){

y1<-a1*exp(-i/t1)
y2<-a2*exp(-i/t2)
y3<-a3*exp(-i/t3)

out[i]<-a0+sum(y1, y2, y3)
  
  
  
}

plot(out)

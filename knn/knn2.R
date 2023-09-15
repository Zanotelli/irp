source('myKnnponderado.R')
rm(list=ls())

#### KNN usando uma média ponderada das distãncias dos pontos

colors<-c('pink', 'purple', 'blue', 'green', 'yellow', 'orange', 'red')
h<-0.1

N<-30
SD<-0.8
xc1<-matrix(rnorm(N*2),ncol=2)*SD + (matrix(c(2,2),ncol=2,nrow=N))
xc2<-matrix(rnorm(N*2),ncol=2)*SD + (matrix(c(4,4),ncol=2,nrow=N))

y1 = array(1,c(N,1))
y2 = y1*(-1)

X = rbind(xc1,xc2)
Y = rbind(y1,y2)

plot(xc1[,1],xc1[,2],col="red",xlim=c(0,6),ylim=c(0,6))
par(new=T)
plot(xc2[,1],xc2[,2],col="blue",xlim=c(0,6),ylim=c(0,6))

for(color in colors){
  seqi<-seq(0.06,6,0.06)
  seqj<-seq(0.06,6,0.06)
  M1 <- matrix(0,nrow=length(seqi),ncol=length(seqj)) 
  ci<-0
  for (i in seqi){
    ci<-ci+1
    cj<-0
    for(j in seqj)
    {
      cj<-cj+1
      
      M1[ci,cj]<- myKnnPonderado(c(i,j), X, Y, 10, h)
      
    }
  }
  par(new=T)
  contour(seqi,seqj,M1,levels=0,xlim=c(0,6),ylim=c(0,6), col=color)
  h<-h+0.1
}

###########
rm(list=ls())

N<-3000
h<-0.1
x<-matrix(rnorm(N, sd=0.7, mean=4))
y<-matrix(0, nrow=N, ncol=1)

xrange<-seq(2,6,0.1)
px<-matrix(ncol=1, nrow=length(xrange))

for(i in 1:length(xrange))
{
  x1<-xrange[i]
  acc<-0
  
  for(j in 1:length(x))
  {
    acc<-acc + exp(-((x1-x[j])^2)/(h^2))
  }
  px[i]<-acc/length(x)
}

plot(x,y, xlim=c(2,6), ylim=c(0,0.2))
par(new=T)
plot(xrange,px, type='l', xlim=c(2,6), ylim=c(0,0.2))



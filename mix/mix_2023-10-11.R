library('plot3D')
library('rgl')
library('mlbench')

pxkdenvar<-function(xrange,X,h){
  
  #########################
  knorm<-function(u,h){
    K <- (1/sqrt(2*pi*h*h)) * exp(-0.5*u^2)
    return(K)  
  }
  
  #########################
  N<-dim(X)[1]
  Nxrange<-dim(xrange)[1]
  nxrange<-dim(xrange)[2]
  
  px<-matrix(nrow=Nxrange,ncol=1)
  
  for (i in 1:Nxrange){
    if (nxrange >= 2){
      xi<-xrange[i,] 
    }else{
      xi<-xrange[i]  
    }
    
    kxixall<-0
    for (j in 1:N){
      xj<-X[j,]
      u<-(sqrt(sum((xi-xj)^2)))/h
      kxixall<-kxixall+knorm(u,h)
    }
    px[i]<-kxixall
  }
  px<-px/(N)
  return(px)
} 


fnormal1var<-function(x,m,r){
  y<-(1/(sqrt(2*pi*r*r)))*exp(-0.5 * ((x-m)/(r))^2)
  return(y)
}


############################
### Aplicação unidimensional


xg1<-rnorm(10)*0.4+2
xg2<-rnorm(50)*0.4+4

x<-rbind(xg1, xg2)
N<-length(x)
mx<-mean(x)
sx<-sd(x)

xrange<-seq(0,6,0.01)
h<-1.06*sx*N^(-1/5)


aux1<-matrix(0, ncol=1, nrow=length(xg1))
aux2<-matrix(0, ncol=1, nrow=length(xg2))

px<-pxkdenvar(matrix(xrange, ncol=1), matrix(x, ncol=1), h)
plot(xg1, aux1, col='red', xlim =c(0,6), ylim = c(0,1), xlab = 'x1', ylab = '')
par(new=T)
plot(xg2, aux2, col='blue', xlim =c(0,6), ylim = c(0,1), xlab = '', ylab = '')
par(new=T)
plot(xrange, px, col='black', type='line', xlim =c(0,6), ylim = c(0,1), xlab = '', ylab = '')



############################
### Aplicação bidimensional

N<-15
m1<-c(2,2)
m2<-c(3.5,4)
m3<-c(1.5,4)

p1<-matrix(rnorm(N*2,sd=0.6),nrow = N,ncol = 2)+matrix(m1,nrow = N,ncol=2,byrow = T)
p2<-matrix(rnorm(N*2,sd=0.6),nrow = N,ncol = 2)+matrix(m2,nrow = N,ncol=2,byrow = T)
p3<-matrix(rnorm(N*2,sd=0.6),nrow = N,ncol = 2)+matrix(m3,nrow = N,ncol=2,byrow = T)
xall<-rbind(p1,p2,p3)

seqi<-seq(0,6,0.2)
seqj<-seq(0,6,0.2)

Mkde<-matrix(1, nrow=length(seqi), ncol=length(seqj))

Mkde1<-matrix(1, nrow=length(seqi), ncol=length(seqj))
Mkde2<-matrix(1, nrow=length(seqi), ncol=length(seqj))
Mkde3<-matrix(1, nrow=length(seqi), ncol=length(seqj))

ci<-0
for(i in seqi){
  ci<-ci+1
  cj<-0
  for(j in seqj){
    cj<-cj+1
    x<-matrix(c(i,j),nrow=1)
    Mkde[ci,cj]<-pxkdenvar(x, xall, 0.4)
    Mkde1[ci,cj]<-pxkdenvar(x, p1, 0.4)
    Mkde2[ci,cj]<-pxkdenvar(x, p2, 0.4)
    Mkde3[ci,cj]<-pxkdenvar(x, p3, 0.4)
  }
}

# Todas as populações juntas
persp3d(seqi, seqj, Mkde, col='yellow')

# Populações separadas
persp3d(seqi, seqj, Mkde1, col='green')
persp3d(seqi, seqj, Mkde2, col='blue', add=T)
persp3d(seqi, seqj, Mkde3, col='red', add=T)

retkm<-kmeans(xall,3)
plot(xall[,1], xall[,2], col=retkm$cluster, xlim =c(0,6), ylim = c(0,6))
par(new=T)
contour(seqi, seqj, Mkde)

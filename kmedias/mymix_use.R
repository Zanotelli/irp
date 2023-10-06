sd1<-0.6
sd2<-0.8
N<-50

xc1<-matrix(rnorm(N*2, sd=sd1), ncol=2, nrow=N) + matrix(c(2,2),ncol=2, nrow=N)
xc2<-matrix(rnorm(N*2, sd=sd1), ncol=2, nrow=N) + matrix(c(4,4),ncol=2, nrow=N)
xc3<-matrix(rnorm(N*2, sd=sd1), ncol=2, nrow=N) + matrix(c(1,3),ncol=2, nrow=N)
xall<-rbind(xc1,xc2,xc3)





###############################
# Implementação com dados reais 

xy<-data(iris)
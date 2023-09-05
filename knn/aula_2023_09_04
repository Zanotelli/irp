
# Recomendado baixar o pacote RGL para visualização 3D

rm(list = ls())


N1<-30
xc1<-matrix(rnorm(N1*2, sd=1.2), ncol=2, nrow=N1) + matrix(c(2,2),ncol=2, nrow=N1)

N2<-30
xc2<-matrix(rnorm(N2*2, sd=1.2), ncol=2, nrow=N2) + matrix(c(4,4),ncol=2, nrow=N2)


plot(xc1[,1],xc1[,2],col='red', xlim=c(0,6), ylim=c(0,6))
par(new=T)
plot(xc2[,1],xc2[,2],col='blue', xlim=c(0,6), ylim=c(0,6))

x<-rbind(xc1,xc2)
y<-rbind(matrix(-1,ncol=1,nrow=N1), matrix(-1,ncol=1,nrow=N2))

dmat<-data.matrix(dist(x, method='euclidean',diag=T,upper=T))

persp(seq(1:60),seq(1:60),dmat, phi=90)

dmatret<-image(dmat)


# Implementação do KNN

ourKNN<-function(xt, xin, yin,k)
{
 xrep<-matrix(xt, byrow=T, nrow=dim(xin)[1], ncol=dim(xin)[2])
 dmat1<-rowSums((xrep-xin)* (xrep-xin)) 

 seqNN<-order(dmat1)
 yhat<-sign(sum(yin[seqNN[1:k]]))

 return(yhat)
}

xt<-x[1,]
yhatret<-ourKNN(xt,x,y,3)
yhatret



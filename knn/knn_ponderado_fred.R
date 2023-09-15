rm(list = ls())

library("mlbench")
data("Sonar")

X = Sonar[,1:60]
Y = Sonar[,61]
Y2 <- Y=='M'
Y2 = Y2*1
Y = (Y2*2)-1

#################################
# primeira maneira de fazer seleção de teste e treino
# problema: pode sair desbalanceado
iseq = sample(208)
iseq[1:20]

xteste = X[iseq[1:20],]
yteste = Y[iseq[1:20]] 
xtreina = X[-iseq[1:20],]
ytreina = Y[-iseq[1:20]] 



#################################
# segunda maneira de fazer seleção de teste e treino
#balanceado

Xc1 = X[which(Y==-1),]
Xc2 = X[which(Y==1),]

iseqC1 = sample(dim(Xc1)[1])
iseqC2 = sample(dim(Xc2)[1])

nc1 = ceiling(length(iseqC1)*0.1)
nc2 = ceiling(length(iseqC2)*0.1)

Xc1_teste = Xc1[iseqC1[1:nc1],]
Xc2_teste = Xc2[iseqC2[1:nc2],]

Xc1_treina = Xc1[-iseqC1[1:nc1],]
Xc2_treina = Xc2[-iseqC2[1:nc2],]

Xtreina = rbind(Xc1_treina,Xc2_treina)
Xteste = rbind(Xc1_teste,Xc2_teste)
Yteste = rbind(matrix(-1,nc1), matrix(1,nc2))
Ytreina = rbind(matrix(-1,(dim(Xc1)[1]-nc1)), matrix(1,(dim(Xc2)[1]-nc2)))




ourKNN <- function(xt,xin,yin,k)
{
  xrep<-matrix(xt,byrow=T,nrow = dim(xin)[1],ncol = dim(xin)[2])
  dmat1<-rowSums((xrep-xin)*(xrep-xin))
  seqNN<-order(dmat1)
  yhat<-sign(sum(Y[seqNN[1:k]]))
  return(yhat)
}

ourKNNponderado <- function(xt,xin,yin,k,h)
{
  xrep<-matrix(xt,byrow=T,nrow = dim(xin)[1],ncol = dim(xin)[2])
  dmat1<-data.matrix(rowSums((xrep-xin)*(xrep-xin)))
  seqNN<-order(dmat1)
  Yknn<-yin[seqNN[1:k]]
  xx<-exp(-dmat1[seqNN[1:k]]/h^2)
  yhat<-sign(sum(Yknn*xx))
  return(yhat)
}

N1<-30
xc1<-matrix(rnorm(N1*2,sd=1.2),ncol = 2,nrow = N1)+matrix(c(2,2),ncol = 2,nrow = N1)
N2<-30
xc2<-matrix(rnorm(N1*2,sd=1.2),ncol = 2,nrow = N1)+matrix(c(4,4),ncol = 2,nrow = N2)
plot(xc1[,1],xc1[,2],col='red',xlim = c(0,6),ylim = c(0,6))
par(new=T)
plot(xc2[,1],xc2[,2],col='blue',xlim = c(0,6),ylim = c(0,6))

X<-rbind(xc1,xc2)
Y<-rbind(matrix(-1,ncol = 1,nrow = N1),matrix(1,ncol = 1,nrow = N2))

dmat<-data.matrix(dist(X,method = "euclidean",diag = T,upper = T))
persp(seq(1:60),seq(1:60),dmat,phi = 90)
image(dmat)
xt<-X[12,]
yhat<-ourKNN(xt,X,Y,1)
seqx1x2<-seq(0,10,0.1)
lseq<-length(seqx1x2)
MZ<-matrix(nrow=lseq,ncol=lseq)
cr<-0
for (i in 1:lseq)
{
  for (j in 1:lseq)
  {
    cr<-cr+1
    x1<-seqx1x2[i]
    x2<-seqx1x2[j]
    xt<-as.matrix((cbind(x1,x2)))
    MZ[i,j]<-ourKNNponderado(xt,X,Y,10,1)
  }
}
plot(xc1[,1],xc1[,2],col='red',xlim = c(0,6),ylim = c(0,6))
par(new=T)
plot(xc2[,1],xc2[,2],col='blue',xlim = c(0,6),ylim = c(0,6))
contour(seqx1x2,seqx1x2,MZ,nlevels=1,xlim=c(-2,10),ylim=c(-2,10),xlab="x1",ylab="x2",add = T,phi = 80)

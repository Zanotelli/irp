rm(list = ls())

library(stringr)
heart <- read.csv('heart.dat', header=FALSE, sep=)

# Separa dados em uma matriz
heart_m<-matrix(as.numeric(str_split_fixed(heart$V1, " ", 14)), nrow = 270, ncol = 14)

# Separa os dados em dois grupos (retirando a última coluna)
xc1<-heart_m[heart_m[,14]==1,-c(14)]
xc2<-heart_m[heart_m[,14]==2,-c(14)]

nc1<-dim(xc1)[1]
nc2<-dim(xc2)[1]

y1<-array(1, dim=c(nc1,1))
y2<-array(-1, dim=c(nc2,1))

# Ordem aleatória dos dados 
iseq1<-sample(nc1)
iseq2<-sample(nc2)

# Criação das variáveis de Teste e Trainamento
yte1<-array(1, dim = c(nc1*.1,1))
yte2<-array(0, dim = c(nc2*.1,1))

nc1<-nc1*.9
nc2<-nc2*.9

xtrain1<-xc1[iseq1[1:nc1],]
xtrain2<-xc2[iseq2[1:nc2],]
xte1<-xc1[-iseq1[1:nc1],]
xte2<-xc2[-iseq2[1:nc2],]

Pc1<-nc1/(nc1+nc2)
Pc2<-nc2/(nc1+nc2)


# Treinamento do modelo
pdfnvar<-function(x,m,K,n)((1/(sqrt(((2*pi)^n)*(det(K)))))*exp(-0.5*((t(x-m)%*%(solve(K)))%*%(x-m))))
u1<-colMeans(xtrain1)
u2<-colMeans(xtrain2)


# Classificação do conjunto
xte<-rbind(xte1, xte2)
yte<-rbind(yte1, yte2)

class<-matrix(0, ncol=1, nrow=dim(xte)[1])
for(i in range(1,dim(xte)[1])){
  Pxdc1<-pdfnvar(xte[1,], u1, cov(xtrain1), dim(xte)[2])
  Pxdc2<-pdfnvar(xte[1,], u2, cov(xtrain2), dim(xte)[2])
  class[i]<-sign(Pxdc1 - Pxdc2)
}

class<-Pxdc1*Pc1/(Pxdc2*Pc2)
sum(abs(((class>1)*1)-yte))






# CLASSIFICADOR BAYESIANO

# - Baseado em probabilidade
# - Tem como hipótese que seus atributos são independentes (ingênuo)

nc<-240
sd1<-0.4
sd2<-0.8

aux1<-matrix(0, ncol=1, nrow=nc)
aux2<-matrix(0, ncol=1, nrow=nc)

xc1<-matrix(rnorm(nc, mean = 2, sd = sd1), ncol=1)
xc2<-matrix(rnorm(nc, mean = 4, sd = sd2), ncol=1)

plot(xc1, aux1, col='red', xlim =c(0,6), ylim = c(0,1), xlab = 'x1', ylab = '')
par(new=T)
plot(xc2, aux2, col='blue', xlim =c(0,6), ylim = c(0,1), xlab = '', ylab = '')

y1<-array(1, dim=c(nc,1))
y2<-array(-1, dim=c(nc,1))

iseq1<-sample(nc)
iseq2<-sample(nc)


xtrain1<-xc1[iseq1[1:(nc*0.9)]]
xtrain2<-xc2[iseq2[1:(nc*0.9)]]
xte1<-xc1[-iseq1[1:(nc*0.4)]]
xte2<-xc2[-iseq2[1:(nc*0.4)]]

yte1<-array(1, dim = c(nc*.1,1))
yte2<-array(0, dim = c(nc*.1,1))

nc<-nc*.9

Pc1<-nc/(nc+nc)
Pc2<-nc/(nc+nc)

### Treian o modelo

pdf1var<-function(x,m,s)((1/sqrt((2*pi*s))))*exp(-0.5*((x-m)^2))

u1<-mean(xtrain1)
u2<-mean(xtrain2)
s1<-sd(xtrain1)
s2<-sd(xtrain2)

### Classifica conjunto de teste

xte<-cbind(xte1, xte2)
yte<-cbind(yte1, yte2)

Pxdc1<-pdf1var(xte, u1, s1)
Pxdc2<-pdf1var(xte, u2, s2)

class<-Pxdc1*Pc1/(Pxdc2*Pc2)

class


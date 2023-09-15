source('myknn.R')

# Cria vetores com os dados aleatórios
N<-30
sd<-c(0.3, 0.5, 0.7, 0.9)

k<-c(2,4,8,16)
colors<-c('blue', 'green', 'orange', 'red')
len1<-length(sd)
len2<-length(k)


par(mfrow=c(2,2))
seqi<-seq(1,len1,1)
seqj<-seq(1,len2,1)


for(i in seqi)
{

xc1<-matrix(rnorm(N*2, sd=sd[i]), ncol=2, nrow=N) + matrix(c(2,2),ncol=2, nrow=N)
xc2<-matrix(rnorm(N*2, sd=sd[i]), ncol=2, nrow=N) + matrix(c(4,4),ncol=2, nrow=N)

plot(xc1[,1], xc1[,2], xlim=c(0,6), ylim= c(0,6), xlab='x1', ylab='x2', col='red')
par(new=T)
plot(xc2[,1], xc2[,2], xlim=c(0,6), ylim= c(0,6), xlab='x1', ylab='x2', col='blue')

y1<-matrix(1,ncol=1,nrow=N)
y2<-matrix(-1,ncol=1,nrow=N)
X<-rbind(xc1,xc2)
Y<-rbind(y1,y2)
seqx<-seq(0.06,6,0.06)
seqy<-seq(0.06,6,0.06)

for(j in seqj)
{
M1<-getKnnMatrix(X,Y,k[j],seqx,seqy)
par(new=T)
contour(seqx,seqy,M1,nlevels=0,xlim=c(0,6),ylim=c(0,6), col=colors[j])
}

}
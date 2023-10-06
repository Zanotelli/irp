sd1<-0.6
sd2<-0.8
N<-50

xc1<-matrix(rnorm(N*2, sd=sd1), ncol=2, nrow=N) + matrix(c(2,2),ncol=2, nrow=N)
xc2<-matrix(rnorm(N*2, sd=sd1), ncol=2, nrow=N) + matrix(c(4,4),ncol=2, nrow=N)
xc3<-matrix(rnorm(N*2, sd=sd1), ncol=2, nrow=N) + matrix(c(1,3),ncol=2, nrow=N)
xall<-rbind(xc1,xc2,xc3)

# Divide em um numero K de clusters
retkm<-kmeans(xall, 5)

plot(xall[,1], xall[,2], xlim=c(0,6), ylim= c(0,6), xlab='x1', ylab='x2', col=retkm$cluster)

# Ordena a matrix pelos clusters
xall_cluster<-cbind(xall, retkm$cluster)
xall_ord<-xall_cluster[order(retkm$cluster),]

image(as.matrix(dist(xall_ord, diag=T, upper=T)))



# FCM (precisa de uma biblioteca ?)
retfcm<-fcm(xall,3)

# Matrix com os pontos classificados em clusters
u_all<-retfcm&u

# Fazendo o calculo da Matriz vezes ela transposta estamos medido a similaridade
# entre os elementos
image(u_all %*% t(u_all))

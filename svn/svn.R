library('kernlab')

xc1=replicate(2, rnorm(50)+4)
xc2=replicate(2, rnorm(50)+2)
xin=rbind(xc1, xc2)
yin=rbind(matrix(-1,50,1), matrix(1,50,1))

plot(xc1[,1], xc1[,2], col='red', type='p', xlim=c(0,6), ylim=c(0,6))
par(new=T)
plot(xc2[,1], xc2[,2], col='blue', type='p', xlim=c(0,6), ylim=c(0,6))

svmtrein=ksvm(xin, yin, type='C-bsvc', kernel='rbfdot', kpar=list(sigma=0.1), C=10)
yhat=predict(svmtrein, xin, type='response')

a=alpha(svmtrein)
ai=SVindex(svmtrein)
nsvec=nSV(svmtrein)

par(new=T)
plot(xin[ai,1], xin[ai,2], col='black', type='p', xlim=c(0,6), ylim=c(0,6))




myKnnPonderado <- function(xt, xin, yin, k, h){
  
  xrep <- matrix(xt, byrow=T, nrow=dim(xin)[1], ncol=dim(xin)[2])
  
  dmat <- data.matrix(rowSums((xrep-xin)*(xrep-xin)))
  seqNN <- order(dmat)
  Yknn <- yin[seqNN[1:k]]
  
  xx <- exp(-dmat[seqNN[1:k]]/h^2)
  yhat <- sign(sum(Yknn*xx))
  
  return(yhat)
}


seq1x2=seq(0,10,0.1)
lseq=length(seq1x2)
MZ=matrix(nrow=lseq, ncol = lseq)
cr=0
for(i in 1:lseq){
  for(j in 1:lseq){
    cr=cr+1
    x1=seq1x2[i]
    x2=seq1x2[j]
    xt=as.matrix(cbind(x1,x2))
    MZ[i,j]=myKnnPonderado()
  }
}












myKnnPonderado <- function(xt, xin, yin, k, h){
  
  xrep <- matrix(xt, byrow=T, nrow=dim(xin)[1], ncol=dim(xin)[2])
  
  dmat <- data.matrix(rowSums((xrep-xin)*(xrep-xin)))
  seqNN <- order(dmat)
  Yknn <- yin[seqNN[1:k]]
  
  xx <- exp(-dmat[seqNN[1:k]]/h^2)
  yhat <- sign(sum(Yknn*xx))
  
  return(yhat)
}

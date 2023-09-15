# X: dados
# Y: vetor de tamanho N de labels (-1 e 1 para duas classes)
# xt: vetor de teste de tamanho n
myKNN<-function(X,Y,xt,k)
{
N<-dim(X)[1]
n<-dim(X)[2]

# vetor de passos
seqc<-seq(1,N,1)
mlist<-matrix(nrow=N, ncol=1)

# itera na sequencia
for(i in seqc)
{
	xc<-X[i,]
	mlist[i] <- sqrt( sum((xc-t(xt))^2) )
}

ordmlist<-order(mlist)
ordY<-Y[ordmlist]

yxt<-sign(sum(ordY[i:k]))
return(yxt)
#retlist<-list(yxt, mlist, ordY)
}

#########################################################

getKnnMatrix<-function(X, Y, k, seqi, seqj)
{
M1 <- matrix(0,nrow=length(seqi),ncol=length(seqj)) 
ci<-0
for (i in seqi){
  ci<-ci+1
  cj<-0
  for(j in seqj)
  {
    cj<-cj+1
    
    M1[ci,cj]<- myKNN(X,Y,matrix(c(i,j),nrow=2),k)
    
  }
}
M1
return(M1)
}


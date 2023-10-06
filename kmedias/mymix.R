rm(list = ls())

# Função de mistura
# inlist -> Lista de matrizes de entrada
# x      -> Lista de clusters
mymix<-function(x, inlist){
  
  # Método de classificação
  pdfnvar<-function(x,m,K,n)((1/(sqrt(((2*pi)^2)*(det(K)))))*exp(-0.5*(t(x-m)%*%(solve(K))%*%(x-m))))
  
  # Inicializa listas
  ng<-length(inlist)
  klist<-list()
  mlist<-list()
  pglist<-list()
  nglist<-list()
  n<-dim(inlist[[1]])[2]
  
  N<-0
  
  for(i in 1:ng)
  {
    klist[[i]]<-cov(inlist[[i]])
    mlist[[i]]<-colMeans(inlist[[i]])
    nglist[[i]]<-dim(inlist[[i]])[1]
    N<-N + dim(inlist[[i]])[1]
  }
  
  for(i in 1:ng)
  {
    pglist[[i]]<-nglist[[i]]/N
  }
  
  Px<-0
  for(i in 1:ng)
  {
    Px<-Px+pglist[[i]]*pdfnvar(x, mlist[[i]], klist[[i]], n)
  }
 
  return(Px) 
}

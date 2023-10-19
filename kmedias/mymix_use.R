rm(list=ls())     # Inicializa todas as variáveis
library('plot3D')
library('rgl')
library('mlbench')
library('caret')
# source("/home/apbraga/PRINCIPAL/livros/LIVRO_R/rotinasGPL/mykmedias.R")


################################
### KNN

mykmedias<-function(X,k){
  
  N<-dim(X)[1]
  n<-dim(X)[2]
  
  mi<-sample(N)
  m<-matrix(nrow=k,ncol=n)
  m<-X[mi[1:k],]
  
  d<-matrix(nrow=k,ncol=1)
  c<-matrix(nrow=N,ncol=1)
  
  for (kk in(1:100)){
    for (i in (1:N)){
      xt<-as.matrix(X[i,])
      for (j in (1:k)) 
        d[j]<-sum((t(xt)-m[j,])^2)
      
      ordd<-order(d,decreasing=FALSE)
      c[i]<-ordd[1]
    }
    
    for (i in (1:k)){
      ici<-which(c[] == i)
      ni<-length(ici)
      acc<-matrix(0,nrow=1,ncol=n)
      
      for (j in (1:ni))
        acc<-acc+t(as.matrix(X[ici[j],]))
      
      m[i,]<-acc/ni
    }
  } 
  
  retlist<-list(m,c)
  return(retlist)
}


################################
### Mix

mymix<-function(x,inlist){
  
  pdfnvar<-function(x,m,K,n) ((1/(sqrt((2*pi)^n*(det(K)))))*exp(-0.5*(t(x-m) %*% (solve(K)) %*% (x-m))))
  
  ng<-length(inlist)
  klist<-list()
  mlist<-list()
  pglist<-list()
  nglist<-list()
  n<-dim(inlist[[1]])[2]
  
  for (i in (1:ng))  {
    
    klist[[i]]<-cov(inlist[[i]])
    mlist[[i]]<-colMeans(inlist[[i]])
    nglist[[i]]<-dim(inlist[[i]])[1]
  }
  
  N<-0
  
  for (i in (1:ng)){
    N<-N+nglist[[i]]
  }
  
  for (i in (1:ng)){
    pglist[[i]]<-nglist[[i]]/N
  }
  
  Px<-0
  for (i in (1:ng)){
    Px<-Px+pglist[[i]]*pdfnvar(x,mlist[[i]],klist[[i]],n)
  }
  
  return(Px)
}


################################
### K Folds

createKFolds<- function(nsamples, nfolds){
  folds=rep(1:nfolds, length.out=nsamples)
  folds=sample(folds)
  return(folds)
}


################################
### Criação das populações

nc=200

p<-mlbench.spirals(400,1,0.08)
ic1<-which(p[[2]]==1)
ic2<-which(p[[2]]==2)
xall<-as.matrix(p[[1]])
xc1<-xall[ic1,]
xc2<-xall[ic2,]


################################
### Treino e teste

y1<-array(1, dim=c(nc,1))
y2<-array(-1, dim=c(nc,1))
yall=rbind(y1,y2)
yte1<-array(1, dim = c(nc*.1,1))
yte2<-array(0, dim = c(nc*.1,1))
yte=rbind(yte1,yte2)

iseq1<-sample(nc)
iseq2<-sample(nc)


nc<-nc*.9

xtrain1<-xc1[iseq1[1:nc],]
xtrain2<-xc2[iseq2[1:nc],]
xte1<-xc1[-iseq1[1:nc],]
xte2<-xc2[-iseq2[1:nc],]

xtrain=rbind(xtrain1, xtrain2)
xte=rbind(xte1, xte2)

######################
### 10 folds

nfolds=10
num_samples = dim(xtrain)[1]
k=1:5

folds=createKFolds(num_samples, nfolds)

c_vec = c(.01, .1, 1, 10)
acc_vec = matrix(0, nrow=length(k), ncol=length(c_vec))

for(k_count in length(k)){
  k1=k[k_count]
  k2=k[k_count]
  
  for(fold in 1:nfolds){
    
    train=which(folds!=fold)
    teste=which(folds==fold)
    
    i_xtrain=xall[train,]
    i_xte=xall[teste,]
    i_ytrain=yall[train]
    i_yte=yall[teste]
    
    i_c1 = which(i_ytrain==1)
    i_c2 = which(i_ytrain==-1)
    i_xc1 = i_xtrain[i_c1,]
    i_xc2 = i_xtrain[i_c2,]
    
    i_c1te = which(i_yte=1)
    i_c2te = which(i_yte=-1)
    i_xc1te = i_xte[i_c1,]
    i_xc2te = i_xte[i_c2,]
    
    n_amostras_te = dim(xte)[1]
    n_amostras_train = dim(xte)[2]
  
    retlist1<-mykmedias(xc1,k1)
    xclusters1<-list()
    for (i in (1:k1)){
      ici<-which(retlist1[[2]]==i)
      xclusters1[[i]]<-xc1[ici,]
    }
    retlist2<-mykmedias(xc2,k2)
    xclusters2<-list()
    for (i in (1:k2)){
      ici<-which(retlist2[[2]]==i)
      xclusters2[[i]]<-xc2[ici,]
    }
    
    seqi<-seq(-1,1,0.05)
    seqj<-seq(-1,1,0.05)
    M1<-matrix(1,nrow=length(seqi),ncol=length(seqj))
    M2<-matrix(1,nrow=length(seqi),ncol=length(seqj))
    M<-matrix(1,nrow=length(seqi),ncol=length(seqj))
    
    
  }
}



# Variamos o Fold, o valor de C e o parâmetro K


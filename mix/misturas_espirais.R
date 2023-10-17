rm(list=ls())     # Inicializa todas as variáveis
library('plot3D')
library('rgl')
library('mlbench')
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
### Criação das populações

s1<-0.5
s2<-0.5
nc<-100

p<-mlbench.spirals(400,1,0.08)
ic1<-which(p[[2]]==1)
ic2<-which(p[[2]]==2)
xall<-as.matrix(p[[1]])
xc1<-xall[ic1,]
xc2<-xall[ic2,]


# xc<-matrix(rnorm(nc*2),ncol=2)*s1 + matrix(c(2,2),nrow=nc,ncol=2)

######## Classe 1

k1<-10
retlist1<-mykmedias(xc1,k1)
xclusters1<-list()

for (i in (1:k1)){
  ici<-which(retlist1[[2]]==i)
  xclusters1[[i]]<-xc1[ici,]
}

######## Classe 2

k2<-10
retlist2<-mykmedias(xc2,k2)
xclusters2<-list()

for (i in (1:k2)){
  ici<-which(retlist2[[2]]==i)
  xclusters2[[i]]<-xc2[ici,]
}

####################

seqi<-seq(-1,1,0.05)
seqj<-seq(-1,1,0.05)

M1<-matrix(1,nrow=length(seqi),ncol=length(seqj))
M2<-matrix(1,nrow=length(seqi),ncol=length(seqj))
M<-matrix(1,nrow=length(seqi),ncol=length(seqj))


ci<-0
for (i in seqi){
  ci<-ci+1
  cj<-0
  for (j in seqj){
    cj<-cj+1
    x<-c(i,j)
    M1[ci,cj]<-mymix(c(i,j),xclusters1)
    M2[ci,cj]<-mymix(c(i,j),xclusters2)
  }
  
}

M<-1*(M1 > M2)

plot(xc1[,1],xc1[,2],type='p',col='blue',xlim = c(-1,1),ylim = c(-1,1),xlab = 'Atributo 1',ylab = 'Atributo 2')
par(new=T)
plot(xc2[,1],xc2[,2],type='p',col='red',xlim = c(-1,1),ylim = c(-1,1),xlab = ' ',ylab = ' ')
contour(seqi,seqj,M1,nlevels=20,xlim = c(-1,1),ylim = c(-1,1),xlab = '',ylab = '',add = T)
contour(seqi,seqj,M2,nlevels=20,xlim = c(-1,1),ylim = c(-1,1),xlab = '',ylab = '',add = T)
contour(seqi,seqj,nlevels=1,M,xlim = c(-1,1),ylim = c(-1,1),xlab = '',ylab = '',add = T)

persp3d(seqi,seqj,M1,col='red')
persp3d(seqi,seqj,M2,col='blue',add = T)
persp3d(seqi,seqj,M,col='black',add = T)
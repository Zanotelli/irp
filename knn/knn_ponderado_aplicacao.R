rm(list=ls())

library('mlbench')
data(Sonar)
data(iris)

## Variaveis auxiliares
colors<-c('purple', 'pink', 'blue', 'green', 'yellow', 'orange', 'red')
h<-0.1
k<-8
N<-30

## Conjuntos de dados
X<- Sonar[,1:60]
Y<-Sonar[61]
Y<- Y == 'M'
Y<- Y*1
Y<- (Y*2) - 1

## Pega amostra de dados

# Filta valores de X em 2 vetores separados por população
xc1<-X[which(Y==1),]
xc2<-X[which(Y==-1),]

# Gera lista com indexs aleatórios
seqi1<-sample(dim(xc1)[1])
seqi2<-sample(dim(xc2)[1])

# Pega 10% da população para teste
nc1<-ceiling(length(seqi1)*0.1)
nc2<-ceiling(length(seqi2)*0.1)

# Cria vetor de teste com 10% de cada população
xc1teste<- xc1[seqi1[1:nc1],]
xc2teste<- xc2[seqi2[1:nc2],]

# Cria vetores de treino com o resto dos dados
xc1treino<- xc1[-seqi1[1:nc1],]
xc2treino<- xc2[-seqi2[1:nc2],]

# Junta os dados em um vetor único
xteste<-rbind(xc1teste, xc2teste)
xtreino<-rbind(xc1treino, xc2treino)

# Cria vetores resultado
yteste<- rbind(matrix(-1,nc1), matrix(1,nc2))
ytreino<- rbind(matrix(-1,(dim(xc1)[1]-nc1)), matrix(1,(dim(xc2)[1]-nc2)))


## Cálculo do resultado
M1 <- matrix(0,nrow=(nc1+nc2),ncol=1)
seqx<-seq(nc1+nc2)
for (i in seqx){
    M1[i]<- myKnnPonderado(as.matrix(xteste[i]), as.matrix(xtreino), as.matrix(ytreino), k, h)
}

M1

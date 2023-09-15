source('treinaperceptron.R')

# Cria vetores com os dados aleatórios
N<-30
xc1<-matrix(rnorm(N*2, sd=0.4), ncol=2, nrow=N) + matrix(c(2,2),ncol=2, nrow=N)
xc2<-matrix(rnorm(N*2, sd=0.4), ncol=2, nrow=N) + matrix(c(4,4),ncol=2, nrow=N)

y1<-matrix(0,ncol=1,nrow=21)
y2<-matrix(1,ncol=1,nrow=1)

X<-rbind(xc1,xc2)
Y<-rbind(y1,y2)


# Plota os dois no gráfico, semparando os dois conjuntos
plot(xc1[,1], xc1[,2], xlim=c(0,6), ylim= c(0,6), xlab='x1', ylab='x2', col='red')
par(new=T)
plot(xc2[,1], xc2[,2], xlim=c(0,6), ylim= c(0,6), xlab='x1', ylab='x2', col='blue')
par(new=T)
abline(6, -1, col='green')


result<-treinaperceptron(xc1, y1, 0.1, 0.01, 100, 1)
result
par(new=T)
contour(result[1],1,levels=0,xlim=c(0,6),ylim=c(0,6))











# vetor de [0,6] com passo 0.2
seqx1x2<-seq(0,6,0.2)
npgrid<-length(seqx1x2)

M<-matrix(nrow=npgrid, ncol=npgrid)
i<-0
w<-as.matrix(c(6,1,1))

for(x1 in seqx1x2)
{
	i<-i+1
	j<-0

	for(x2 in seqx1x2){
		j<-j+1
		xin<-as.matrix(cbind(-1, x1, x2))
		M[i, j] <- 1*((xin %*% w) >= 0)
	}
}


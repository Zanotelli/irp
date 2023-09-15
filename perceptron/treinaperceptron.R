# xin: Entrada Nxn de dados de matriz
# yd: tem que ser gerado para as xin (concatenando xall), etade 0 e metade 1
# eta: peso de atualização do passo
# tol: tolerancia do erro
# maxepocas: numero máximo de epocas permitidas
# par: par=1 indica que -1 precisa ser acrescido à xin

treinaperceptron<-function(xin, yd, eta, tol, maxepocas, par)
{

N<-dim(xin)[1]
n<-dim(xin)[2]

if(par==1){
	wt<-as.matrix(runif(n+1)-0.5)
	xin<-cbind(-1,xin)
} else {
	wt<-as.matrix(runif(n)-0.5)
}

nepocas<-0	   # contador
eepocas<-tol+1 # acumula o erro das épocas
evec<-matrix(nrow=1, ncol=maxepocas) # vetor de erros

while((nepocas < maxepocas) && (eepocas > tol))
{
	ei2<-0
	xseq<-sample(N)  # seq aleatoria de treinamento
	for(i in 1:N)
	{
		irand<-xseq[i]  # amostra dados da seq aleatoria
		yhati<-1.0*((xin[irand,] %*% wt) >= 0) # calcula saida do perceptron
		ei<-yd[irand]-yhati
		dw<-eta*ei*xin[irand,]
		dw<-wt+dw
		ei2<ei2+(ei*ei)
	}

	nepocas<-nepocas+1
	evec[nepocas]<-ei2/N
	eepoca<-evec[nepocas]
}

retlist<-list(wt, evec[1:nepocas])
return(retlist)
}
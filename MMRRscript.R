# MMRR performs Multiple Matrix Regression with Randomization analysis
# Y is a dependent distance matrix
# X is a list of independent distance matrices (A-Z, must be changed to vector X before starting run)


gen=read.table("GD2.txt")
Y=as.matrix(gen)
geo=read.table("GGD2.txt")
ge2=data.matrix(geo)
chla=read.table("CHLA_Annual.txt")
ch2=data.matrix(chla)
poc=read.table("POC_Annual.txt")
po2=data.matrix(poc)


X=list(ge2)
B=list(ch2)
C=list(ge2,ch2)
D=list(po2)
E=list(ge2,po2)
F=list(ch2,po2)
G=list(ge2,po2,ch2)



names(X)=c("geo")
MMRR<-function(Y,X,nperm=9999){
	#compute regression coefficients and test statistics
	nrowsY<-nrow(Y)
	y<-unfold(Y)
	if(is.null(names(X)))names(X)<-paste("X",1:length(X),sep="")
        Xmats<-sapply(X,unfold)
        fit<-lm(y~Xmats)
	coeffs<-fit$coefficients
	summ<-summary(fit)
	r.squared<-summ$r.squared
	tstat<-summ$coefficients[,"t value"]
	Fstat<-summ$fstatistic[1]
	tprob<-rep(1,length(tstat))
	Fprob<-1

	#perform permutations
	for(i in 1:nperm){
		rand<-sample(1:nrowsY)
		Yperm<-Y[rand,rand]
		yperm<-unfold(Yperm)
		fit<-lm(yperm~Xmats)
		summ<-summary(fit)
                Fprob<-Fprob+as.numeric(summ$fstatistic[1]>=Fstat)
                tprob<-tprob+as.numeric(abs(summ$coefficients[,"t value"])>=abs(tstat))
	}

	#return values
	tp<-tprob/(nperm+1)
	Fp<-Fprob/(nperm+1)
	names(r.squared)<-"r.squared"
	names(coeffs)<-c("Intercept",names(X))
	names(tstat)<-paste(c("Intercept",names(X)),"(t)",sep="")
	names(tp)<-paste(c("Intercept",names(X)),"(p)",sep="")
	names(Fstat)<-"F-statistic"
	names(Fp)<-"F p-value"
	return(list(r.squared=r.squared,
		coefficients=coeffs,
		tstatistic=tstat,
		tpvalue=tp,
		Fstatistic=Fstat,
		Fpvalue=Fp))
}

# unfold converts the lower diagonal elements of a matrix into a vector
# unfold is called by MMRR

unfold<-function(X){
	x<-vector()
	for(i in 2:nrow(X)) x<-c(x,X[i,1:i-1])
	return(x)
}


# to run MMRR

MMRR (Y,X,nperm=9999)

#graphs
#creates the objects to graph from the MMRR script
y<-unfold(Y)
Xmats<-sapply(X,unfold)

#A
postscript("GGD-GD-Annual.eps", horizontal=FALSE,onefile=FALSE,paper="special",height=10,width=10)
plot(y~Xmats[,1],xlab="Geographic distance",ylab="Genetic distance")
regline1=abline(lm(y~Xmats[,1]))
title(paste("R2=0.375, p=0.001"), adj=0)
dev.off()

#B
postscript("CHLA-GD-Annual.eps", horizontal=FALSE,onefile=FALSE,paper="special",height=10,width=10)
plot(y~Xmats[,1],xlab="Environmental distance (CHLA)",ylab="Genetic distance")
abline(lm(y~Xmats[,1]))
title(paste("R2=0.345, p=0.0002"), adj=0)
dev.off()

#C
postscript("CHLA-GGD-GD-Annual.eps", horizontal=FALSE,onefile=FALSE,paper="special",height=10,width=10)
plot(y~c(Xmats[,1]+Xmats[,2]),xlab="0.041 Geographic + 0.577 Environmental (CHLA) distance",ylab="Genetic distance")
regline1=abline(lm(y~Xmats[,1]))
title(paste("R2=0.346,p<0.0007"), adj=0)
dev.off()

#D
postscript("POC-GD-Annual.eps", horizontal=FALSE,onefile=FALSE,paper="special",height=10,width=10)
plot(y~Xmats[,1],xlab="Environmental distance (POC)",ylab="Genetic distance")
abline(lm(y~Xmats[,1]))
title(paste("R2=0.405, p=0.0002"), adj=0)
dev.off()

#E
postscript("POC-GGD-GD-Annual.eps", horizontal=FALSE,onefile=FALSE,paper="special",height=10,width=10)
plot(y~c(Xmats[,1]+Xmats[,2]),xlab="0.015 Geographic + 0.632 Environmental (POC) distance",ylab="Genetic distance")
regline1=abline(lm(y~Xmats[,1]))
title(paste("R2=0.405,p<0.0001"), adj=0)
dev.off()

plot(Xmats[,1]~(Xmats[,2]),xlab="geo",ylab="POC")
abline(lm(Xmats[, 2] ~ Xmats[, 1]))

plot(Xmats[,2]~(Xmats[,3]),xlab="chla",ylab="POC")
abline(lm(Xmats[, 2] ~ Xmats[, 3]))


file<-read.csv("Propydevt.csv",sep=";", dec=",", header = TRUE);
newFile<-file[order(file$Ãð..Òÿæåñòü),]

sink("statistics2.txt", split=TRUE)

K<-length(newFile[newFile[[1]]==1,])
p1<-5; p2<-4 # p1 - levels of gruz, levels of Na-Li
 	for(i in 1:p1){
		len<-length(newFile[newFile[[1]]==i,]$Na.Li)
		print(K)
		if (len < K) K<-len
	}
K<-K%/%p2 # for similar count in each 
vA<-p1-1
vB<-p2-1
vAB<-(p1-1)*(p2-1)
vE<-(p1-1)*(p2-1)



	print(paste0("vA: ", vA))
	print(paste0("vB: ", vB))
	print(paste0("vAB: ", vAB))
	print(paste0("vE: ", vE))

	arr<- array(1, dim=c(p1,p2,K))
	 for(i in 1:p1){
		for (k in 1:K) {
			
			for(j in 1:p2) {
				arr[i, j, k]<-newFile[newFile[[1]]==i,]$ÔÈÁÐ[[j]]
			}
		}
	}

	SSA<-0; SSB<-0; SSE<-0
	n<-p1*p2
	generalMean<-0
	 for(i in 1:p1){
		for(j in 1:p2){
			generalMean<-generalMean + arr[i, j, K]
		}
	}
	generalMean<-generalMean/n
	meanA<-list()
	meanB<-list()
	 for(i in 1:p1){
		meanA[[i]]<-mean(arr[i, , K])
	}
	 for(i in 1:p2){
		meanB[[i]]<-mean(arr[ , i, K])
	}

	for(i in 1:p1){
		SSA<-SSA + (meanA[[i]] - generalMean)^2
	}
	SSA<-SSA*p2

	for(i in 1:p2){
		SSB<-SSB + (meanB[[i]] - generalMean)^2
	}
	SSB<-SSB*p1

	print(paste0("SSA: ", SSA))
	print(paste0("SSB: ", SSB))

	SSE<-0
	for(i in 1:p1){
		for(j in 1:p2){
			SSE<-SSE + (arr[i, j, K] - meanA[[i]] - meanB[[j]] + generalMean)^2
		}
	}
	print(paste0("SSE: ", SSE))

	FstatA<-(SSA/ vA) / (SSE/ vE)
	FstatB<-(SSB/ vA) / (SSA/ vE)


	fACriterium<-pf(FstatA, vA, vE);
	alphaA<-1-fACriterium;

	fBCriterium<-pf(FstatB, vB, vE);
	alphaB<-1-fBCriterium;

print(paste0("alphaA: ", alphaA))
print(paste0("alphaB: ", alphaB))







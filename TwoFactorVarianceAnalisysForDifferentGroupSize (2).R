file<-read.csv("Propydevt.csv",sep=";", dec=",", header = TRUE);
sink("statisticsDA2F.txt", split=TRUE)
p1<-5
p2<-4
n<- length(file$ÔÈÁÐ)

values<- array(list(), dim=c(p1, p2))
means<- array(1, dim=c(p1,p2))
lengths<-array(1, dim=c(p1, p2))
	 for(i in 1:p1){
		weightArr<-file[file$Ãð..Òÿæåñòü==i,]
		for (j in 1:p2) {
			quartAndWeigthArr<-weightArr[weightArr$quart==j,]$ÔÈÁÐ
			values[[i, j]] <-quartAndWeigthArr
			means[i, j]<-mean(quartAndWeigthArr)
			lengths[i, j]<-length(quartAndWeigthArr)
		
		}	
	}

SSE<-0
for (i in 1:p1) {
	for (j in 1:p2) {
		nij<-length(values[[i, j]]);
		for (k in 1: nij) {
			SSE<-SSE + (values[[i, j]][k] - means[i,j])^2
		}
	}
}
SSE<-SSE/(n-p1*p2)
print(paste0("SSE: ", SSE))

GA<-0;
GB<-0;
meansj<-array(1, dim = c(p2))
meansi<-array(1, dim = c(p1))
for(j in 1: p2) {
	meansj[j]<-mean(file[file$quart==j,]$ÔÈÁÐ)
}
for(i in 1: p1) {
	meansi[i]<-mean(file[file$Ãð..Òÿæåñòü==i,]$ÔÈÁÐ)
}

for (j in 1:p2) {
	for (i in 1:p1) {
		nij<-length(values[[i, j]]);
		for (k in 1: nij) {
			GA<-GA+ (values[[i, j]][k] - meansj[j])^2
		}
	}
}

for (i in 1:p1) {
	for (j in 1:p2) {
		nij<-length(values[[i, j]]);
		for (k in 1: nij) {
			GB<-GB+ (values[[i, j]][k] - meansi[i])^2
		}
	}
}

	print(paste0("GA: ", GA))
	print(paste0("GB: ", GB))

	FAStat<- (GA-SSE)/((p1 - 1)* SSE)
	FBStat<- (GB-SSE)/((p2 - 1)* SSE)
	print(paste0("FAStat: ", FAStat))
	print(paste0("FBStat: ", FBStat))

	fCriteriumA<-pf(FAStat, p1-1, n-p1*p2);
	fCriteriumB<-pf(FBStat, p2-1, n-p1*p2);
	print(paste0("fCriteriumA: ", fCriteriumA))
	print(paste0("fCriteriumB: ", fCriteriumB))

	alphaA<-1-fCriteriumA;
	alphaB<-1-fCriteriumB;
	print(paste0("alphaA: ", alphaA))
	print(paste0("alphaB: ", alphaB))


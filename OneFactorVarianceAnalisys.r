file<-read.csv("Propydevt.csv",sep=";", dec=",", header = TRUE);
sink("statistics.txt", split=TRUE)
p<-5;

calculateStatistics<-function(field , name) {

	n<-length(field );
	for(i in 1:p){group[[i]]<-field[file[[1]]==i]}
	sumSquares<-sum(field ^2)
	generalMean<-mean(field )

	print(name);
 	meanYj<-list()
 	for(i in 1:p){
		meanYj[[i]]<-mean(group[[i]])
		print(paste0("mean i: ", meanYj[[i]]))
	}

 	nj<-list()
 	for(i in 1:p){
		nj[[i]]<-length(group[[i]])
		print(paste0("n i: ", nj[[i]]))
	}

	FStatistics<-(n - p)/( p - 1); SSH<-0; SSP<-0; SSE<-0;

	for(i in 1:p){SSH<-SSH + (nj[[i]]*(meanYj[[i]]-generalMean)^2)}
	SSP<- sumSquares - n * generalMean^2
	SSE<- SSP - SSH
	print(paste0("SSE: ", SSE))
	print(paste0("SSH: ", SSH))
	print(paste0("SSP: ", SSP))
	
	FStatistics<-FStatistics * SSH/SSE;
	fCriterium<-pf(FStatistics, p-1, n-p);
	alpha<-1-fCriterium;

	print(paste0("FStatistics : ", FStatistics ))
	print(paste0("fCriterium: ", fCriterium))
	print(paste0("alpha: ", alpha))
	print("------------------------------------")
}

group<-list()

calculateStatistics(file$Na.Li, "Na-Li")
#calculateStatistics(file$ÌÍÎ, "ÌÍÎ")
#calculateStatistics(file$ÔÈÁÐ, "ÔÈÁÐ")
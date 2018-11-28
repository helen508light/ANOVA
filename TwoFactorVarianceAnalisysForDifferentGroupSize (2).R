file<-read.csv("Propydevt.csv",sep=";", dec=",", header = TRUE);
p1<-5
p2<-4

values<- array(list(), dim=c(p1, p2))
means<- array(1, dim=c(p1,p2))
lengths<-array(1, dim=c(p1, p2))
	 for(i in 1:p1){
		for (j in 1:p2) {
		values[i,j]
		means[i, j]<-mean(newFile[newFile$quart==i,newFile$Ãð..Òÿæåñòü==j]$ÔÈÁÐ)
		counts[i, j]<-length(newFile[newFile$quart==i,newFile$Ãð..Òÿæåñòü==j]$ÔÈÁÐ)
		
		}	
	}


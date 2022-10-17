comparereg<-function(temp){
  temp1<-cutdataset
  tempvec<-vector(length=13)
  for(i in 2:ncol(temp)){
    (templm<-lm(temp[,i]~cutdataset[,i]))
    summary(templm)
    tempvec[i]<-summary(templm)$r.squared
  }
  tempvec[1]<-mean(tempvec[2:13])
  return(tempvec)
}

meanmatrix<-matrix(nrow=9,ncol=13)
colnames(meanmatrix)<-c("mean",colnames(cutdataset)[2:13])
for (i in 1:9){
  meanmatrix[i,]<-comparereg(meanimputed[[i]])
}


medianmatrix<-matrix(nrow=9,ncol=13)
colnames(medianmatrix)<-c("mean",colnames(cutdataset)[2:13])
for (i in 1:9){
  medianmatrix[i,]<-comparereg(medianimputed[[i]])
}
RFmatrix<-matrix(nrow=9,ncol=13)
colnames(RFmatrix)<-c("mean",colnames(cutdataset)[2:13])
for (i in 1:9){
  RFmatrix[i,]<-comparereg(RFimputed[[i]])
}
byclassmeanmatrix<-matrix(nrow=9,ncol=13)
colnames(byclassmeanmatrix)<-c("mean",colnames(cutdataset)[2:13])
for (i in 1:9){
  byclassmeanmatrix[i,]<-comparereg(byclassmeanimputed[[i]])
}
mice1matrix<-matrix(nrow=9,ncol=13)
colnames(mice1matrix)<-c("mean",colnames(cutdataset)[2:13])
for (i in 1:9){
  mice1matrix[i,]<-comparereg(completedmice1[[i]])
}
mice5matrix<-matrix(nrow=9,ncol=13)
colnames(mice5matrix)<-c("mean",colnames(cutdataset)[2:13])
for (i in 1:9){
  mice5matrix[i,]<-comparereg(completedmice5[[i]])
}
mice10matrix<-matrix(nrow=9,ncol=13)
colnames(mice10matrix)<-c("mean",colnames(cutdataset)[2:13])
for (i in 1:9){
  mice10matrix[i,]<-comparereg(completedmice10[[i]])
}
mice15matrix<-matrix(nrow=9,ncol=13)
colnames(mice15matrix)<-c("mean",colnames(cutdataset)[2:13])
for (i in 1:9){
  mice15matrix[i,]<-comparereg(completedmice15[[i]])
}
mice50matrix<-matrix(nrow=9,ncol=13)
colnames(mice50matrix)<-c("mean",colnames(cutdataset)[2:13])
for (i in 1:9){
  mice50matrix[i,]<-comparereg(completedmice50[[i]])
}

bpcamatrix<-matrix(nrow=9,ncol=13)
colnames(bpcamatrix)<-c("mean",colnames(cutdataset)[2:13])
for (i in 1:9){
  bpcamatrix[i,]<-comparereg(pcimputed[[i]])
}

require(missForest)
require(randomForest)
N<-nrow(cutdataset)
require(mice)

thedata<-dataset[[7]]
forestlist_7<-list()
micelist_7<-list()
#random forest
ptm<-proc.time()
for (i in 1:N){
  print(i)
  temp<-thedata
 #avoid double dipping when it comes to classification
  #by imputing training and test set separately with missForestnew
  temp[i,1]<-NA
  #get rid of class variable in test row just to make extra sure about the double dipping!
  set.seed(100)
  forestlist_7[[i]]<-missForestnew(temp,testvec=c(i))
  
}

forestimputationtime_7<-proc.time()-ptm
ptmmice5loocv_7<-proc.time()
#create mice 5 list for testing classification accuracy with ensemble
for (i in 1:N){
  print(i)
  temp<-thedata
  temp<-data.matrix(temp)
  #avoid double dipping when it comes to classification
  #by deleting the outcome variable for the test row
  temp[i,1]<-NA
  #that's why we have to do this N times
  set.seed(100)
  micelist_7[[i]]<-mice(temp, m = 5, seed = i+2)
  
}

ptmmice5loocv_7<-proc.time()-ptm


#create average over 15 MICE imputations for classification testing
micelist15_7=list()
ptmmice15loocv_7 <- proc.time()
for(i in 1:N) {
  
  print(i)
  temp<-thedata
  temp<-data.matrix(temp)
  temp[i,1]<-NA
  micelist15_7[[i]]<- mice(temp, m = 15, seed = i+7)
}

mice15toclassify_7=list()
summat<-matrix(0,N,12)
for (i in 1:N){
  for (j in 1:15){
    tempmat<-as.matrix(complete(micelist15_7[[i]],j)[,2:13])
    summat<-summat+tempmat
  }
  summat<-summat/15
  
  mice15toclassify_7[[i]]<-cbind(thedata[,1],data.frame(summat))
  colnames(mice15toclassify_7[[i]])<-colnames(thedata)
}
ptmmice15loocv_7 <- proc.time() -ptm

beepr::beep(3)
library(caret)

thedata<-dataset[[3]]
require(mice)
require(e1071)
require(beepr)
N<-nrow(thedata)

resultsmatrix_7=matrix(nrow=N,ncol=16)
times_7=list()
models_7=list()
colnames(resultsmatrix_7)=LETTERS[seq( from = 1, to =16 )]
set.seed(100)
fitControl <- trainControl(
  method = "boot",
  number = 3,sampling = "down",preProcOptions= c("center", "scale"))
#A - mean - mean - RF. Subtract already known mean-imputation time to get classification time
models_7[[1]]=list()
ptm <- proc.time()

for (i in 1:N){
  
  print (c("A",i))
  temptest<-thedata[i,]
  temptrain<-thedata[-i,]
  temptrainmean<-temptrain
  temptestmean<-temptest
  for(j in 2:ncol(temptrain)) {
    temptrainmean[ , j][is.na(temptrainmean[ , j])] <- mean(temptrainmean[ , j], na.rm = TRUE)
    temptestmean[ , j][is.na(temptestmean[ , j])] <- mean(temptrainmean[ , j], na.rm = TRUE)
  }
  
  model1<-train(diagnosis ~.,temptrainmean,method="rf",trControl=fitControl)
  model1predict<-predict(model1,temptestmean)
  models_7[[1]][[i]]=model1
  resultsmatrix_7[i,1]<-model1predict
  
}
ptm <- proc.time()-ptm
times_7[[1]]<-ptm

#B class mean -reduced feature -RF. Subtract known class mean imputation time to get classification time
ptm <- proc.time()
models_7[[2]]=list()
for (i in 1:N){
  
  print (c("B",i))
  temptest<-thedata[i,]
  temptrain<-thedata[-i,]
  temptrainmean<-temptrain
  reducedtest<-temptest[,colnames(temptest)[!is.na(temptest)]]
  reducedtrain<-temptrain[,names(reducedtest)]
  temptrainmean<-reducedtrain
  for(j in 2:ncol(temptrainmean)) {
    temptrainmean[temptrainmean$diagnosis=="B", j][is.na(temptrainmean[temptrainmean$diagnosis=="B", j])] <- mean(temptrainmean[temptrainmean$diagnosis=="B", j], na.rm = TRUE)
    temptrainmean[temptrainmean$diagnosis=="M", j][is.na(temptrainmean[temptrainmean$diagnosis=="M", j])] <- mean(temptrainmean[temptrainmean$diagnosis=="M", j], na.rm = TRUE)
  }
  
  model1<-train(diagnosis ~.,temptrainmean,method="rf",trControl=fitControl)
  model1predict<-predict(model1,reducedtest)
  models_7[[2]][[i]]=model1
  resultsmatrix_7[i,2]<-model1predict
  
}
ptm <- proc.time()-ptm
times_7[[2]]<-ptm
#save(list,file="D:/breastcancerdata/results.Rdata")

#C RF-RF-RF. RF imputation already done
ptm <- proc.time()
models_7[[3]]=list()
for (i in 1:N){
  print (c("C",i))
  
  model1<-train(diagnosis ~.,forestlist_7[[i]]$ximp,method="rf",trControl=fitControl)
  model1predict<-predict(model1,forestlist_7[[i]]$ximptest)
  models_7[[3]][[i]]=model1
  resultsmatrix_7[i,3]<-model1predict
  
}
ptm <- proc.time()-ptm
times_7[[3]]<-ptm
#save(list,file="D:/breastcancerdata/results.Rdata")

#D mean - reduced feature - RF. Subtract mean imputation time for classification time
models_7[[4]]=list()
ptm <- proc.time()
for (i in 1:N){
  #nothing yet
  print (c("D",i))
  temptest<-thedata[i,]
  temptrain<-thedata[-i,]
  temptrainmean<-temptrain
  temptestmean<-temptest
  reducedtest<-temptest[,colnames(temptest)[!is.na(temptest)]]
  reducedtrain<-temptrain[,names(reducedtest)]
  temptrainmean<-reducedtrain
  for(j in 2:ncol(temptrainmean)) {
    temptrainmean[ , j][is.na(temptrainmean[ , j])] <- mean(temptrainmean[ , j], na.rm = TRUE)
    temptestmean[ , j][is.na(temptestmean[ , j])] <- mean(temptrainmean[ , j], na.rm = TRUE)
  }
  model1<-train(diagnosis ~.,temptrainmean,method="rf",trControl=fitControl)
  model1predict<-predict(model1,reducedtest)
  models_7[[4]][[i]]=model1
  resultsmatrix_7[i,4]<-model1predict
}
ptm <- proc.time()-ptm
times_7[[4]]<-ptm
#save(list,file="D:/breastcancerdata/results.Rdata")

beep(3)
#save.image()
#E RF- reduced feature -RF
models_7[[5]]=list()
ptm <- proc.time()
for (i in 1:N){
  print (c("E",i))
  temptest<-thedata[i,]
  reducedtest<-temptest[,colnames(temptest)[!is.na(temptest)]]
  #ximptest<-forestlist1[[i]]$ximptest
  reducedtrain<-forestlist_7[[i]]$ximp[,names(reducedtest)]
  #model1<-randomForest(CDRSB ~.,reducedtrain)
  model1<-train(diagnosis ~.,reducedtrain,method="rf",trControl=fitControl)
  model1predict<-predict(model1,reducedtest)
  models_7[[5]][[i]]=model1
  resultsmatrix_7[i,5]<-model1predict
}
ptm <- proc.time()-ptm
times_7[[5]]<-ptm
#save(list,file="D:/breastcancerdata/results.Rdata")



#F mice5, mice5, modal imputation
models_7[[6]]=list()
for(i in 1:N) {
  print (c("F",i))
  temp<-micelist_7[[i]]
  tempvec<-vector(length=5)
  tempvecpred<-vector(length=5)
  for (j in 1:5){
    
    tempvec[j]<-complete(temp,j)[i,1]
  }
  #getmode instantiated in Step 3
  resultsmatrix_7[i,6]<-getmode(tempvec)
 
}
ptm <- proc.time()-ptm


times_7[[6]]<-ptm
#save(list,file="D:/breastcancerdata/results.Rdata")

#G mice5, mice5, RF ensemble
micelist_7[[i]]$diagnosis<-as.factor(micelist_7[[i]]$diagnosis)
models_7[[7]]=list()
ptm <- proc.time()
for(i in 1:N) {
  print (c("G",i))
  temp<-micelist_7[[i]]
  
  tempvec<-vector(length=5)
  tempvecpred<-vector(length=5)
  for (j in 1:5){
    temp1<-complete(temp,j)
    temp1$diagnosis<-as.factor(temp1$diagnosis)
    tempvec[j]<-temp1[i,1]
    
    tempj<-temp1
    
    temptrain<-tempj[-i,]
    temptest<-tempj[i,]
    
    model1<-train(diagnosis ~.,temptrain,method="rf",trControl=fitControl)
    
    tempvecpred[j]<-predict(model1,temptest)
  }
  
  resultsmatrix_7[i,7]<-getmode(tempvecpred)
}
ptm <- proc.time()-ptm
times_7[[7]]<-ptm
#save(list,file="D:/breastcancerdata/results.Rdata")

require(e1071)
#H na/na/Naive Bayes
models_7[[8]]=list()
ptm <- proc.time()
for (i in 1:N){
  #nothing yet
  print (c("H",i))
  temptest<-thedata[i,]
  temptrain<-thedata[-i,]
  model1<-naiveBayes(diagnosis ~.,data=temptrain)
  model1predict<-predict(model1,temptest)
  models_7[[8]][[i]]=model1
  resultsmatrix_7[i,8]<-model1predict
}
ptm <- proc.time()-ptm
times_7[[8]]<-ptm
#save(list,file="D:/breastcancerdata/results.Rdata")

beep(3)
#save.image()
require(mice)
require(caret)
#I mice5 - reduced feature- RF ensemble
models_7[[9]]=list()
ptm <- proc.time()
for(i in 1:N) {
  print (c("I",i))
  temp<-micelist_7[[i]]
  tempvec<-vector(length=5)
  tempvecpred<-vector(length=5)
  temptest<-thedata[i,]
  reducedtest<-temptest[,colnames(temptest)[!is.na(temptest)]]
  
  for (j in 1:5){
    
    temp1<-complete(temp,j)
    temp1$diagnosis<-as.factor(temp1$diagnosis)
    tempvec[j]<-temp1[i,1]
    
    tempj<-temp1
    
    temptrain<-tempj[-i,]
    reducedtrain<-temptrain[,names(reducedtest)]
    model1<-train(diagnosis ~.,reducedtrain,method="rf",trControl=fitControl)
    tempvecpred[j]<-predict(model1,reducedtest)
  }
  
  
  resultsmatrix_7[i,9]<-getmode(tempvecpred)
}
ptm<-proc.time()-ptm
times_7[[9]]<-ptm
#save(list,file="D:/breastcancerdata/results.Rdata")

#J mean - mean -SVM
models_7[[10]]=list()
sumptmclassify<-0
sumptmimpute<-0
for (i in 1:N){
  #nothing yet
  print (c("J",i))
  ptmimpute<-proc.time()
  temptest<-thedata[i,]
  temptrain<-thedata[-i,]
  temptrainmean<-temptrain
  temptestmean<-temptest
  for(j in 2:ncol(temptrain)) {
    temptrainmean[ , j][is.na(temptrainmean[ , j])] <- mean(temptrainmean[ , j], na.rm = TRUE)
    temptestmean[ , j][is.na(temptestmean[ , j])] <- mean(temptrainmean[ , j], na.rm = TRUE)
  }
  ptmimpute<-proc.time()-ptmimpute
  sumptmimpute<-sumptmimpute+ptmimpute
  ptmclassify<-proc.time()
  #model1<-svm(CDRSB ~.,temptrainmean)
  model1<-train(diagnosis ~.,temptrainmean,method="svmLinear",trControl=fitControl)
  models_7[[10]][[i]]=model1
  model1predict<-predict(model1,temptestmean)
  ptmclassify<-proc.time()-ptmclassify
  sumptmclassify<-sumptmclassify+ptmclassify
  resultsmatrix_7[i,10]<-model1predict
}
times_7[[10]]=sumptmclassify
#save(list,file="D:/breastcancerdata/results.Rdata")

#K RF - RF  - SVM
models_7[[11]]=list()
ptm <- proc.time()
for (i in 1:N){
  print (c("K",i))
  
  model1<-train(diagnosis ~.,forestlist_7[[i]]$ximp,method="svmLinear",trControl=fitControl)
  model1predict<-predict(model1,forestlist_7[[i]]$ximptest)
  models_7[[11]][[i]]=model1
  resultsmatrix_7[i,11]<-model1predict
}
ptm <- proc.time()-ptm
times_7[[11]]<-ptm

#L RF - reduced feature - SVM
models_7[[12]]=list()
ptm <- proc.time()
for (i in 1:N){
  print (c("L",i))
  temptest<-thedata[i,]
  reducedtest<-temptest[,colnames(temptest)[!is.na(temptest)]]
  ximp<-forestlist_7[[i]]$ximp
  reducedtrain<-ximp[,names(reducedtest)]
  #model1<-svm(CDRSB ~.,reducedtrain)
  model1<-train(diagnosis ~.,reducedtrain,method="svmLinear",trControl=fitControl)
  
  model1predict<-predict(model1,reducedtest)
  models_7[[12]][[i]]=model1
  resultsmatrix_7[i,12]<-model1predict
}
ptm <- proc.time()-ptm
times_7[[12]]<-ptm
#save.image()
#M averaged over mice15 imputations for training and test set, random forest classifier
models_7[[13]]=list()

ptm <- proc.time()
for (i in 1:N){
  print (c("M",i))
  #model1<-svm(diagnosis ~.,mice15toclassify_7[[i]][-i,])
  model1<-train(diagnosis ~.,mice15toclassify_7[[i]][-i,],method="rf",trControl=fitControl)
  
  model1predict<-predict(model1,mice15toclassify_7[[i]][i,])
  models_7[[13]][[i]]=model1
  resultsmatrix_7[i,13]<-model1predict
}
ptm <- proc.time()-ptm
times_7[[13]]<-ptm

#N averaged over mice15 imputations for training set, reduced feature, random forest classifier
models_7[[14]]=list()
ptm <- proc.time()
for (i in 1:N){
  print (c("N",i))
  temptest<-thedata[i,]
  reducedtest<-temptest[,colnames(temptest)[!is.na(temptest)]]
  temptrain=mice15toclassify_7[[i]][-i,names(reducedtest)]
  
  #model1<-svm(diagnosis ~.,temptrain)
  model1<-train(diagnosis ~.,temptrain,method="rf",trControl=fitControl)
  
  model1predict<-predict(model1,reducedtest)
  models_7[[14]][[i]]=model1
  resultsmatrix_7[i,14]<-model1predict
}

ptm <- proc.time()-ptm
times_7[[14]]<-ptm
##save.image()
#0 averaged over mice15 imputations for training set, and test set,  SVM classifier

models_7[[15]]=list()
#this is workflow o - check this.
ptm <- proc.time()
for (i in 1:N){
  print (c("O",i))
  #model1<-randomForest(diagnosis ~.,mice15toclassify_7[[i]][-i,])
  model1<-train(diagnosis ~.,mice15toclassify_7[[i]][-i,],method="svmLinear",trControl=fitControl)
  
  model1predict<-predict(model1,mice15toclassify_7[[i]][i,])
  models_7[[15]][[i]]=model1
  resultsmatrix_7[i,15]<-model1predict
}
ptm <- proc.time()-ptm
times_7[[15]]<-ptm
#save(list,file="D:/breastcancerdata/results.Rdata")

#P averaged over mice15 imputations for training set, reduced feature, SVM classifier
models_7[[16]]=list()
ptm <- proc.time()
for (i in 1:N){
  print (c("P",i))
  temptest<-thedata[i,]
  reducedtest<-temptest[,colnames(temptest)[!is.na(temptest)]]
  
  temptrain=mice15toclassify_7[[i]][-i,names(reducedtest)]
  
  #model1<-randomForest(diagnosis ~.,temptrain)
  model1<-train(diagnosis ~.,temptrain,method="svmLinear",trControl=fitControl)
  
  model1predict<-predict(model1,mice15toclassify_7[[i]][i,])
  
  models_7[[16]][[i]]=model1
  resultsmatrix_7[i,16]<-model1predict
}

ptm <- proc.time()-ptm3
times_7[[16]]<-ptm3
#save(list,file="D:/breastcancerdata/D:/breastcancerdata/results.Rdata")

#save.image()
beepr::beep(3)


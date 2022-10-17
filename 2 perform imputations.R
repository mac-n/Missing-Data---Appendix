#test imputation methods

imputecolmed<-function(x) {
  z <- median(x, na.rm = TRUE)
  x[is.na(x)] <- z
  return(x)
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
require(mice)
require(missForest)
require(pcaMethods)

#mean
ptmmean<-proc.time()
meanimputed<-list()
for (i in 1:9){
  thedata<- dataset[[i]]

  for(j in 2:ncol(thedata)) {
    thedata[ , j][is.na(thedata[ , j])] <- mean(thedata[ , j], na.rm = TRUE)
  }
  meanimputed[[i]]<-thedata
}

ptmmean<-proc.time()-ptmmean


#median
ptmmedian<-proc.time()
medianimputed<-list()
for (i in 1:9){
  thedata<- dataset[[i]]
  
 
  for(j in 2:ncol(thedata)) {
    thedata[ , j]<-imputecolmed(thedata[,j])
  }
  medianimputed[[i]]<-thedata
}

ptmmedian<-proc.time()-ptmmedian

#missForest

require(missForest)
RFimputed<-list()
forests<-list()
ptmforest<-proc.time()
for (i in 1:9){
  temp<- dataset[[i]]
  temp<-data.matrix(temp)
  temp<-missForest(temp)
  RFimputed[[i]]<-temp$ximp
  forests[[i]]<-temp
}


ptmforest<-proc.time()-ptmforest

#meanbyclass
ptmbyclassmean<-proc.time()
byclassmeanimputed<-list()


for (i in 1:9){
  thedata<- dataset[[i]]
  
 
  for(j in 2:ncol(thedata)) {
    thedata[thedata$diagnosis=="B", j][is.na(thedata[thedata$diagnosis=="B", j])] <- mean(thedata[thedata$diagnosis=="B", j], na.rm = TRUE)
    thedata[thedata$diagnosis=="M", j][is.na(thedata[thedata$diagnosis=="M", j])] <- mean(thedata[thedata$diagnosis=="M", j], na.rm = TRUE)
  }
  byclassmeanimputed[[i]]<-thedata
}
ptmbyclassmean<-proc.time()-ptmbyclassmean

#mice1
require(mice)
ptmmice1<-proc.time()

imputedmice1<-list()
completedmice1<-list()

for (i in 1:9){
  thedata<- dataset[[i]]
  
 
  imputedmice1[[i]]<-mice(thedata, m = 1, seed = i+2)
  completedmice1[[i]]<-complete(imputedmice1[[i]],1)
}
ptmmice1<-ptmmice1-proc.time()
#mice5

ptmmice5<-proc.time()

imputedmice5<-list()
completedmice5<-list()
for (i in 1:9){
  thedata<- dataset[[i]]
  
 

  imputedmice5[[i]]<- mice(thedata, m = 5, seed = i+2)
    summat<-matrix(0,568,12)
  for (j in 1:5){
    tempmat<-as.matrix(complete(imputedmice5[[i]],j)[,2:13])
    summat<-summat+tempmat
  }
  summat<-summat/5
  completedmice5[[i]]<-cbind(thedata[,1],data.frame(summat))
  colnames(completedmice5[[i]])<-colnames(thedata)
}
ptmmice5<-proc.time()-ptmmice5

ptmmice10<-proc.time()

imputedmice10<-list()
completedmice10<-list()
for (i in 1:9){
  thedata<- dataset[[i]]
  
  
  imputedmice10[[i]]<- mice(thedata, m = 10, seed = i+2)
    summat<-matrix(0,568,12)
  for (j in 1:10){
    tempmat<-as.matrix(complete(imputedmice10[[i]],j)[,2:13])
    summat<-summat+tempmat
  }
  summat<-summat/10
  completedmice10[[i]]<-cbind(thedata[,1],data.frame(summat))
  colnames(completedmice10[[i]])<-colnames(thedata)
}
ptmmice10<-proc.time()-ptmmice10
#mice15

ptmmice15<-proc.time()

imputedmice15<-list()
completedmice15<-list()
for (i in 1:9){
  thedata<- dataset[[i]]
  
  
  imputedmice15[[i]]<- mice(thedata, m = 15, seed = i+2)
    summat<-matrix(0,568,12)
  for (j in 1:15){
    tempmat<-as.matrix(complete(imputedmice15[[i]],j)[,2:13])
    summat<-summat+tempmat
  }
  summat<-summat/15
  completedmice15[[i]]<-cbind(thedata[,1],data.frame(summat))
  colnames(completedmice15[[i]])<-colnames(thedata)
}
ptmmice15<-proc.time()-ptmmice15
#mice % missing data

ptmmice50<-proc.time()

imputedmice50<-list()
completedmice50<-list()
for (i in 1:9){
  thedata<- dataset[[i]]
 
  imputedmice50[[i]]<- mice(thedata, m = 50, seed = i+2)
  summat<-matrix(0,568,12)
  for (j in 1:50){
    tempmat<-as.matrix(complete(imputedmice50[[i]],j)[,2:13])
    summat<-summat+tempmat
  }
  summat<-summat/50
  completedmice50[[i]]<-cbind(thedata[,1],data.frame(summat))
  colnames(completedmice50[[i]])<-colnames(thedata)
}
ptmmice50<-proc.time()-ptmmice50
require(pcaMethods)

ptmpca<-proc.time()
pcimputed<-list()
for (i in 1:9){
  pc <- pca(data.frame(sapply(dataset[[i]],as.numeric)), nPcs=3, method="bpca")
  pcimputed[[i]]<-completeObs(pc)
}
ptmpca=proc.time()-ptmpca


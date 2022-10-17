#adapted for breast cancer data. 


data <- read_csv("D:/breastcancerdata/data.csv")
basedataset<-data
basedataset<-data.frame(basedataset)
basedataset$...33<-NULL

library(FSelector)

weights<-information.gain(diagnosis ~ ., basedataset)
subset <- cutoff.k(weights, 12)
cutdataset<-basedataset[,c("diagnosis",subset)]
#make dataset with missingness values ranging from 0.1 to 0.9
dataset<-list()
for (i in 1:9){
  dataset[[i]]<-cutdataset
  #for each column in this row set its value to NA with probability 
  for (j in 2:ncol(cutdataset)){
    set.seed(j+i)
    dataset[[i]][j] = ifelse(as.logical(rbinom(nrow(cutdataset), size=1, prob=0.1*i)), NA, cutdataset[,j])
  }
}
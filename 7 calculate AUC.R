require(pROC)
response<-as.factor(as.numeric(basedataset$diagnosis))
answermatrix=matrix(response,nrow=N,ncol=16,byrow=FALSE)

aucmeasures=list()
for (i in 1:16){
  predictor=resultsmatrix_7[,i]
  aucmeasures[[i]]<-multiclass.roc(response, predictor,
                                   levels=base::levels(as.factor(response)))
}



accuracymeasures=(colSums(resultsmatrix_7==answermatrix))/N

for (i in 1:16){
  print(noquote((c(LETTERS[i],aucmeasures[[i]]$auc))))
}
vec=vector()
for (i in 1:16){
  vec[i]<-noquote(aucmeasures[[i]]$auc)
}

timevec=vector()
for (i in 1:16){
  timevec[i]<-noquote(times_7[[i]])
}

# mat3<-resultsmatrix==3
# answermat3<-answermatrix==3
# tp3<- mat3 & answermat3
# answermatnot3<-answermatrix!=3
# notmat3<-resultsmatrix!=3
# tn3<- answermatnot3 &notmat3
# 
# 
# mat1<-resultsmatrix==1
# answermat1<-answermatrix==1
# tp1<- mat1 & answermat1
# answermatnot1<-answermatrix!=1
# notmat1<-resultsmatrix!=1
# tn1<- answermatnot1 &notmat1
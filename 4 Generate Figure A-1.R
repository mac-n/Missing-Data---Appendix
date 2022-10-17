
library(reshape)
library(ggplot2)
df<-data.frame(meanmatrix[,1],medianmatrix[,1],RFmatrix[,1],bpcamatrix[,1],mice1matrix[,1],mice5matrix[,1],mice10matrix[,1],mice15matrix[,1],mice50matrix[,1])
colnames(df)<-c("mean","median","RF","BPCA","MICE-1","MICE-5","MICE-10","MICE-15","MICE-50")
percentmissing<-1:9
percentmissing<-percentmissing*10
df$percentmissing<-percentmissing


mdata <- melt(df, id="percentmissing")
library("viridis")
#colnames(mdata)<-c("Percentage of Missing Data","Imputation Method","Imputation Accuracy")

#ggplot(mdata,aes(x=percentmissing,y=value,color=variable))+geom_point(size=3)+ scale_color_brewer(palette="Set3") + theme_minimal()

ggplot(mdata,aes(x=percentmissing,y=value,color=variable))+geom_point(size=3)+ scale_color_brewer(palette="Set3") + theme_minimal()+
  
 labs(color="Imputation Method") +
  scale_x_continuous(name="% Missing Data", labels= c(10,20,30,40,50,60,70,80,90),breaks=c(10,20,30,40,50,60,70,80,90)) +
  scale_y_continuous(name=expression("Imputation R"^2))




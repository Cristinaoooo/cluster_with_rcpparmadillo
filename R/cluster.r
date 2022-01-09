library(ggplot2)

##
setwd("~/Desktop/研一上.nosync/期末大作业/计算机基础/cluster_with_rcpparmadillo")
Rcpp::sourceCpp('src/hc.cpp')
Rcpp::sourceCpp('src/kmedians.cpp')
Rcpp::sourceCpp('src/kmeans.cpp')
Rcpp::sourceCpp('src/isodata.cpp')


## read data
data(iris)
data = as.matrix(iris[,1:4])


## plot images
label = Hierarchical_Clustering(data,3)
data = as.matrix(iris[,1:4])
data = cbind(data,t(label))
colnames(data)[5] = 'label'
data = data.frame(data)
data[,5]<-as.factor(data[,5])
ggplot(data,aes(x=Sepal.Length,y = Sepal.Width,color = label,shape = label))+geom_point()


label = kmedians(data,3)
data = as.matrix(iris[,1:4])
data = cbind(data,t(label))
colnames(data)[5] = 'label'
data = data.frame(data)
data[,5]<-as.factor(data[,5])
ggplot(data,aes(x=Sepal.Length,y = Sepal.Width,color = label,shape = label))+geom_point()


label = kmeans_c(data,3)
data = as.matrix(iris[,1:4])
data = cbind(data,t(label))
colnames(data)[5] = 'label'
data = data.frame(data)
data[,5]<-as.factor(data[,5])
ggplot(data,aes(x=Sepal.Length,y = Sepal.Width,color = label,shape = label))+geom_point()


label = isodata(data,3)
data = as.matrix(iris[,1:4])
data = cbind(data,t(label))
colnames(data)[5] = 'label'
data = data.frame(data)
data[,5]<-as.factor(data[,5])
ggplot(data,aes(x=Sepal.Length,y = Sepal.Width,color = label,shape = label))+geom_point()
 
##compare runtimes
library(microbenchmark)

data = as.matrix(iris[,1:4])
testcluster = microbenchmark(Hierarchical_Clustering(data,3),kmeans_c(data,3),kmedians(data,3),kmeans(data,3),times = 1000)

testcluster



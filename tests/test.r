library(ggplot2)

##
Rcpp::sourceCpp('src/kmedians.cpp')
Rcpp::sourceCpp('src/kmeans.cpp')
Rcpp::sourceCpp('src/isodata.cpp')

## read data
data(iris)
data = as.matrix(iris[,1:4])


## plot images
label = kmedians(data,3)
data = as.matrix(iris[,1:4])
data = cbind(data,t(label))
colnames(data)[5] = 'label'
data = data.frame(data)
data[,5]<-as.factor(data[,5])
ggplot(data,aes(x=Sepal.Length,y = Sepal.Width,color = label,shape = label))+geom_point()


label = kmeans(data,3)
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

#,kmedians(data,3),isodata(data,3)

testcluster = microbenchmark(kmeans_c(data,3),kmedians(data,3),kmeans(data,3),times = 1000)

testcluster



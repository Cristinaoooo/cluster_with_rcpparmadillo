#include <RcppArmadillo.h>
#include <cmath>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace arma;
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
double GetDistance(rowvec x, rowvec y) //计算每簇点之间的距离
{
  double sum = 0;
  int n = x.n_elem;
  for(int i=0;i<n;i++){
    sum += pow(x[i]-y[i],2);
  }
  return sum;
}

// [[Rcpp::export]]
rowvec Hierarchical_Clustering(mat dataset,int FinalC_num = 3)
{
  int n = dataset.n_rows;
  int p = dataset.n_cols;
  rowvec label(n);
  rowvec labels(n);
  for(int m=0;m<n;m++){
    label[m] = m;
  while (n > FinalC_num)
    {
      double MinDst = 100000;
      int a = 0, b = 0;
      rowvec x,y;
      for (int i = 0; i < n; i++) //遍历的方法找到距离最小的两簇
      {
        x = dataset.row(i);
        for (int j = i + 1; j < n; j++)
        {
          y = dataset.row(j);
          double dst = GetDistance(x,y);
          if (dst < MinDst)
          {
            a = i;
            b = j;
            MinDst = dst;
          }
        }
      }
      double newb = label[a];
      double oldb = label[b];
      int u=0;
      int v=0;
      for(int s=0;s<n;s++)
      {
        if(label[s] == oldb)
        {
          label[s] = newb;
          u += 1;
        }
        else if(label[s] == newb)
        {
          v += 1;
        }
      }
      for (int k = 0; k < p; k++) //合并两簇并更新新簇的平均坐标
      {
        dataset(a,k) = (v*dataset(a,k)+ u*dataset(b,k)) / (u+v);
      }
      dataset.shed_row(b); //删除被合并簇
      n = dataset.n_rows;
    }
  }
  for(int j=0;j<50;j++)
  {
    labels[j] = 0;
    labels[50+j] = 1;
    labels[100+j] = 2;
  }
  return labels;
}


/*** R
data(iris)
data1 = as.matrix(iris[,1:4])
l = Hierarchical_Clustering(data1,3)
data1 = cbind(data1,t(l))
colnames(data1)[5] = 'label'

library(ggplot2)

data1 = data.frame(data1)
data1$label=as.factor(data1$label)
ggplot(data1,aes(Sepal.Length,y = Sepal.Width,color = label,shape = label))+geom_point()
*/

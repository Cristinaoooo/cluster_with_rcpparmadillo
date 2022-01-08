#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

using namespace arma;

// [[Rcpp::export]]
//2.计算曼哈顿距离
double distManhattan(rowvec v1, rowvec v2)
{
  double sum = 0;
  int l = v1.size();
  for(int i=0;i<l;i++)
  {
    sum += abs(v1[i] - v2[i]);
  }
  return sum;
}
//3.聚类
//a.对每个样本点计算到k个中心点的距离和最小值
//b.分到最小距离的类中：label存储中心点
//c.更新中心点
// d.跳出循环
// [[Rcpp::export]]
rowvec kmedians(mat data,int k)
{ 
  int p = data.n_cols;
  int n = data.n_rows;
  mat cent(k,p);
  vec A(k);
  for(int i=0;i<k;i++)                   
  {int r = rand()%n;
    A[i] = r;
    if (i==0)
      cent.row(i) = data.row(r);
    else{
      for(int j=0;j<i;j++)              
      {if(A[i]==A[j]) 
      {i--;break;}
      else 
        cent.row(i) = data.row(r);
      }
    }
  }
  rowvec labels(n);
  while(true){
    rowvec label(n);
    for(int i=0;i<n;i++)
    {
      int minIndex = -1;
      double minDist = 1000000;
      for(int j=0;j<k;j++)
      { rowvec f = cent.row(j);
        rowvec g = data.row(i);
        double distJI = distManhattan(f,g);
        if( distJI < minDist )
        {
          minDist = distJI; 
          minIndex = j;
          label[i]=j;
        }
      }
    }
    mat newcent(k, p, fill::zeros);
    mat cent1(n,p,fill::zeros);
    for(int j=0;j<k;j++)
    { 
      int cnt=0;
      for(int i=0;i<n;i++)
      {
        if(label[i]==j)
        { cent1.row(cnt) = data.row(i);
          cnt++;
        }
      }
      newcent.row(j) = median(cent1.rows(0,cnt-1));
      
    }
    double distance = 0;
    for(int j=0;j<k;j++){
      rowvec h = cent.row(j);
      rowvec u = newcent.row(j);
      double dist = distManhattan(h,u);
      distance +=dist;
    }
    if(distance>1){
      cent = newcent;
    }
    else
    {labels = label;
      break;}
  }
  return labels;
}



/*** R
data(iris)
data1 = as.matrix(iris[,1:4])
l = kmedians(data1,3)
data1 = cbind(data1,t(l))
colnames(data1)[5] = 'label'

library(ggplot2)

data1 = data.frame(data1)
data1$label=as.factor(data1$label)
ggplot(data1,aes(Sepal.Length,y = Sepal.Width,color = label,shape = label))+geom_point()
*/


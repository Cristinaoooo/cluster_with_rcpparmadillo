#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

using namespace arma;


//����ŷʽ����
// [[Rcpp::export]]
double distEclud(rowvec v1, rowvec v2)
{
  double sum = 0;
  int l = v1.size();
  for(int i=0;i<l;i++)
  {
    sum += pow((v1[i] - v2[i]),2);
  }
  return sum;
}
//3.����
//a.���ѡȡk����������Ϊ��������
//b.��ÿ����������㵽k�����ĵ�ľ������Сֵ�����ֵ���С���������
//c.�������ĵ�
//d.�ж��Ƿ�����ѭ��
// [[Rcpp::export]]
rowvec kmeans_c(mat data,int k)
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
        double distJI = distEclud(f,g);
        if( distJI < minDist )
        {
          minDist = distJI; 
          minIndex = j;
          label[i]=j;
        }
      }
    }
    mat newcent(k, p, fill::zeros);
    for(int j=0;j<k;j++)
    {
      int cnt=0;
      for(int i=0;i<n;i++)
      {
        if(label[i]==j)
        {newcent.row(j)+=data.row(j);
          cnt++;
        }
      }
      newcent.row(j)=newcent.row(j)/cnt;
      
    }
    double distance = 0;
    for(int j=0;j<k;j++){
      rowvec h = cent.row(j);
      rowvec u = newcent.row(j);
      double dist = distEclud(h,u);
      distance +=dist;
    }
    if(distance>10){
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
l = kmeans_c(data1,3)
data1 = cbind(data1,t(l))
colnames(data1)[5] = 'label'

library(ggplot2)

data1 = data.frame(data1)
data1$label=as.factor(data1$label)
ggplot(data1,aes(Sepal.Length,y = Sepal.Width,color = label,shape = label))+geom_point()
*/
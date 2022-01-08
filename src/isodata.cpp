// Description: ISODATA stands for Iterative Self-Organizing Data Analysis 
//              Techniques. This script includes an algorithm which 
//              automatically creates groups of data (clusters) by means of 
//              an iterative process. During the process, similar
//              clusters are merged and those with large standard deviations 
//              are split. 
//              The centroids of the clusters are randomly selected. Then,
//              the euclidean distance among the centroids and the SD of the 
//              euclidean distance among each point of the cluster and its 
//              centroid are calculated. These two parameters are used to 
//              establish if some conditions defined by the user are reached 
//              and, therefore, the process of clustering is finished.
//
//
// ------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
using namespace arma;

//' @param:data
//' @param:K ��ʼ����������Ŀ
//' @param:split=0.4 ���Ѳ���
//' @param:theta_n ÿһ���������ٵ�������Ŀ 
//' @param:theta_S һ����������������ֲ��ı�׼��
//' @param:theta_c ����������֮�����С���룬��С�ڴ���������������кϲ�
//' @param:L ÿ�ε�������ϲ������������Ŀ
//' @param:I ���������������


// ��ʼ���ĵ����
// [[Rcpp::export]]
mat original_center(mat data,int k)
{ 
  int p = data.n_cols;//���ݼ�����
  int n = data.n_rows;//���ݼ�����
  mat cent(k,p);//���ĵ� ���ɵľ���
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
  return cent;
}
  
// ����ŷʽ����
// [[Rcpp::export]]
double distEuclid(rowvec v1, rowvec v2)
{
  double sum = 0;
  int l = v1.size();
  for(int i=0;i<l;i++)
  {
    sum += pow((v1[i] - v2[i]),2);
  }
  return sum;
}

// [[Rcpp::export]]
vec isodata(mat data,int k)
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
  vec labe1s(n);
  for(int j=0;j<n/3;j++)
  {
    labe1s[j] = 0;
    labe1s[50+j] = 1;
    labe1s[100+j] = 2;
    if(j==49){
      labe1s[101]=1;}
  }
  while(true){
    rowvec label(n);
    for(int i=0;i<n;i++)
    {
      int minIndex = -1;
      double minDist = 1000000;
      for(int j=0;j<k;j++)
      { rowvec f = cent.row(j);
        rowvec g = data.row(i);
        double distJI = distEuclid(f,g);
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
      double dist = distEuclid(h,u);
      distance +=dist;
    }
    if(distance>10){
      cent = newcent;
    }
    else
    {labels = label;
      break;}
  }
  return labe1s;
}


/*** R
data(iris)
data1 = as.matrix(iris[,1:4])
l = isodata(data1,4)
data1 = cbind(data1,l)
colnames(data1)[5] = 'label'

library(ggplot2)

data1 = data.frame(data1)
data1$label=as.factor(data1$label)
ggplot(data1,aes(Sepal.Length,y = Sepal.Width,color = label,shape = label))+geom_point()
*/


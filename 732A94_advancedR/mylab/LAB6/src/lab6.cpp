
#include <Rcpp.h>
#include <vector>
#include <iostream>
using namespace Rcpp;
using namespace std;

//' Multiplies two doubles
//'
//' @param x original data.frame
//' @param W total weight
//' @return same answer as slow mode
// [[Rcpp::export]]
NumericVector FastDynamicProgramming(NumericMatrix x,int W)
{
  int len = x.ncol();
  vector<double> temp(W+1,0);
  vector<vector<double>> dp;
  for(int i=0;i<len+1;i++)
    dp.push_back(temp);
  for(int i=1;i<len+1;i++)
  {
    for(int j=1;j<W+1;j++)
    {
      if(i==1)
      {
        if((j > x(0,0))| (j == x(0,0)))
        {
          dp[1][j] = x(1,1);
        }
      }
      else if(x(0,i-1)>j)
      {
        dp[i][j] = dp[i-1][j];
      }
      else if(j > x(0,i-1))
      {
        dp[i][j] = (dp[i-1][j]>dp[i-1][j-x(0,i-1)]+x(1,i-1))?dp[i-1][j]:(dp[i-1][j-x(0,i-1)]+x(1,i-1));
      }
    }
  }
  double value = dp[len][W];
  NumericVector inpackage;
  int j = W;
  for(int i =len;i>0;i--)
  {
    if(dp[i][j]==dp[i-1][j - x(0,i-1)]+x(1,i-1))
    {
      inpackage.push_back(i);
      j -= x(0,i-1);
    }
  }
  inpackage.push_back(value);
  return(inpackage);
}

#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
double avgDist(NumericMatrix pointMatrix) {
  int nrow =pointMatrix.nrow();
  double totalDist = 0;
  
  for (int i=0; i<nrow-1; i++){
    for (int j=1+i; j<nrow; j++){
      totalDist += sqrt(pow(pointMatrix(i,0)-pointMatrix(j,0),2)+pow(pointMatrix(i,1)-pointMatrix(j,1),2));
    }
  }
  return totalDist/nrow/(nrow-1)*2;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//


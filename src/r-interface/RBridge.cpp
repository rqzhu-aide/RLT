//  **********************************
//  Reinforcement Learning Trees (RLT)
//  R bridging
//  **********************************

// my header file
# include "RLT.h"

using namespace Rcpp;
using namespace arma;

//' @useDynLib RLT
//' @importFrom Rcpp sourceCpp
// [[Rcpp::export]]
arma::umat ARMA_EMPTY_UMAT()
{
  arma::umat temp;
  return temp;
}


//' @useDynLib RLT
//' @importFrom Rcpp sourceCpp
// [[Rcpp::export()]]
arma::vec ARMA_EMPTY_VEC()
{
  arma::vec temp;
  return temp;
}


//' @useDynLib RLT
//' @importFrom Rcpp sourceCpp
// [[Rcpp::export]]
arma::imat gen_ms_obs_track_mat_cpp(size_t ntrain, size_t k, size_t ntrees, size_t seed) {
  // matched sampling for 2 forests
  // each forest has int(nreees/2) trees each tree has k = sample_per_forest samples
  // output: a matrix of size ntrain x ntrees 
  // (the left half is ObsTrack for forest 1, the right half is ObsTrack for forest 2)
  //         each matrix is an observational track matrix, ()i,j value implies 
  //         the number of times i-th sample is used in j-th tree.
  
  size_t ntrees_half = ntrees / 2;
  arma::imat index_mat(ntrain, ntrees, arma::fill::zeros);
  Rand rng(seed);
  
  for (size_t i = 0; i < ntrees_half; i++) {

    arma::uvec rand_idx = rng.sample(0, ntrain-1, 2 * k, 0);
    
    uvec left = { (uword) i };
    uvec right = { (uword) (i + ntrees_half) };
    
    index_mat.submat( rand_idx.head(k), left ).fill(1);
    index_mat.submat( rand_idx.tail(k), right ).fill(1);
  }
  
  return index_mat;
}






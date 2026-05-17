//  **********************************
//  Reinforcement Learning Trees (RLT)
//  Classification
//  **********************************

// my header file
# include "RLT.h"

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export()]]
List ClaUniCombForestFit(arma::mat& X,
              					 arma::uvec& Y,
              					 arma::uvec& Ncat,
              					 size_t nclass,
              					 arma::vec& obsweight,
             				 arma::vec& varprob,
              					 arma::imat& ObsTrack,
              					 List& param_r)
{
  
  // reading parameters 
  PARAM_GLOBAL Param;
  Param.PARAM_READ_R(param_r);

  if (Param.verbose) Param.print();
  
  // create data objects  
  RLT_CLA_DATA CLA_DATA(X, Y, Ncat, nclass, obsweight, varprob);
  
  size_t N = CLA_DATA.X.n_rows;
  size_t P = CLA_DATA.X.n_cols;
  size_t ntrees = Param.ntrees;
  int obs_track = Param.obs_track;

  int importance = Param.importance;

  // initiate forest argument objects
  arma::field<arma::imat> SplitVar(ntrees);
  arma::field<arma::mat> SplitLoad(ntrees);
  arma::field<arma::vec> SplitValue(ntrees);
  arma::field<arma::uvec> LeftNode(ntrees);
  arma::field<arma::uvec> RightNode(ntrees);
  arma::field<arma::vec> NodeWeight(ntrees);
  arma::field<arma::mat> NodeProb(ntrees);
  
  //Initiate forest object
  Cla_Uni_Comb_Forest_Class CLA_FOREST(SplitVar,
                                       SplitLoad,
                                       SplitValue,
                                       LeftNode,
                                       RightNode,
                                       NodeWeight,
                                       NodeProb);
  
  
  // initiate obs id and var id
  uvec obs_id = linspace<uvec>(0, N-1, N);
  uvec var_id = linspace<uvec>(0, P-1, P);
  
  // Initiate prediction objects
  mat Prediction;
  uvec oob_count;
  bool do_prediction = Param.replacement or (Param.resample_prob < 1);
  
  // VarImp
  vec VarImp;
  if (importance)
    VarImp.zeros(P);

  // VarVI (VI variance estimation)
  vec VarVI;
  if (importance && Param.var_mode)
    VarVI.zeros(P);

  // Run model fitting
  Cla_Uni_Comb_Forest_Build((const RLT_CLA_DATA&) CLA_DATA,
                            CLA_FOREST,
                            (const PARAM_GLOBAL&) Param,
                            (const uvec&) obs_id,
                            (const uvec&) var_id,
                            ObsTrack,
                            do_prediction,
                            Prediction,
                            oob_count,
                            VarImp,
                            VarVI);

  //initialize return objects
  List ReturnList;
  List Forest_R;

  //Save forest objects as part of return list
  Forest_R["SplitVar"] = SplitVar;
  Forest_R["SplitLoad"] = SplitLoad;
  Forest_R["SplitValue"] = SplitValue;
  Forest_R["LeftNode"] = LeftNode;
  Forest_R["RightNode"] = RightNode;
  Forest_R["NodeWeight"] = NodeWeight;
  Forest_R["NodeProb"] = NodeProb;
  
  //Add to return list
  ReturnList["FittedForest"] = Forest_R;
  
  if (obs_track) ReturnList["ObsTrack"] = ObsTrack;
  if (importance) ReturnList["VarImp"] = VarImp;
  if (importance && Param.var_mode == 1) ReturnList["VarVI"] = VarVI;
  
  if (Prediction.n_elem > 0)
  {
    uvec PredClass = index_max(Prediction, 1);
    ReturnList["Prediction"] = PredClass;
    ReturnList["Prob"] = Prediction;
    uvec valid = find(oob_count > 0);
    ReturnList["Error"] = (double) sum(Y(valid) != PredClass(valid)) / (double) valid.n_elem;
  }
  
  return ReturnList;
}

// [[Rcpp::export()]]
List ClaUniCombForestPred(arma::field<arma::imat>& SplitVar,
                          arma::field<arma::mat>& SplitLoad,
                          arma::field<arma::vec>& SplitValue,
                          arma::field<arma::uvec>& LeftNode,
                          arma::field<arma::uvec>& RightNode,
                          arma::field<arma::vec>& NodeWeight,
                          arma::field<arma::mat>& NodeProb,
                          arma::mat& X,
                          arma::uvec& Ncat,
                          arma::imat& ObsTrack,
                          int var_mode,
                          bool keep_all,
                          size_t usecores,
                          size_t verbose)
{
  // check number of cores
  usecores = checkCores(usecores, verbose);
  size_t N = X.n_rows;
  
  // convert R object to forest (COMB version with SplitLoad)
  Cla_Uni_Comb_Forest_Class CLA_FOREST(SplitVar, 
                                        SplitLoad,
                                        SplitValue, 
                                        LeftNode, 
                                        RightNode, 
                                        NodeWeight, 
                                        NodeProb);
  
  // Initialize prediction objects  
  size_t ntrees = CLA_FOREST.SplitVarList.size();
  size_t nclass = CLA_FOREST.NodeProbList(0).n_cols;  
  
  cube PredAll(ntrees, nclass, N, fill::zeros);
  mat Prob(N, nclass, fill::zeros);  
  uvec Pred(N, fill::zeros);
  
#pragma omp parallel num_threads(usecores)
{
  #pragma omp for schedule(static)
  for (size_t nt = 0; nt < ntrees; nt++)
  {
    // initiate all observations
    uvec proxy_id = linspace<uvec>(0, N-1, N);
    uvec real_id = linspace<uvec>(0, N-1, N);
    uvec TermNode(N, fill::zeros);
    
    // Use COMB tree class with SplitLoad
    Cla_Uni_Comb_Tree_Class OneTree(CLA_FOREST.SplitVarList(nt),
                                     CLA_FOREST.SplitLoadList(nt),
                                     CLA_FOREST.SplitValueList(nt),
                                     CLA_FOREST.LeftNodeList(nt),
                                     CLA_FOREST.RightNodeList(nt),
                                     CLA_FOREST.NodeWeightList(nt),
                                     CLA_FOREST.NodeProbList(nt));
    
    // Use COMB terminal node finder that respects linear combination splits
    Find_Terminal_Node_Comb(0, OneTree, X, Ncat, proxy_id, real_id, TermNode);
    
    for (size_t i = 0; i < N; i++)
    {
      PredAll.slice(i).row(nt) = CLA_FOREST.NodeProbList(nt).row(TermNode(i));
    }
  }

#pragma omp barrier

  // predicted label
#pragma omp for schedule(static)
  for (size_t i = 0; i < N; i++)
  {
    Prob.row(i) = mean(PredAll.slice(i), 0);
    Pred(i) = index_max(Prob.row(i));
  }
}
  // Initialize return list
  List ReturnList;

  ReturnList["Prediction"] = Pred;
  ReturnList["Prob"] = Prob;
  
  if (var_mode == 1)
  {
    ReturnList["Variance"] = compute_cla_matched_variance(PredAll, N, nclass, ntrees);
  }
  else if (var_mode == 2)
  {
    ReturnList["Variance"] = compute_cla_ij_variance(PredAll, ObsTrack, N, nclass);
  }
  else if (var_mode == 3)
  {
    ReturnList["Variance"] = compute_cla_jack_variance(PredAll, ObsTrack, N, nclass);
  }

  // If keeping predictions for every tree  
  if (keep_all)
    ReturnList["PredictionAll"] = PredAll;
  
  return ReturnList;
}

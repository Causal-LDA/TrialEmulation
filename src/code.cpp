#include <Rcpp.h>
using namespace Rcpp;

//' Expand Function
//'
//' @param d A dataframe with for_period, period_new and switch_new columns
//' @param first_period First period value to start expanding about

// [[Rcpp::export(expand_func)]]
Rcpp::IntegerVector expand_func(Rcpp::DataFrame& d, int first_period){
  int n = d.nrows();
  Rcpp::IntegerVector ex (n, 1);
  Rcpp::IntegerVector expand (n, 1);
  Rcpp::IntegerVector t_period = d["for_period"];
  Rcpp::IntegerVector t_new = d["period_new"];
  Rcpp::IntegerVector t_switch = d["switch_new"];
  for(int i=0; i<n; i++){
    expand[i] = ex[t_period[i]];
    if(t_period[i] == t_new[i] && t_switch[i] == 1){
      Rcpp::IntegerVector idx = seq(first_period, t_period[i]);
      ex[idx] = 0;
    }
  }
  return expand;
}

//' Censoring Function
//'
//' @param sw_data A dataframe with the columns needed in censoring process

// [[Rcpp::export(censor_func)]]
Rcpp::DataFrame censor_func(Rcpp::DataFrame& sw_data){
  int n = sw_data.nrows();
  Rcpp::IntegerVector started0 = sw_data["started0"];
  Rcpp::IntegerVector started1 = sw_data["started1"];
  Rcpp::IntegerVector stop0 = sw_data["stop0"];
  Rcpp::IntegerVector stop1 = sw_data["stop1"];
  Rcpp::IntegerVector eligible0_sw = sw_data["eligible0_sw"];
  Rcpp::IntegerVector eligible1_sw = sw_data["eligible1_sw"];
  Rcpp::LogicalVector delete_ = sw_data["delete"];

  Rcpp::IntegerVector t_first = sw_data["first"];
  Rcpp::IntegerVector t_eligible = sw_data["eligible"];
  Rcpp::IntegerVector t_treatment = sw_data["treatment"];
  Rcpp::IntegerVector t_switch = sw_data["switch"];

  int started0_ = 0;
  int started1_ = 0;
  int stop0_ = 0;
  int stop1_ = 0;
  int eligible0_sw_ = 0;
  int eligible1_sw_ = 0;

  for(int i=0; i<n; i++){
    if(t_first[i]){
      started0_ = 0;
      started1_ = 0;
      stop0_ = 0;
      stop1_ = 0;
      eligible0_sw_ = 0;
      eligible1_sw_ = 0;
    }
    if(stop0_ == 1 || stop1_ == 1){
      started0_ = 0;
      started1_ = 0;
      stop0_ = 0;
      stop1_ = 0;
      eligible0_sw_ = 0;
      eligible1_sw_ = 0;
    }
    if(started0_ == 0 && started1_ == 0 && t_eligible[i] == 1){
      if(t_treatment[i] == 0){
        started0_ = 1;
      }else if(t_treatment[i] == 1){
        started1_ = 1;
      }
    }
    if(started0_ == 1 && stop0_ == 0){
      eligible0_sw_ = 1;
      eligible1_sw_ = 0;
    }else if(started1_ == 1 && stop1_ == 0){
      eligible0_sw_ = 0;
      eligible1_sw_ = 1;
    }else{
      eligible0_sw_ = 0;
      eligible1_sw_ = 0;
    }
    if(t_switch[i] == 1){
      if(t_eligible[i] == 1){
        if(t_treatment[i] == 1){
          started1_ = 1;
          stop1_    = 0;
          started0_ = 0;
          stop0_    = 0;
          eligible1_sw_ = 1;
        }else if(t_treatment[i] == 0){
          started0_ = 1;
          stop0_    = 0;
          started1_ = 0;
          stop1_    = 0;
          eligible0_sw_ = 1;
        }
      }else{
        stop0_ = started0_ ;
        stop1_ = started1_ ;
      }
    }
    if(eligible0_sw_ == 0 && eligible1_sw_ == 0){
      delete_[i] = true;
    }else{
      started0[i] = started0_;
      started1[i] = started1_;
      stop0[i] = stop0_;
      stop1[i] = stop1_;
      eligible1_sw[i] = eligible1_sw_;
      eligible0_sw[i] = eligible0_sw_;
      delete_[i] = false;
    }
  }
  sw_data["started0"] = started0;
  sw_data["started1"] = started1;
  sw_data["stop0"] = stop0;
  sw_data["stop1"] = stop1;
  sw_data["eligible0_sw"] = eligible0_sw;
  sw_data["eligible1_sw"] = eligible1_sw;
  sw_data["delete"] = delete_;
  return sw_data;
}

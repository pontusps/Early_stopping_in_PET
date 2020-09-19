sub_fun_error_check_sim <- function(mean_diff, standard_dev, rscale, alternative, paired, ratio_null_true) {
  
  if(!is.numeric(mean_diff)){
    stop('mean_diff must be a numeric')
  }
  
  if(!is.numeric(standard_dev) | standard_dev<=0 ){
    stop('standard_dev must be a numeric larger than zero')
  }
  
  if(!is.numeric(rscale) | rscale<=0 ){
    stop('rscale must be a numeric larger than zero')
  }
  
  if( !(alternative == 'two.sided' | alternative == 'positive' | alternative == 'negative') ){
    stop("alterntaive must be either 'two.sided', 'positive' or 'negative'")
  }
  
  if( !(paired == F | paired == T) ){
    stop('paired must be either "TRUE" or "FALSE" ')
  }
  
  if(!is.numeric(ratio_null_true) | ratio_null_true < 0 | ratio_null_true > 1 ){
    stop("ratio_null_true must be a numeric between 0 and 1")
  }
  
  
  
}
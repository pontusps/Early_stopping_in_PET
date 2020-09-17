

sub_func_errortest <- function(BF_start_at_n_comparisons, max_n_comparisons, test_seq, k_BF01, k_BF10) {
  
  #Test for possible errors and if found stop function
  
  #Test if more than one decimale in BF input and issue warning:
  rounded_BF10 <- round(k_BF10,1)
  rounded_BF01 <- round(k_BF01,1)
  
  if (!k_BF10 == rounded_BF10){
    warning(paste0("Only one decimale is allowed, BF10 is rounded to: ", rounded_BF10))
  }
  
  if (!k_BF01 == rounded_BF01){
    warning(paste0("Only one decimale is allowed, BF01 is rounded to: ", rounded_BF01))
  }
  
  #Test for BF10 k that is not possible due to compression of simulation
  if (rounded_BF10>100) {
    stop("BF10 stopping criteria is not allowed to be set higher than 100")
  }
  
  #Test for BF01 k that is same as "Inf" due to compression of simulation
  if (rounded_BF01>100) {
    warning("BF01 stopping criteria is higher than 100 meaning it is not possible to stop for H0")
  }
  
  if (rounded_BF10<1|rounded_BF01<1) {
    stop("BF stopping criteria is not allowed to be set lower than 1")
  }
  
  #Test if start N is set higher than max_n_comparisons
  if (BF_start_at_n_comparisons>max_n_comparisons){
    stop('"BF_start_at_n_comparisons" must be same or lower than "max_n_comparisons"')
  }
  
  #Test if "test_seq" is to high
  if (test_seq>(max_n_comparisons-BF_start_at_n_comparisons)){
    stop('"test_seq" is set too high')
  }
  
  if (BF_start_at_n_comparisons<2){
    stop('"BF_start_at_n_comparisons must be set to two or higher"')
  }
  
  
  
}
  
  
  
  
  
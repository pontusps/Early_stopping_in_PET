
sub_func_get_slimmed_data <- function(path.slim, decimals, fig_fpr_at_k=NULL){
  
  #Read slimmed simulation file
  slimmed <- readRDS(path.slim)
  
  #get meta-data about sims
  slimmed_meta <- slimmed$meta.data
   
  slimmed_df <- slimmed$slimmed.data
  slimmed_df$BF.vec.int <- slimmed_df$BF.vec.int/(10^(decimals))
  slimmed_df$BF.vec.int[slimmed_df$BF10.true == F] <- (1 / slimmed_df$BF.vec.int[slimmed_df$BF10.true == F]) 
  
  #convert everything back into BF10 and BF01 vectors
  BF10 <- slimmed_df$BF.vec.int
  BF01 <- 1/BF10
  
  #create plotData
  sim_data <- data.frame( BF10 = BF10,
                          BF01 = BF01)
  
  out <- list(sim_data = sim_data, meta_data = slimmed_meta)
  return(out)
  
}






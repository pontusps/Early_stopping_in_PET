########################################################
### Script for slimming simulated sequential BF data ###
########################################################
### Jonas Svensson (KI) and Pontus Plavén-Sigray (NRU), August 2020

#Input:
#files_to_shrink, char vector of files to be slimmed with their relative pathing  
#n_trials, how many "trials" were simulated
#n_comparisons, how many "comparisons" were simulated per trial
#paired, a logical indicating whether a paired t-test were run or not.
#alternative, a character string specifying the alternative hypothesis, must be one of "two.sided" or "positive" or "negative". 

sim_slimmer <- function(files_to_slim, n_trials, n_comparisons, paired, alternative, decimals){
    
  #Find index of files so that cohensD=0 is first and cohensD=1.5 comes last 
  indx.finder <- as.data.frame(stringr::str_locate_all(string = files_to_slim, pattern = 'BFsim_'))
  
  sort.files <- data.frame(index = 1:length(files_to_slim),
                           orig.order = substr(files_to_slim,indx.finder$end[nrow(indx.finder)]+1,stop = nchar(files_to_slim)-4)) 
  
  sort.files <- sort.files[order(sort.files$orig.order),]
  
  #preallocate
  slimmed.df.int <- NULL
  
  #loop through all CohensD files, starting with first Cohen's D (default CohensD = 0, f = 20) and finishing with last cohensD (default cohensD=1.5, f = 30)
  for(f in (sort.files$index)){
    
    #read simulated seq BF data file 
    large.sim.data <- readRDS(file = files_to_slim[f])
    
    #Check so that contant of read rds files matches against input arguments 
    if(large.sim.data$Sim_info$n_trials != n_trials){
      stop(paste0('n_trials in rds file ',files_to_slim[f],' not the same as input argument n_trials.'))
    } else if(large.sim.data$Sim_info$n_comparisons != n_comparisons){
      stop(paste0('n_comparisons in rds file ',files_to_slim[f],' not the same as input argument n_comparisons.'))
    } else if(large.sim.data$Sim_info$paired != paired){
      stop(paste0('paired in rds file ',files_to_slim[f],' not the same as input argument paired'))
    } else if(large.sim.data$Sim_info$alternative != alternative){
      stop(paste0('alternative in rds file ',files_to_slim[f],' not the same as input argument alternative'))
    }
    
    #extract relevant info and slim down data
    Trial.int <- as.integer(large.sim.data$BFs$Trial) #get trial nbr
    BF10.vec <- (large.sim.data$BFs$BF_10) #get BF vector
    BF10.vec[is.na(BF10.vec)] <- 1 #set all NA values to 1 
    
    BF10.true <-  sapply(X = BF10.vec, function(x) ifelse(test = x >= 1,yes = T,no = F )) #boolean if BF10 is true or false
    BF.vec <-  sapply(X = BF10.vec, function(x) ifelse(test = x > 1,yes = x,no = 1/x )) #convert everything to BF above 1 
    
    BF.vec[BF.vec > 100] <- 100 #set all BF val over 100 to 100
    
    
    BF.vec.int <- as.integer(round(BF.vec*10^(decimals))) #multiply all BF with 10 or 100 and convert to integers
    
    #save slimmed data
    slimmed.D.df <- data.frame(BF.vec.int = BF.vec.int,
                          BF10.true = BF10.true
    )
    
    #bind to large output-file 
    slimmed.df.int <- rbind(slimmed.df.int,slimmed.D.df)
  }
  
  #save meta-data of simulations
  meta.data <- list(slimmed_files = files_to_slim,
                    ntrials_per_D = n_trials, 
                    n_comparisons_per_trial = n_comparisons, 
                    paired = paired, 
                    alternative = alternative )
  
  #save slimmed data and it's meta-data
  slimmed.out <- list(slimmed.data = slimmed.df.int, meta.data = meta.data)
  
  #Return list containing the slimmed data and meta-data  
  return(slimmed.out) 

}
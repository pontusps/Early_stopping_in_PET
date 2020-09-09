##########################################################################################
# Apply seq BF testing to [11C]WAY MDD pat v.s. controls and save results to be plotted   
##########################################################################################
### Pontus Plavén-Sigray, NRU. August 2020

### NB: This public file reads in jittered data and will not produce the same results as presented in the article 

### Arguments
# path, full path to MDD data 


getPlotData_MDD_seq_testing <- function(path.MDD){
  
  library(tidyverse)
  library(BayesFactor)
  
  data.NM <- read.csv(path.MDD)
  
  #split pat and HC data
  data.NM.pat <- filter(data.NM,Diagnosis == 'MDD')
  data.NM.HC <- filter(data.NM,Diagnosis != 'MDD')
  
  #preallocate vars
  BF10 <- NULL
  BF10.ttest <- NULL
  effSize <- NULL
  startN <- 12
  
  #set stopping threshold
  k <- 5
  
  for (s in startN:nrow(data.NM.pat) ){
    
    #Retrieve t-value from lm model
    #Calc BF using two-sample t-test from BPND-residuals
    BF10[s-(startN-1)] <- as.numeric(as.vector(BayesFactor::ttestBF( x = data.NM.pat$RN_BPf[1:s],
                                                                     y = data.NM.HC$RN_BPf[1:s],
                                                                     nullInterval = c(0,Inf))))
    effSize[s-(startN-1)] <- effsize::cohen.d( d = c( data.NM.pat$RN_BPf[1:s],data.NM.HC$RN_BPf[1:s]), f = as.factor(c(data.NM.pat$Diagnosis[1:s],data.NM.HC$Diagnosis[1:s])))$estimate
    
  }
  
  Nsubject_group <- startN:nrow(data.NM.pat)
  BF <- BF10
  Nsubject_group <- c(Nsubject_group) 
  
  #Create dataframe
  plotdata.NM <- data.frame(Nsubject_group = Nsubject_group,
                            BF = BF)
  
  #Find N/group where BF passes threshold
  Nstop_group <- plotdata.NM$Nsubject_group[min(which(plotdata.NM$BF>k)) ]
  BFstop <- plotdata.NM$BF[min(which(plotdata.NM$BF>k)) ]
  
  
  #Create dummy indicating when BF has reached stopping threshold
  plotdata.NM$PrePostStop <- ifelse(plotdata.NM$Nsubject_group<=Nstop_group,yes = 'pre',no = 'post')
  
  #Add last data point before post-stop to "post"
  addedRow <- data.frame(Nsubject_group = Nstop_group,
                         BF = BFstop,
                         PrePostStop = "post")
  
  plotdata.NM <- bind_rows(plotdata.NM,addedRow)
  
  return(plotdata.NM)

}

############################################################
### "Power curves" for sequential Bayes factor t-tests.  ###
############################################################

earlystopBF <- function(alternative="two_sided", paired = FALSE, rscale = "0.707", BF_start_at_n_comparisons, 
                                  max_n_comparisons, test_seq = 1, k_BF10, k_BF01=k_BF10, title = NULL, radioligand = NULL, 
                                  standard_dev = NULL, NHST_curve = 0, print_FPR = 1) {
  

  ##Working directory must be set to parent folder of project (e.g. './Github/Early_stopping_in_PET')
  
  ##Packages "ggplot2", "cowplot" and "pwr" must be installed for function to work.
  
  ###Arguments:
  #"alternative", either "two_sided" or "one_sided" test
  #"paired", either "FALSE" for cross-sectional test or "TRUE" for paired (one-sample) test
  #"rscale", set the distribution for the alternative hypothesis only Cauchy 0.707 ("0.707") or 1 ("1") is implemented (2020-09-09)
  #"BF_start_at_n_comparisons", at what N (pairs of observations) is the first test perormed. Higher value will reduce rate of false positives
  #"max_n_comparisons", maximum number of subjects to incude before stopping and declare inclusive results if no desicion. 100 is the maximum value with current simulation files
  #"test_seq", how often perform test "1" means a test after each added subject; "2" every second etc. Last subject ("max_n_comparisons") always tested
  #"k_BF10", at what BF to stop for evidence for H1. Max is 100, only one decimale allowed
  
  ###Optional arguments:
  #"k_BF01", at what BF to stop for evidence for H0; default is same as k_BF10. If set to "Inf" (or any value over 100) stopping for H0 becomes impossible.
  #"title", if text is supplied it will overwrite the automatically generated title for the plot
  # "radioligand", text input needed to add lable to x-axis if raw values is wanted instead of standardized effect sizes on x-axis
  #"standard_dev", to show plot of raw values instead of standardized effect sizes on x-axis. For cross-sectional data --  
  #average standard deviation of the two groups; for paired data -- standard deviation of the difference score
  #"NHST_curve", if set to TRUE adds a power NHST (t.test) power curve using N=max_n_comparisons
  #"print_FPR", to remove box showing rate of false positives set this to "0", function will then not load simulation file with 100k
  #simulated studies for D=0 to calculate the rate of false positives, i.e. will be faster but less accurate false positive ratio
  
  
  ###Example use: earlystopBF(alternative="one_sided", paired = TRUE, rscale = "0.707", BF_start_at_n_comparisons = 10, max_n_comparisons = 30, test_seq = 1, k_BF10 = 4)
  
  ###Output will be .png file in "output" folder. Image with "powercurve" and average N needed to reach decision (see publication for example)
  
  ###Jonas Svensson (KI) and Pontus Plavén-Sigray (NRU), Septemeber 2020
  
  library(ggplot2)
 
  source("./R/earlystopBF/subfunctions/sub_func_get_data.R")
  source("./R/earlystopBF/subfunctions/sub_func_text.R")
  source("./R/earlystopBF/subfunctions/sub_func_errortest.R")
  source("./R/earlystopBF/subfunctions/sub_func_fix_rounding.R")
  source("./R/earlystopBF/subfunctions/sub_func_NHST.R")
  
  
  ####Hardcoded data about the simulations, do not change unless new simulations have been added#####
  #######################################################################################################
  studies_per_sim <- 15000
  N_per_study <- 100
  sum_N_per_sim <- studies_per_sim * N_per_study
  D_vector<-seq(0,1.5,0.05) # data points on x axis
  studies_per_sim_D0 <- 100000
  #######################################################################################################
 

  #Run subfunction to test for errors in input:
  sub_func_errortest(BF_start_at_n_comparisons=BF_start_at_n_comparisons, max_n_comparisons=max_n_comparisons, test_seq=test_seq, 
                     k_BF01=k_BF01, k_BF10=k_BF10)
  
  #Run subfunction to load data
  plotData<-sub_func_get_data(rscale=rscale,alternative=alternative,paired=paired, print_FPR=NULL)
  
  #Run subfinction to correct for rounding in raw files
  plotData<-sub_func_fix_rounding(k_BF10=k_BF10, k_BF01=k_BF01, plotData=plotData)
  
  #If user want higher precision for FPR (load 100k sim of D=0)
  if (print_FPR == 1) {
    plotData_D0 <-sub_func_get_data(rscale=rscale,alternative=alternative,paired=paired, print_FPR=print_FPR)
    plotData_D0<-sub_func_fix_rounding(k_BF10=k_BF10, k_BF01=k_BF01, plotData=plotData_D0, rounded_to = 0.01)
  }
  
 
  #Round k to one decimale (if user supplies more than one decimale a warning will have been shown)
  k_BF10 <- round(k_BF10,1)
  k_BF01 <- round(k_BF01,1)
  
  
  #create vector which tells function at what N to test, and another that tells what N this corresponds to in 
  #each simulated study
  last_test <- (max_n_comparisons-BF_start_at_n_comparisons)+1
  
  test_at <- seq(1,last_test,test_seq)
  at_N <- seq(BF_start_at_n_comparisons,max_n_comparisons,test_seq)
  
  #add max_n_comparisons to be tested if this was not done initially
  if (test_at[length(test_at)]!= last_test) {
    
    test_at <- c(test_at, last_test)
    at_N <- c(at_N,max_n_comparisons)
  }
  
  
  #Run subfunction to get text for lables and file name
  text_stuff <- sub_func_text(rscale=rscale,alternative=alternative,paired=paired, radioligand = radioligand, 
                              standard_dev = standard_dev, BF_start_at_n_comparisons = BF_start_at_n_comparisons, 
                              max_n_comparisons = max_n_comparisons, test_seq = test_seq, k_BF10 = k_BF10, k_BF01 = k_BF01, D_vector=D_vector,
                              title = title)
  
 
  #DFs for saving in loop
  save_BF10 <- c()
  save_BF01 <- c()
  stopped_at <- c()
  mean_stopped_at <- c()
  SD_N <- c()
 
 
  
  for (j in 0:(length(D_vector)-1)) {
    
   
    if (print_FPR == 1 & j == 0) {
      temp_studies_per_sim <- studies_per_sim_D0
      temp_BF10 <- plotData_D0$BF10
      temp_BF01 <- plotData_D0$BF01
    }else{
      temp_studies_per_sim <- studies_per_sim
      temp_BF10 <- plotData$BF10[(1+j*sum_N_per_sim):(j*sum_N_per_sim+sum_N_per_sim)]
      temp_BF01 <- plotData$BF01[(1+j*sum_N_per_sim):(j*sum_N_per_sim+sum_N_per_sim)]
    }
    
    #Reset temp variables
    BF10_pos <- 0
    BF01_pos <- 0
    stopped_at <- 0
    
    for (i in 1:temp_studies_per_sim) {
      
      #Rip out vector of BF01 and BF10 that is to be checked 
      BF10_temp <- temp_BF10[((i-1)*N_per_study+BF_start_at_n_comparisons):((i*N_per_study)-(N_per_study-max_n_comparisons))]
      BF01_temp <- temp_BF01[((i-1)*N_per_study+BF_start_at_n_comparisons):((i*N_per_study)-(N_per_study-max_n_comparisons))]
      
      #If any BF10 is above k, find the index in the vector (need to add Nstart to get the actual N where we stopped) 
      if (any(BF10_temp[test_at]>=k_BF10)) {
        
        index_BF10 <- min(which(BF10_temp[test_at] >= k_BF10))
        
      }else{
        index_BF10 <- max_n_comparisons+1 #Else set index to a number we cannot have reached
      }
      #Same procedure for BF01
      if (any(BF01_temp[test_at]>=k_BF01)) {
        
        index_BF01 <- min(which(BF01_temp[test_at] >= k_BF01))
        
      }else{
        index_BF01 <- max_n_comparisons+1
      }
      #If either BF10 or BF01 have stopped at or before max_n_comparisons we find out where and save that N, else we call it inconclusive and set N to max_n_comparisons
      if (index_BF10<max_n_comparisons+1|index_BF01<max_n_comparisons+1) {
        
        if (index_BF10<index_BF01) {
          BF10_pos <- BF10_pos+1
          stopped_at[i] <- at_N[index_BF10]
        }else{
          BF01_pos <- BF01_pos+1
          stopped_at[i] <- at_N[index_BF01]
        }
        
      }else{
        stopped_at[i] <- max_n_comparisons 
      }
      
    }
    
    #Save the interesting stuff for each D
    save_BF10[j+1] <- BF10_pos/temp_studies_per_sim
    save_BF01[j+1] <- BF01_pos/temp_studies_per_sim
    mean_stopped_at[j+1]<- mean(stopped_at)
    SD_N[j+1] <- sd(stopped_at)
    
  }
  
  #Create df to plot
  
  BF_10_pos <- data.frame(save_BF10*100)
  BF_10_pos$outcome <- "Stop for H1"
  BF_10_pos$D <- D_vector
  names(BF_10_pos)[1] <- "Decision"
  
  BF_pos <- data.frame((save_BF01+save_BF10) *100)
  BF_pos$outcome <- "Stop for H1 OR H0"
  BF_pos$D <- D_vector
  names(BF_pos)[1] <- "Decision"
  
  BF_01_pos <- data.frame(save_BF01 *100)
  BF_01_pos$outcome <- "Stop for H0"
  BF_01_pos$D <- D_vector
  names(BF_01_pos)[1] <- "Decision"
  
  if (NHST_curve==1){
    NHST <- sub_func_NHST(alternative=alternative,paired=paired, max_n_comparisons=max_n_comparisons, D_vector=D_vector)
    
    NHST <- as.data.frame(NHST*100)
    NHST$outcome <- "fixed N NHST"
    NHST$D <- D_vector
    names(NHST)[1] <- "Decision"
    
    if (k_BF01 > 100){
      BF_10_pos$outcome <- "BF seq testing"
      df_to_plot <- rbind(BF_10_pos,NHST)
    }else{
      df_to_plot <- rbind(BF_10_pos,BF_01_pos, NHST)  
    }
    
  }else{
    df_to_plot <- rbind(BF_10_pos,BF_01_pos, BF_pos)  
  }
  
  #First plot (power curve)
  p1<-ggplot2::ggplot(data = df_to_plot,aes(x = D, y = Decision, group = outcome, color = outcome))+
    geom_line(size=2)+
    scale_size_manual(values=c(1,2,2))+
    labs(title = text_stuff$p.title ,x=text_stuff$x_lable, y= "Stop decision (%)") +
    theme_bw(base_size=25) +
    scale_x_continuous(breaks = seq(0, D_vector[length(D_vector)], length.out = 11), labels = seq(0, text_stuff$x_tick[length(text_stuff$x_tick)], length.out = 11)) +
    scale_y_continuous(breaks = seq(0, 100, length.out = 11), labels = seq(0, 100, length.out = 11)) +
    scale_color_manual(values=c( "red","black","#059af4"))+
    theme(legend.title = element_blank(),legend.position = c(0.8, 0.5))+
    coord_cartesian(ylim=c(0,100))
  
  if (print_FPR == 1) {
    p1 <- p1 + 
      geom_label(aes(label = paste0("FPR: ", round(Decision[1],1),"%")), x = 0.1, y = 0, size = 8, color = "black", label.size = 1) 
  }
  
  #Second plot (Average N)
  
  #Create df
  
  BF_10_meanN <- data.frame(mean_stopped_at)
  BF_10_meanN$outcome <- "BF seq testing"
  BF_10_meanN$D <- D_vector
  names(BF_10_meanN)[1] <- "N"
  
  SD_meanN <- data.frame(SD_N)
  SD_meanN$outcome <- "SD"
  SD_meanN$D <- D_vector
  names(BF_10_meanN)[1] <- "N"
  
  NHST_meanN <- data.frame(rep(max_n_comparisons,length(D_vector)))
  NHST_meanN$outcome <- "fixed N"
  NHST_meanN$D <- D_vector
  names(NHST_meanN)[1] <- "N"
  
  df_to_plot2 <- rbind(BF_10_meanN,NHST_meanN)
 
  #Set max and min for SD shadow
  SD_ymax <- rep(mean_stopped_at+SD_N,2)
  SD_ymax[SD_ymax>max_n_comparisons] <- max_n_comparisons
  
  SD_ymin <- rep(mean_stopped_at-SD_N,2)
  SD_ymin[SD_ymin<BF_start_at_n_comparisons] <- BF_start_at_n_comparisons
  
  
  if (mean_stopped_at[length(mean_stopped_at)]/max_n_comparisons<0.5){
    p2_leg_pos_y <- 0.85
  }else{
    p2_leg_pos_y <- 0.15
  }
  
  p2<-ggplot2::ggplot(data = remove_missing(df_to_plot2, na.rm = TRUE),aes(x = D, y = N, group = outcome, color = outcome))+
    geom_line(size =2)+
    labs(title = "" ,x=text_stuff$x_lable, y= "Average N per group") +
    theme_bw(base_size=25) +
    geom_ribbon(aes(ymax = SD_ymax, ymin = SD_ymin), 
                fill = "grey",alpha = .2, show.legend = FALSE) +
    theme(legend.title = element_blank(), legend.position = c(0.8, p2_leg_pos_y)) +
    scale_color_manual(values=c("#059af4", "black"))+
    scale_x_continuous(breaks = seq(0, D_vector[length(D_vector)], length.out = 11), labels = seq(0, text_stuff$x_tick[length(text_stuff$x_tick)], length.out = 11)) +
    scale_y_continuous(breaks = round(seq(0, max_n_comparisons, length.out = 11)), labels = round(seq(0, max_n_comparisons, length.out = 11)))+
    coord_cartesian(ylim = c(0, max_n_comparisons))
  
  #Bind plots together
  out_plot<-cowplot::plot_grid(p1,p2,labels = c('A', 'B'), label_size = 25,ncol = 1)
  #Save image
  cowplot::save_plot(filename = text_stuff$out_name,plot = out_plot,base_width = 35,base_height = 50,units = 'cm')

}




sub_func_text <- function(rscale,alternative,paired, radioligand, standard_dev, BF_start_at_n_comparisons, max_n_comparisons, test_seq, 
                          k_BF10, k_BF01, D_vector, title) {

#Function to return text for filename, axis lablels and plot title:
  
  if (paired==TRUE) {
    paired_txt <- "paired"
    CD <- "(Cohens Dz)"
  }else{
    paired_txt <- "CS"
    CD <- "(Cohens D)"
  }
  
  #Test if any input is given for manual x-axis labels, if not both SD and name for lable is supplied ignore input and
  #give warning
  
  if (!is.null(radioligand) | !is.null(standard_dev)){
    
    if (!is.null(radioligand) & !is.null(standard_dev)){
      xaxis_raw = 1
    }else{
      xaxis_raw = 0
      warning('Both "radioligand" and "standard_dev" must be supplied, ignoring input.')
    }
    
  }else{
    xaxis_raw = 0
  }

  
  
if (xaxis_raw == 1){
  out_name <- paste0("./R/earlystopBF/output/rscale_", rscale,"_",paired_txt,"_",alternative,"_N",BF_start_at_n_comparisons,"-",max_n_comparisons,
                     "_BF",k_BF10,"_",k_BF01,"_test_every",test_seq,"_raw_",radioligand,".png")
  x_lable <- expression(paste("Population difference (",italic("BP"[ND]),")"))
  x_tick <-  round(D_vector*standard_dev,2)
  p.title <- paste0(radioligand, "; rscale=", as.character(rscale),"; ",paired_txt,", ",alternative," test; N: ",
                    as.character(BF_start_at_n_comparisons),"-",as.character(max_n_comparisons),
                    ";\n BF10: ",as.character(k_BF10)," BF01: ",as.character(k_BF01),"; test at every ",as.character(test_seq))
  
  
}else{
  out_name <- paste0("./R/earlystopBF/output/rscale_", rscale,"_",paired_txt,"_",alternative,"_N",BF_start_at_n_comparisons,"-",max_n_comparisons,
                     "_BF",k_BF10,"_",k_BF01,"_test_every",test_seq,".png")
  x_lable <- paste0("Population effect ", CD)
  x_tick <-  D_vector
  p.title <- paste0("rscale=", as.character(rscale),"; ",paired_txt,", ",alternative," test; N: ",
                    as.character(BF_start_at_n_comparisons),"-",as.character(max_n_comparisons),
                    ";\n BF10: ",as.character(k_BF10)," BF01: ",as.character(k_BF01),"; test at every ",as.character(test_seq))
  
  #If user supplies her own title this will overwrite automaticlly generated title
  if (!is.null(title)){
    p.title = title
    }

}
  

  out <- list(out_name = out_name,
              x_lable = x_lable,
              x_tick = x_tick,
              p.title = p.title)
  
  return(out)

  
  
}
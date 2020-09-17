
sub_func_get_data <- function(rscale,alternative,paired, print_FPR = NULL) {
  
  #Function to locate and load correct input of simulation files
  
  if (paired==TRUE) {
    paired_txt <- "Paired"
  }else if (paired==FALSE){
    paired_txt <- "CrossSectional"
  }
  
  if (alternative=="one_sided") {
    alt_text <- "OneSided"
  }else if (alternative=="two_sided") {
    alt_text <- "TwoSided"
  }
  
  if (rscale=="0.707"){
    get_file <- "/"
  }else if (rscale=="1"){
    get_file <- "/AppData/Cauchy_1/"
    
    
  }
  
  
  
  if (is.null(print_FPR)){
    
  #Check if file exists, if not stop function
  if (!file.exists(paste0("./DerivedData/SlimmedData/",paired_txt,"/",alt_text,get_file,"BFsims.rds"))){
    stop(paste0("The file: ./DerivedData/SlimmedData/",paired_txt,"/",alt_text,get_file,"BFsims.rds, does not exist"))
  }
   out.int.master <- readRDS(paste0("./DerivedData/SlimmedData/",paired_txt,"/",alt_text,get_file,"BFsims.rds"))
   
   div_with <- 10 #bulk of slimmed simulations saved with one decimale
  
  }else{
    #Check if file exists, if not stop function
    if (!file.exists(paste0("./DerivedData/SlimmedData/",paired_txt,"/",alt_text,get_file,"Enlarged_BFsim_D_0/BFsims.rds"))){
      stop(paste0("The file: ./DerivedData/SlimmedData/",paired_txt,"/",alt_text,get_file,"Enlarged_BFsim_D_0/BFsims.rds, does not exist, set print_FPR=0 to run function without this file"))
    }
    out.int.master <- readRDS(paste0("./DerivedData/SlimmedData/",paired_txt,"/",alt_text,get_file,"Enlarged_BFsim_D_0/BFsims.rds"))
    
    div_with <- 100 #100k D=0  slimmed simulations saved with two decimales
    
  }
  
  #convert everything back into BF10*10
  
  out.int.master$slimmed.data$BF.vec.int <- out.int.master$slimmed.data$BF.vec.int/div_with
  out.int.master$slimmed.data$BF.vec.int[out.int.master$slimmed.data$BF10.true == F] <- 
    (1 / out.int.master$slimmed.data$BF.vec.int[out.int.master$slimmed.data$BF10.true == F]) 
  
  #convert eveything back into BF10 and BF01 vectors
  BF10 <- out.int.master$slimmed.data$BF.vec.int
  BF01 <- 1/BF10
  
  plotData <- data.frame( BF10 = BF10,
                          BF01 = BF01)
  
  return(plotData)
  
}






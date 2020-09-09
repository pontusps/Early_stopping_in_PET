sub_func_NHST <- function(alternative,paired, max_n_comparisons, D_vector, sig.level) {

#Subfunction to get powercurve for NHST


library("pwr")

if (paired==TRUE){
  type = "paired"
}else if (paired == FALSE){
  type = "two.sample"
}

if (alternative == "two_sided") {
  alternative = "two.sided"
}else if (alternative == "one_sided") {
  alternative = "greater"
}

temp <- pwr.t.test(n = max_n_comparisons, d = D_vector, sig.level = 0.05, power = NULL,
           type = type, alternative = alternative)

return(temp$power)

}
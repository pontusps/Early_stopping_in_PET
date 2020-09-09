#############################################################
# Example 1 in article: Uncorrected sequential NHST testing #
#############################################################
### Pontus Plavén-Sigray, NRU Sept 2020

fpr <- 0
n_trials <- 50000
n_comparisons <- 20 

for(t in 1:n_trials){

a <- rnorm(n = 20)
b <- rnorm(n = 20)
p_vals <- NULL

for (i in 2:n_comparisons){
  
 p_vals[i-1] <- t.test(a[1:i],b[1:i])$p.value 
 
}

if (any(p_vals<=0.05)){
  fpr <- fpr + 1
} 

}

fpr/n_trials


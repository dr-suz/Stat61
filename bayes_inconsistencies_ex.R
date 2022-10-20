
## Parameter of interest: \theta = \sigma, std dev of IID normal mean 0 data 
dat <- rnorm(n=60, mean=0, sd= 1.4) 

## prior 
sim_theta <- function(){
  return{rgamma(n=1, shape = 2, rate =2)}
}


sim_post_unnormalized <- function(data, theta_sim){
  if (theta_sim >= 0) return{theta_sim^{1-n}*exp(-sum(data^2)/(2*theta_sim^2) - 2*\theta_sim)}
}




## Idea: use conjugage prior 
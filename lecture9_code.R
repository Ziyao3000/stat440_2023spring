jackknife_vec = function(samples, est_func) {
  #' 
  #' Function for performing jackknife estimation for
  #' vector-valued functions
  #' 
  #' @param samples vector of samples
  #' @param est_func scalar-valued function 
  
  n = length(samples)
  jackknife_samps = sapply(
    # for each index in the sample...
    1:n,
    # ...calculate the statistic at all but the current index
    function(j) { est_func(samples[-j]) }
  )
  
  # calculate the jackknife estimate
  theta_est = mean(jackknife_samps)  # hat{theta}_{J}
  
  # calculate the jackknife variance estimate
  var_est = (
    (n-1) / n * sum((jackknife_samps - theta_est)**2)
  )
  
  # calculate the jackknife bias estimate
  bias_est = (
    (n - 1) * (theta_est - est_func(samples))
  )
  
  # return all three outputs
  list(
    theta_est,
    bias_est,
    var_est
  )
}

naive_sample_var = function(x) { 
  #' biased sample variance
  #' @param x sample vector
  (length(x) - 1) / (length(x)) * var(x)
} 

set.seed(440)
bias_experiment_res = replicate(
  10000,
  {
    # generate original samples 
    samples = rnorm(9, 0, 3) 
    
    # calculate biased estimator
    biased_est = naive_sample_var(samples) # \hat{\sigma}^2
    jackknife_res = jackknife_vec(samples, naive_sample_var)
    
    # calculate jackknife estimator
    jackknife_est = jackknife_res[[1]]
    
    # calculate jackknife estimator with bias correction
    jackknife_bc_est = biased_est - jackknife_res[[2]]
    
    # return results
    c(biased_est, jackknife_est, jackknife_bc_est)
  }
)

# results for analyzing
bias_res = as.data.frame(t(bias_experiment_res))
names(bias_res) = c("Biased", "Jackknife", "JackknifeBC")


colMeans(bias_res) - 9








# cholera
library(HistData)
df = HistData::Cholera
# select only Thames river data
df = df[df$water != "New River",
        c("water", "cholera_drate")]
ggplot(data=df) +
  geom_histogram(aes(x=cholera_drate),
                 bins=7, color="black", fill="white") +
  facet_grid(water ~ .) +
  ylab("Frequency") +
  xlab("Cholera mortality rate")

jackknife_df = function(samples, est_func) {
  #'
  #'Function for performing jackknife estimation for
  #'row-wise data.frame-valued functions
  #'
  #'@param samples data.frame of samples
  #'@param est_func data.frame-valued function
  n = dim(samples)[1]
  jackknife_samps = sapply(
    # for each index in the sample...
    1:n,
    # ...calculate the statistic at all but the current row index
    function(j) { est_func(samples[-j, ]) }
  )
  # calculate the jackknife estimate
  theta_est = mean(jackknife_samps)
  # calculate the jackknife variance estimate
  var_est = (
    (n-1) / n * sum((jackknife_samps - theta_est)**2)
  )
  # calculate the jackknife bias estimate
  bias_est = (
    (n - 1) * (theta_est - est_func(samples))
  )
  # return all three outputs
  list(
    theta_est,
    bias_est,
    var_est
  )
}

cholera_est = function(test_df) {
  mean(test_df[test_df$water == "Battersea","cholera_drate"]) /
    mean(test_df[test_df$water == "Kew","cholera_drate"])}
res = jackknife_df(df, cholera_est)
res

round(
  c(
    "Lower"=res[[1]] - 2*sqrt(res[[3]]),
    "Estimate"=res[[1]],
    "Upper"=res[[1]] + 2*sqrt(res[[3]])
  ),
  1
)

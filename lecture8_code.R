# Example1: Testing for Similar Distributions
same_dist_perm_test = function(
    n_perms, xs, ys, test_statistic
) {
  #' 
  #' perform a generic permutation test 
  #' @param n_perms number of permutations to generate
  #' @param xs vector of samples from distribution X
  #' @param yx vector of samples from distribution y
  #' @param test_statistic function that calculates the test statistic
  
  # calculate the number of samples in X and Y
  n = length(xs) 
  m = length(ys) 
  # define labels (1 = X samples, 0 = y samples)
  labels = c(rep(1, n), rep(0, m))
  all_data = c(xs, ys)
  
  # for every permutation replication
  replicate(
    n_perms, {
      # permute label orders
      permuted_labels = sample(labels)
      
      # generate new test statistic under permutation
      test_statistic(all_data[permuted_labels == 1], # new Xs or X permutated
                     all_data[permuted_labels == 0]) # new Ys or Y permuated
    }
    
  )
}
set.seed(123)
# create example data
x_obs = rnorm(100, mean = 5, sd = 0.5)
y_obs = rnorm(100, mean = 5, sd = 0.7)
# x_obs = rbinom(n = 100000, size = 1, prob = 0.5)
# y_obs = rbinom(n = 100000, size = 1, prob = 0.501)
# generate permutation samples of T
obs_perms = same_dist_perm_test(
  1000, 
  x_obs, 
  y_obs,
  function(a, b) { mean(a) - mean(b) }
)
obs_perms
p_value = mean(abs(obs_perms) >= abs(mean(x_obs) - mean(y_obs)))
p_value


# plot histogram of T
library(ggplot2)

ggplot(data=data.frame(x=obs_perms)) + 
  geom_histogram(aes(x=x, y=..density..), bins=30,
                 color="black", fill="white") + 
  geom_vline(xintercept=mean(x_obs) - mean(y_obs), 
             color="blue", size=1) + 
  geom_vline(xintercept=-(mean(x_obs) - mean(y_obs)), 
             color="blue", size=1) +
  xlab("Difference in sample means") + 
  ylab("Density")


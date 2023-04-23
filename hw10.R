library(tidyverse)
rm(list = ls())
# read in data
student_scores <- read_csv("student_scores.csv")
x <- student_scores$EntranceExam
y <- student_scores$GPA
cor(x,y)
# 0.7763745

# 1. Write your own function to perform non-parametric bootstrap (10,000 replications)
# and then find an approximate 95% confidence interval for ρ. 
# Use set.seed(440) in your simulation.
nboot = 10000
non_parametric_bootstrap <- function(x, y, nboot) {
  n <- length(x)
  Bboot <- numeric(nboot)
  for (i in 1:nboot) {
    sample_idx <- sample(n, replace = TRUE)
    Bboot[i] <- cor(x[sample_idx], y[sample_idx])
  }
  list(boot_samples = Bboot, boot_mean = mean(Bboot), boot_sd = sd(Bboot))
}
set.seed(440)
non_parametric_bootstrap = non_parametric_bootstrap(x, y, nboot)

boot_samples = non_parametric_bootstrap$boot_samples
boot_mean = non_parametric_bootstrap$boot_mean
boot_sd = non_parametric_bootstrap$boot_sd

# Compute confidence interval by percentail
alpha <- 0.05
lower_ci <- quantile(boot_samples, alpha/2)
upper_ci <- quantile(boot_samples, 1 - alpha/2)

CI = round(c(
  lower = lower_ci,
  est = boot_mean,
  upper = upper_ci
), 3)
CI
# lower.2.5%         est upper.97.5% 
# 0.461       0.771       0.961 

plot(density(non_parametric_bootstrap$boot_samples))
abline(v = CI,col = "lightgray")


# 2. Read the documentation of function boot from R package boot, 
# use this function to perform non-parametric bootstrap (10,000 replications) 
# and then find an approximate 95% confidence interval for ρ. 
# Use set.seed(440) in your simulation. 
# Compare the interval with the interval from the previous part.
library(boot)
set.seed(440)
nboot = 10000
x_vector <- student_scores$EntranceExam
y_vector <- student_scores$GPA

# define function to compute correlation coefficient from a data frame
cor_func <- function(data, indices) {
  x = data[indices, 1]
  y = data[indices, 2]
  mean_x <- mean(x)
  mean_y <- mean(y)
  rho = sum((x - mean_x) * (y - mean_y)) / sqrt(sum((x - mean_x)^2) * sum((y - mean_y)^2))
}
# generate bootstrap samples and compute correlation coefficients
df = cbind(x_vector, y_vector)
boot_samples <- boot(data = df, statistic = cor_func, R = nboot)
# calculate the 95% confidence interval for the correlation coefficient
boot.ci(boot_samples, type = c("norm", "basic", "perc", "bca"))
# Level     Percentile     
# 95%   ( 0.4628,  0.9621 )


# 3. Compute the sample mean vector ˆμ and the sample covariance matrix ˆΣ 
# on the data of n = 15 students.
mu_hat = colMeans(df)
mu_hat
#   x_vector   y_vector 
# 600.266667   3.094667 

sigma_hat = cov(df)
sigma_hat
#             x_vector  y_vector
# x_vector 1746.780952 7.9015238
# y_vector    7.901524 0.0592981

# 4. Simulate 10,000 random vectors from a bivariate normal distribution N(ˆμ, ˆΣ), 
# where ˆμ and ˆΣ are computed in the previous part. 
# Compute the sample mean vector and sample covariance matrix based on 
# the 10,000 simulated random vectors, 
# and then compare them with the values of ˆμ and ˆΣ from the previous part. 
# Use set.seed(440) in your simulation.
library(MASS)
bootstrap = replicate(
  nboot,
  # one randomly generated MVN sample per row
  mvrnorm(dim(df)[1], mu_hat, sigma_hat)
)

# Sample mean of the bootstrap vectors
boot_mu <- colMeans(bootstrap)
x_vectors_mean <- mean(boot_mu[1,])
x_vectors_mean
# 600.4184
y_vectors_mean <- mean(boot_mu[2,])
y_vectors_mean
# 3.095019

bootstrap_covs = replicate(
  nboot,
  # calculate sample covariance using...
  cov(
    # one randomly generated MVN sample per row
    mvrnorm(dim(df)[1], mu_hat, sigma_hat)
  )[1, 2]
)
# aggregate our estimates using bootstrap rules
bootstrap_cov = mean(bootstrap_covs)
bootstrap_cov
# 7.877355

# 5. Use parametric bootstrap together with simulated samples from N(ˆμ, ˆΣ) 
# from the previous part to find an approximate 95% confidence interval for ρ. 
# Compare the interval with the intervals based on non-parametric bootstrap.
# calculate sample correlation coefficients for each vector
set.seed(440)
cor_vec <- apply(bootstrap, 3, function(x) cor(x[,1], x[,2]))
mean(cor_vec)
# 0.7655189

# Compute confidence interval by percentail
alpha <- 0.05
lower_ci <- quantile(cor_vec, alpha/2)
upper_ci <- quantile(cor_vec, 1 - alpha/2)

CI = round(c(
  lower = lower_ci,
  est = mean(cor_vec),
  upper = upper_ci
), 3)
CI
# lower.2.5%         est upper.97.5% 
#   0.472       0.764       0.926 

plot(density(cor_vec))
abline(v = CI,col = "lightgray")






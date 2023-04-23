rm(list = ls())

dmixdist = function(x) {
  #' PDF of mixture distribution
  #' @param x vector of test points
  1/3 * (
    dnorm(x, -3, 1) + dnorm(x, 0, 2) + dnorm(x, 3, 1)
  )
}

pmixdist = function(x) {
  #' CDF of mixture distribution
  #' @param x vector of test points
  1/3 * (
    pnorm(x, -3, 1) + pnorm(x, 0, 2) + pnorm(x, 3, 1)
  )
}

rmixdist = function(n) {
  #' random samples from mixture distribution
  #' @param n sample size
  as.vector(
    cbind(rnorm(n, -3, 1), rnorm(n, 0, 2), rnorm(n, 3, 1)) %*%
      rmultinom(n, 1, rep(1/3, 3))
  )
}

library(ggplot2)

set.seed(440)
# construct test points for x vector
x_test = seq(-6, 6, .01) 

# sample from mixture distribution
x_samp = rmixdist(100)

x_dist = data.frame(
  x=x_test,
  pdf=dmixdist(x_test),
  cdf=pmixdist(x_test)
)

ggplot() + 
  geom_histogram(aes(x=x, y=..density..), data=data.frame(x=x_samp),
                 color="black", fill="white", bins=30) + 
  geom_line(aes(x=x,y=pdf), data=x_dist, color="blue")

# empirical CDF of our sample
x_ecdf = ecdf(x_samp)

# evaluate empirical CDF at test points
x_dist$cdf_est = x_ecdf(x_dist$x)


ggplot(data=x_dist) + 
  geom_line(aes(x=x,y=cdf), color="blue") + 
  geom_line(aes(x=x,y=cdf_est), color="black")



bootstrap = function(samples, B, estimator) {
  #' generic bootstrap function 
  #' @param samples vector of samples
  #' @param B number of bootstrap estimates
  #' @param estimator estimating function 
  
  # generate matrix of bootstrap resamples
  n = length(samples)
  resamples = matrix(
    sample(samples, size=n*B, replace=TRUE),
    nrow=B
  )
  
  # apply to each row
  bootstrap_ests = apply(resamples, 1, estimator) 
  
  # return bootstrap mean and standard error estimates
  c(
    mean(bootstrap_ests),
    sqrt(var(bootstrap_ests))
  )
}

bootstrap_median = bootstrap(x_samp, 1000, median)

round(c(
  lower=bootstrap_median[1] - 2 * bootstrap_median[2],
  est=bootstrap_median[1],
  upper=bootstrap_median[1] + 2 * bootstrap_median[2]
), 3)

xr = seq(-6, 6, .1)
cdf_ci = sapply(
  xr, 
  function(t) {
    bootstrap(x_samp, 100, function(z) {mean(z <= t)})
  }
)

# multiple-testing adjusted confidence level
za = qnorm(1 - .05 / (2 * length(xr)))

cdf_ci_df = data.frame(
  x=xr,
  bootstrap_est = cdf_ci[1, ],
  bootstrap_lower = cdf_ci[1, ] - za * cdf_ci[2, ],
  bootstrap_upper = cdf_ci[1, ] + za * cdf_ci[2, ]
)

ggplot(data=cdf_ci_df) + 
  geom_line(mapping=aes(x=x, y=bootstrap_est), color="black") + 
  geom_line(mapping=aes(x=x, y=bootstrap_lower), color="red") + 
  geom_line(mapping=aes(x=x, y=bootstrap_upper), color="red") + 
  geom_line(aes(x=x,y=cdf), data=x_dist, color="blue")







########## parameteric boostrap #######
library(HistData)

df = HistData::Galton

mu = colMeans(df)
sigma = cov(df)

library(MASS)

bootstrap_covs = replicate(
  1000, 
  # calculate sample covariance using...
  cov(
    # one randomly generated MVN sample per row
    mvrnorm(dim(df)[1], mu, sigma)
  )[1, 2]
)

# aggregate our estimates using bootstrap rules
bootstrap_cov_est = mean(bootstrap_covs)
bootstrap_cov_se = sqrt(var(bootstrap_covs))

# estimation of cov and its CI
round(c(
  lower=bootstrap_cov_est - 2 * bootstrap_cov_se,
  est=bootstrap_cov_est,
  upper=bootstrap_cov_est + 2 * bootstrap_cov_se
), 3)

# estimation of cor and its CI
round(c(
  lower=(bootstrap_cov_est - 2 * bootstrap_cov_se) / sqrt(prod(diag(sigma))),
  est=bootstrap_cov_est / sqrt(prod(diag(sigma))), # sigma_{12}/sqrt( sigma_{11} * sigma_{22} )
  upper=(bootstrap_cov_est + 2 * bootstrap_cov_se) / sqrt(prod(diag(sigma)))
), 3)

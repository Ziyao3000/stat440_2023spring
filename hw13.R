rm(list = ls())
normal_known_var_post <- function(n, y_bar, sigma, mu0, sigma0) {
  
  mu0_ <- ((n * y_bar / sigma^2) + (mu0 / sigma0^2)) / ((n / sigma^2) + (1 / sigma0^2))
  cat("Posterior mean (mu0'):", mu0_, "\n")
  
  sigma2_0_ <- 1 / (n / sigma^2 + 1 / sigma0^2)
  cat("Posterior variance (sigma2_0'):", sigma2_0_, "\n")
  
  # theta|y_bar = 150
  lower_bound_θ <- qnorm(0.025, mean = mu0_, sd = sqrt(sigma2_0_))
  cat("Lower bound for 95% CI (theta|y_bar):", lower_bound_θ, "\n")
  upper_bound_θ <- qnorm(0.975, mean = mu0_, sd = sqrt(sigma2_0_))
  cat("Upper bound for 95% CI (theta|y_bar):", upper_bound_θ, "\n")
  
  # y~|y_bar = 150
  sigma2_0y_ <- sigma2_0_ + sigma^2
  cat("Posterior variance (sigma2_0y' + sigma^2):", sigma2_0y_, "\n")
  lower_bound_ŷ <- qnorm(0.025, mean = mu0_, sd = sqrt(sigma2_0y_))
  cat("Lower bound for 95% CI (y~|y_bar):", lower_bound_ŷ, "\n")
  upper_bound_ŷ <- qnorm(0.975, mean = mu0_, sd = sqrt(sigma2_0y_))
  cat("Upper bound for 95% CI (y~|y_bar):", upper_bound_ŷ, "\n")
}

normal_known_var_post(n = 10, y_bar = 150, sigma = 20, mu0 = 180, sigma0 = 40)
normal_known_var_post(n = 100, y_bar = 150, sigma = 20, mu0 = 180, sigma0 = 40)
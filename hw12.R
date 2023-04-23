rm(list = ls())
library(ggplot2)
library(gridExtra)

# Define data
n <- 4
x <- 3

# Define priors
prior1 <- c(4, 2)
prior2 <- c(16, 6)

# Define function to compute posterior distribution
compute_posterior <- function(prior) {
  # Extract prior parameters
  alpha <- prior[1]
  beta <- prior[2]
  
  # Compute posterior parameters
  alpha_post <- alpha + x
  beta_post <- beta + n - x
  
  # Generate sequence of theta values
  theta_seq <- seq(0, 1, length.out = 1000)
  
  # Compute prior, likelihood, and posterior densities
  prior_dens <- dbeta(theta_seq, alpha, beta)
  likelihood_dens <- dbinom(x, n, theta_seq)
  posterior_dens <- dbeta(theta_seq, alpha_post, beta_post)
  
  # Plot densities
  p <- ggplot() +
    geom_line(aes(x = theta_seq, y = prior_dens), color = "blue", linetype = "dashed") +
    geom_line(aes(x = theta_seq, y = likelihood_dens), color = "green", linetype = "dashed") +
    geom_line(aes(x = theta_seq, y = posterior_dens), color = "red", linetype = "solid") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, max(posterior_dens) * 1.2)) +
    labs(title = paste0("Prior: Beta(", alpha, ", ", beta, ")   ",
                        "Likelihood: Binomial(", n, ", ", x, ")   ",
                        "Posterior: Beta(", alpha_post, ", ", beta_post, ")"),
         x = "Theta", y = "Density")
  return(p)
}

# Compute posterior distributions for both priors
posterior1 <- compute_posterior(prior1)
posterior2 <- compute_posterior(prior2)

# Display plots side by side
grid.arrange(posterior1, posterior2, ncol = 2)
















plot_posterior = function(n, x, prior_a, prior_b) {
  #'
  #'@param n sample size
  #'@param x number of heads
  #'@param prior_a prior alpha value
  #'@param prior_b prior beta value
  #'
  
  # define the range of theta values
  xs <- seq(0, 1, length.out = 1000)
  
  # compute the likelihood function
  likelihood <- dbinom(x, n, xs)
  
  # compute the prior density
  prior <- dbeta(xs, prior_a, prior_b)
  
  # calculate alpha and beta values for the posterior density
  post_a <- prior_a + x
  post_b <- prior_b + n - x
  
  # compute the posterior density
  posterior <- dbeta(xs, post_a, post_b)
  
  # create plot
  ggplot(data = data.frame(x = xs, y = posterior),
         mapping = aes(x = x, y = y)) +
    geom_line(color = "blue", size = 1) +
    geom_line(aes(x = xs, y = likelihood), color = "green", size = 1) +
    geom_line(aes(x = xs, y = prior), color = "red", size = 1) +
    geom_vline(xintercept = qbeta(c(0.025, 0.975), post_a, post_b),
               color = "blue", linetype = "dashed") +
    ggtitle(sprintf("n = %s, x = %s, prior_a = %s, prior_b = %s",
                    n, x, prior_a, prior_b)) +
    xlab("Theta") +
    ylab("Density")
}

# dataset 1: 1 head and 3 tails observed in 4 tosses
p1 = plot_posterior(4, 1, 4, 2)

# dataset 2: 5 heads and 15 tails observed in 20 tosses
p2 = plot_posterior(20, 5, 4, 2)

# dataset 3: 25 heads and 75 tails observed in 100 tosses
p3 = plot_posterior(100, 25, 4, 2)

# arrange the plots in a column
grid.arrange(p1, p2, p3, ncol = 1)


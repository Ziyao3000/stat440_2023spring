rm(list = ls())
#' ##################################################teacher's
#' ########part2
#' library(ggplot2)
#' set.seed(440)
#' # f = function(x){
#' #   if (x>6 | x<0){
#' #     0
#' #     }else{
#' #       (1/(3 * (1 - exp(-2))))*exp(-x/3)
#' #     }
#' # }
#' f = function(x){ifelse((x>6) | (x<0), 0, 1/(3 * (1 - exp(-2)))*exp(-x/3))}
#' g = function(x){exp(-x/3)/3}
#' 
#' rejection_sampling = function(n, alpha){
#'   #' Perform beta rejection sampling #' @param n output sample size #' @param alpha parameter
#'   # set alpha value and calculate constant c
#'   c <- 1/ (1-exp(-2))
#'   # initialize a vector of samples
#'   x_sample <- numeric(n)
#'   current_n <- 1
#'   total_samples <- 0
#'   # while loop for all samples
#'   while(current_n <= n){
#'     # generate uniform proposal
#'     y <- rexp(1, rate = 1/3)
#'     total_samples <- total_samples + 1
#'     # determine if we accept sample by Bernoulli sampling
#'     a <- rbinom(1, 1, f(y)/(g(y)*c))
#'     # if we accept the sample, insert it to results
#'     if ((a == 1) & (y <= 6)){
#'       x_sample[current_n] = y
#'       current_n <- current_n + 1
#'     }
#'     # return distribution samples and total proposed
#'     list(
#'       x_sample,
#'       total_samples,
#'       c
#'     )
#'   }
#' }
#' 
#' # run algorithm and plot results
#' xset <- seq(0,  6, 0.01)
#' true_line <- data.frame(x=xset,y=f(xset))
#' rejection_results = rejection_sampling(10000,0)
#' 
#' ggplot() +
#'   geom_histogram(data=data.frame(x=rejection_results[[1]]),
#'                  mapping=aes(x=x, y=..density..),
#'                  bins=30, fill="white", color="black") +
#'   geom_line(data = true_line,
#'             color="blue",
#'             mapping=aes(x=x, y=y))
#' rejection_results
#' # compute acceptance rate
#' acceptance_rate = 10000 / rejection_results[[2]]
#' rejection_rate = 1 - acceptance_rate
#' rejection_rate




##################################################teacher's(fixed)
########part2
library(ggplot2)
set.seed(440)
f = function(x){ifelse((x>6) | (x<0), 0, 1/(3 * (1 - exp(-2)))*exp(-x/3))}
g = function(x){exp(-x/3)/3}
n = 10000
#' Perform beta rejection sampling #' @param n output sample size #' @param alpha parameter
# set alpha value and calculate constant c
c <- 1/ (1-exp(-2))
# initialize a vector of samples
x_sample <- numeric(n)
current_n <- 1
total_samples <- 0
# while loop for all samples
while(current_n <= n){
  # generate uniform proposal
  y <- rexp(1, rate = 1/3)
  total_samples <- total_samples + 1
  # determine if we accept sample by Bernoulli sampling
  a <- rbinom(1, 1, f(y)/(g(y)*c))
  # if we accept the sample, insert it to results
  if ((a == 1) & (y <= 6)){
    x_sample[current_n] = y
    current_n <- current_n + 1
  }
  # return distribution samples and total proposed
  rejection_results <- list(
    x_sample,
    total_samples,
    c
  )
}
rejection_results
# run algorithm and plot results
xset <- seq(0,  6, 0.001)
true_line <- data.frame(x=xset,y=f(xset))
#rejection_results = rejection_sampling(10000,0)

ggplot() +
  geom_histogram(data=data.frame(x=rejection_results[[1]]),
                 mapping=aes(x=x, y=..density..),
                 bins=30, fill="white", color="black") +
  geom_line(data = true_line,
            color="blue",
            mapping=aes(x=x, y=y))

# compute acceptance rate
acceptance_rate = n / rejection_results[[2]]
rejection_rate = 1 - acceptance_rate
c(acceptance_rate, rejection_rate)



##################################################part4
########part2
library(ggplot2)
set.seed(440)
f = function(x){ifelse((x>6) | (x<0), 0, 1/(3 * (1 - exp(-2)))*exp(-x/3))}
g = function(x){exp(-x/2)/2}
n = 10000
#' Perform beta rejection sampling #' @param n output sample size #' @param alpha parameter
# set alpha value and calculate constant c
c <- (2*exp(1))/ (3*(1-exp(-2)))
# initialize a vector of samples
x_sample <- numeric(n)
current_n <- 1
total_samples <- 0
# while loop for all samples
while(current_n <= n){
  # generate uniform proposal
  y <- rexp(1, rate = 1/2)
  total_samples <- total_samples + 1
  # determine if we accept sample by Bernoulli sampling
  a <- rbinom(1, 1, f(y)/(g(y)*c))
  # if we accept the sample, insert it to results
  if ((a == 1) & (y <= 6)){
    x_sample[current_n] = y
    current_n <- current_n + 1
  }
  # return distribution samples and total proposed
  rejection_results <- list(
    x_sample,
    total_samples,
    c
  )
}
rejection_results
# run algorithm and plot results
xset <- seq(0,  6, 0.001)
true_line <- data.frame(x=xset,y=f(xset))
exp2 <- data.frame(x=xset,y=dexp(xset, rate = 1/2))
exp3 <- data.frame(x=xset,y=dexp(xset, rate = 1/3))
#rejection_results = rejection_sampling(10000,0)

ggplot() +
  geom_histogram(data=data.frame(x=rejection_results[[1]]),
                 mapping=aes(x=x, y=..density..),
                 bins=30, fill="white", color="black") +
  geom_line(data = true_line,
            color="blue",
            mapping=aes(x=x, y=y))+
  geom_line(data = exp2,
            color="red",
            mapping=aes(x=x, y=y))+
  geom_line(data = exp3,
            color="yellow",
            mapping=aes(x=x, y=y))

# compute acceptance rate
acceptance_rate = n / rejection_results[[2]]
rejection_rate = 1 - acceptance_rate
c(acceptance_rate, rejection_rate)

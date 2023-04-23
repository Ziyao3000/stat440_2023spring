rm(list = ls())
set.seed(440)
# simulate Y=Z1-Z2
z1<-rexp (10000,2)
z2<-rexp (10000, 2) 
y<-z1-z2
# compute true density of Y
xset <-seq(-4,4,.001)
fx<-NULL
for(i in 1:length(xset)){
  fx[i]=exp(-2 * abs(xset[i]))
}
true_line<-data.frame (x=xset,y=fx)

# compare results
ggplot()+
  geom_histogram(data=data.frame(x=y),
                       mapping=aes(x=x,y=..density..),
                       bins=30, fill= "white", color="black") +
  geom_line(data=true_line, color= "blue", mapping=aes (x=x, y=y))


########################################part4
rm(list = ls())
library(ggplot2)
set.seed(440)
x <-seq(-4,4,.001)
alpha = 1
g = function(x){exp(-abs(x))/2}
n = 10000
#' Perform beta rejection sampling #' @param n output sample size #' @param alpha parameter
# set alpha value and calculate constant c
c <- sqrt(2*exp(1)/pi)
# initialize a vector of samples
x_sample <- numeric(n)
current_n <- 1
total_samples <- 0
# while loop for all samples
while(current_n <= n){
  # generate proposal
  y <- rexp(1,1)-rexp(1,1)
  total_samples <- total_samples + 1
  # determine if we accept sample by Bernoulli sampling
  a <- rbinom(1, 1, dnorm(y,0,1)/(g(y)*c))
  # if we accept the sample, insert it to results
  if ((a == 1)){
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
xset <- seq(-3,3,.001)
true_line <- data.frame(x=xset,y=dnorm(xset))
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


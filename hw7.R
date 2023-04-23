#((λ(T-t))^(N-n) / (N-n)!) *exp(-λ(T-t)) *( (λt)^n/n!)*exp(-λt) /(((λT)^N / N!)* exp(-λT))
rm(list = ls())
df = read.csv("./arrival_times.csv")
#plot(density(as.numeric(df$arrival_time)))

# Create histogram
ggplot(df, aes(x = arrival_time)) +
  geom_histogram(binwidth = .5, color = "black", fill = "white") +
  labs(x = "Arrival time", y = "Frequency", title = "Histogram of student arrival times") +
  # Add vertical lines for mean and median arrival times
  geom_vline(xintercept = mean(df$arrival_time), color = "red") +
  geom_vline(xintercept = median(df$arrival_time), color = "green")

lambda_hat <- nrow(df)/8
lambda_hat
# 10

## use this function to simulate realizations of the poisson process
make_sample_df <- function(run, tmax, lambda)
{
  set.seed(440)
  ## set the starting time
  x <- 0
  ## while the cumulation of time is within the time T,
  ## we keep generating new count
  while(sum(x) < tmax) x <- c(x, rexp(1, lambda))
  ## output is a dataframe with cumulated time points,
  ## number of counts and how many samples we want to generate
  data.frame(t = cumsum(x), N = seq_along(x), run = rep(run, length(x)))
}

max_time = 8
lambda = 5
sample_df <- make_sample_df(1,max_time,lambda)
# compute lambda_hat
lambda_hat <- max(sample_df$N/max_time)
lambda_hat 
# 5.375

# Create histogram
ggplot(sample_df, aes(x = t)) +
  geom_histogram(binwidth = .5, color = "black", fill = "white") +
  labs(x = "Arrival time", y = "Frequency", title = "Histogram of student arrival times") +
  # Add vertical lines for mean and median arrival times
  geom_vline(xintercept = mean(sample_df$t), color = "red") +
  geom_vline(xintercept = median(sample_df$t), color = "green")



reps = 10000
set.seed(440)

X_bar_sim = rep(0, reps) # contain X_bar_{i}
for(i in 1:reps){
  make_sample_df <- function(run, tmax, lambda)
  {
    ## set the starting time
    x <- 0
    ## while the cumulation of time is within the time T,
    ## we keep generating new count
    while(sum(x) < tmax) x <- c(x, rexp(1, lambda))
    ## output is a dataframe with cumulated time points,
    ## number of counts and how many samples we want to generate
    data.frame(t = cumsum(x), N = seq_along(x), run = rep(run, length(x)))
  }
  max_time = 8
  lambda = 5
  X_bar_sim[i] = max(make_sample_df(i,max_time,lambda)$N/max_time)
}
mean(X_bar_sim)



# (x_bar_{i} - mu0)^2
test_sim = (X_bar_sim - lambda)^2
# (x_bar - mu0)^2
test_obs = (lambda_hat - lambda)^2

mean(ifelse(test_sim >=  test_obs, 1, 0 ))
#0.6992



for(i in 1:reps){
p_value = mean(sum(X_bar_sim[i] - lambda)^2/reps > (mean(X_bar_sim) - lambda)^2 )
}
p_value
sum(X_bar_sim[i] - lambda)^2
(mean(X_bar_sim) - lambda)^2 





library(ggplot2)

# Q1
set.seed(440)
ns = 1:1000
us = runif(1000)

hs = exp(-us)

# calculate Monte Carlo estimate at each sample size

# (h(x1), average of h(x1) + h(x2), average of h(x1) + h(x2) + h(x3), ..., average of h(x1) + h(x2) + .. + h(1000))

mc_ests = cumsum(hs) / ns
# plot results
ggplot(data=data.frame(n=ns, est=mc_ests)) +
  geom_line(mapping=aes(x=n, y=est), color="blue") +
  geom_hline(yintercept=(1 - exp(-1)), linetype="dashed") +
  xlab("Number of Monte Carlo Samples") +
  ylab("Integral approximation")

# Q2
# int_{a to b} h(x) dx
bounded_mc_estimate = function(h, n, a, b) {
  # generate uniform samples
  us = runif(n, min=a, max=b)
  # return MC estimate
  (b - a) * sum(h(us)) / n
}

bounded_mc_estimate(
  h=function(x) { 2 * sqrt(1 - x**2) },
  n=1000000,
  a=-1,
  b=1
)
integrate( f = function(x){2 * sqrt(1 - x**2)}, lower = -1, upper = 1)

bounded_mc_estimate(
  h=function(x) { x^2 },
  n=100000,
  a=-1,
  b=1
)
integrate( f = function(x){x^2}, lower = -1, upper = 1)


# Q3 CDF
ns = 1:10000
xs = rnorm(10000)
hs = ifelse(xs >= 2, 1, 0)
# calculate Monte Carlo estimate at each sample size
mc_ests = cumsum(hs) / ns
# plot results
ggplot(data=data.frame(n=ns, est=mc_ests)) +
  geom_line(mapping=aes(x=n, y=est), color="blue") +
  geom_hline(yintercept= 1 - pnorm(2), linetype="dashed") +
  xlab("Number of Monte Carlo Samples") +
  ylab("Integral approximation")



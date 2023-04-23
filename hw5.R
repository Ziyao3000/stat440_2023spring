rm(list = ls())
h = function(x){(cos(50*x)+sin(20*x))^2}
x = seq(0,1,.0001)
y = h(x)
plot(x,y)

# generate uniform random samples
set.seed(440)
ns = 1:10000
us = runif(10000)

hs = h(us)
# calculate Monte Carlo estimate at each sample size
mc_ests = cumsum(hs) / ns
# plot results
library(ggplot2)
ggplot(data=data.frame(n=ns, est=mc_ests)) +
  geom_line(mapping=aes(x=n, y=est), color="blue") +
  geom_hline(yintercept=(1 - exp(-1)), linetype="dashed") +
  xlab("Number of Monte Carlo Samples") +
  ylab("Integral approximation")
mc_ests[9995:10000]

integrate(h, 0,1)

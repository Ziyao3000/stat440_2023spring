# # plot CDF of Unif(0, 1) distribution
# xset <- seq(-.5, 1.5, .001)
# udat <- data.frame(x=xset,y=punif(xset, min=0, max=1))
# ggplot(data=udat,mapping=aes(x=x, y=y)) +
#   geom_line() + ylab("CDF") + ggtitle("CDF of Unif(0, 1)")
# Plot the PDF with µ = 3 and σ = 2.
# logistic(3, 2)
# dlogis(x, location = 0, scale = 1, log = FALSE)
## Plot the Logistic  probability dist
# create a sequence of x values
rm(list = ls())

##################################mine
library(ggplot2)
x <- seq(-10,15, by=0.02)
mu=3
sigma=2
## Compute the Logistic pdf for each x
y <-dlogis(x,mu,sigma)
plot(x,y,type="l",xlim=c(-10,15),ylim=c(0,max(y)),
     lwd=3, col="darkred",ylab="f(x)",
     main=expression(paste("PDF of Logis(",
                           mu,"=3, ",sigma,"=2)")))
abline(v=c(3),lty=2,col="gray")

####################################teacher's
# get the pdf of logistic regression
pdf_logistic = function (x, mu, sigma) {
  return (exp( -(x-mu)/sigma) / ( sigma* (1+exp(- (x-mu) /sigma))^2 ) )
}
# get the evaluation points and their
# corresponding values
x = seq (-10, 15,by = 0.01)
y = pdf_logistic(x, mu = 3, sigma = 2)
# plot the pdf
plot (x,y, type = 'l')

##################################mine
## Compute the Logistic cdf for each x
y <- plogis(x,mu,sigma)
plot(x,y,type="l",xlim=c(-10,15),ylim=c(0,max(y)),
     lwd=3, col="darkred",ylab="F(x)",
     main=expression(paste("CDF of Logis(",
                           mu,"=3, ",sigma,"=2)")))
abline(v=c(3),lty=2,col="gray")

####################################teacher's
#Prove via CDF. We plot the CDF in R.
# get the cdf
cdf_logistic = function (x, mu, sigma) {
  return( 1/(1+exp(-(x-mu)/sigma)))
}
# get the eveluation
x = seq (-20,20,by = 0.01)
y = cdf_logistic(x, mu = 3, sigma = 2)
# plot cdf
plot (x,y, type = 'l')



##################################mine
#set PRNG seed
set.seed(440)
# Step 1: generate uniform samples
unif_samples <- runif(10000, min=0, max=1)
# Step 2: apply inverse transformation
Logis_samples <- -2*log((1/unif_samples)-1) +3


# plot histogram of samples along with true density
ggplot(data=data.frame(x=Logis_samples)) +
  # create histogram of samples
  geom_histogram(mapping=aes(x=x, y=..density..),
                 fill="white", color="black", bins=30) +
  
  # add true density using 'dlogis'function
  geom_line(data=data.frame(x= seq(-10,15, by=0.02),
                            y = dlogis(x,mu,sigma)),
            mapping=aes(x=x, y=y),color="blue") +
  ggtitle("logistic(3,2) simulation vs truth") 

####################################teacher's
#Below is the R code for the inverse transformation sampling and the visualization.
set.seed (440)
# Step 1: generate uniform samples
unif_samples <- runif (10000, min=0, max=1)
# Step 2: apply inverse transformation
exp_samples <- 3-2*log (1/unif_samples-1)
# plot histogram of samples along with true density
ggplot (data=data.frame (x=exp_samples)) +
  # create histogram of samples
  geom_histogram(mapping=aes (x=x, y=..density..),
                 fill="white", color="black", bins=30) +
  # add true density using' dlogis 'function
  geom_line(data=data.frame(x = seq (-15, 15, .001), 
                            y = dlogis(seq(-15, 15,  .001), location=3,scale=2)), 
                                         mapping=aes(x=x, y=y),color="blue") +
                                           ggtitle("simulation vs truth")



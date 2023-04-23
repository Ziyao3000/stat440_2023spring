rm(list = ls())
set.seed(440)
x = rep(0,10000)
x[1] = .3 #initialize MH: I've set this arbitrarily to .3

a = 2.7
b = 6.3

target = function(x){
  return(dbeta(x,shape1 = a, shape2 = b))
}

for(i in 2:10000){
  current_x = x[i-1]
  proposed_x = current_x + rnorm(1,mean=0,sd=1)
  A = target(proposed_x)/target(current_x)
  if(runif(1)<A){
    x[i] = proposed_x # accept move with probability min(1,A)
  } else {
    x[i] = current_x # otherwise "reject" move, and stay where we are
  }
}
plot(x,pch=20,col="grey",main="Values of x visited by the MH algorithm")
hist(x,xlim=c(0,1),probability = TRUE, main="Histogram of values of x visited by MH algorithm")
xx = seq(0,1,length=100)
lines(xx,target(xx),col="red")

# The sample mean and variance of the simulated Markov chain
mean(x)
var(x)

# Theoretical values of the target distribution
mean = a/(a+b)
mean
var = a*b/(((a+b)^2)*(a+b+1))
var  
  
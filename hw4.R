rm(list = ls())
set.seed(440)
# define sample size
n <- 10000
# define dimension
d <- 2
# define the mean vector
mu <- rep(0,times=d)
print(mu)

#Sigma <- exp(-tau*abs(dist_for_sig))
Sigma <- matrix(c(2,1,1,2),nrow = 2)
print(round(Sigma, 3))


mvrnorm_eig <- function(n, mu, Sigma) {
  #' sample from N_d (mu, Sigma) with eigendecomposition 
  #' @param n sample size 
  #' @param mu mean vector 
  #' @param Sigma covariance matrix

  # appiy eigendecomposition to sigma
  Sig_eig <- eigen(Sigma)
  # determine the dimension
  p <- length(mu)
  # find the square-root matrix
  A <- diag(sqrt(Sig_eig$values)) %*% t(Sig_eig$vectors)
  # sample from N_d (mu, Sigma)
  X <- mu + t(A) %*% matrix(rnorm(n*p), nrow=p,ncol=n)
  # return the sample
  return (t(X))
}
X_eig <- mvrnorm_eig(n,mu,Sigma)
mu
colMeans(X_eig)
round(Sigma, 3)
round(cov(X_eig), 3)

#####################################################teacher's
# To solve this part we need to simulate Z from N(0, I) 
# using the R function rnorm and then simulate X = BZ
rm(list = ls())
set.seed(440)
z1<-rnorm(10000,0,1) 
z2<-rnorm(10000,0,1) 
z3<-rnorm(10000,0,1)
Z<-rbind(z1,z2,z3)
B<-matrix(c(1, 1, 1, 0,0, 1),2,3)
X<-B%*%Z
X1<-X[1,]
X2<-X[2,]
var(X1)
var(X2)
cov(X1,X2)
mean(X1)
mean(X2)
##########################################################part3
rm(list = ls())
set.seed(440)
# define sample size
n <- 10000
# define dimension
d <- 2
# define the mean vector
mu <- rep(0,times=d)
print(mu)

#Sigma <- exp(-tau*abs(dist_for_sig))
Sigma <- matrix(c(2,1,1,2),nrow = 2)
print(round(Sigma, 3))

mvrnorm_chol<-function(n,mu, Sigma) {
  #' sample from N_d (mu, Sigma) with Cholesky decomposition
  #' Cparam n sample size 
  #' #' @param mu mean vector 
  #' #' param Sigma covariance matrix

  # apply Cholesky decomposition to Sigma
  Sig_chol<-chol(Sigma)
  # determine the dimension
  p<-length(mu)
  # sample from N_d (mu, Sigma)
  X <- mu+t(Sig_chol)%*% matrix(rnorm(n*p), nrow=p,ncol=n)
  # return the sample
  return(t(X))
  }

X_chol<- mvrnorm_chol(n,mu,Sigma)
mu
colMeans(X_chol)
round(Sigma, 3)
round(cov(X_chol), 3)
###########################################teacher's
rm(list = ls())
set.seed(440)
mvrnorm_chol<-function (n, mu, Sigma) {
  #' sample from N_d(mu, Sigma) with Cholesky decomposition 
  #' #' @param n sample size 
  #' #' @param mu mean vector 
  #' #' @param Sigma covariance matrix
  # apply Cholesky decomposition to Sigma
  Sig_chol<-chol(Sigma)
  # determine the dimension
  p<-length(mu)
  # sample from N_d(mu, Sigma)
  X <- mu + t (Sig_chol)%*%matrix (rnorm(n*p) ,nrow=p, ncol=n)
  # return the sample
  return(t(X))
}
mu = rep(0, 2)
Sigma = matrix(c(2, 1,1,2), nrow = 2)
X_chol<-mvrnorm_chol(10000,mu,Sigma)
X1 = X_chol[, 1]
X2 = X_chol[, 2]
var(X1)
var(X2)
cov(X1, X2) 
mean(X1) 
mean(X2)
#################################################part4
library(MASS)
rm(list = ls())
set.seed(440)
# define sample size
n <- 10000
# define dimension
d <- 2
# define the mean vector
mu <- rep(0,times=d)
print(mu)

#Sigma <- exp(-tau*abs(dist_for_sig))
Sigma <- matrix(c(2,1,1,2),nrow = 2)
print(round(Sigma, 3))

X_mvrnorm <- mvrnorm(n,mu,Sigma)
mu
colMeans(X_mvrnorm)
round(Sigma, 3)
round(cov(X_mvrnorm), 3)
mean(X2)
##########################teacher's
library(MASS)
set.seed(440)
X_mass <- mvrnorm(10000, mu, Sigma)
X1 = X_mass[,1]
X2 = X_mass[,2]
var(X1)
var(X2)
cov(X1,X2) 
mean(X1)
mean(X2)






# create iid standard normal samples with 
# size n and dimension p
n = 5000
p  =10
Z = matrix(rnorm(n*p), nrow = n, ncol = p)

dim(Z)
Z
colMeans(Z)
cov(Z)



# generate multivariate normal samples
n = 10000
p = 5

mu = 1:p
mu # true mean vector

tau = 1
dist_in_sig = outer(1:p, 1:p, FUN = "-")
Sigma = exp( -tau*abs(dist_in_sig))


# X_{1}, X_{2}, ...., X_{n} ~ N( true_mu, true_Sigma ) is our target

# eigendecomposition
Sig_eigen = eigen(Sigma)
Lambda = diag( Sig_eigen$values)
P = Sig_eigen$vectors



# Sigma^{1/2}  = P * Lambda^{1/2}
Sig_root = P%*%sqrt(Lambda)



Z = matrix(rnorm(n*p), nrow = n, ncol = p)
X = mu + Sig_root%*%t(Z)
X = t(X)
dim(X)

colMeans(X)
cov(X) - Sigma

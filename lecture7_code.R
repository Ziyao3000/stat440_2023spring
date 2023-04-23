# Example 1: One-sample Proportion Test
set.seed(440)
# Set sample size of the dataset
n<-50
# Set true value of 'p'
p<-1/3
# Set 'p'under H0
p0<-1/2
# Observe data (n=50) from Bernoulli(1/3)
X<-rbinom(n,size=1,p=p)
X
# Estimate 'p'from the observed dataset
phat<-mean(X)
phat
# Simulate data (n=50) from Bernoulli(1/2) for 10,000 times
reps<-10000
X_sim<-matrix(rbinom(n*reps,size=1,p=p0),nrow=reps)
X_sim
# Estimate 'p'from each of 10,000 simulated datasets
phat_sim<-rowMeans(X_sim)
phat_sim
# Visualize simulations
hist(phat_sim,prob=TRUE,breaks=30)
dens_est<-density(phat_sim, bw="nrd")
points(dens_est$x,dens_est$y,type="l",col="blue")
abline(v=phat,col="red",lty=2)
abline(v=1-phat,col="red",lty=2)

# Convert test statistics to better handle "both tails"
test_stat<-(phat-p0)^2
test_stat
test_sim<-(phat_sim-p0)^2
test_sim
# Visualize simulations
hist(test_sim,prob=TRUE,breaks=30)
abline(v=test_stat,col="red",lty=2)
# Compute a p-value based on simulations
mean(test_sim >= test_stat)

# Let’s compare this simulation-based p-value with p-values from traditional methods.
prop.test(sum(X),n=n,p=p0,alternative="two.sided")
t.test(X,mu=p0,alternative="two.sided")
binom.test(sum(X),n=n,p=p0,alternative="two.sided")

###############################################################
# Example 2: One-sample Mean Test
set.seed(440)
# Set true values of 'mu'and 'sigma'
mu<-3.5
sigma<-5
# Set the value of 'mu'under H0
mu0<-3
# Set the sample size of the dataset
n<-100
# Set the number of simulations to run
reps<-1000
# Observe data (n=100) from N(3.5, 5^2)
X<-rnorm(n=n,mean=mu,sd=sigma)
X
# Estimate 'mu'from the observed dataset
muhat<-mean(X)
muhat
# Simulate data (n=100) from N(3, 5^2) for 1000 times
X_sim<-matrix(rnorm(n=n*reps,mean=mu0,sd=sigma),nrow=reps)
X_sim
# Estimate 'mu'from each of 1000 simulated datasets
muhat_sim<-rowMeans(X_sim)
muhat_sim
# Visualize simulations
hist(muhat_sim,prob=TRUE,breaks=30)
dens_est<-density(muhat_sim, bw="nrd")
points(dens_est$x,dens_est$y,type="l",col="blue")
abline(v=muhat,col="red",lty=2)

# Compute two-sided p-value based on simulations
mean((muhat_sim-mu0)^2>=(muhat-mu0)^2)
# Let’s compare this simulation-based p-value with p-values from traditional methods.
t.test(X,mu=mu0,alternative="two.sided")
wilcox.test(X,mu=mu0,alternative="two.sided")

###############################################################
# Example3: Fishers’ exact test



























set.seed(440)

sigma = 5

mu = 3.5 # is the ground truth, X_1, ... X_N ~ N(4, 5^2)

# H_{0} mu = 3 vs H_{1} mu not =  3
mu0 = 3

n = 100

# genearating obsevred data in real world and calculating the test statistic
X_obs = rnorm(n  = n, mean = mu, sd = 5)
x_bar = mean(X_obs)

# generating simulated distribution
reps = 100000
X_sim = matrix(NA, nrow = reps, ncol = 100) # contain X_{i} from N(3, 5^2) (distribution under H_{0})
X_bar_sim = rep(0, reps) # contain X_bar_{i}
for(i in 1:reps){
  X_sim[i,] = rnorm(n  = n, mean = mu0, sd = 5)
  X_bar_sim[i] = mean(X_sim[i,]) 
}


# p-value =  1/(reps) \sum_{i=1}&{reps} 1_{  (x_bar_{i} - mu0)^2 >  (x_bar - mu0)^2  }

# (x_bar_{i} - mu0)^2
test_sim = (X_bar_sim - mu0)^2
# (x_bar - mu0)^2
test_obs = (x_bar - mu0)^2

mean(ifelse(test_sim >=  test_obs, 1, 0 ))



#  p-value from t.test is small alpha (0.05), but the monte carlo p-value will be greater than alpha. 
t.test(X_obs, mu = mu0, alternative = 'two.sided')


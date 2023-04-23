library(tidyverse)
rm(list = ls())
# read in data
student_scores <- read_csv("student_scores.csv")
# extract variables
x <- student_scores$EntranceExam
y <- student_scores$GPA
# calculate means
mean_x <- mean(x)
mean_y <- mean(y)
# calculate sample correlation
r <- sum((x - mean_x) * (y - mean_y)) / sqrt(sum((x - mean_x)^2) * sum((y - mean_y)^2))
r
cor(x,y)

# print sample correlation
cat("Sample correlation coefficient: ", r, "\n")

# create scatter plot
plot(x, y, xlab = "Entrance Exam Score", ylab = "First-Year GPA")

jackknife_df = function(samples, est_func) {
  #'
  #'Function for performing jackknife estimation for
  #'row-wise data.frame-valued functions
  #'
  #'@param samples data.frame of samples
  #'@param est_func data.frame-valued function
  n = dim(samples)[1]
  jackknife_samps = sapply(
    # for each index in the sample...
    1:n,
    # ...calculate the statistic at all but the current row index
    function(j) { est_func(samples[-j, ]) }
  )
  # calculate the jackknife estimate
  theta_est = mean(jackknife_samps)
  # calculate the jackknife variance estimate
  var_est = (
    (n-1) / n * sum((jackknife_samps - theta_est)**2)
  )
  # calculate the jackknife bias estimate
  bias_est = (
    (n - 1) * (theta_est - est_func(samples))
  )
  # return all three outputs
  list(
    theta_est,
    bias_est,
    var_est
  )
}

est_func = function(student_scores) {
  cor(student_scores$EntranceExam,student_scores$GPA)
  }
res = jackknife_df(student_scores, est_func)
res

round(
  c(
    "Lower"=res[[1]] - 2*sqrt(res[[3]]),
    "Estimate"=res[[1]],
    "Upper"=res[[1]] + 2*sqrt(res[[3]])
  ),
  3
)

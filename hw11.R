library(tidyverse)
rm(list = ls())
# read in data
df <- read_csv("student_scores.csv")
x <- df$EntranceExam
y <- df$GPA

model1 <- glm(y~x,data=df)
summary(model1)
model2 <- glm(y~poly(x,2),data=df)
summary(model2)

# 1. Use the leave-one-out cross validation to compare the two models.
library(boot)
loocv.err1 <- cv.glm(df, model1)
loocv.err1$delta[1]

loocv.err2 <- cv.glm(df, model2)
loocv.err2$delta[1]

# 2. Use the 3-fold cross validation to compare the two models.
k3cv.err1 <- cv.glm(df, model1, K = 3)
k3cv.err1$delta[1]

k3cv.err2 <- cv.glm(df, model2, K = 3)
k3cv.err2$delta[1]




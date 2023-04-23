library('tidyverse')
rm(list = ls())
df = read.csv('rainfall.csv')
rainfall = as.numeric(df$Rainfall)
# Kernel Density Plot
d <- density(rainfall) # returns the density data
plot(d) # plots the results

#gamma(4.26, 6.07)
#2mm-3mm
df %>%
  count(rainfall>=2 & rainfall <= 3)
20/4558
#0.004387889

#exact:
pgamma(q = 3, shape = 4.26, rate = 6.07)-pgamma(q = 2, shape = 4.26, rate = 6.07)
#0.002861257


# MC
set.seed(440)
n = 10000
x = runif(n, min = 2, max = 3)

ya = dgamma(x,shape = 4.26, rate = 6.07)
sum(ya)/length(ya)
# plot(x,ya)



#IS
set.seed(440)
# sample from g:gamma(4.26,1.704)
xb = rgamma(n,shape = 4.26, rate = 1.704)
xb = xb[(xb>=2)&(xb<=3)]
# compute the ratio f/g
fb = dgamma(xb,shape = 4.26, rate = 6.07)
gb = dgamma(xb,shape = 4.26, rate = 1.704)
w = fb/gb

mean(w*gb)

# mean(fb)
# mean(gb)












plot(xb,fb,xlim = c(2, 3))
plot(xb,w*gb,xlim = c(2, 3))
plot(xb,gb,xlim = c(2, 3))





plot(w)
d <- density(w) # returns the density data

plot(d,xlim = c(0, 3))

w
















y = dgamma(x = xseq, shape = 4.26, rate = 6.07)
plot(xseq,y)



xseq <- seq(-.1, 3, .01)

library(ggplot2)
ggplot() +
  geom_line(data=data.frame(x=xseq, y=dgamma(xseq,shape = 3, rate = 4)),
            mapping=aes(x=x,y=y),
            color="blue",
            linetype="dashed") 




  geom_line(data=data.frame(data=df),
            mapping=aes(x=rainfall,y=..density..),
            color="red",
            linetype="dashed") +
  ylab("density")

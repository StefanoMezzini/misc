library(mgcv)
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw())
set.seed(2)

# A Tweedie random variable with 1<p<2 is a sum of N gamma random variables,
# where N has a Poisson distribution.

generate.data  <- function(lambda, shape, rate, beta) {
  
  if(length(beta) != 3) stop('beta must be of length 3.')
  
  # number of gamma random variable
  N <- rpois(n = 1, lambda = lambda)
  
  # generate residuals
  e <- sapply(1:1e3, function(x) sum(rgamma(n = N, shape = shape, rate = rate)))
  
  e
  
  # generate data
  d <- expand_grid(x1 = 1:10,
                   x2 = seq(0.5, 3, length.out = 10),
                   x3 = seq(5, 41, length.out = 10)) %>%
    as.matrix()
  y <- as.numeric(d %*% beta + e)
  
  bind_cols(as_tibble(d), y = y)
}

d <- generate.data(4, 1, 1, beta = c(2, -5, 3))[sample(1:1e3, size = 30),]

ggplot(pivot_longer(d, -y, names_to = 'var', values_to = 'x'), aes(x, y)) +
  facet_grid(. ~ var, scales = 'free_x') +
  geom_point()

# fit model
m <- gam(list(y ~ x1 + x2 + x3,
              ~ 1,
              ~ 1),
         twlss(),
         data = d,
         method = 'REML')

layout(t(1:3))
plot(m, all.terms = TRUE, scale = 0)
layout(1)
head(m$fitted.values) # power outside [1, 2], negative scale!

#rm(generate.data, d, m)

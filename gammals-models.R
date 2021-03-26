# code from ?mgcv::gammals
library(mgcv)
## simulate some data
f0 <- function(x) 2 * sin(pi * x)
f1 <- function(x) exp(2 * x)
f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 * 
  (10 * x)^3 * (1 - x)^10
f3 <- function(x) 0 * x
n <- 400;set.seed(9)
x0 <- runif(n);x1 <- runif(n);
x2 <- runif(n);x3 <- runif(n);
mu <- exp((f0(x0)+f2(x2))/5)
th <- exp(f1(x1)/2-2)
y <- rgamma(n,shape=1/th,scale=mu*th)

b1 <- gam(list(y~s(x0)+s(x2),~s(x1)+s(x3)),family=gammals)
plot(b1,pages=1)
summary(b1)
gam.check(b1)
plot(mu,fitted(b1)[,1]);abline(0,1,col=2)
plot(log(th),fitted(b1)[,2]);abline(0,1,col=2)

# but shape should be k = mu / theta, where theta is the scale?
set.seed(1)
k <- mu / th
y <- rgamma(n, shape = k, scale = th)

b2 <- gam(list(y ~ s(x0) + s(x2), ~ s(x1) + s(x3)), family = gammals)
plot(mu, fitted(b2)[ , 1]); abline(0, 1, col = 2)
plot(log(k), fitted(b2)[ , 2]); abline(0, 1, col = 2)
plot(-log(k), fitted(b2)[ , 2]); abline(0, 1, col = 2) # fit = -log(shape)?

# changing the scale in rgamma() does not change the estimated scale:
library('tibble')
rm(list = ls())

foo <- function(x) {
  set.seed(1)
  d <- tibble(x1 = runif(1e3), y = rgamma(1e3, shape = 2, scale = x))
  m <- gam(list(y ~ 1, ~ 1), family = gammals(), data = d, method = 'REML')
  
  tibble(mu = exp(m$coefficients[1]),
         scale = exp(-7 + log1p(exp(m$coefficients[2]))),
         `1/scale` = 1/scale)
}

foo(1)
foo(100)

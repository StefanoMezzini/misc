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

# but shape should be k = mu / theta, where scale = theta?
set.seed(1)
y <- rgamma(n,shape=mu/th,scale=th)

b2 <- gam(list(y~s(x0)+s(x2),~s(x1)+s(x3)),family=gammals)
plot(mu,fitted(b2)[,1]);abline(0,1,col=2)
plot(log(th), fitted(b2)[,2]);abline(0,1,col=2)

# a different example:
library('tibble')
library('dplyr')
library('purrr')
library('ggplot2')
library('cowplot')
theme_set(theme_bw())

set.seed(1)
K <- 100
d <- tibble(x1 = 1:K,
            x2 = 1:K * 0.5,
            k = x1 - x2, # shape
            theta = 0.05 * x2, # scale
            mu = k * theta,
            sigma2 = mu * theta) %>%
  mutate(y = map2_dbl(k, theta,
                      function(x, y) rgamma(n = 1, shape = x, scale = y)))
m <- gam(list(y ~ s(x1) + s(x2), ~ s(x2)),
         family = gammals(), data = d, method = 'REML')

# too lazy to prevent warning
link <- bind_cols(d, as_tibble(predict(m, type = 'link')))
resp <- bind_cols(d, as_tibble(predict(m, type = 'response'))) %>%
  mutate(s2 = exp(log(V1) + V2))

# link scale: red, response scale: blue dashes
plot_grid(
  # mean: simple log link
  ggplot() +
    geom_point(aes(x1, mu), d) + # true mean
    geom_line(aes(x1, exp(V1)), link, color = 'red', lwd = 1.5) +
    geom_line(aes(x1, V1), resp, color = 'blue', lwd = 1.5, lty = 'dashed'),
  # shape: log(resp) = b + log(1+exp(link))
  ggplot() +
    geom_point(aes(x2, log(k)), d) + # true shape
    geom_line(aes(x2, -7 + log(1+exp(V2))), link, color = 'red', lwd = 1.5) +
    geom_line(aes(x2, V2), resp, color = 'blue', lwd = 1.5, lty = 'dashed'),
  ncol = 1)

# gammals() uses a simple log() link for mean
as_tibble(cbind(predict(m, type = 'response'),
                predict(m, type = 'link'))) %>%
  mutate(disagree = V1 != exp(V3)) %>%
  pull(disagree) %>%
  sum() # all values agree

# V2 on response scale = -7 + log(1 + exp(V2 on link scale))
# how to transform back to actual shape parameter?
# see details in ?gammals
as_tibble(cbind(predict(m, type = 'response'),
                predict(m, type = 'link'))) %>%
  mutate(disagree = V2 != -7 + log(1 + exp(V4))) %>%
  pull(disagree) %>%
  sum() # all values agree

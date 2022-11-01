# rbeta() parameterized by mean and variance
rbeta2 <- function(mu, sigma2, N) {
  # mu = a / (a + b)
  # sigma2 = a * b / (a + b)^2 * (a + b + 1)
  a <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  b <- a * (1 / mu - 1)
  
  #' generate random values using `rbeta()`
  rbeta(n = N, shape1 = a, shape2 = b)
}

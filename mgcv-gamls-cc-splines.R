library('dplyr') # for data wrangling
library('mgcv')  # for GAMs

d <- tibble(x = seq(0.0, 0.5, length.out = 100),
            s = sinpi(x) + 1.3,
            r = 0.1 * cospi(x) + 1.3,
            y = rgamma(n = length(x), shape = s, rate = r))

plot(y ~ x, d)

m <- gam(list(y ~ s(x, bs = 'cc'), ~ s(x, bs = 'cc')),
         family = gammals(), data = d, method = 'REML')

plot(m, pages = 1, scale = 0, xlim = c(0, 2), scheme = 1,
     main = 'With default knots based on range(y)')

m <- gam(list(y ~ s(x, bs = 'cc'), ~ s(x, bs = 'cc')),
         family = gammals(), data = d, method = 'REML',
         knots = list(x = c(0, 1)))

plot(m, pages = 1, scale = 0, xlim = c(0, 2), scheme = 1,
     main = 'With knots set at 0 and 1')

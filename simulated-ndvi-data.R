library('dplyr')
source('https://github.com/StefanoMezzini/misc/raw/main/rbeta2.R')

ndvi_data <- tibble(date = seq(from = as.Date('2020-01-01'),
                               to = as.Date('2021-12-31'),
                               by = 1),
                    year = lubridate::year(date),
                    doy = lubridate::yday(date),
                    dec_date = lubridate::decimal_date(date),
                    mu = sinpi(2 * dec_date) * 0.3 + 0.5 +
                      (dec_date / 2021 - 1) * 300,
                    sigma2 = (cospi(2 * dec_date) + 1) / 30 + 0.01,
                    ndvi = rbeta2(mu, sigma2, length(mu)) * 1.5 - 0.5)

if(FALSE) {
layout(matrix(1:3, ncol = 1))
plot(mu ~ date, ndvi_data, ylim = c(0, 1))
plot(sigma2 ~ date, ndvi_data)
plot(ndvi ~ date, ndvi_data, ylim = c(-1, 1))
layout(1)
}

ndvi_data <- select(ndvi_data, date, year, doy, ndvi)

if(FALSE) {
  plot(ndvi ~ date, ndvi_data, ylim = c(-1, 1))
}

rm(rbeta2)

library('tidyr')
library('dplyr')
library('raster')
library('sf')
library('spData')
library('ggplot2')

K <- 100
r <- expand_grid(x = seq(-180, 180, length.out = K),
                 y = seq(30, 90, length.out = K)) %>%
  mutate(z = sinpi(x / 15) * sinpi(y / 15)) %>%
  rasterFromXYZ()

plot(r)

r2 <- mask(r, world)
plot(r2)

r3 <- rasterToPoints(r2) %>%
  as_tibble()

ggplot(r3, aes(x, y, fill = z)) +
  geom_tile() +
  coord_map('azequidistant') +
  cowplot::theme_map() +
  theme(legend.position = 'none')

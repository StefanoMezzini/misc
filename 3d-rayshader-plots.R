library('ggplot2')     # for fancy plots
library('dplyr')       # for easier data wrangling
library('rayshader')   # for 3D ggplots
theme_set(theme_classic())

df <-
  expand.grid(x = seq(0, 10, length.out = 100),
              y = seq(0, 12, length.out = 100)) %>%
  mutate(z = sin(x) - 3 * cos(y))

plt <-
  ggplot(df, aes(x, y, fill = z)) +
  geom_raster() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = 'none'); plt

plot_gg(plt, height_aes = 'fill', preview = FALSE)

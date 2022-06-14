library('tibble') # for fancy data frames
library('ggplot2') # for fancy plots
library('gganimate') # for animated fancy plots

theme_set(theme_bw()) # change default theme

NFRAMES <- 500 # number of frames
d <- tibble(x = seq(0, 30 * pi, length.out = NFRAMES),
            fx = sin(3 * x^(1/2)), # f(x)
            slope.fx = cos(3 * x^(1/2)) * 3/2 * x^(-1/2), # f'(x), slope for the tangent
            int.fx = fx - slope.fx * x, # intercept for the tangent
            i = 1:NFRAMES) # counter for animation

# plot all the lines
p <-
  ggplot(d[, -5], aes(x, fx)) +
  geom_line() +
  geom_point(data = d, col = 'red', size = 2) +
  geom_abline(aes(slope = slope.fx, intercept = int.fx), d) +
  labs(y = 'f(x)')
p

p_anim <- p + transition_reveal(1:NFRAMES)

p_anim # frames will be saved as individual images if `gifski` package is not installed
animate(p_anim, nframes = NFRAMES + 10, end_pause = 10)

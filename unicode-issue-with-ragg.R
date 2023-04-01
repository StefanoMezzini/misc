#' *fixed by changing backend graphics to AGG*:
# go to Tools > Global Options > Graphics (topof the window)

library('ragg')
plot(1, xlab = '\U1D53C doesn\'t work')
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(1, 1)) +
  ggplot2::xlab('\U1D53C doesn\'t work')

# restart R (Ctrl + Shift + F10)

plot(1, xlab = '\U1D53C')
library('ragg')
plot(1, xlab = '\U1D53C now works')
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(1, 1)) +
  ggplot2::xlab('\U1D53C now works')

# restart R (Ctrl + Shift + F10)

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(1, 1)) +
  ggplot2::xlab('\U1D53C')
library('ragg')
plot(1, xlab = '\U1D53C now works')
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(1, 1)) +
  ggplot2::xlab('\U1D53C now works')

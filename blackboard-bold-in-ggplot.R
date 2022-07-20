library('ggplot2')
windowsFonts('Symbola' = windowsFont('Symbola')) # import font for blackboard bold

ggplot() +
  annotate('text', 0, 0, label = '\U1D53C \U1D54D', size = 50) +
  theme_void() +
  theme(text = element_text(family = 'Symbola'))

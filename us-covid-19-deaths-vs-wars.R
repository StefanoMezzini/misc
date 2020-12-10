library('tibble')
library('ggplot2')
library('dplyr')
library('cowplot')

pop <- tibble(decade = ((177:202) *10),
              population = c( 2148076, 2780369, 3929214, 5308483, 7239881,
                              9638453, 12866020, 17069453, 23191876, 31443321,
                              38558371, 50189209, 62979766, 76212168, 92228496,
                              106021537, 123202624, 132164569, 151325798,
                              179323175, 203211926, 226545805, 248709873,
                              281421906, 308745538, 332639000))

x <- tibble(cause = c('COVID-19', 'World War II','American Civil War',
                      'World War I', 'Vietnam War', 'Korean War',
                      'American Revolutionary War', 'Iraq War', 'War of 1812',
                      'War in Afghanistan', 'Mexican-American War'),
            start = c(2020, 1941, 1861, 1917, 1955, 1950, 1775, 2003, 1812,
                      2001, 1846),
            decade = floor(start / 10) * 10,
            end = c(NA, 1945, 1865, 1918, 1975, 1953, 1783, 2011, 1815, NA,
                    1849),
            Ongoing = is.na(end),
            length = if_else(Ongoing, 2021 - start, end - start),
            deaths = c(248867, 291557, 218222, 53402, 47434, 33686, 8000, 3836,
                       2260, 1833, 1733)) %>%
  mutate(cause = factor(cause, levels = cause),
         yearly.deaths = deaths / length) %>%
  left_join(pop, 'decade') %>%
  mutate(per.mil = deaths / population * 1e6)

note <-
  expression(italic('(averages are approximate, COVID-19 count is incomplete)'))

plot_grid(ggplot(x, aes(deaths, cause, fill = Ongoing)) +
            geom_bar(stat = 'identity') +
            labs(x = 'Deaths', y = NULL, title = 'Total deaths',
                 subtitle = '') +
            scale_fill_brewer(type = 'qual', palette = 6, direction = -1,
                              labels = c('No', 'Yes')) +
            theme(legend.position = 'bottom'),
          ggplot(x, aes(deaths / length, cause, fill = Ongoing)) +
            geom_bar(stat = 'identity') +
            labs(x = 'Deaths', y = NULL, title = 'Yearly deaths',
                 subtitle = note) +
            scale_fill_brewer(type = 'qual', palette = 6, direction = -1,
                              labels = c('No', 'Yes')) +
            theme(legend.position = 'bottom'))

ggsave('us-COVID-19-deaths-vs-wars.png', height = 6, width = 16, dpi = 300)

ggplot(x, aes(per.mil, cause, fill = Ongoing)) +
  geom_bar(stat = 'identity') +
  labs(x = 'Deaths per million', y = NULL, title = 'Deaths per million',
       subtitle = '(relative to population at the beginning of the first decade)') +
  scale_fill_brewer(type = 'qual', palette = 6, direction = -1,
                    labels = c('No', 'Yes')) +
  theme(legend.position = 'bottom')

ggsave('us-COVID-19-deaths-vs-wars-per-million.png',
       height = 6, width = 8, dpi = 300)

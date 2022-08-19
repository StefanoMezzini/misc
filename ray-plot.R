library('dplyr')   # for data wrangling (e.g., tibble, %>%, transmute())
library('purrr')   # for functional programming (e.g., map_***(), map2_***())
library('tidyr')   # for data wrangling (e.g., pivot_*())
library('ggplot2') # for fancy plots

theme_set(theme_void() + theme(legend.position = 'top'))

add_coords <- function(.d, colname, unique_labels, rad_offset = 0) {
  colnames(.d)[colnames(.d) == colname] <- 'label'
  
  .d <-
    .d %>%
    mutate(label_angle =
             map_dbl(label,
                     function(x) {
                       # find position
                       i <- which(unique_labels == x) - 1
                       
                       # find angle on unit circle
                       angle <- 0.5 - 2 / length(unique_labels) * i + rad_offset
                       return(angle)
                     }),
           label_x = cospi(label_angle),
           label_y = sinpi(label_angle))
  
  label_indices <- which(grepl('label', colnames(.d)))
  label_cols <- colnames(.d)[label_indices]
  new_cols <- stringi::stri_replace(label_cols, replacement = colname, regex = 'label')
  
  colnames(.d)[label_indices] <- new_cols
  
  return(.d)
}

ids <- letters[1:10]

d <- combn(ids, m = 2) %>% # create all combinations of a to j (not repeated)
  t() %>% # transpose the matrix (switch rows and columns)
  data.frame() %>% # convert to a data.frame to name columns
  tibble() %>% # convert to a tibble for ease of data wrangling
  # rename columns and create new ones, but drop the old ones
  transmute(from = X1, # starting individual at each line
            to = X2, # ending individual of each line
            connection = map2_chr(X1, X2, \(..x, ..y) paste(..x, ..y, sep = ' to ')),
            relatedness = rbeta(n(), shape1 = 1, shape2 = 3)) %>% # between two vertices
  add_coords(colname = 'from', unique_labels = ids) %>% # add coords on unit circle
  add_coords(colname = 'to', unique_labels = ids)

# find all vertices for labels
vertices <- tibble(label = unique(c(d$from, d$to))) %>%
  add_coords(colname = 'label', unique_labels = ids)

# basic plot with segments
ggplot() +
  coord_equal() +
  geom_segment(aes(x = from_x, xend = to_x, y = from_y, yend = to_y, alpha = relatedness),
               d, lwd = 1) +
  geom_point(aes(label_x, label_y), vertices) +
  geom_label(aes(label_x, label_y, label = label), vertices) +
  scale_alpha(range = c(0, 1), limits = c(0, 1))

d %>%
  select(-c(from_angle, to_angle)) %>%
  pivot_longer(c(from_x, from_y, to_x, to_y), names_sep = '_',
               names_to = c('position', 'coord')) %>%
  pivot_wider(names_from = coord, values_from = value) %>%
  bind_rows(tibble(connection = unique(d$connection),
                   x = 0,
                   y = 0,
                   position = 'center') %>%
              left_join(select(d, connection, relatedness), by = 'connection')) %>%
  mutate(position = factor(position, levels = c('from', 'center', 'to'))) %>%
  arrange(connection, position) %>%
  ggplot() +
  coord_equal() +
  ggforce::geom_bezier(aes(x, y, group = connection, alpha = relatedness)) +
  geom_label(aes(label_x, label_y, label = label), vertices) +
  scale_alpha(range = c(0, 1), limits = c(0, 1))

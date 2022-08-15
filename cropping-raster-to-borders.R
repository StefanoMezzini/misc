library('dplyr')          # for data wrangling
library('raster')         # for raster data
library('sf')             # for simple features
library('rgeoboundaries') # for country/world boundaries
library('ggplot2')        # for fancy plots

# select a country's boundaries for cropping
map_boundary <- geoboundaries(country = 'Italy')

# create an arbitrary
K <- 1500 # number of pixels in each direction (longitude, latitude)
r <- tidyr::expand_grid(x = seq(-180, 180, length.out = K),
                        y = seq(-90, 90, length.out = K)) %>%
  mutate(z = sinpi(x / 15) * sinpi(y / 15)) %>% # add a column for fill
  rasterFromXYZ() # convert from tibble to raster

plot(r)

# set values outside the boundaries to NA
r2 <- mask(r, map_boundary)
plot(r2)

# remove values outside the boundaries
r3 <- crop(r2, map_boundary)
plot(r3)

# convert back to tibble format
r3 <- rasterToPoints(r2) %>%
  as_tibble()

# plot the masked and cropped raster
ggplot(r3, aes(x, y, fill = z)) +
  geom_tile() +
  coord_map('mercator') +
  cowplot::theme_map() +
  theme(legend.position = 'none') +
  scale_fill_viridis_c()

library(rgdal)   # for reading shapefiles
library(ggplot2) # for plotting
library(dplyr)   # for filter() and %>%
library(sp)      # for subset.Spatial()
library(sf)      # for st_as_sf()
theme_set(theme_void())

# shapefiles available from StatCanada:
# https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2016-eng.cfm

# import canada shapefile
canada <- readOGR('free-time-shenanigans/canadian-provinces/lpr_000b16a_e.shp',
                  verbose = FALSE) %>%
  st_as_sf()

# subset provinces
sk <- subset(canada, PRENAME == 'Saskatchewan')
ab <- subset(canada, PRENAME == 'Alberta')

ggplot() +
  geom_sf(aes(geometry = geometry), canada) +
  geom_sf(aes(geometry = geometry), sk, color = 'red') +
  geom_sf(aes(geometry = geometry), ab, color = 'red')

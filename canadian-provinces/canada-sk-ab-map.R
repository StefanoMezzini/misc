library(rgdal)   # for reading shapefiles
library(ggplot2) # for plotting
library(dplyr)   # for filter() and %>%
library(sp)      # for subset.Spatial()
library(sf)      # for st_as_sf()
theme_set(theme_void())

# shapefiles available from StatCanada:
# https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2016-eng.cfm

# import canada shapefile
canada.sf <- readOGR('misc/canadian-provinces/lpr_000b16a_e.shp', verbose = FALSE) %>%
  st_as_sf()

# subset provinces
sk <- subset(canada.sf, PRENAME == 'Saskatchewan') %>%
  as_Spatial() %>%
  fortify() %>%
  as_tibble()
ab <- subset(canada.sf, PRENAME == 'Alberta') %>%
  as_Spatial() %>%
  fortify() %>%
  as_tibble()

canada <- map_data('world', region = 'Canada') 

# without data, geom_map() only plots SK and AB
ggplot() +
  coord_map(projection = 'albers', parameters =c(50, 70)) +
  geom_polygon(aes(long, lat, group = group), canada, fill = 'grey', col = 'black',
               size = 0.5) +
  geom_polygon(aes(long, lat), sk, color = 'red', fill = 'transparent') +
  geom_polygon(aes(long, lat), ab, color = 'red', fill = 'transparent')

# geom_sf() plots all of canada, but this may take a while...
ggplot() +
  geom_sf(aes(geometry = geometry), canada.sf) +
  geom_sf(aes(geometry = geometry), sk, color = 'red') +
  geom_sf(aes(geometry = geometry), ab, color = 'red')

# with data, geom_map() plots the relevant parts of canada
range(canada$long)
range(canada$lat)

tibble(long = c(-140, -100, -50),
       lat = c(42, 80, 45)) %>%
  ggplot() +
  geom_map(map = canada, aes(group = group, map_id = region), canada,
           fill = 'grey', col = 'black', size = 0.5) +
  geom_sf(aes(geometry = geometry), sk, color = 'red', stat = 'sf') +
  geom_sf(aes(geometry = geometry), ab, color = 'red', stat = 'sf') +
  geom_point(aes(long, lat), color = 'red')

as_Spatial(sk) %>%
  fortify() %>%
  as_tibble()

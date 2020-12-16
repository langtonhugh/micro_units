library(patchwork)
library(osmdata)
library(ggplot2)
library(dplyr)
library(sf)

# get milton keynes boundary
mk_bb_sf <- getbb("Milton Keynes united kingdom", format_out = "sf_polygon")

# get major boundary (rather than the village) and project
mk_sf <-mk_bb_sf %>% 
  slice(1) %>% 
  st_transform(crs = 27700)

# check
ggplot(data = mk_sf) +
  geom_sf()

# read in street networks for the same region
sp_sf <- st_read("data/SP_RoadLink.shp")

# remove highways
sp_roads_sf <- sp_sf %>% 
  filter(function. != "Motorway")

# check
ggplot() +
  geom_sf(data = sp_roads_sf) +
  geom_sf(data = mk_sf, col = "red")

# intersect for MK
sp_mk_sf <- st_intersection(sp_roads_sf, mk_sf)

# check
ggplot() +
  geom_sf(data = mk_sf) +
  geom_sf(data = sp_mk_sf) 

# get 'centre'
mk_centre_df <- tibble(lat = 52.0350798, lon = -0.7498433)

# make spatial
mk_centre_sf <- mk_centre_df %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(27700)

# create buffer
mk_buffer_sf <- st_buffer(mk_centre_sf, 7500)

# check
ggplot() +
  geom_sf(data = mk_sf) +
  geom_sf(data = mk_buffer_sf)

# clip to buffer
sp_mk_centre_sf <- st_intersection(sp_roads_sf, mk_buffer_sf)

# check (good enough!)
ggplot() +
  geom_sf(data = mk_sf) +
  geom_sf(data = sp_mk_sf) +
  geom_sf(data = sp_mk_centre_sf, col = "red")

# descriptives
mean(sp_mk_centre_sf$length)
median(sp_mk_centre_sf$length)
min(sp_mk_centre_sf$length)
max(sp_mk_centre_sf$length)

# visualize (raw)
ggplot(data = sp_mk_centre_sf) +
  geom_histogram(mapping = aes(x = length), bins = 100) +
  geom_vline(xintercept = mean(sp_mk_centre_sf$length), col = "red") +
  geom_vline(xintercept = median(sp_mk_centre_sf$length), col = "blue")

# visualize (removed outliers)
ggplot(data = filter(sp_mk_centre_sf, length < 500)) +
  geom_histogram(mapping = aes(x = length), bins = 100) +
  geom_vline(xintercept = mean(sp_mk_centre_sf$length), col = "red") +
  geom_vline(xintercept = median(sp_mk_centre_sf$length), col = "blue")

# but do these end and begin at nodes?
case_study_buffer_sf <- st_buffer(mk_centre_sf, 500)
case_study_sf <- st_intersection(sp_mk_centre_sf, case_study_buffer_sf)

# check (yes)
ggplot(data = case_study_sf) +
  geom_sf(mapping = aes(colour = identifier), size = 1.5) +
  theme(legend.position = "none")

# convert to points (vertices along each line)
case_study_pts_sf <- case_study_sf %>% 
  st_cast(to = "POINT")

# calcualte distance between each vertices along line
case_study_pts_sf <- case_study_pts_sf %>% 
  mutate(sinous = st_distance(case_study_pts_sf))

# get the beginning and end points

case_study_ends_sf <- case_study_pts_sf$sinous %>% 
  as_tibble() %>% 
  slice(1, nrow(.)) 


class(case_study_pts_sf$sinous)
min(case_study_pts_sf$sinous)
max(case_study_pts_sf$sinous)
mean(case_study_pts_sf$sinous)
median(case_study_pts_sf$sinous)
sum(is.na(case_study_pts_sf$sinous))

# compare
p1 <- ggplot(data = case_study_pts_sf) +
  geom_density(mapping = aes(x = length))

p2 <- ggplot(data = case_study_pts_sf) +
  geom_density(mapping = aes(x = as.numeric(sinous)))

p1 / p2



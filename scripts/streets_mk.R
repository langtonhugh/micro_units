library(tmap)
library(patchwork)
library(osmdata)
library(ggplot2)
library(dplyr)
library(tidyr)
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
# ggplot() +
#   geom_sf(data = sp_roads_sf) +
#   geom_sf(data = mk_sf, col = "red")

# intersect for MK
sp_mk_sf <- st_intersection(sp_roads_sf, mk_sf)

# check
# ggplot() +
#   geom_sf(data = mk_sf) +
#   geom_sf(data = sp_mk_sf) 

# get 'centre'
mk_centre_df <- tibble(lat = 52.0350798, lon = -0.7498433)

# make spatial
mk_centre_sf <- mk_centre_df %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(27700)

# create buffer
mk_buffer_sf <- st_buffer(mk_centre_sf, 7500)

# check
# ggplot() +
#   geom_sf(data = mk_sf) +
#   geom_sf(data = mk_buffer_sf)

# clip to buffer
sp_mk_centre_sf <- st_intersection(sp_roads_sf, mk_buffer_sf)

# check (good enough!)
# ggplot() +
#   geom_sf(data = mk_sf) +
#   geom_sf(data = sp_mk_sf) +
#   geom_sf(data = sp_mk_centre_sf, col = "red")

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

# calculate sinuosity
sp_mk_centre_simple_sf <- sp_mk_centre_sf %>% 
  mutate(length_raw = st_length(.)) %>%          # more accurate than OS lengths which are rounded
  st_simplify(dTolerance = 99) %>%               # simplify lines to straight lines
  mutate(length_str = st_length(.),              # calculate length of straight lines
         length_dif = length_raw-length_str,     # calculate difference between raw and simplified
         sinuosity = length_raw/length_str) %>%  # raw length / start-end length
  filter(is.finite(sinuosity))                   # these are closed roads i.e. start == end

# check the very high cases - manual checks suggest could be errors
outliers_sf <- sp_mk_centre_simple_sf %>% 
  filter(as.numeric(sinuosity) > 3)

# filter out these outliers from the raw roads
outliers_roads_sf <- sp_mk_centre_sf %>% 
  filter(identifier %in% outliers_sf$identifier)

# plot
# tm_shape(outliers_roads_sf) +
#   tm_lines() +
#   tm_facets(by = "identifier") 

# interestingly, many of these are 'places' which tend to be enclosed suburban aroads - common in the UK

# descriptives

# function for mode (https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mk_stats_df <- sp_mk_centre_simple_sf %>% 
  as_tibble() %>% 
  select(identifier, length_raw:sinuosity) %>% 
  mutate(across(length_raw:sinuosity, round, 2),
         across(length_raw:sinuosity, as.numeric)) %>% 
  summarise(mean_raw    = mean(length_raw),
            median_raw  = median(length_raw),
            sd_raw      = sd(length_raw),
            min_raw     = min(length_raw),
            max_raw     = max(length_raw),
            mean_diff   = mean(length_dif),
            median_diff = median(length_dif),
            sd_diff     = sd(length_dif),
            min_diff    = min(length_dif),
            max_diff    = max(length_dif),
            mode_diff   = Mode(length_dif),
            mean_sin    = mean(sinuosity),
            median_sin  = median(sinuosity),
            sd_sin      = sd(sinuosity),
            max_sin     = max(sinuosity),
            min_sin     = min(sinuosity),
            mode_sin    = Mode(sinuosity)) %>% 
  mutate(across(mean_raw:mode_sin, round, 2),
         id = 1) %>% 
  pivot_longer(cols = -id, names_to = "statistic", values_to = "mk_metres") %>% 
  select(-id)

# save
sp_mk_centre_simple_sf %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  write_csv(path = "results/mk_roads_simple_df.csv")

write_csv(x = mk_stats_df, path = "results/mk_stats.csv")

# for visuals
sp_mk_centre_mini_df <- sp_mk_centre_simple_sf %>% 
  as_tibble() %>% 
  select(identifier, length_raw:sinuosity) %>% 
  mutate(across(length_raw:sinuosity, round, 2),
         across(length_raw:sinuosity, as.numeric)) %>% 
  pivot_longer(cols = -identifier, names_to = "statistic", values_to = "value")

# visualise raw distributions
ggplot(data = sp_mk_centre_mini_sf) +
  geom_histogram(mapping = aes(x = value), bins = 200) +
  facet_wrap(~statistic, scales = "free")

# outlier function (https://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset)
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# remove outliers
sp_mk_centre_mini_no_df <- sp_mk_centre_simple_sf %>% 
  as_tibble() %>% 
  select(identifier, length_raw:sinuosity) %>% 
  mutate(across(length_raw:sinuosity, as.numeric),
         across(length_raw:sinuosity, remove_outliers)) %>% # makes them missing, not dropped
  pivot_longer(cols = -identifier, names_to = "statistic", values_to = "value")

ggplot(data = sp_mk_centre_mini_no_df) +
  geom_histogram(mapping = aes(x = value), bins = 100) +
  facet_wrap(~statistic, scales = "free")

ggplot(data = filter(sp_mk_centre_mini_df, statistic == "length_raw" | statistic == "length_str")) +
  geom_histogram(mapping = aes(x = value, group = statistic, fill = statistic), bins = 200)



# # case study to find a way to calculate sinuosity of these street segments
# # but do these end and begin at nodes?
# case_study_buffer_sf <- st_buffer(mk_centre_sf, 100)
# case_study_sf <- st_intersection(sp_mk_centre_sf, case_study_buffer_sf)
# 
# # check (yes)
# ggplot(data = case_study_sf) +
#   geom_sf(mapping = aes(colour = identifier), size = 1.5) +
#   theme(legend.position = "none")
# 
# # convert to points (vertices along each line)
# case_study_pts_sf <- case_study_sf %>% 
#   st_cast(to = "POINT")
# 
# ggplot() +
#   geom_sf(data = case_study_sf, mapping = aes(colour = identifier), size = 1) +
#   geom_sf(data = case_study_pts_sf, mapping = aes(colour = identifier), size = 2) +
#   theme(legend.position = "none")
# 
# # calcualte distance between each vertices along line
# case_study_pts_sf <- case_study_pts_sf %>% 
#   mutate(sinous = st_distance(case_study_pts_sf))
# 
# # try simplify method
# case_study_simple_sf <- st_simplify(case_study_sf, dTolerance = 9999)
# 
# 
# ggplot() +
#   geom_sf(data = case_study_sf, mapping = aes(colour = identifier), size = 2, alpha = 0.5) +
#   geom_sf(data = case_study_pts_sf, mapping = aes(colour = identifier), size = 2, alpha = 0.5) +
#   geom_sf(data = case_study_simple_sf, mapping = aes(colour = identifier), size = 1) +
#   theme(legend.position = "none")
# 
# case_study_simple_sf <- case_study_simple_sf %>% 
#   mutate(length_str = st_length(.))




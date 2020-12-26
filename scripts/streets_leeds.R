library(osmdata)
library(ggplot2)
library(dplyr)
library(sf)


# get leeds boundary
leeds_osm_bb_sf <- getbb("leeds united kingdom", format_out = "sf_polygon")

# check
ggplot(data = leeds_osm_bb_sf) +
  geom_sf()

# project to BNG
leeds_osm_bb_sf <- leeds_osm_bb_sf %>% 
  st_transform(crs = 27700)

# pull city
leeds_bb_sf <- leeds_osm_bb_sf[1,]

# check
ggplot(data = leeds_bb_sf) +
  geom_sf()

# read in street networks - leedsingham crosses over three separate OS squares. Already BNG.
SE_sf <- st_read("data/SE_RoadLink.shp")

leeds_lines_sf <- st_intersection(SE_sf, leeds_bb_sf)

leeds_roads_sf <- leeds_lines_sf %>% 
  filter(function. != "Motorway")

# check
ggplot() +
  geom_sf(data = leeds_bb_sf) +
  geom_sf(data = leeds_roads_sf)

# calculate sinuosity
leeds_simple_sf <- leeds_roads_sf %>% 
  mutate(length_raw = st_length(.)) %>%          # more accurate than OS lengths which are rounded
  st_simplify(dTolerance = 99) %>%               # simplify lines to straight lines
  mutate(length_str = st_length(.),              # calculate length of straight lines
         length_dif = length_raw-length_str,     # calculate difference between raw and simplified
         sinuosity = length_raw/length_str) %>%  # raw length / start-end length
  filter(is.finite(sinuosity))                   # these are closed roads i.e. start == end

# descriptives

# function for mode (https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

leeds_stats_df <- leeds_simple_sf %>% 
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
  pivot_longer(cols = -id, names_to = "statistic", values_to = "leeds_metres") %>% 
  select(-id)

# save
leeds_simple_sf %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  write_csv(path = "results/leeds_roads_simple_df.csv")

write_csv(x = leeds_stats_df, path = "results/leeds_stats.csv")

# for visuals
leeds_simple_mini_sf <- leeds_simple_sf %>% 
  as_tibble() %>% 
  select(identifier, length_raw:sinuosity) %>% 
  mutate(across(length_raw:sinuosity, round, 2),
         across(length_raw:sinuosity, as.numeric)) %>% 
  pivot_longer(cols = -identifier, names_to = "statistic", values_to = "value")

# visualise raw distributions
ggplot(data = leeds_simple_mini_sf) +
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
leeds_simple_mini_no_sf <- leeds_simple_sf %>% 
  as_tibble() %>% 
  select(identifier, length_raw:sinuosity) %>% 
  mutate(across(length_raw:sinuosity, as.numeric),
         across(length_raw:sinuosity, remove_outliers)) %>% # makes them missing, not dropped
  pivot_longer(cols = -identifier, names_to = "statistic", values_to = "value")

ggplot(data = leeds_simple_mini_no_sf) +
  geom_histogram(mapping = aes(x = value), bins = 100) +
  facet_wrap(~statistic, scales = "free")

ggplot(data = filter(leeds_simple_mini_no_sf, statistic == "length_raw" | statistic == "length_str")) +
  geom_histogram(mapping = aes(x = value, group = statistic, fill = statistic), bins = 200)

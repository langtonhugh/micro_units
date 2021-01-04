# Load required packages.
library(osmdata)
library(dplyr)
library(sf)
library(purrr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(cowplot)
library(patchwork)

# Name cities + country for OSM query to define study regions.
cities_vec <- paste(c("Birmingham", "Leeds"     , "Milton Keynes",
                      "Edinburgh" , "Manchester", "Liverpool",
                      "Newcastle" , "York"),
                    c("United Kingdom"))

# Make query to OSM API.
osm_cities_list <- list()

for (i in cities_vec){
  query_i <- getbb(i, format_out = "sf_polygon")
  osm_cities_list[[i]] <- query_i
}

# Check.
# View(osm_cities_list)

# Some pull more than one polygon, but the first is usually the main city,
# so we subset each element for this.
osm_cities_clean_list <- lapply(osm_cities_list, function(x){
  x[1, ]
})

# Check visually.
# city_boundaries <- lapply(osm_cities_clean_list, function(x){ggplot(x) + geom_sf()})
# wrap_plots(city_boundaries, ncol = 2)

# Name elements of list.
names(osm_cities_clean_list) <- cities_vec

# Load in the relevant Ordnance Survey squares. These have been selected
# to cover the cities pulled from OSM.
SK_sf <- st_read("data/SK_RoadLink.shp") # Birmingham
SO_sf <- st_read("data/SO_RoadLink.shp") # Birmingham
SP_sf <- st_read("data/SP_RoadLink.shp") # Birmingham / Milton Keynes 
NT_sf <- st_read("data/NT_RoadLink.shp") # Edinburgh
SE_sf <- st_read("data/SE_RoadLink.shp") # Leeds / York
NZ_sf <- st_read("data/NZ_RoadLink.shp") # Newcastle
SD_sf <- st_read("data/SD_RoadLink.shp") # Manchester
SJ_sf <- st_read("data/SJ_RoadLink.shp") # Manchester / Liverpool

# Bind rows for Birmingham.
SK_SO_SP_sf <- bind_rows(SK_sf, SO_sf, SP_sf)

# Bind rows for Manchester.
SD_SJ_sf <- bind_rows(SD_sf, SJ_sf)

# Create list of these roads, matching the cities vec.
roads_list <- list(SK_SO_SP_sf, SE_sf, SP_sf, NT_sf, SD_SJ_sf, SJ_sf, NZ_sf, SE_sf)

# Name elements according to the cities.
names(roads_list) <- cities_vec

# Remove exiting to save memory.
rm(SK_sf, SO_sf, SP_sf, NT_sf, SE_sf, NZ_sf, SK_SO_SP_sf, SD_SJ_sf, SJ_sf)

# Remove motorways for each road lines.
roads_list <- lapply(roads_list, function(x){
  x %>% 
    filter(function. != "Motorway")
})

# Project CRS of cities for intersection. Roads are already BNG.
osm_cities_clean_list <- lapply(osm_cities_clean_list, function(x){
  x %>% 
    st_transform(crs = 27700)
})

# Run intersection for the roads list against the cities list.
cities_roads_list_sf <- map2(roads_list, osm_cities_clean_list, st_intersection)

# Save and load. Run as appropriate.
# save.image(file = "auto_cities.RData")
# load(file = "auto_cities.RData")

# Street segment example.
manc_sf <- cities_roads_list_sf[["Manchester United Kingdom"]]
manc_sr_sf <- osm_cities_clean_list[["Manchester United Kingdom"]]

# Create 500 metres buffer in centre.
manc_buf_sf <- manc_sr_sf %>% 
  st_centroid() %>% 
  st_buffer(dist = 500) 

# Clip roads to buffer.
manc_ex_sf <- st_intersection(manc_sf, manc_buf_sf)

# Plot visual.
manc_ex_gg <- ggplot() +
  geom_sf(data = manc_buf_sf) + 
  geom_sf(data = manc_ex_sf, 
          mapping = aes(colour = identifier), size = 1.2) +
  theme(legend.position = "none",
        axis.text = element_text(size = 6))

# Save visual.
# ggsave(plot = manc_ex_gg, filename = "visuals/manc_ex.png",
#        height = 15, width = 15, unit = "cm", dpi = 200)

# Study region visuals.
study_viz_fun <- function(x, y){
  ggplot() +
    geom_sf(data = y, colour = "black", size = 0.2) +
    geom_sf(data = x, fill = "transparent", size = 1.1) +
    theme_void() 
}

# Create visual of study regions.
study_regions <- map2(osm_cities_clean_list, cities_roads_list_sf, study_viz_fun)
study_regions_gg <- plot_grid(plotlist = study_regions, ncol = 2,
                              labels = str_remove_all(cities_vec, " United Kingdom"))

# Save.
# ggsave(plot = study_regions_gg, filename = "visuals/study_regions_hq.png",
#        height = 60, width = 30, unit = "cm", dpi = 600)
ggsave(plot = study_regions_gg, filename = "visuals/study_regions_lq.png",
       height = 60, width = 30, unit = "cm", dpi = 300)

# Creat function to calculate new length variables and sinuosity.
sin_fun <- function(x){
  x %>% 
  mutate(length_raw = st_length(.)) %>%          # more accurate than OS lengths which are rounded
  st_simplify(dTolerance = 99) %>%               # simplify lines to straight lines
  mutate(length_str = st_length(.),              # calculate length of straight lines
         length_dif = length_raw-length_str,     # calculate difference between raw and simplified
         sinuosity = length_raw/length_str) %>%  # raw length / start-end length
  filter(is.finite(sinuosity))                   # these are closed roads i.e. start == end
}

# Loop function through.
cities_roads_simple_list_sf <- lapply(cities_roads_list_sf, sin_fun)

# Function for mode (https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode).
# Mode <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }

# Function to generate descriptives. Columns named to make it a decent table.
stat_fun <- function(x) {
x %>% 
  as_tibble() %>% 
  select(identifier, length_raw:sinuosity) %>% 
  mutate(across(length_raw:sinuosity, as.numeric)) %>%
  summarise(`Mean length (metres)`    = mean(length_raw),
            `Median length (metres)`  = median(length_raw),
            `SD length (metres)`      = sd(length_raw),
            `Min. length (metres)`    = min(length_raw),
            `Max. length (metres)`    = max(length_raw),
            `Mean diff. (metres)`     = mean(length_dif),
            `Median diff. (metres)`   = median(length_dif),
            `SD diff. (metres)`       = sd(length_dif),
            `Min. diff. (metres)`     = min(length_dif),
            `Max. diff. (metres)`     = max(length_dif),
            `Mean sin.`      = mean(sinuosity),
            `Median sin.`    = median(sinuosity),
            `SD sin.`        = sd(sinuosity),
            `Min. sin.`      = min(sinuosity),
            `Max. sin.`      = max(sinuosity)) %>% 
    mutate(across(`Mean length (metres)`:`Max. sin.`, round, 6))
}

# Loop through to generate stats.
cities_roads_stats_list <- lapply(cities_roads_simple_list_sf, stat_fun)

# Create ID column in each df of list.
cities_roads_stats_list <- cities_roads_stats_list %>% 
  map2(cities_vec, ~.x %>% mutate(City = .y))

# Calculate proportion of streets with a sinuosity of ~1.
# First, round sinuoity to 6 decimal places (arbitary but reasonable, I think).
round_fun <- function(x){
  x %>% 
    mutate(sin_rounded = round(as.numeric(sinuosity), 6))
}

cities_roads_simple_list_sf <- lapply(cities_roads_simple_list_sf, round_fun)

# Pull out random examples of high and low sinuosity.
set.seed(1612)

example_simple_sf <- cities_roads_simple_list_sf[[1]] %>%
  arrange(sin_rounded) %>%
  filter(sin_rounded > 2) %>%
  sample_n(size = 1)

example_orig_sf <- cities_roads_list_sf[[1]] %>%
  filter(identifier == example_simple_sf$identifier)

example_simple_sf$sin_rounded

high_sin <- ggplot() +
  geom_sf(data = example_orig_sf, size = 2) +
  geom_sf(data = example_simple_sf, col = "Red") +
  labs(title = "High sinuosity (~7)") +
  theme(axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6, angle = 45, vjust = 0.5),
        axis.ticks = element_blank())

set.seed(1612)

example_simple_sf <- cities_roads_simple_list_sf[[1]] %>%
  arrange(sin_rounded) %>%
  filter(sin_rounded > 1 & sin_rounded < 1.5) %>%
  sample_n(size = 1)

example_orig_sf <- cities_roads_list_sf[[1]] %>%
  filter(identifier == example_simple_sf$identifier)

example_simple_sf$sin_rounded

low_sin <- ggplot() +
  geom_sf(data = example_orig_sf, size = 2) +
  geom_sf(data = example_simple_sf, col = "Red") +
  labs(title = "Low sinuosity (~1.08)") +
  theme(axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6, angle = 45, vjust = 0.5),
        axis.ticks = element_blank())

sin_example_gg <- plot_grid(low_sin, high_sin, ncol = 2)

# ggsave(plot = sin_example_gg, filename = "visuals/sin_example.png",
#        height = 8, width = 12)

# Produce percentages.
sin_one_fun <- function(x) {
prop.table(table(x$sin_rounded))[[1]]
}

one_sins_df <- as.data.frame(lapply(cities_roads_simple_list_sf, sin_one_fun)) %>% 
  mutate(id = 1) %>% 
  pivot_longer(cols = -id, values_to = "Prop. straight", names_to = "City") %>% 
  select(`Prop. straight`)

# Join each.
stats_df <- cities_roads_stats_list %>% 
  bind_rows() %>%
  select(City, `Mean length (metres)`:`Max. sin.`) %>%
  mutate(City = str_remove_all(City, " United Kingdom"),
         City = if_else(condition = City == "Milton Keynes", true = "M. Keynes",
                        false = City)) %>%
  bind_cols(one_sins_df) %>% # same order
  mutate(across(`Mean length (metres)`:`Prop. straight`, round, 2)) %>% 
  select(City: `Max. length (metres)`, `Mean sin.`:`Prop. straight`)
  
# Save.
write_csv(x = stats_df, path = "results/cities_roads_stats.csv")

# Outlier function (https://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset).
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# Transform raw data for visuals. Include vars with outliers as missings.
transform_fun <- function(x){
  x %>% 
    as_tibble() %>% 
    select(identifier, length_raw:sinuosity) %>% 
    mutate(across(length_raw:sinuosity, round, 6),
           across(length_raw:sinuosity, as.numeric),
           length_raw_no = remove_outliers(length_raw),
           length_str_no = remove_outliers(length_str),
           length_dif_no = remove_outliers(length_dif),
           sinuosity_no  = remove_outliers(sinuosity)) 
}

# Run function through list.
cities_roads_list_df <- lapply(cities_roads_simple_list_sf, transform_fun)

# Create ID column for each df in the list.
cities_roads_list_df <- cities_roads_list_df %>% 
  map2(cities_vec, ~.x %>% mutate(city_id = .y))

# Reorder columns to be more intuitive.
cities_roads_list_df <- lapply(cities_roads_list_df, function(x){
  x %>% 
    select(city_id, identifier:sinuosity_no)
})

# Bind the different city data frames together
cities_roads_df <- bind_rows(cities_roads_list_df)

# Create samples from each to equalise the N street segments.
set.seed(1612)
cities_roads_samples_df <- cities_roads_df %>% 
  group_by(city_id) %>% 
  sample_n(size = 1000, replace = FALSE) %>% 
  ungroup() %>% 
  mutate(city_id = str_remove_all(city_id, " United Kingdom"))

# Check sample sizes.
table(cities_roads_samples_df$city_id)

# Visualise the raw length and non-outlier length of street segments.
# Raw.
length_facet_gg <- ggplot(data = cities_roads_samples_df) +
  geom_histogram(mapping = aes(length_raw, fill = city_id), bins = 100) +
  facet_wrap(~ city_id, scale = "free", nrow = 2) +
  labs(y = NULL, x = "metres", title = "Raw lengths, free axis (N = 1000)") +
  theme_bw() +
  theme(legend.position = "free",
        axis.text = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 7),
        strip.background = element_rect(fill = "transparent"),
        plot.title = element_text(size = 8))

# No outliers.
length_no_facet_gg <- ggplot(data = cities_roads_samples_df) +
  geom_histogram(mapping = aes(length_raw_no, fill = city_id), bins = 100) +
  facet_wrap(~ city_id, scale = "fixed", nrow = 2) +
  labs(y = NULL, x = "metres", title = "Outliers removed, fixed axis (N = 1000)") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 7),
        strip.background = element_rect(fill = "transparent"),
        plot.title = element_text(size = 8))

# Combine.
facets_full <- length_facet_gg / length_no_facet_gg

# Save facet.
ggsave(plot = facets_full, filename = "visuals/facets_length.png",
       height = 20, width = 18, unit = "cm", dpi = 300)

# Save individual.
ggsave(plot = length_facet_gg, filename = "visuals/length.png",
       height = 10, width = 15, unit = "cm", dpi = 300)
ggsave(plot = length_no_facet_gg, filename = "visuals/length_no.png",
       height = 10, width = 15, unit = "cm", dpi = 300)






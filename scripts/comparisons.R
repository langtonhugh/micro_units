
library(readr)
library(dplyr)
library(ggplot2)

# load in existing results
leeds_roads_simple_df <- read_csv("results/leeds_roads_simple_df.csv")
leeds_stats_df <- read_csv("results/leeds_stats.csv")

mk_roads_simple_df <- read_csv("results/mk_roads_simple_df.csv")
mk_stats_df <- read_csv("results/mk_stats.csv")

birm_roads_simple_df <- read_csv("results/birm_roads_simple_df.csv")
birm_stats_df <- read_csv("results/birm_stats.csv")

edin_roads_simple_df <- read_csv("results/edin_roads_simple_df.csv")
edin_stats_df <- read_csv("results/edin_stats.csv")

# combine the stats
stats_df <- left_join(leeds_stats_df, mk_stats_df) %>%
  left_join(birm_stats_df) %>% 
  left_join(edin_stats_df)

# combine the raw data
leeds_sub_df <- leeds_roads_simple_df %>% 
  select(length_raw:sinuosity) %>% 
  mutate(city = "leeds")

mk_sub_df <- mk_roads_simple_df %>% 
  select(length_raw:sinuosity) %>% 
  mutate(city = "milton keynes")

birm_sub_df <- birm_roads_simple_df %>% 
  select(length_raw:sinuosity) %>% 
  mutate(city = "birmingham")

edin_sub_df <- edin_roads_simple_df %>% 
  select(length_raw:sinuosity) %>% 
  mutate(city = "edinburgh")

total_sub_df <- bind_rows(leeds_sub_df, mk_sub_df, birm_sub_df, edin_sub_df)

# gini for each city (higher gini = less uniform in length)
gini_results <- total_sub_df %>% 
  group_by(city) %>% 
  summarise(length_gini = lorenzgini::gini(length_raw),
            sin_gini    = lorenzgini::gini(sinuosity)) %>% 
  mutate(statistic = "gini") 
  

# total length of roads in each city
total_sub_df %>% 
  group_by(city) %>% 
  select(city_length) %>% 
  distinct()

# outlier function (https://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset)
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# create new cols with outliers missing, and calculate proportional
# figure for each road.
total_sub_df <- total_sub_df %>% 
  group_by(city) %>% 
  mutate(sinuosity_no    = remove_outliers(sinuosity),
         length_raw_no   = remove_outliers(length_raw),
         length_dif_no   = remove_outliers(length_dif),
         city_length     = sum(length_raw_no, na.rm = T),
         prop_raw_length = 100*(length_raw_no/city_length)) %>% 
  ungroup()



samples_df <- total_sub_df %>% 
  group_by(city) %>% 
  sample_n(size = 1000, replace = FALSE) %>% 
  ungroup()

table(samples_df$city) 

# remove outliers
samples_df <- samples_df %>%
  group_by(city) %>% 
  mutate(length_raw_no = remove_outliers(length_raw))


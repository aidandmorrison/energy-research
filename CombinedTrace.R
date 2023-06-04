source("trace_tools.R")
library(plotly)

wind_cut_list <- c(
  "West NSW",
  "New England",
  "Riverina",
  "Central Highlands",
  "Northern SA",
  "Mid-North SA",
  "Western Victoria",
  "Central North Vic",
  "Fitzroy",
  "Hamilton",
  "Lilyvale",
  "^[A-Z]+\\d$",
  "NSW",
  "VIC"
)

solar_cut_list <- c(
  "West NSW",
  "New England",
  "Riverina",
  "Central Highlands",
  "Northern SA",
  "Mid-North SA",
  "Western Victoria",
  "Central North Vic",
  "Fitzroy",
  "North East Tasmania",
  "Hamilton",
  "Lilyvale"
  
)

location_state_map <- list(
  "White Rock" = "NSW",
  "Central-West Orana" = "NSW",
  "Riverland" = "SA",
  "Darling Downs" = "QLD",
  "Clare" = "QLD",
  "Winton" = "VIC",
  "Happy Valley" = "VIC",
  "Quorn Park" = "NSW",
  "Stockyard Hill" = "VIC",
  "Mortlake South" = "VIC"
)

#### Get wind data -------
wind_traces <- list.files("../AEMO_data/wind")
wind_names <- wind_traces %>% all_wind_names_from_traces()
wind_names <- wind_names %>% filter_on_location_cut_list(cut_list = wind_cut_list)
wind_locations <- geocode_from_names(wind_names, location_state_map) %>% 
  mutate(resource = "wind")
source_df_wind <- load_source_df(wind_names, wind_traces, "wind")
long_df_wind <- gather_source_data(source_df_wind) %>% 
  mutate(resource = "wind")


###### Get solar data ------
solar_traces <- list.files("../AEMO_data/solar")
solar_names <- solar_traces %>% all_names_from_traces()
solar_names <- solar_names %>% filter_on_location_cut_list(cut_list = solar_cut_list)
solar_locations <- geocode_from_names(solar_names, location_state_map) %>% 
  mutate(resource = "solar")
source_df_solar <- load_source_df(solar_names, solar_traces)
long_df_solar <- gather_source_data(source_df_solar) %>% 
  mutate(resource = "solar")

#### Combine and basic visuals ----
long_df <- long_df_solar %>% 
  bind_rows(long_df_wind)
geocoded_locations <- solar_locations %>% 
  bind_rows(wind_locations)


### replicated from plot_locations
world <- ne_countries(scale = "medium", returnclass = "sf")
xmin <- geocoded_locations$long %>% min(na.rm = T) + 0
xmax <- geocoded_locations$long %>% max(na.rm = T) + 0
ymin <- geocoded_locations$lat %>% min(na.rm = T) - 0
ymax <- geocoded_locations$lat %>% max(na.rm = T) + 0


# A plot to put a mark on all the locations
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  geom_point(data = geocoded_locations, aes(x = long, y = lat, col = resource), size = 4) +
  geom_text_repel(
    data = geocoded_locations,
    aes(x = long, y = lat, label = location, col = resource),
    size = 3,
    force = 0.5,
    max.overlaps = 50,
    segment.color = "grey50",
    segment.size = 0.2,
    nudge_x = 0.1,
    nudge_y = 0.1
  )


# Iterate through wind locations and find the closest solar location
pairs_df <- data.frame()
solar_candidates <- solar_locations
# for (i in 1:nrow(wind_locations)) {
#   wind_lat <- wind_locations$lat[i]
#   wind_lon <- wind_locations$long[i]
#   
#   closest_solar <- find_closest_solar(wind_lat, wind_lon, solar_candidates)
#   
#   pairs_df <- bind_rows(pairs_df, cbind(wind_locations[i, ], solar = solar_candidates$location))
#   solar_candidates <- solar_candidates %>% 
#     filter(location != closest_solar$location[[1]])
# }

# Loop through wind_locations
for (i in 1:nrow(wind_locations)) {
  
  # Calculate the haversine distance between the current wind location and all solar_candidates
  distances <- mapply(haversine_distance,
                      lat1 = wind_locations$lat[i],
                      lon1 = wind_locations$long[i],
                      lat2 = solar_candidates$lat,
                      lon2 = solar_candidates$long)
  
  # Find the index of the closest solar location
  closest_solar_index <- which.min(distances)
  
  # Create a new data frame with the required columns for the current pair
  current_pair <- data.frame(
    wind = wind_locations$location[i],
    solar = solar_candidates$location[closest_solar_index],
    pair = i
  )
  
  # Bind the current_pair with the pairs_df
  pairs_df <- bind_rows(pairs_df, current_pair)
  
  # Update solar_candidates by removing the matched location
  solar_candidates <- solar_candidates %>% 
    filter(location != solar_candidates$location[closest_solar_index])
}


solar_keys <- pairs_df %>% 
  select(solar, pair) %>% 
  mutate(resource = "solar")
colnames(solar_keys) <- c("location", "pair", "resource")
wind_keys <- pairs_df %>% 
  select(wind, pair) %>% 
  mutate(resource = "wind")
colnames(wind_keys) <- c("location", "pair", "resource")

combined_keys <- solar_keys %>% 
  bind_rows(wind_keys)

geocoded_locations <- geocoded_locations %>% 
  left_join(combined_keys, by = c("location", "resource"))

long_df <- long_df %>% 
  left_join(combined_keys, by = c("location", "resource"))

# A plot to put a mark on all the locations
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  geom_point(data = geocoded_locations, aes(x = long, y = lat, col = as.factor(pair)), size = 4) +
  geom_text_repel(
    data = geocoded_locations,
    aes(x = long, y = lat, label = location, col = as.factor(pair)),
    size = 3,
    force = 0.5,
    max.overlaps = 50,
    segment.color = "grey50",
    segment.size = 0.2,
    nudge_x = 0.1,
    nudge_y = 0.1
  )


##### Pairs Analysis -----
pairs_corr <- correlation_of_pairs(long_df, geocoded_locations)


ggplot(pairs_corr, aes(x = correlation)) +
  geom_density(fill = "lightblue") +
  labs(x = "Correlation",
       y = "Density",
       title = "Density of Correlations Between Solar and Wind Pairs") +
  theme_minimal()

weird_solar <- long_df %>% 
  filter(resource == "solar") %>% 
  filter(half_hour < 6) %>% 
  filter(value > .5)

weird_solar_names <- weird_solar$location %>% unique()

weird_solar$location %>% unique()

non_weird_names <- geocoded_locations %>% 
  filter(!location %in% weird_solar_names) %>%
  pull(location)

shared_sites <- geocoded_locations %>% 
  group_by(location) %>% 
  summarise(count = n()) %>% 
  filter(count == 2) %>% 
  pull(location) %>% 
  unique()

long_df %>% 
  # filter(location %in% (non_weird_names %>% head(40) %>% tail(20))) %>%
  filter(location %in% non_weird_names) %>%
  filter(location %in% shared_sites) %>%
  filter(!location %>% str_detect("ango")) %>% 
  filter(Year == 2027) %>% 
  ggplot(aes(x = half_hour, y = value, col = resource))+
  geom_point(alpha = 0.02) + 
  stat_smooth()+
  facet_wrap("location")

days_summ_paired <- long_df %>% 
  group_by(resource, pair, location, Year, Month, Day) %>% 
  summarise(value = mean(value))

pairs_day_corr <- correlation_of_pairs(days_summ_paired, geocoded_locations)

pairs_day_corr %>% 
  ggplot(aes(x = correlation)) +
  geom_density(fill = "lightblue") +
  labs(x = "Correlation",
       y = "Density",
       title = "Density of Correlations Between Solar and Wind Pairs by day") +
  theme_minimal()


duplicate_summ <- days_summ_paired %>% 
  group_by(resource, pair, Year, Month, Day) %>% 
  summarise(value = mean(value),
            n_reps = n()) %>% 
  ungroup()

duplicate_summ %>% skim()
duplicate_summ %>% filter(n_reps > 1) %>% pull(pair) %>% unique()
geocoded_locations %>% filter(pair == 2) %>% pull(location) %>% unique()


values_wide <- days_summ_paired %>% 
  filter(location %in% non_weird_names) %>% 
  filter(!is.na(pair)) %>% 
  filter(!pair %in% (duplicate_summ %>% filter(n_reps > 1) %>% pull(pair) %>% unique())) %>% 
  ungroup() %>% 
  select(-location) %>% 
  pivot_wider(names_from = resource, values_from = value)

location_labels <- pairs_df %>%
  select(pair, wind_location = wind, solar_location = solar) %>%
  distinct()
values_wide <- values_wide %>%
  left_join(location_labels, by = "pair") %>%
  mutate(location_label = paste(wind_location, solar_location, sep = " / "))


values_wide %>% 
  ggplot(aes(x = solar, y = wind, col= Month))+
  geom_point(alpha = 0.2)+
  facet_wrap(~location_label)+
  scale_color_gradientn(colors = rainbow(5))

#### Combining traces

plot <- long_df %>% 
  filter(location == "Broken Hill") %>% 
  filter(Year == 2027) %>% 
  mutate(day_as_halfhour = Day*48) %>% 
  mutate(monthly_half = day_as_halfhour + half_hour) %>% 
  ggplot(aes(x = monthly_half, y = value, col = resource))+
  geom_line()+
  facet_wrap("Month")
ggplotly(plot)


days_summ_paired %>% 
  group_by(resource) %>% 
  summarise(med = mean(value))


### Modeling the use of storage and dispatch targets

# Set battery_half_hours and dispatch_target
battery_half_hours <- 8 # For example
dispatch_target <- 0.5 # For example

# Split the data frame by location
generator_groups <- long_df %>% group_by(resource, location) %>% group_split()

# Apply the function to each group and bind the results back together
processed_data <- lapply(generator_groups, process_generator, battery_half_hours, dispatch_target) %>% bind_rows()


# Step 1: Calculate mean spillage and mean dispatch_fraction

summary_stats <- processed_data %>%
  mutate(at_zero = case_when(dispatch_fraction == 0 ~ 1, TRUE ~ 0),
         fell_short = case_when(dispatch_fraction < 1 ~ 1, TRUE ~ 0)) %>% 
  group_by(location, resource) %>%
  summarise(spillage = mean(spillage, na.rm = TRUE),
            dispatch_fraction = mean(dispatch_fraction, na.rm = TRUE),
            shortfall = mean(shortfall, na.rm = TRUE),
            time_at_zero = mean(at_zero, na.rm = TRUE),
            time_falling_short = mean(fell_short, na.rm = TRUE)) 



summary_stats_long <- summary_stats %>% 
  gather(key = "key", value = "value", -location, -resource)


summary_stats_long %>% 
  ggplot(aes(x = value, fill = resource))+
  geom_density(alpha = 0.4)+
  facet_grid("key", scales = "free")


### Building multiple stats testing storage and generation effects ------


battery_hours_list <- c(0, 1, 2, 3, 4)
dispatch_target_list <- seq(0.2, 1, 0.2)

stat_sensitivies <- data.frame()

for(b in battery_hours_list){
  for(d in dispatch_target_list){
    filler <- get_dispatch_stats(long_df, b, d)
    stat_sensitivies <- stat_sensitivies %>% bind_rows(filler)
  }
}


trio_names <- c("Broken Hill", "Darling Downs", "Port Augusta")


# # Set up parallel backend
# cores <- detectCores()
# registerDoParallel(cores - 2)
# 
# stat_sensitivities <- foreach(b = battery_hours_list, .combine = 'rbind') %:%
#   foreach(d = dispatch_target_list, .combine = 'rbind') %dopar% {
#     get_dispatch_stats(long_df, b, d)
#   }
# 
# # Stop the parallel backend
# stopImplicitCluster()

stat_sens_long <- stat_sensitivies %>% 
  gather(key = "key", value = "value", -location, -resource, -battery_hours, -dispatch_target)

key_stats <- c("shortfall", "time_at_zero", "time_falling_short", "spillage")
stat_sens_long %>% 
  filter(!((location %in% weird_solar_names) & (resource == "solar"))) %>%
  filter(dispatch_target == 0.2,
         key %in% key_stats) %>%
  ggplot(aes(x = value, fill = as.factor(battery_hours)))+
  geom_density(alpha = 0.4)+
  facet_grid(key ~ resource, scales = "free")

chosen_stat = "spillage"
stat_sens_long %>% 
  filter(!((location %in% weird_solar_names) & (resource == "solar"))) %>%
  filter(key == chosen_stat) %>%
  filter(dispatch_target < 1) %>% 
  ggplot(aes(x = value, fill = as.factor(battery_hours)))+
  geom_density(alpha = 0.4)+
  facet_grid(dispatch_target ~ resource, scales = "free")+
  ggtitle(chosen_stat %>% str_replace_all("_", " "))+
  labs(fill = "Battery Hours") +
  theme(legend.position = "top")


chosen_name <- "Darling Downs"
chosen_name <- "Port Augusta"
chosen_name <- "Broken Hill"
stat_sens_long %>% 
  filter(location == chosen_name) %>% 
  filter(key %in% key_stats) %>% 
  ggplot(aes(x = dispatch_target, y = battery_hours, fill = value))+
  geom_tile()+
  geom_text(aes(label = round(value, 5)), size = 3, color = "black") +
  facet_grid(key ~ resource, scales = "free")+
  scale_fill_gradientn(colours = rainbow(6))+
  scale_x_continuous(breaks = dispatch_target_list)+
  scale_y_continuous(breaks = battery_hours_list)+
  ggtitle(paste("Dispatch Statistics ", chosen_name))


stat_sens_short <- stat_sens_long %>% 
  filter(!((location %in% weird_solar_names) & (resource == "solar"))) %>% 
  select(-location) %>% 
  group_by(resource, battery_hours, dispatch_target, key) %>% 
  summarise_all(median)

stat_sens_short %>% 
  filter(battery_hours == 8) %>% 
  filter(dispatch_target == 0.2) %>% 
  filter(resource == "wind")


#### Doing trio analysis -----

# Dispatch merged from each site (storage NOT pooled)

generator_groups <- long_df %>% 
  filter(location %in% trio_names) %>% 
  group_by(resource, location) %>% 
  group_split()

trio_resource_dispatch_merged <- data.frame()
for(b in battery_hours_list){
  for (d in dispatch_target_list){
    battery_half_hours = b * 2
    
    processed_data <- lapply(generator_groups, process_generator, battery_half_hours, d) %>% bind_rows()
    
    filler <- processed_data %>% 
      group_by(location, Year, Month, Day, half_hour) %>% 
      summarise(value = mean(dispatched)) %>% 
      mutate(resource = "wind_and_solar_own_storage") %>% 
      get_dispatch_stats(b, d)
      
    trio_resource_dispatch_merged <- trio_resource_dispatch_merged %>% bind_rows(filler)
  }
}

spillage_excerpt = stat_sens_long %>% 
  filter(location %in% trio_names) %>% 
  filter(key == "spillage") %>% 
  group_by(location, battery_hours, dispatch_target, key) %>% 
  summarise(value = mean(value)) %>% 
  mutate(resource = "wind_and_solar_own_storage")

trio_resource_dispatch_merged_long <- trio_resource_dispatch_merged %>% 
  gather(key = "key", value = "value", -location, -resource, -battery_hours, -dispatch_target) %>% 
  filter(key != "spillage") %>% 
  bind_rows(spillage_excerpt)

trio_resource_dispatch_merged_long %>% 
  filter(key %in% key_stats) %>% 
  ggplot(aes(x = dispatch_target, y = battery_hours, fill = value))+
  geom_tile()+
  geom_text(aes(label = round(value, 3)), size = 3, color = "black") +
  facet_grid(key ~ location, scales = "free")+
  scale_fill_gradientn(colours = rainbow(6))+
  scale_x_continuous(breaks = dispatch_target_list)+
  scale_y_continuous(breaks = battery_hours_list)+
  ggtitle(paste("Dispatch Statistics wind+solar dispatch merged without pooled storage"))

# Resources merged at each site (storage pooled at each site prior to dispatch throttling)
trio_long_df_resource_merged <- long_df %>% 
  filter(location %in% trio_names) %>% 
  group_by(location, Year, Month, Day, half_hour) %>% 
  summarise(value = mean(value))
trio_long_df_resource_merged <- trio_long_df_resource_merged %>% 
  mutate(resource = "wind_and_solar")

trio_sensitivies <- data.frame()

for(b in battery_hours_list){
  for(d in dispatch_target_list){
    filler <- get_dispatch_stats(trio_long_df_resource_merged, b, d)
    trio_sensitivies <- trio_sensitivies %>% bind_rows(filler)
  }
}

trio_sens_long <- trio_sensitivies %>% 
  gather(key = "key", value = "value", -location, -resource, -battery_hours, -dispatch_target)

trio_sens_long %>% 
  filter(key %in% key_stats) %>% 
  ggplot(aes(x = dispatch_target, y = battery_hours, fill = value))+
  geom_tile()+
  geom_text(aes(label = round(value, 3)), size = 3, color = "black") +
  facet_grid(key ~ location, scales = "free")+
  scale_fill_gradientn(colours = rainbow(6))+
  scale_x_continuous(breaks = dispatch_target_list)+
  scale_y_continuous(breaks = battery_hours_list)+
  ggtitle(paste("Dispatch Statistics wind+solar merged (pooled storage)"))


# Sites merged for dispatch, but storage only pooled at each site

trio_dispatch_merged <- data.frame()
for(b in battery_hours_list){
  for (d in dispatch_target_list){
    battery_half_hours = b * 2
    
    processed_data <- lapply(generator_groups, process_generator, battery_half_hours, d) %>% bind_rows()
    
    filler <- processed_data %>% 
      group_by(Year, Month, Day, half_hour) %>% 
      summarise(value = mean(dispatched)) %>% 
      mutate(resource = "wind_and_solar") %>% 
      mutate(location = "Broken Hill - Darling Downs - Port Augusta") %>% 
      get_dispatch_stats(b, d)
    
    trio_dispatch_merged <- trio_dispatch_merged %>% bind_rows(filler)
  }
}

spillage_excerpt_combined <- trio_sens_long %>% 
  filter(key == "spillage") %>% 
  group_by(key, battery_hours, dispatch_target) %>% 
  summarise(value = mean(value)) %>% 
  mutate(location = "Broken Hill - Darling Downs - Port Augusta") %>% 
  mutate(resource = "wind_and_solar")

trio_dispatch_merged_long <- trio_dispatch_merged %>% 
  gather(key = "key", value = "value", -location, -resource, -battery_hours, -dispatch_target) %>% 
  filter(key != "spillage") %>% 
  bind_rows(spillage_excerpt_combined)

trio_dispatch_merged_long %>% 
  filter(key %in% key_stats) %>% 
  ggplot(aes(x = dispatch_target, y = battery_hours, fill = value))+
  geom_tile()+
  geom_text(aes(label = round(value, 5)), size = 3, color = "black") +
  facet_grid(key ~ location, scales = "free")+
  scale_fill_gradientn(colours = rainbow(6))+
  scale_x_continuous(breaks = dispatch_target_list)+
  scale_y_continuous(breaks = battery_hours_list)+
  ggtitle(paste("Dispatch Statistics all generators dispatch merged, storage pooled locally"))

# All generators (resources at all three sites) all pooled storage
trio_long_df_all_merged <- long_df %>% 
  filter(location %in% trio_names) %>% 
  group_by(Year, Month, Day, half_hour) %>% 
  summarise(value = mean(value)) %>% 
  mutate(resource = "All six generators merged") %>% 
  mutate(location = "Broken Hill - Darling Downs - Port Augusta")

trio_merged_sensitivies <- data.frame()
for(b in battery_hours_list){
  for(d in dispatch_target_list){
    filler <- get_dispatch_stats(trio_long_df_all_merged, b, d)
    trio_merged_sensitivies <- trio_merged_sensitivies %>% bind_rows(filler)
  }
}

trio_merged_long <- trio_merged_sensitivies %>% 
  gather(key = "key", value = "value", -location, -resource, -battery_hours, -dispatch_target)

trio_merged_long %>% 
  filter(key %in% key_stats) %>% 
  ggplot(aes(x = dispatch_target, y = battery_hours, fill = value))+
  geom_tile()+
  geom_text(aes(label = round(value, 5)), size = 3, color = "black") +
  facet_grid(key ~ location, scales = "free")+
  scale_fill_gradientn(colours = rainbow(6))+
  scale_x_continuous(breaks = dispatch_target_list)+
  scale_y_continuous(breaks = battery_hours_list)+
  ggtitle(paste("Dispatch Statistics all generators merged"))


trio_merged_long %>% 
  filter(key == "time_falling_short") %>% 
  ggplot(aes(x = dispatch_target, y = value, col = as.factor(battery_hours)))+
  geom_line()


#### A plot just to insepct a single location closely
chosen_location = "Isaac"

plot <- processed_data %>% 
  filter(location == chosen_location) %>% 
  filter(Year == 2027) %>% 
  mutate(day_as_halfhour = Day*48) %>% 
  mutate(monthly_half = day_as_halfhour + half_hour) %>% 
  filter(Month %in% c(6, 12)) %>% 
  select(resource, Month, monthly_half, value, dispatched, charge_flow, spillage, battery_charge, dispatch_fraction, shortfall) %>% 
  gather(key = "key", value = "value", -resource, -Month, -monthly_half) %>% 
  ggplot(aes(x = monthly_half, y = value, col = key))+
  geom_line()+
  facet_grid(resource ~ Month)+
  ggtitle(chosen_location)
ggplotly(plot)



# Calculate dispatch_fraction_stats

dispatch_fraction_stats <- processed_data %>%
  group_by(location, resource) %>%
  summarise(
    mean_spillage = mean(spillage, na.rm = TRUE),
    dispatch_quantile_0 = dispatch_fraction %>% quantile(c(0), na.rm = TRUE),
    dispatch_quantile_10 = dispatch_fraction %>% quantile(c(0.1), na.rm = TRUE),
    dispatch_quantile_20 = dispatch_fraction %>% quantile(c(0.2), na.rm = TRUE),
    dispatch_quantile_30 = dispatch_fraction %>% quantile(c(0.3), na.rm = TRUE),
    dispatch_quantile_40 = dispatch_fraction %>% quantile(c(0.4), na.rm = TRUE),
    dispatch_quantile_50 = dispatch_fraction %>% quantile(c(0.5), na.rm = TRUE),
    dispatch_quantile_60 = dispatch_fraction %>% quantile(c(0.6), na.rm = TRUE),
    dispatch_quantile_70 = dispatch_fraction %>% quantile(c(0.7), na.rm = TRUE),
    dispatch_quantile_80 = dispatch_fraction %>% quantile(c(0.8), na.rm = TRUE),
    dispatch_quantile_90 = dispatch_fraction %>% quantile(c(0.9), na.rm = TRUE),
    dispatch_quantile_100 = dispatch_fraction %>% quantile(c(1), na.rm = TRUE),
  )

dispatch_fraction_stats %>% 
  ggplot( aes(x = mean_spillage)) +
    geom_density() +
    labs(x = "Average Spillage",
         y = "Density")+
  facet_grid("resource", scales = "free")

dispatch_fraction_stats_long <- dispatch_fraction_stats %>% 
  select(location, resource, contains("dispatch_quantile")) %>% 
  gather(key = "percentile", value = "value", -location, -resource) %>% 
  mutate(percentile = percentile %>% str_remove("dispatch_quantile_") %>% as.numeric())

dispatch_fraction_stats_long %>% 
  ggplot(aes(x = value, fill = resource))+
  geom_density(alpha = 0.5)+
  facet_grid("percentile", scales = "free")

time_meeting_dispatch_stats <- processed_data %>% 
  mutate(met0 = case_when(dispatch_fraction == 0 ~ 1, TRUE ~ 0),
         met10 = case_when(dispatch_fraction < 0.1 ~ 1, TRUE ~ 0),
         met20 = case_when(dispatch_fraction < 0.2 ~ 1, TRUE ~ 0),
         met30 = case_when(dispatch_fraction < 0.3 ~ 1, TRUE ~ 0),
         met40 = case_when(dispatch_fraction < 0.4 ~ 1, TRUE ~ 0),
         met50 = case_when(dispatch_fraction < 0.5 ~ 1, TRUE ~ 0),
         met60 = case_when(dispatch_fraction < 0.6 ~ 1, TRUE ~ 0),
         met70 = case_when(dispatch_fraction < 0.7 ~ 1, TRUE ~ 0),
         met80 = case_when(dispatch_fraction < 0.8 ~ 1, TRUE ~ 0),
         met90 = case_when(dispatch_fraction < 0.9 ~ 1, TRUE ~ 0),
         met100 = case_when(dispatch_fraction < 1 ~ 1, TRUE ~ 0)
         ) %>% 
  group_by(location, resource) %>% 
  summarise(short_0 = mean(met0),
            short_10 = mean(met10),
            short_20 = mean(met20),
            short_30 = mean(met30),
            short_40 = mean(met40),
            short_50 = mean(met50),
            short_60 = mean(met60),
            short_70 = mean(met70),
            short_80 = mean(met80),
            short_90 = mean(met90),
            short_100 = mean(met100)
            ) %>% 
  gather(key = "stat", value = "fraction_falling_short", -location, -resource)
  
time_meeting_dispatch_stats <- time_meeting_dispatch_stats %>% 
  mutate(stat = stat %>% str_remove("short_") %>% as.numeric())

processed_data %>% 
  filter(!is.na(pair)) %>% 
  filter(!location %in% weird_solar_names) %>% 
  ggplot(aes(x = dispatch_fraction, fill = resource))+
  geom_density(alpha = 0.4)+
  facet_wrap("pair", scales = "free")

# Calculate trough_stats
# trough_def <- seq(0, 1, by = 0.1)
# trough_stats <- list()
# 
# for (t in trough_def) {
#   temp <- processed_data %>%
#     group_by(location, resource) %>%
#     mutate(trough = dispatch_fraction < t) %>%
#     rle(trough) %>%
#     tibble(trough_lengths = lengths, trough_values = values) %>%
#     filter(trough_values) %>%
#     group_by(location, resource) %>%
#     summarise(mean_length = mean(trough_lengths),
#               min_length = min(trough_lengths),
#               max_length = max(trough_lengths),
#               !!!setNames(lapply(percentiles, function(p) quantile(trough_lengths, probs = p / 100)), paste0("p", percentiles, "_trough_length"))) %>%
#     mutate(trough_def = t)
#   
#   trough_stats[[length(trough_stats) + 1]] <- temp
# }
# 
# trough_stats <- bind_rows(trough_stats)
# 
# # Combine summary statistics into a single table
# summary_table <- summary_stats %>%
#   left_join(dispatch_fraction_stats, by = c("location", "resource")) %>%
#   left_join(trough_stats, by = c("location", "resource"))
# 
# summary_table



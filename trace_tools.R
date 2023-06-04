library(tidyverse)
library(skimr)
library(tidygeocoder)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(corrplot)
library(patchwork)
library(ggrepel)
library(parallel)
library(foreach)
library(doParallel)

all_names_from_traces <- function(all_traces){
  all_names <- all_traces %>% 
    lapply(str_remove_all, "(?i)_FFP_.*") %>% 
    lapply(str_remove_all, "(?i)_SAT_.*") %>% 
    lapply(str_remove_all, "(?i)_CST_.*") %>% 
    lapply(str_remove_all, "(?i)REZ_[:alpha:][:digit:]_") %>% 
    lapply(str_replace_all, "_", " ") %>% 
    unlist() %>% 
    tibble() %>% 
    unique()
  colnames(all_names) <- c("location")
  return(all_names)
}

all_wind_names_from_traces <- function(all_traces){
  all_names <- all_traces %>% 
    lapply(str_remove_all, "(?i)_FFP_.*") %>% 
    lapply(str_remove_all, "(?i)_SAT_.*") %>% 
    lapply(str_remove_all, "(?i)_CST_.*") %>% 
    lapply(str_remove_all, "(?i)REZ_[:alpha:][:digit:]_") %>% 
    lapply(str_remove_all, "_RefYear3012.csv") %>% 
    lapply(str_remove_all, "[A-Z]\\d_W[A-Z]_") %>% 
    lapply(str_replace_all, "_", " ") %>% 
    unlist() %>% 
    tibble() %>% 
    unique()
  colnames(all_names) <- c("location")
  return(all_names)
}

add_states_to_location <- function(df, location_state_map) {
  for (location_name in names(location_state_map)) {
    state_name <- location_state_map[[location_name]]
    df <- df %>%
      mutate(location = ifelse(str_detect(location, location_name),
                               paste0(location_name, ", ", state_name),
                               location))
  }
  return(df)
}

remove_states_from_location <- function(df, location_state_map) {
  for (location_name in names(location_state_map)) {
    state_name <- location_state_map[[location_name]]
    df <- df %>%
      mutate(location = ifelse(str_detect(location, paste0(location_name, ", ", state_name)),
                               location_name,
                               location))
  }
  return(df)
}

filter_on_location_cut_list <- function(df, cut_list){
  for(c in cut_list){
    df <- df %>% 
      filter(!(location %>% str_detect(c)))
  }
  return(df)
}

geocode_from_names <- function(all_names, location_state_map){
  cascade_results1 <- all_names %>%
    add_states_to_location(location_state_map) %>% 
    mutate(location = paste0(location, ", Australia")) %>% 
    geocode_combine(
      queries = list(
        list(method = 'census'),
        list(method = 'osm')
      ),
      global_params = list(address = 'location')
    )
  
  cascade_results1 <- cascade_results1 %>% 
    mutate(location = location %>% str_remove(", Australia")) %>% 
    remove_states_from_location(location_state_map) %>% 
    unique() %>% 
    drop_na("lat") %>% 
    select(-query)
  return(cascade_results1)
}

plot_locations <- function(geocoded_locations){
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  xmin <- geocoded_locations$long %>% min(na.rm = T) + 0
  xmax <- geocoded_locations$long %>% max(na.rm = T) + 0
  ymin <- geocoded_locations$lat %>% min(na.rm = T) - 0
  ymax <- geocoded_locations$lat %>% max(na.rm = T) + 0
  
  
  # A plot to put a mark on all the locations
  ggplot(data = world) +
    geom_sf() +
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
    geom_point(data = geocoded_locations, aes(x = long, y = lat), size = 4) +
    geom_text_repel(
      data = geocoded_locations,
      aes(x = long, y = lat, label = location),
      size = 3,
      force = 0.5,
      max.overlaps = 50,
      segment.color = "grey50",
      segment.size = 0.2,
      nudge_x = 0.1,
      nudge_y = 0.1
    )
}

load_source_df <- function(all_names, all_traces, folder = "solar") {
  all_names_vec <- all_names %>%
    mutate(location = location %>% str_replace_all(" ", "_")) %>% 
    as.list() %>% 
    unlist()
  
  for (i in 1:length(all_traces)) {
    t <- all_traces[i]
    matching_indices <- which(str_detect(all_traces[i], all_names_vec))
    if (length(matching_indices) == 0) {
      next
    }
    
    # Select the longest matching substring
    location_name <- all_names_vec[matching_indices][which.max(nchar(all_names_vec[matching_indices]))]
    
    filename <- paste0("../AEMO_data/", folder, "/", t)
    if (!exists("source_df")) {
      source_df <- read_csv(filename, show_col_types = FALSE)
      source_df <- source_df %>% mutate(location = location_name)
    } else {
      filler <- read_csv(filename, show_col_types = FALSE) %>%
        mutate(location = location_name)
      source_df <- source_df %>% bind_rows(filler)
    }
  }
  
  number_cols <- source_df %>% select(-Year, -Month, -Day, -location) %>% colnames()
  
  source_df <- source_df %>%
    select(location, Year, Month, Day, all_of(number_cols)) %>%
    filter(Year < 2035, Year > 2023) %>% 
    massage_names()
  
  return(source_df)
}

gather_source_data <- function(source_df){
  long_df <- source_df %>% 
    gather(key = "half_hour", value = "value", -location, -Year, -Month, -Day) %>% 
    mutate(half_hour = half_hour %>% as.numeric()) %>% 
    arrange(location, Year, Month, Day, half_hour) %>% 
    distinct(location, Year, Month, Day, half_hour, .keep_all = TRUE) %>% 
    massage_names()
  return(long_df)
}

rescale_for_tod_month <- function(long_df){
  # Pivot the data
  wide_df <- long_df %>% 
    pivot_wider(names_from = location, values_from = value)
  
  # Group and nest the data
  grouped_data <- wide_df %>%
    group_by(Month, half_hour) %>%
    nest()
  
  # Rescale the nested data
  rescaled_data <- grouped_data %>%
    mutate(data = map(data, ~mutate_at(.x, 3:ncol(.x), scale))) %>%
    unnest(cols = c(data)) %>% 
    ungroup() %>% 
    drop_na() %>% 
    select(-Year, -Month, -Day, -half_hour)
  
  return(rescaled_data)
}


get_day_summ <- function(long_df){
  day_summ <- long_df %>% 
    arrange(location, Year, Month, Day) %>% 
    group_by(location, Year, Month, Day) %>% 
    summarise(value = sum(value))
  return(day_summ)
}

create_nday_summ <- function(day_summ, n){
  nday_summ <- day_summ %>%
    group_by(location) %>%
    mutate(Period = (row_number() - 1) %/% n) %>% # Create a new column for n-day periods
    group_by(location, Period) %>% # Group by the new column
    summarise(value = mean(value))
  return(nday_summ)
}

raw_correlation_plot <- function(wide_df){
  
  M <- wide_df %>%
    cor()
  
  lower_triangle <- M[lower.tri(M)]
  min_corr <- min(lower_triangle)
  q25_corr <- quantile(lower_triangle, 0.25, na.rm = TRUE)
  median_corr <- median(lower_triangle, na.rm = TRUE)
  q75_corr <- quantile(lower_triangle, 0.75, na.rm = TRUE)
  max_corr <- max(lower_triangle)
  
  title <- sprintf("Correlation between sites half hour \nMin: %.2f, Q25: %.2f, Median: %.2f, Q75: %.2f, Max: %.2f",
                   min_corr, q25_corr, median_corr, q75_corr, max_corr)
  M %>% corrplot(method = 'circle', type = 'lower',
                 order = 'AOE', diag=FALSE)
  title(main = title, line = -1)
}

plot_correlation <- function(day_summ, n) {
  nday_summ <- create_nday_summ(day_summ, n)
  
  corr_plot_nday <- nday_summ %>% 
    pivot_wider(names_from = location, values_from = value) %>% 
    ungroup()
  
  nD <- corr_plot_nday %>% select(-Period) %>% cor()
  
  lower_triangle <- nD[lower.tri(nD)]
  min_corr <- min(lower_triangle)
  q25_corr <- quantile(lower_triangle, 0.25, na.rm = TRUE)
  median_corr <- median(lower_triangle, na.rm = TRUE)
  q75_corr <- quantile(lower_triangle, 0.75, na.rm = TRUE)
  max_corr <- max(lower_triangle)
  
  title <- sprintf("Correlation between sites %d day aggregates\nMin: %.2f, Q25: %.2f, Median: %.2f, Q75: %.2f, Max: %.2f",
                   n, min_corr, q25_corr, median_corr, q75_corr, max_corr)
  
  nD %>% corrplot(method = 'circle', type = 'lower',
                  order = 'AOE', diag=FALSE)
  
  title(main = title, line = -1)
}

get_max_daily_solar_by_location <- function(day_summ){
  max_value_n1 <- create_nday_summ(day_summ, 1) %>% 
    group_by(location) %>% 
    summarise(max_value = max(value, na.rm = TRUE))
  return (max_value_n1)
}

summary_stats <- function(n, day_summ, max_value_n1) {
  nday_summ <- create_nday_summ(day_summ, n)
  
  indiv_nday_summ <- nday_summ %>% 
    left_join(max_value_n1, by = "location") %>% # Join max_value_n1 to the data
    ungroup() %>%
    mutate(value = value / max_value) %>% # Normalize the value by dividing by the max value for n = 1
    select(-max_value) %>% # Remove the max_value column
    group_by(location) %>% 
    summarise(min = min(value),
              q25 = quantile(value, 0.25, na.rm = TRUE),
              median = median(value, na.rm = TRUE),
              q75 = quantile(value, 0.75, na.rm = TRUE),
              max = max(value, na.rm = TRUE)) %>%
    mutate(n = n) # Add a column for 'n' aggregated days
  
  return(indiv_nday_summ)
}

massage_names <- function(df){
  
  current_names_count <- df$location %>% unique() %>% length()
  final_names_count <- current_names_count - 1
  truncation = 16
  
  while(final_names_count < current_names_count){
    df_massaged <- df %>% 
      mutate(location = location %>% str_remove_all(", Australia")) %>% 
      mutate(location = location %>% str_replace_all("_", " ")) %>% 
      mutate(location = location %>% str_trunc(truncation))
    final_names_count <- df_massaged$location %>% unique() %>% length()
    truncation = truncation + 3
  }
  
  return(df_massaged)
  
}

plot_normalised_output_distributions <- function(day_summ, days_to_aggregate = c(1, 3, 5)){
  
  # Calculate max_value for each location when n = 1
  max_value_n1 <- get_max_daily_solar_by_location(day_summ)
  
  # Apply summary_stats function for different 'n' values and bind the resulting data frames together
  combined_data <- map_dfr(days_to_aggregate, summary_stats, day_summ = day_summ, max_value_n1 = max_value_n1)
  
  # Calculate median value for each location when n = 1
  location_order <- summary_stats(1, day_summ, max_value_n1) %>%
    arrange(median) %>%
    pull(location)
  
  # Reorder location factor levels based on the median value for n = 1
  combined_data <- combined_data %>%
    mutate(location = factor(location, levels = location_order))
  
  # Create the ggplot with facetting
  ggplot(combined_data, aes(x = location)) +
    geom_point(aes(y = min, color = "min")) +
    geom_point(aes(y = q25, color = "q25")) +
    geom_point(aes(y = median, color = "median")) +
    geom_point(aes(y = q75, color = "q75")) +
    geom_point(aes(y = max, color = "max")) +
    scale_color_manual(values = c("min" = "blue", "q25" = "cyan", "median" = "green", "q75" = "orange", "max" = "red"),
                       name = "Statistic") +
    labs(x = "Location",
         y = "Normalized Value",
         title = "Summary Statistics of Normalized Daily Solar Output by Location") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          strip.text = element_text(face = "bold", size = 12)) +
    facet_wrap(~paste(n, "days aggregated"), nrow = 5, scales = "free_y") # Add facets for different 'n' values with more prominent titles
  
}

haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371 # Earth's radius in km
  dlat <- (lat2 - lat1) * (pi / 180)
  dlon <- (lon2 - lon1) * (pi / 180)
  a <- sin(dlat / 2) * sin(dlat / 2) + cos(lat1 * (pi / 180)) * cos(lat2 * (pi / 180)) * sin(dlon / 2) * sin(dlon / 2)
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  d <- R * c
  return(d)
}

get_correlation_distances <- function(corr_matrix, locations_df){
  location_coords <- cascade_results1 %>% 
    select(location, lat, long) 
  
  distance_matrix <- expand.grid(location1 = location_coords$location,
                                 location2 = location_coords$location) %>%
    left_join(location_coords, by = c("location1" = "location")) %>%
    rename(lat1 = lat, lon1 = long) %>%
    left_join(location_coords, by = c("location2" = "location")) %>%
    rename(lat2 = lat, lon2 = long) %>%
    mutate(distance = haversine_distance(lat1, lon1, lat2, lon2)) %>% 
    unique() %>% 
    drop_na()
  
  # Create a data frame with correlations and distances between each location pair
  correlations_distances <- data.frame(
    location1 = rownames(corr_matrix)[row(corr_matrix)],
    location2 = colnames(corr_matrix)[col(corr_matrix)],
    correlation = c(corr_matrix)
  ) %>%
    filter(location1 != location2) %>%
    left_join(distance_matrix, by = c("location1", "location2")) %>% 
    drop_na()
  
  return(correlations_distances)
}

plot_correlation_vs_distance <- function(correlation_distance){
  correlations_distances <- correlations_distances %>%
    mutate(ns_sep = abs(lat1 - lat2),
           ew_sep = abs(lon1 - lon2))
  
  # Calculate min and max values for ns_sep and ew_sep
  min_ns_sep <- min(correlations_distances$ns_sep, na.rm = TRUE)
  max_ns_sep <- max(correlations_distances$ns_sep, na.rm = TRUE)
  min_ew_sep <- min(correlations_distances$ew_sep, na.rm = TRUE)
  max_ew_sep <- max(correlations_distances$ew_sep, na.rm = TRUE)
  
  # Find the overall min and max
  min_limit <- min(min_ns_sep, min_ew_sep)
  max_limit <- max(max_ns_sep, max_ew_sep)
  
  min_corr <- min(correlations_distances$correlation) %>% round(digits = 1)
  
  # Create the ggplot with two subplots
  plot1 <- ggplot(correlations_distances, aes(x = distance, y = correlation, color = ns_sep)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(method = "nls", formula = y ~ a * exp(-b * x), se = FALSE, linetype = "dashed", method.args = list(start = c(a = 1, b = 0.01))) +
    labs(x = "Distance (km)",
         y = "Correlation",
         title = "Correlation vs Distance (colored by North-South separation)") +
    theme_minimal() +
    scale_color_gradientn(colors = rainbow(7), name = "NS Separation (deg)", limits = c(min_limit, max_limit))+
    scale_y_continuous(limits = c(min_corr, 1.0), breaks = seq(min_corr, 1.0, by = 0.1))
  
  plot2 <- ggplot(correlations_distances, aes(x = distance, y = correlation, color = ew_sep)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(method = "nls", formula = y ~ a * exp(-b * x), se = FALSE, linetype = "dashed", method.args = list(start = c(a = 1, b = 0.01))) +
    labs(x = "Distance (km)",
         y = "Correlation",
         title = "Correlation vs Distance (colored by East-West separation)") +
    theme_minimal() +
    scale_color_gradientn(colors = rainbow(7), name = "EW Separation (deg)", limits = c(min_limit, max_limit))+
    scale_y_continuous(limits = c(min_corr, 1.0), breaks = seq(min_corr, 1.0, by = 0.1))
  
  # Combine the two subplots into one using the patchwork package
  (plot1 / plot2) + plot_layout(guides = "collect")
  
}

get_corr_matrix <- function(nday_summ){
  corr_matrix <- nday_summ %>% 
    pivot_wider(names_from = location, values_from = value) %>% 
    ungroup() %>% 
    select(-Period) %>% 
    cor()
  return(corr_matrix)
}

# Function to find the closest solar location to a given wind location
find_closest_solar <- function(wind_lat, wind_lon, solar_df) {
  solar_df <- solar_df %>%
    mutate(distance = haversine_distance(wind_lat, wind_lon, lat, long))
  
  closest_solar <- solar_df %>%
    filter(distance == min(distance))
  
  return(closest_solar)
}

correlation_of_pairs <- function(paired_data, geocoded_locations){
  pairs_corr <- data.frame()
  
  for (p in geocoded_locations$pair %>% unique()){
    solar_location <- geocoded_locations %>%
      filter(resource == "solar", pair == p) %>%
      select(location) %>%
      as.character()
    
    wind_location <- geocoded_locations %>%
      filter(resource == "wind", pair == p) %>%
      select(location) %>%
      as.character()
    
    
    
    w <- paired_data %>% 
      filter(resource == "wind") %>% 
      filter(pair == p)
    print(wind_location)
    wn <- w %>% nrow()
    print(wn)
    
    s <- paired_data %>% 
      filter(resource == "solar") %>% 
      filter(pair == p) 
    print(solar_location)
    sn = s %>% nrow()
    print(sn)
    
    if (wn == sn){
      c <- cor(w$value, s$value)
      
      pairs_corr <- rbind(pairs_corr, data.frame(pair = p, 
                                                 solar_location = solar_location,
                                                 wind_location = wind_location,
                                                 correlation = c))
      print(pairs_corr %>% tail(1))
    }
  }
  return(pairs_corr)
}

get_dispatch_stats <- function(long_df, battery_hours, dispatch_target){
  # Set battery_half_hours and dispatch_target
  battery_half_hours <- battery_hours * 2 
  
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
              time_falling_short = mean(fell_short, na.rm = TRUE)) %>% 
    mutate(battery_hours = battery_hours,
           dispatch_target = dispatch_target)
  print(paste(" battery: ", battery_hours, "dispatch target:", dispatch_target))
  return (summary_stats)
  
}


process_generator <- function(data, battery_half_hours, dispatch_target) {
  n <- nrow(data)
  
  dispatched <- numeric(n)
  charge_flow <- numeric(n)
  spillage <- numeric(n)
  dispatch_fraction <- numeric(n)
  battery_charge <- numeric(n)
  shortfall <- numeric(n)
  
  for (i in 1:n) {
    value <- data$value[i]
    if (i > 1) {
      prev_battery_charge <- battery_charge[i - 1]
    } else {
      prev_battery_charge <- 0
    }
    
    if (value >= dispatch_target) {
      dispatched[i] <- dispatch_target
      charge_flow[i] <- value - dispatch_target
      battery_charge[i] <- min((prev_battery_charge * battery_half_hours + charge_flow[i]) / battery_half_hours, 1)
      spillage[i] <- max(charge_flow[i] - (battery_charge[i] * battery_half_hours - prev_battery_charge * battery_half_hours), 0)
    } else {
      charge_flow[i] <- min(dispatch_target - value, prev_battery_charge * battery_half_hours)
      battery_charge[i] <- max((prev_battery_charge * battery_half_hours - charge_flow[i]) / battery_half_hours, 0)
      dispatched[i] <- value + charge_flow[i]
      charge_flow[i] <- -charge_flow[i]  # Make the charge_flow negative when the battery is discharging
      spillage[i] <- 0
    }
    dispatch_fraction[i] <- dispatched[i] / dispatch_target
    shortfall[i] <- (1 - dispatch_fraction[i])
    
  }
  
  data$dispatched <- dispatched
  data$charge_flow <- charge_flow
  data$spillage <- spillage
  data$dispatch_fraction <- dispatch_fraction
  data$battery_charge <- battery_charge
  data$shortfall <- shortfall
  # print(paste("Processed", data$location %>% head(1), data$resource %>% head(1)))
  return(data)
}

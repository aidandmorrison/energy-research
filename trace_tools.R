library(tidyverse)
library(skimr)
library(tidygeocoder)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(corrplot)
library(patchwork)
library(ggrepel)

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
  for(c in manual_cut_list){
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

load_source_df <- function(all_names, all_traces) {
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
    
    filename <- paste0("../AEMO_data/solar/", t)
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
    drop_na()
  
  return(rescaled_data)
}


get_day_summ <- function(long_df){
  day_summ <- long_df %>% 
    group_by(location, Year, Month, Day) %>% 
    summarise(value = sum(value))
  return(day_summ)
}

create_nday_summ <- function(day_summ, n){
  nday_summ <- day_summ %>%
    arrange(location, Year, Month, Day) %>% # Ensure data is sorted by date
    group_by(location) %>%
    mutate(NDayPeriod = (row_number() - 1) %/% n) %>% # Create a new column for n-day periods
    group_by(location, NDayPeriod) %>% # Group by the new column
    summarise(value = mean(value))
  return(nday_summ)
}

raw_correlation_plot <- function(wide_df){
  
  M <- wide_df %>%
    select(-Year, -Month, -Day, -half_hour) %>%
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
  
  nD <- corr_plot_nday %>% select(-NDayPeriod) %>% cor()
  
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
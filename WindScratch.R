source("trace_tools.R")

all_traces <- list.files("../AEMO_data/wind")

all_names <- all_traces %>% all_wind_names_from_traces()

manual_cut_list <- c(
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

all_names <- all_names %>% filter_on_location_cut_list(cut_list = manual_cut_list)

cascade_results1 <- geocode_from_names(all_names, location_state_map)

plot_locations(cascade_results1)

source_df <- load_source_df(all_names, all_traces, "wind")

long_df <- gather_source_data(source_df)

corr_scaled <- long_df %>% rescale_for_tod_month()

###### half-hour correlation plot
corr_scaled %>% raw_correlation_plot()

day_summ <- get_day_summ(long_df)


plot_normalised_output_distributions(day_summ, c(1,3,7,14))
########

# Use the function to plot the correlation with n = 2
plot_correlation(day_summ, 1)
plot_correlation(day_summ, 5)
#### linear model correlation investigation -----

# Compute the correlation matrix for nday_summ with n = 1
nday_summ <- create_nday_summ(day_summ, 5)

corr_matrix <- get_corr_matrix(nday_summ)

# Prepare the data frame with distances between each location pair
correlations_distances <- get_correlation_distances(corr_matrix, cascade_results1)
# correlations_distances <- get_correlation_distances(corr_scaled %>% cor(), cascade_results1)

# Run a linear model to see the relationship between distance and correlation
linear_model <- lm(correlation ~ distance, data = correlations_distances)
summary(linear_model)


plot_correlation_vs_distance(correlation_distances)

######### extra ------
weird <- correlations_distances %>%
  filter(distance < 500) %>%
  filter(correlation < .3)

weirdgood <- correlations_distances %>%
  filter(correlation < .25) %>%
  filter(distance < 300)

weirdbad <- correlations_distances %>%
  filter(correlation > 0.5) %>%
  filter(distance > 700)

normal <- correlations_distances %>% 
  filter(distance < 50) %>% 
  filter(correlation > 0.8)

###### PA and Lincolng Gap ----
library(plotly)

generate_plots <- function(locations, day_summ, long_df) {
  
  # Filter the day_summ dataframe for the desired locations
  filtered_day_summ <- day_summ %>% filter(location %in% locations) %>% 
    mutate(Date = lubridate::ymd(paste(Year, Month, Day, sep = "-")))
  
  # Create the time series plot
  plot1 <- filtered_day_summ %>% 
    filter(Year == 2025) %>% 
    ggplot(aes(x = Date, y = value, color = location)) +
    geom_line() +
    labs(x = "Date",
         y = "Value",
         title = paste("Time Series for", paste(locations, collapse = " and "))) +
    theme_minimal()
  
  print(ggplotly(plot1))
  
  # Filter the long_df dataframe for the desired locations
  filtered_long_df <- long_df %>% filter(location %in% locations) %>% 
    mutate(hour = (half_hour - 1) %/% 2,
           minute = (half_hour - 1) %% 2 * 30,
           Datetime = lubridate::ymd_hm(paste(Year, Month, Day, hour, minute, sep = "-"), tz = "UTC")) %>%
    select(-hour, -minute)
  
  # Create the time series plot
  plot2 <- filtered_long_df %>% 
    filter(Year == 2025) %>% 
    ggplot(aes(x = Datetime, y = value, color = location)) +
    geom_line() +
    labs(x = "Datetime",
         y = "Value",
         title = paste("Half-hourly Time Series for", paste(locations, collapse = " and "))) +
    theme_minimal()
  
  print(ggplotly(plot2))
  
  reshaped_data <- filtered_long_df %>%
    select(Datetime, location, value) %>%
    pivot_wider(names_from = location, values_from = value)
  
  # Create the scatter plot
  plot3 <- ggplot(reshaped_data, aes(x = reshaped_data[[2]], y = reshaped_data[[3]])) +
    geom_point(alpha = 0.1) +
    labs(x = locations[1],
         y = locations[2],
         title = paste("Scatter Plot:", paste(locations, collapse = " vs "))) +
    theme_minimal()
  
  print(plot3)
}

# Call the function with the desired pair of locations
generate_plots(c("Lincoln Gap", "Port Augusta"), day_summ, long_df)
generate_plots(c("Rye Park", "Bango 973"), day_summ, long_df)






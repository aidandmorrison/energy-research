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


plot_normalised_output_distributions(day_summ, c(1,3,5))
########

# Use the function to plot the correlation with n = 2
plot_correlation(day_summ, 1)
plot_correlation(day_summ, 3)
plot_correlation(day_summ, 5)
#### linear model correlation investigation -----

# Compute the correlation matrix for nday_summ with n = 1
nday_summ <- create_nday_summ(day_summ, 1)

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


##### PCA ------

wide_data <- nday_summ %>% 
  pivot_wider(names_from = location, values_from = value) %>% 
  ungroup() %>% 
  select(-Period)


# Perform PCA
pca_results <- prcomp(wide_data, center = TRUE, scale. = TRUE)

# Print summary of PCA results
summary(pca_results)

# Calculate the proportion of variance explained by each principal component
explained_variance_ratio <- pca_results$sdev^2 / sum(pca_results$sdev^2)

# Print the explained variance ratio
explained_variance_ratio

# Visualize the explained variance ratio in a scree plot

explained_variance_df <- data.frame(Principal_Component = 1:length(explained_variance_ratio),
                                    Explained_Variance_Ratio = explained_variance_ratio)

scree_plot <- ggplot(explained_variance_df, aes(x = Principal_Component, y = Explained_Variance_Ratio)) +
  geom_point() +
  geom_line() +
  labs(x = "Principal Component",
       y = "Explained Variance Ratio",
       title = "Scree Plot of PCA Explained Variance") +
  theme_minimal()

print(scree_plot)

# Get the loadings
loadings <- pca_results$rotation

# Print the loadings for the first few principal components
loadings[, 1:3]

# Well that didn't exactly work very well. 

##### Fourier -----
# Step 1: Convert long_df to wide format
wide_df <- long_df %>%
  mutate(Datetime = lubridate::ymd_hm(paste(Year, Month, Day, (half_hour - 1) %/% 2, (half_hour - 1) %% 2 * 30, sep = "-"), tz = "UTC")) %>%
  select(Datetime, location, value) %>%
  pivot_wider(names_from = location, values_from = value)

# Remove the 'Datetime' column and store the location names
location_names <- colnames(wide_df)[-1]
wide_df <- wide_df %>% select(-Datetime)

# Step 2: Calculate the magnitude of the Fourier transform for each location
fourier_magnitudes <- lapply(wide_df, function(x) {
  ft <- fft(as.numeric(x), inverse = FALSE)
  return(Mod(ft)) # Compute the magnitude of the Fourier transform
})

# Convert the list to a data frame
fourier_magnitudes_df <- as.data.frame(fourier_magnitudes)
colnames(fourier_magnitudes_df) <- location_names

# Step 3: Identify the dominant frequency components for each location
# You can analyze the Fourier magnitudes data frame (fourier_magnitudes_df) to find dominant frequency components for each location

plot_psd <- function(long_df, location_name) {
  # Filter the data for the specified location
  location_data <- long_df %>% filter(location == location_name)
  
  # Convert the filtered data to a time series
  time_series <- ts(location_data$value, frequency = 48)
  
  # Subtract the mean from the time series
  centered_time_series <- time_series - mean(time_series)
  
  # Calculate the Fourier Transform
  fourier <- fft(centered_time_series)
  
  # Calculate the Power Spectral Density (PSD)
  psd <- (abs(fourier)^2) / length(centered_time_series)
  
  # Create a data frame with the frequencies and PSD values
  psd_df <- tibble(
    frequency = (0:(length(psd) - 1)) / length(centered_time_series),
    psd = psd
  )
  
  # Create the PSD plot
  plot <- ggplot(psd_df, aes(x = frequency, y = psd)) +
    geom_line() +
    labs(x = "Frequency", y = "Power Spectral Density", title = paste("PSD for", location_name)) +
    theme_minimal() +
    xlim(0, 0.5) # Limit the x-axis to display only up to the Nyquist frequency
  
  # Return the plot
  return(plot)
}


plot_psd_period <- function(psd, location) {
  psd_df <- data.frame(frequency = psd$freq, spec = psd$spec)
  psd_df <- psd_df %>% mutate(period = 1 / frequency) # Calculate the period
  
  plot <- ggplot(psd_df, aes(x = period, y = spec)) +
    geom_line() +
    labs(x = "Period (hours)",
         y = "Power Spectral Density",
         title = paste("PSD for", location)) +
    theme_minimal() +
    scale_x_continuous(breaks = seq(0, 48, by = 1)) + # adjust breaks as needed
    coord_cartesian(xlim = c(0.5, 24 * 7)) # Limit the x-axis to show periods between 0.5 hours and 7 days
  
  return(plot)
}

calculate_psd <- function(long_df, location) {
  # Filter the long_df dataframe for the desired location
  filtered_long_df <- long_df %>% filter(location == location)
  
  # Convert the dataframe to a time series
  ts_data <- filtered_long_df %>% 
    arrange(Year, Month, Day, half_hour) %>% 
    pull(value) %>% 
    ts(frequency = 48) # half-hourly data has a frequency of 48
  
  # Remove the mean from the time series
  ts_data <- ts_data - mean(ts_data)
  
  # Calculate the power spectral density
  psd <- spec.pgram(ts_data, taper = 0, log = "no", spans = NULL, fast = TRUE)
  
  return(psd)
}


location <- "Lincoln Gap"
psd <- calculate_psd(long_df, location)
plot <- plot_psd_period(psd, location)
print(plot)


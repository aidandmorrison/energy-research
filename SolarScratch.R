library(tidyverse)
library(skimr)
library(tidygeocoder)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(corrplot)
library(patchwork)
library(ggrepel)
source("trace_tools.R")


#------ mapping --------
all_traces <- list.files("../AEMO_data/solar")

all_names <- all_traces %>% all_names_from_traces()
           
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
  "North East Tasmania",
  "Hamilton",
  "Lilyvale"
  
)

location_state_map <- list(
  "White Rock" = "NSW",
  "Central-West Orana" = "NSW",
  "Riverland" = "SA",
  "Darling Downs" = "QLD",
  "Clare" = "QLD"
)

all_names <- all_names %>% filter_on_location_cut_list(cut_list = manual_cut_list)

cascade_results1 <- geocode_from_names(all_names, location_state_map)

plot_locations(cascade_results1)

source_df <- load_source_df(all_names, all_traces)

long_df <- gather_source_data(source_df)

corr_scaled <- long_df %>% rescale_for_tod_month()

###### half-hour correlation plot
corr_scaled %>% raw_correlation_plot()

##### n day correlation plot --------

day_summ <- get_day_summ(long_df)

# nday_summ <- create_nday_summ(day_summ, 1)
# 
# indiv_nday_summ <- nday_summ %>% 
#   mutate(max_value = max(value, na.rm = TRUE)) %>% # Calculate the max value for each location
#   ungroup() %>%
#   mutate(value = value / max_value) %>% # Normalize the value by dividing by the max value
#   select(-max_value) %>% 
#   group_by(location) %>% 
#   summarise(min = min(value),
#             q25 = quantile(value, 0.25, na.rm = TRUE),
#             median = median(value, na.rm = TRUE),
#             q75 = quantile(value, 0.75, na.rm = TRUE),
#             max = max(value, na.rm = TRUE))
# 
# # Reorder location factor levels based on the median value
# indiv_nday_summ <- indiv_nday_summ %>%
#   mutate(location = factor(location, levels = location[order(median)]))
# 
# # Create the ggplot
# ggplot(indiv_nday_summ, aes(x = location)) +
#   geom_point(aes(y = min, color = "min")) +
#   geom_point(aes(y = q25, color = "q25")) +
#   geom_point(aes(y = median, color = "median")) +
#   geom_point(aes(y = q75, color = "q75")) +
#   geom_point(aes(y = max, color = "max")) +
#   scale_color_manual(values = c("min" = "blue", "q25" = "cyan", "median" = "green", "q75" = "orange", "max" = "red"),
#                      name = "Statistic") +
#   labs(x = "Location",
#        y = "Normalized Value",
#        title = "Summary Statistics of Normalized Daily Solar Output by Location") +
#   theme_minimal()

#####

plot_normalised_output_distributions(day_summ, c(1,3,7))
########

# Use the function to plot the correlation with n = 2
plot_correlation(day_summ, 5)

#### linear model correlation investigation -----

# Compute the correlation matrix for nday_summ with n = 1
nday_summ <- create_nday_summ(day_summ, 2)
corr_matrix <- nday_summ %>% 
  pivot_wider(names_from = location, values_from = value) %>% 
  ungroup() %>% 
  select(-NDayPeriod) %>% 
  cor()

# Prepare the data frame with distances between each location pair
location_coords <- cascade_results1 %>% 
  select(location, lat, long) %>% 
  mutate(location = location %>% str_remove(", Australia") %>% str_trunc(20))

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

# Run a linear model to see the relationship between distance and correlation
linear_model <- lm(correlation ~ distance, data = correlations_distances)
summary(linear_model)

ggplot(correlations_distances, aes(x = distance, y = correlation)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "solid") +
  labs(x = "Distance (km)",
       y = "Correlation",
       title = "Correlation vs. Distance between Locations") +
  theme_minimal()

## Working on identifying cause of outliers to trend. 
location_medians <- day_summ %>% 
  group_by(location) %>% 
  summarise(median = median(value))
# Merge data frames to get median solar values for location1 and location2
correlations_distances_medians <- correlations_distances %>%
  left_join(location_medians, by = c("location1" = "location")) %>%
  rename(median1 = median) %>%
  left_join(location_medians, by = c("location2" = "location")) %>%
  rename(median2 = median)

# Melt the data frame to have a "facet" column for easy plotting
correlations_distances_medians_melt <- correlations_distances_medians %>%
  pivot_longer(cols = c(median1, median2), names_to = "facet", values_to = "median")

# Create the ggplot with facetting and colored points based on median solar values
ggplot(correlations_distances_medians_melt, aes(x = distance, y = correlation, color = median)) +
  geom_point(alpha = 0.2) +
  scale_color_gradientn(colours = rainbow(4)) +
  labs(x = "Distance (km)",
       y = "Correlation",
       title = "Correlation vs. Distance between Locations (Colored by Median Solar)") +
  facet_wrap(~facet, nrow = 2, scales = "free_y") +
  theme_minimal()

# investigating east-west and north-south separation
# Calculate north-south and east-west separation distances (in degrees)
correlations_distances <- correlations_distances %>%
  mutate(ns_sep = abs(lat1 - lat2),
         ew_sep = abs(lon1 - lon2))

correlations_distances_subset <- correlations_distances %>% 
  filter(distance < 500)

# Create the ggplot with two subplots
plot1 <- ggplot(correlations_distances, aes(x = distance, y = correlation, color = ns_sep)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Distance (km)",
       y = "Correlation",
       title = "Correlation vs Distance (colored by North-South separation)") +
  theme_minimal() +
  scale_color_gradientn(colors = rainbow(7), name = "NS Separation (deg)")

plot2 <- ggplot(correlations_distances, aes(x = distance, y = correlation, color = ew_sep)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Distance (km)",
       y = "Correlation",
       title = "Correlation vs Distance (colored by East-West separation)") +
  theme_minimal() +
  scale_color_gradientn(colors = rainbow(7), name = "EW Separation (deg)")

# Combine the two subplots into one using the patchwork package
(plot1 / plot2) + plot_layout(guides = "collect")

weird <- correlations_distances %>% 
  filter(distance < 500) %>% 
  filter(correlation < .3)

weirdgood <- correlations_distances %>% 
  filter(correlation < .1) %>% 
  filter(distance > 1000) %>% 
  filter(distance < 1750)

weirdbad <- correlations_distances %>% 
  filter(correlation > 0.25) %>% 
  filter(distance > 2500)
#------ individual visuals and summar ----
df <- read_csv("../AEMO_data/solar/Winton_SAT_RefYear3012.csv")
df <- read_csv("../AEMO_data/solar/Gunnedah_SAT_RefYear3012.csv")
df <- read_csv("../AEMO_data/solar/REZ_Q1_Far_North_QLD_CST_RefYear3012.csv")
skim(df)
plot_df <- df %>%
  filter(Year < 2035,
         Year > 2023) %>%
  # filter(Year %in% 2023) %>%
  # filter(Month == 12) %>%
  gather(key = "Xvar", value = "value", -Day, -Month, -Year) %>%
  mutate(halfhour = Xvar %>% str_remove("X") %>% as.numeric()) %>%
  select(-Xvar) %>%
  mutate(day_id = paste(Year, Month, Day, sep = "_"))

all_days <- plot_df %>%
  group_by(Year, Month, Day) %>%
  summarise(total = sum(value)) %>% 
  mutate(day_id = paste(Year, Month, Day, sep = "_"))

low_day_ids <- all_days %>% arrange(total) %>% head(200) %>% pull(day_id)
high_day_ids <- all_days %>% arrange(total) %>% tail(200) %>% pull(day_id)

plot_df %>%
  filter(day_id %in% high_day_ids) %>%
  ggplot(aes(x = halfhour, y = value, col = as.factor(Day)))+
  geom_line()+
  facet_grid(Month~Year)

low_days %>%
  ggplot(aes(x = total))+
  geom_density()


library(tidyverse)
library(patchwork)
library(gganimate)
library(magick)

background_power <- 0.25
peak_power <- 7
peak_duration <- 3
day_mins <- 24*60
central_dinner_start <- 12*60
dinner_sd <- 120

title_text <- paste("background:", background_power, "peak power:", peak_power, 
                    "peak duration:", peak_duration, "window sd:", dinner_sd)

# Generate a sequence of minutes for a day
day_minutes <- seq(0, day_mins - 1, by = 1)

# Convert the x-axis to hours of the day
day_hours <- day_minutes / 60

example_cumulation <- 15
# Sample 5 displacement values from the normal distribution
displacements <- rnorm(example_cumulation, mean = 0, sd = dinner_sd)

# Calculate the peak start time for each household
household_peak_starts <- central_dinner_start + displacements

# Define a function that calculates the power demand
power_demand <- function(time, peak_start, peak_duration, background_power, peak_power) {
  if (time >= peak_start && time <= (peak_start + peak_duration)) {
    return(peak_power)
  } else {
    return(background_power)
  }
}

# Calculate the power demand for each household and store it in a long-format dataframe
household_demands <- lapply(seq_along(household_peak_starts), function(i) {
  peak_start <- household_peak_starts[i]
  demands <- sapply(day_minutes, function(t) power_demand(t, peak_start, peak_duration, background_power, peak_power))
  data.frame(household = i, time_minutes = day_minutes, power_demand = demands)
})

household_demands_long <- bind_rows(household_demands)

# Convert the x-axis to hours of the day
household_demands_long$day_hours <- household_demands_long$time_minutes / 60
# Calculate the total demand for each minute
total_demand <- household_demands_long %>%
  group_by(time_minutes) %>%
  summarize(total_power_demand = sum(power_demand))

# Add a column to identify the plot type: individual or total
household_demands_long$plot_type <- "Individual"
total_demand$plot_type <- "Total"

# Combine the individual and total demand data
combined_demands <- bind_rows(household_demands_long, total_demand)

# Convert the x-axis to hours of the day
combined_demands$day_hours <- combined_demands$time_minutes / 60

# Create a plot showing the power demand for each household
individual_demands_plot <- ggplot(household_demands_long, aes(x = day_hours, y = power_demand, color = as.factor(household))) +
  geom_line() +
  labs(x = "Hours of the Day",
       y = "Power Demand (kW)",
       title = paste("Power Demand for", example_cumulation, "Households"),
       color = "Household") +
  theme_minimal()

# Create a plot showing the total power demand
total_demands_plot <- ggplot(total_demand, aes(x = day_hours, y = total_power_demand)) +
  geom_line(color = "black") +
  labs(x = "Hours of the Day",
       y = "Total Power Demand (kW)",
       title = "Total Power Demand") +
  theme_minimal()

# Combine the two plots using patchwork
combined_plot <- individual_demands_plot / total_demands_plot
print(combined_plot)


###### video --------

generate_households <- function(n_households, background_power, peak_power, peak_duration, day_mins, central_dinner_start, dinner_sd) {
  
  # Sample displacement values from the normal distribution
  displacements <- rnorm(n_households, mean = 0, sd = dinner_sd)
  
  # Calculate the peak start time for each household
  household_peak_starts <- central_dinner_start + displacements
  
  # Generate a sequence of minutes for a day
  day_minutes <- seq(0, day_mins - 1, by = 1)
  
  # Calculate the power demand for each household and store it in a long-format dataframe
  household_demands <- lapply(seq_along(household_peak_starts), function(i) {
    peak_start <- household_peak_starts[i]
    demands <- sapply(day_minutes, function(t) power_demand(t, peak_start, peak_duration, background_power, peak_power))
    data.frame(household = i, time_minutes = day_minutes, power_demand = demands)
  })
  
  # Combine household demands into a single dataframe
  household_demands_df <- bind_rows(household_demands, .id = "household")
  household_demands_df$household <- as.integer(household_demands_df$household)
  
  return(household_demands_df)
}

# Create a function to calculate the total power demand for multiple households
calculate_total_demand <- function(individual_demands) {
  total_demands <- individual_demands %>%
    group_by(day_minutes) %>%
    summarize(total_power_demand = sum(power_demand)) %>%
    mutate(day_hours = day_minutes / 60)
  return(total_demands)
}

start_value <- 1
max_value <- 20000

# Calculate the length of the vector
vector_length <- ceiling(log2(max_value / start_value)) + 1

# Generate the vector
steps <- sapply(0:(vector_length - 1), function(i) start_value * 2^i)

# Generate the individual power demands for a total of 125 households
all_individual_demands <- generate_households(steps %>% tail(1), background_power, peak_power, peak_duration, day_mins, central_dinner_start, dinner_sd)

# Initialize an empty dataframe to store the total demands for each step
all_total_demands <- data.frame()
demands_summary <- data.frame()

# Loop through the steps
for (step in steps) {
  # Filter all_individual_demands to include only households of that step number or lower
  filtered_demands <- all_individual_demands %>% filter(household <= step)
  
  # Summarize total_power_demand by grouping only by time_minutes
  total_demands_step <- filtered_demands %>%
    group_by(time_minutes) %>%
    summarize(total_power_demand = sum(power_demand)) %>%
    mutate(group = step,
           day_hours = time_minutes / 60)
  
  step_summary <- total_demands_step %>% 
    summarise(max_power = max(total_power_demand),
              base_load = min(total_power_demand)) %>% 
    mutate(group = step,
           peak_component = max_power - base_load,
           aggregation_dividend = (step * peak_power) - base_load - peak_component)
  
  # Bind the resultant dataframe to all_total_demands
  all_total_demands <- bind_rows(all_total_demands, total_demands_step)
  demands_summary <- bind_rows(demands_summary, step_summary)
}
demands_summary_long <- demands_summary %>% 
  select(-max_power) %>% 
  # select(-aggregation_dividend) %>% 
  gather(key = "key", value = "value", -group) %>% 
  mutate(dummyx = "Power")

demands_summary_long %>% 
  ggplot(aes(x = as.factor(group), y = value, fill = key))+
  geom_bar(position = "fill", stat = "identity")+
  ggtitle("Composition of Maximum Possible Power Demand")

# Create the animated plot
demand_summary_plot <- demands_summary_long %>% 
  ggplot(aes(fill = key, y = value, x = as.factor(dummyx))) +
  geom_bar(position = "stack", stat = "identity")+
  labs(y = "Power Demand (kW)",
       title = "Demand Summary for {closest_state} Households") +
  theme_minimal() +
  transition_states(group, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)
animate(demand_summary_plot, renderer = gifski_renderer(), fps = 20, duration = 5)



# Create the animated plot
followed_demands_animation <- ggplot(all_total_demands, aes(x = day_hours, y = total_power_demand, group = group)) +
  geom_line(color = "black") +
  labs(x = "Hours of the Day",
       y = "Total Power Demand (kW)",
       title = "Total Power Demand for {closest_state} Households") +
  theme_minimal() +
  transition_states(group, transition_length = 4, state_length = 1) +
  view_follow()

unfollowed_demands_animation <- ggplot(all_total_demands, aes(x = day_hours, y = total_power_demand, group = group)) +
  geom_line(color = "black") +
  labs(x = "Hours of the Day",
       y = "Total Power Demand (kW)",
       title = "Total Power Demand for {closest_state} Households") +
  theme_minimal() +
  transition_states(group, transition_length = 4, state_length = 1) 

# Save and view the animation at 4 frames per second
followed <- followed_demands_animation %>% 
  animate(renderer = gifski_renderer(), fps = 20, duration = 5)
unfollowed <- unfollowed_demands_animation %>% 
  animate(renderer = gifski_renderer(), fps = 20, duration = 5)

a_mgif <- image_read(followed)
b_mgif <- image_read(unfollowed)
add_frames_length <- length(b_mgif)
new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(i in 2:add_frames_length){
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  new_gif <- c(new_gif, combined)
}

new_gif


anim_save("appliances.gif", new_gif)



### Delete the plots
# List all files in the working directory
files <- list.files(pattern = "gganim_plot")
# Remove files with 'gganim_plot' in the filename
file.remove(files)
# library(tidyverse)
# library(skimr)
# library(tidygeocoder)
# library(sf)
# library(rnaturalearth)
# library(rnaturalearthdata)
# library(corrplot)
# library(patchwork)
# library(ggrepel)
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
  "Clare" = "QLD",
  "Winton" = "VIC",
  "Happy Valley" = "VIC",
  "Quorn Park" = "NSW"
)

all_names <- all_names %>% filter_on_location_cut_list(cut_list = manual_cut_list)

cascade_results1 <- geocode_from_names(all_names, location_state_map)

plot_locations(cascade_results1)

source_df <- load_source_df(all_names, all_traces)

long_df <- gather_source_data(source_df)

corr_scaled <- long_df %>% rescale_for_tod_month()

###### half-hour correlation plot
corr_scaled %>% raw_correlation_plot()

day_summ <- get_day_summ(long_df)


plot_normalised_output_distributions(day_summ, c(1,2,3,4,5))
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
  filter(correlation < .1) %>%
  filter(distance > 1000) %>%
  filter(distance < 1750)

weirdbad <- correlations_distances %>%
  filter(correlation > 0.5) %>%
  filter(distance > 900)
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


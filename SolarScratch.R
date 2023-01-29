library(tidyverse)
library(skimr)
library(tidygeocoder)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


#------ mapping --------
all_traces <- list.files("../AEMO_data/solar")

all_names <- all_traces %>% 
  lapply(str_remove_all, "(?i)_FFP_.*") %>% 
  lapply(str_remove_all, "(?i)_SAT_.*") %>% 
  lapply(str_remove_all, "(?i)_CST_.*") %>% 
  lapply(str_remove_all, "(?i)REZ_[:alpha:][:digit:]_") %>% 
  lapply(str_replace_all, "_", " ") %>% 
  lapply(paste0, ", Australia") %>% 
  unlist() %>% 
  tibble()

colnames(all_names) <- c("location")
cascade_results1 <- all_names %>%
  geocode_combine(
    queries = list(
      list(method = 'census'),
      list(method = 'osm')
    ),
    global_params = list(address = 'location')
  )

world <- ne_countries(scale = "medium", returnclass = "sf")

xmin <- cascade_results1$long %>% min(na.rm = T) + 18
xmax <- cascade_results1$long %>% max(na.rm = T) + 1
ymin <- cascade_results1$lat %>% min(na.rm = T) - 2
ymax <- cascade_results1$lat %>% max(na.rm = T) + 1

ggplot(data = world) +
  geom_sf()+
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE)+
  geom_point(data = cascade_results1, aes(x = long, y = lat), size = 4)


#----- working on multiple downloads -----
# for (t in all_traces %>% head(1)){
#   filename <- paste0("../AEMO_data/solar/", t)
#   
#   if (exists("df"){
#     df <- read_csv(filename, show_col_types = FALSE)
#   }
#   
# }


#------ individual visuals and summar ----
df <- read_csv("../AEMO_data/solar/Winton_SAT_RefYear3012.csv")
skim(df)
plot_df <- df %>%
  filter(Year < 2034,
         Year > 2023) %>%
  # filter(Year %in% 2023) %>%
  # filter(Month == 12) %>%
  gather(key = "Xvar", value = "value", -Day, -Month, -Year) %>%
  mutate(halfhour = Xvar %>% str_remove("X") %>% as.numeric()) %>%
  select(-Xvar) %>%
  mutate(day_id = paste(Year, Month, Day, sep = "_"))

low_days <- plot_df %>%
  group_by(Year, Month, Day) %>%
  summarise(total = sum(value)) %>% 
  mutate(day_id = paste(Year, Month, Day, sep = "_"))

low_day_ids <- low_days %>% arrange(total) %>% head(200) %>% pull(day_id)
high_day_ids <- low_days %>% arrange(total) %>% tail(200) %>% pull(day_id)

plot_df %>%
  filter(day_id %in% low_day_ids) %>%
  ggplot(aes(x = halfhour, y = value, col = as.factor(Day)))+
  geom_line()+
  facet_grid(Month~Year)

low_days %>%
  ggplot(aes(x = total))+
  geom_density()


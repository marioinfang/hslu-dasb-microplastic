library(dplyr)
library(ggplot2)
library(FNN)

df_buoy <- read.csv('C:/Users/minfang/HSLU/DASB/hslu-dataset-experiments/currents/currents_by_buoy_time.csv')
df_microplastic <- read.csv('C:/Users/minfang/HSLU/DASB/hslu-dataset-experiments/microplastics/Microplastics_Cleaned.csv')

# Find nearest microplastic for each buoy measures
nearest_mp <- get.knnx(data = df_microplastic[, c("Longitude", "Latitude")], 
                       query = df_buoy[, c("lon", "lat")], 
                       k = 1)

df_buoy$Concentration.Class <- df_microplastic$Concentration.Class[nearest_mp$nn.index]
df_buoy$mp_lon <- df_microplastic$Longitude[nearest_mp$nn.index]
df_buoy$mp_lat <- df_microplastic$Latitude[nearest_mp$nn.index]

# Select only buoys with more than 80 measures
bojen_ids <- df_buoy %>%
  count(id) %>%
  filter(n > 80) %>%
  pull(id)

# filter buoys with more than 80 measures and mutate concentration value
df_filtered <- df_buoy %>%
  filter(id %in% bojen_ids) %>%
  mutate(
    Concentration.Class = factor(Concentration.Class, 
                                 levels = c("Very Low", "Low", "Medium", "High", "Very High"))
  )

# Konzentrationsklassen in numerische Werte umwandeln
concentration_map <- c(
  "Very Low" = 1,
  "Low" = 2,
  "Medium" = 3,
  "High" = 4,
  "Very High" = 5
)

df_filtered <- df_buoy %>%
  filter(id %in% bojen_ids) %>%
  mutate(
    Concentration.Class = factor(Concentration.Class, 
                                 levels = names(concentration_map)),
    Concentration.Value = concentration_map[as.character(Concentration.Class)]
  )

# Durchschnitt pro Boje berechnen
avg_concentration <- df_filtered %>%
  group_by(id) %>%
  summarise(avg_concentration = mean(Concentration.Value, na.rm = TRUE)) %>%
  arrange(desc(avg_concentration))

plot_buoyes_movement_top_microplastic <- function(number) {
  req(number)
  
  ggplot(df_filtered %>% filter(id %in% head(avg_concentration, number)$id), aes(x = lon, y = lat, group = id)) +
    geom_path(color = "grey70") +
    geom_point(aes(color = Concentration.Class), size = 1.5, alpha = 0.9) +
    facet_wrap(~id, scales = "free") +
    theme_minimal() +
    scale_color_manual(values = c(
      "Very Low" = "lightblue",
      "Low" = "skyblue",
      "Medium" = "orange",
      "High" = "red",
      "Very High" = "darkred"
    )) +
    labs(title = "Top 10",
         subtitle = "Die 10 Bojen mit der höchsten Mikroplastik-Konzentration",
         x = "Longitude",
         y = "Latitude")
}

plot_buoyes_movement_bottom_microplastic <- function(number) {
  req(number)
  
  ggplot(df_filtered %>% filter(id %in% tail(avg_concentration, number)$id), aes(x = lon, y = lat, group = id)) +
    geom_path(color = "grey70") +
    geom_point(aes(color = Concentration.Class), size = 1.5, alpha = 0.9) +
    facet_wrap(~id, scales = "free") +
    theme_minimal() +
    scale_color_manual(values = c(
      "Very Low" = "lightblue",
      "Low" = "skyblue",
      "Medium" = "orange",
      "High" = "red",
      "Very High" = "darkred"
    )) +
    labs(title = "Bottom 10",
         subtitle = "Die 10 Bojen mit der niedrigsten Mikroplastik-Konzentration",
         x = "Longitude",
         y = "Latitude")
}

plot_buoyes_movement_top_microplastic <- function(number) {
  ggplot(df_filtered %>% filter(id %in% head(avg_concentration, number)$id), aes(x = lon, y = lat, group = id)) +
    geom_path(color = "grey70") +
    geom_point(aes(color = Concentration.Class), size = 1.5, alpha = 0.9) +
    facet_wrap(~id, scales = "free") +
    theme_minimal() +
    scale_color_manual(values = c(
      "Very Low" = "lightblue",
      "Low" = "skyblue",
      "Medium" = "orange",
      "High" = "red",
      "Very High" = "darkred"
    )) +
    labs(title = "Top 10",
         subtitle = "Die 10 Bojen mit der höchsten Mikroplastik-Konzentration",
         x = "Longitude",
         y = "Latitude")
}
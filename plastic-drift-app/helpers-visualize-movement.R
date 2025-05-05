library(dplyr)
library(ggplot2)
library(FNN)
library(arrow)

buoy_data <- read_parquet("datasources/currents_by_buoy_time.parquet")
microplastic_data <- read_parquet("datasources/cleaned_microplastics.parquet")

nearest_mp <- get.knnx(data = microplastic_data[, c("Longitude", "Latitude")], 
                       query = buoy_data[, c("lon", "lat")], 
                       k = 1)

buoy_data$Concentration.Class <- microplastic_data$Concentration.Class[nearest_mp$nn.index]
buoy_data$mp_lon <- microplastic_data$Longitude[nearest_mp$nn.index]
buoy_data$mp_lat <- microplastic_data$Latitude[nearest_mp$nn.index]

buoy_ids <- buoy_data %>%
  count(id) %>%
  filter(n > 80) %>%
  pull(id)

df_filtered <- buoy_data %>%
  filter(id %in% buoy_ids) %>%
  mutate(
    Concentration.Class = factor(Concentration.Class, 
                                 levels = c("Very Low", "Low", "Medium", "High", "Very High"))
  )

concentration_map <- c(
  "Very Low" = 1,
  "Low" = 2,
  "Medium" = 3,
  "High" = 4,
  "Very High" = 5
)

df_filtered <- buoy_data %>%
  filter(id %in% buoy_ids) %>%
  mutate(
    Concentration.Class = factor(Concentration.Class, 
                                 levels = names(concentration_map)),
    Concentration.Value = concentration_map[as.character(Concentration.Class)]
  )

avg_concentration <- df_filtered %>%
  group_by(id) %>%
  summarise(avg_concentration = mean(Concentration.Value, na.rm = TRUE)) %>%
  arrange(desc(avg_concentration))

plot_buoyes_movement_bottom_microplastic <- function(number) {
  req(number)
  
  ggplot(df_filtered %>% filter(id %in% tail(avg_concentration, number)$id), aes(x = lon, y = lat, group = id)) +
    geom_path(color = "grey70") +
    geom_point(aes(color = Concentration.Class), size = 1.5, alpha = 0.9) +
    facet_wrap(~id, scales = "free") +
    theme_minimal() +
    scale_color_manual(
      name = "Color mapping",
      values = c(
      "Very Low" = "lightblue",
      "Low" = "skyblue",
      "Medium" = "orange",
      "High" = "red",
      "Very High" = "darkred"
    )) +
    labs(x = "Longitude",
         y = "Latitude")
}

plot_buoyes_movement_top_microplastic <- function(number) {
  ggplot(df_filtered %>% filter(id %in% head(avg_concentration, number)$id), aes(x = lon, y = lat, group = id)) +
    geom_path(color = "grey70") +
    geom_point(aes(color = Concentration.Class), size = 1.5, alpha = 0.9) +
    facet_wrap(~id, scales = "free") +
    theme_minimal() +
    scale_color_manual(
      name = "Color mapping",
      values = c(
      "Very Low" = "lightblue",
      "Low" = "skyblue",
      "Medium" = "orange",
      "High" = "red",
      "Very High" = "darkred"
    )) +
    labs(x = "Longitude",
         y = "Latitude")
}
library(ggplot2)
library(viridis)
library(dplyr)
library(ggpubr)
library(jpeg)
library(purrr)

equirectangular_projection <- readJPEG("assets/equirectangular_projection.jpg")


plot_ocean_measurements_global <- function(fill_var, title, fill_label, data) {
  ggplot() +
    background_image(equirectangular_projection) +
    coord_fixed(ratio = 1, xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE, clip = "on") +
    geom_point(
      data = data, aes(x = lon, y = lat, fill = !!sym(fill_var)),
      shape = 21, size = 3, alpha = 0.1, color = "black", stroke = 0.3
    ) +
    scale_fill_viridis_c(name = fill_label, option = "C") +
    geom_point(
      data = data[!is.na(data$Concentration.Class), ],
      aes(x = mp_lon, y = mp_lat, color = Concentration.Class),
      alpha = 0.7, size = 0.7
    ) +
    scale_color_manual(
      name = "Density Class",
      values = c(
        "Very Low" = "white", "Low" = "green",
        "Medium" = "blue", "High" = "orange", "Very High" = "red"
      )
    ) +
    theme_minimal() +
    labs(title = title, x = "Longitude", y = "Latitude") +
    theme(legend.position = "right")
}

plot_ocean_measurements_regional <- function(data, fill_var, title, fill_label) {
  lat_range_raw <- range(data$lat, na.rm = TRUE)
  lon_range_raw <- range(data$lon, na.rm = TRUE)

  # Round ranges to nearest 5: e.g. (50.1, 69.9) -> (50, 70)
  lat_range <- c(floor(lat_range_raw[1] / 5) * 5, ceiling(lat_range_raw[2] / 5) * 5)
  lon_range <- c(floor(lon_range_raw[1] / 5) * 5, ceiling(lon_range_raw[2] / 5) * 5)

  ggplot() +
    coord_fixed(ratio = 1, xlim = lon_range, ylim = lat_range, expand = FALSE, clip = "on") +
    geom_point(
      data = data, aes(x = lon, y = lat, fill = !!sym(fill_var)),
      shape = 21, size = 3, alpha = 0.4, color = "black", stroke = 0.3
    ) +
    scale_fill_viridis_c(name = fill_label, option = "C") +
    geom_point(
      data = data[!is.na(data$Concentration.Class), ],
      aes(x = mp_lon, y = mp_lat, color = Concentration.Class),
      alpha = 0.7, size = 1
    ) +
    scale_color_manual(
      name = "Density Class",
      values = c(
        "Very Low" = "white", "Low" = "green",
        "Medium" = "blue", "High" = "orange", "Very High" = "red"
      )
    ) +
    theme_minimal() +
    labs(title = title, x = "Longitude", y = "Latitude") +
    theme(legend.position = "right")
}

filter_region <- function(data, region) {
  switch(region,
    "North Atlantic" = data %>% filter(lon >= -80 & lon <= -20 & lat >= 10 & lat <= 70),
    "Mediterranean" = data %>% filter(lon >= -10 & lon <= 40 & lat >= 30 & lat <= 45),
    "North Pacific" = data %>% filter((lon >= -180 & lon <= -120) & lat >= 5 & lat <= 40),
    "Northern North Atlantic" = data %>% filter(lon >= -80 & lon <= -5 & lat >= 50 & lat <= 80),
    "Mid Atlantic (Upper South America)" = data %>% filter(lon >= -60 & lon <= -20 & lat >= -5 & lat <= 10),
    "South Atlantic" = data %>% filter(lon >= -80 & lon <= 20 & lat >= -60 & lat <= -10),
    data
  )
}

show_correlation <- function(data, var) {
  if (!("Concentration.Class" %in% colnames(data)) || all(is.na(data$Concentration.Class))) {
    return("No microplastic concentration data available for this region.")
  }

  data$Concentration.Class <- factor(data$Concentration.Class,
    levels = c("Very Low", "Low", "Medium", "High", "Very High"),
    labels = 1:5
  )

  cor_test <- cor.test(data[[var]], as.numeric(data$Concentration.Class))

  cat(
    "Correlation:", round(cor_test$estimate, 3), "\n",
    "T-value:", round(cor_test$statistic, 2), "\n",
    "Degrees of Freedom:", cor_test$parameter, "\n",
    "P-value:", format.pval(cor_test$p.value, digits = 3, eps = 0.001), "\n",
    "95% CI: [", round(cor_test$conf.int[1], 3), ", ", round(cor_test$conf.int[2], 3), "]\n"
  )
}


plot_sample_buoys <- function(currents, nr_of_buoys) {
  unique_ids <- unique(currents$id)
  sample_ids <- sample(unique_ids, nr_of_buoys)
  currents_sample <- currents[currents$id %in% sample_ids, ]

  ggplot(currents_sample, aes(x = lon, y = lat, color = as.factor(id))) +
    background_image(equirectangular_projection) +
    coord_fixed(ratio = 1, xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE, clip = "on") +
    geom_point(size = 0.1) +
    # scale_color_gradient(low="blue", high="red") +
    scale_color_manual(values = rainbow(nr_of_buoys)) +
    theme_minimal() +
    labs(
      title = sprintf("Sample of %s Buoys", nr_of_buoys),
      x = "Longitude", y = "Latitude", color = "none"
    ) +
    guides(color = "none")
}

plot_largest_buoys <- function(currents, nr_of_buoys) {
  largest_currents <- currents %>%
    group_by(id) %>%
    group_split() %>%
    .[order(map_int(., nrow), decreasing = TRUE)] %>%
    .[1:nr_of_buoys]

  largest_current_ids <- largest_currents %>%
    map(~ .$id) %>%
    unlist() %>%
    unique()

  largest_currents <- currents[currents$id %in% largest_current_ids, ]

  ggplot(largest_currents, aes(x = lon, y = lat, color = as.factor(id))) +
    background_image(equirectangular_projection) +
    coord_fixed(ratio = 1, xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE, clip = "on") +
    geom_point(size = 0.1 + largest_currents$count * 0.001) +
    scale_color_manual(values = rainbow(nr_of_buoys)) +
    theme_minimal() +
    labs(
      title = paste0("Largest ", nr_of_buoys, " Buoyes"),
      x = "Longitude", y = "Latitude", color = NULL
    ) +
    guides(color = "none")
}

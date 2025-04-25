library(ggplot2)
library(dplyr)
library(ggpubr)
library(jpeg)
library(purrr)

equirectangular_projection <- readJPEG("assets/equirectangular_projection.jpg")

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

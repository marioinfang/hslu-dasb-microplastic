library(dplyr)

calculate_all_correlations <- function(data, regions, attributes) {
  correlation_coefficients_list <- list()
  for (region in regions) {
    region_data <- filter_region(data, region)
    if (!("Concentration.Class" %in% colnames(region_data)) || all(is.na(region_data$Concentration.Class))) {
      correlation_coefficients_list[[region]] <- NULL
      next
    }

    region_data$Concentration.Class <- factor(region_data$Concentration.Class,
      levels = c("Very Low", "Low", "Medium", "High", "Very High"),
      labels = 1:5
    )

    region_coeffs_df <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(region_coeffs_df) <- c("Attribute", "Correlation")

    for (var in attributes) {
      cor_test <- cor.test(region_data[[var]], as.numeric(region_data$Concentration.Class))
      region_coeffs_df <- rbind(region_coeffs_df, data.frame(Attribute = var, Correlation = cor_test$estimate))
    }
    correlation_coefficients_list[[region]] <- region_coeffs_df
  }
  return(correlation_coefficients_list)
}

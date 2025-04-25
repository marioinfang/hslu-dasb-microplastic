library(ggplot2)
library(dplyr)
library(nnet)
library(tidyr)
library(arrow)

# get, split, train
linked_data <- read_parquet("datasources/currents_with_microplastics.parquet")

linked_data <- linked_data %>%
  filter((lon >= -180 & lon <= -120) & lat >= 5 & lat <= 40)

set.seed(123)
train_index <- sample(1:nrow(linked_data), 0.7 * nrow(linked_data))
train_data <- linked_data[train_index, ]
test_data <- linked_data[-train_index, ]

train_data$Concentration.Class <- factor(train_data$Concentration.Class, levels = c("Very Low", "Low", "Medium", "High", "Very High"), ordered = FALSE)
test_data$Concentration.Class <- factor(test_data$Concentration.Class, levels = c("Very Low", "Low", "Medium", "High", "Very High"), ordered = FALSE)

model_1 <- multinom(Concentration.Class ~ measurement_count, data = train_data)
model_2 <- multinom(Concentration.Class ~ measurement_count + speed_sum, data = train_data)
model_3 <- multinom(Concentration.Class ~ measurement_count + speed_sum + buoy_count, data = train_data)
model_4 <- multinom(Concentration.Class ~ measurement_count + speed_sum + buoy_count +  ve_avg + vn_avg, data = train_data)
model_5 <- multinom(Concentration.Class ~ measurement_count + speed_sum + buoy_count +  lat + lon, data = train_data)
model_6 <- multinom(Concentration.Class ~ measurement_count + speed_sum + buoy_count +  ve_avg + vn_avg + lat + lon, data = train_data)

model_interaction_1 <- multinom(Concentration.Class ~ measurement_count * speed_sum, data = train_data)
model_interaction_2 <- multinom(Concentration.Class ~ measurement_count * buoy_count * speed_sum, data = train_data)
model_interaction_3 <- multinom(Concentration.Class ~ measurement_count * buoy_count + speed_sum, data = train_data)
model_interaction_4 <- multinom(Concentration.Class ~ measurement_count * speed_sum + buoy_count, data = train_data)

model_transformed_extended <- multinom(Concentration.Class ~ log(measurement_count) * poly(speed_sum, 2) + buoy_count, data = train_data)

# linear models
anova_output <- anova(model_1, model_2, model_3, model_4, model_5, model_6)

aic_values <- AIC(model_1, model_2, model_3, model_4, model_5, model_6)
bic_values <- BIC(model_1, model_2, model_3, model_4, model_5, model_6)

get_predictors <- function(model) {
  formula_str <- as.character(formula(model))
  predictors_part <- formula_str[3]
  predictors <- trimws(strsplit(predictors_part, "+", fixed = TRUE)[[1]])
  return(predictors)
}

# visualise predictors
generate_prediction_data <- function(model, test_data, predictor_to_visualize) {
  predictor_to_visualize <- predictor_to_visualize

  predictor_range <- seq(min(test_data[[predictor_to_visualize]], na.rm = TRUE),
                           max(test_data[[predictor_to_visualize]], na.rm = TRUE),
                           length.out = 100)

  other_predictors <- setdiff(names(test_data), c("Concentration.Class", predictor_to_visualize))
  new_data_predict <- data.frame(predictor_value = predictor_range)

  for (predictor in other_predictors) {
    new_data_predict[[predictor]] <- mean(test_data[[predictor]], na.rm = TRUE)
  }
  colnames(new_data_predict)[colnames(new_data_predict) == "predictor_value"] <- predictor_to_visualize
  return(new_data_predict)
}

create_prediction_plot <- function(model, new_data_predict, predictor_to_visualize) {
  predicted_probabilities <- predict(model, newdata = new_data_predict, type = "probs")

  predicted_probs_long <- predicted_probabilities %>%
    as_tibble() %>%
    mutate(!!predictor_to_visualize := new_data_predict[[predictor_to_visualize]]) %>%
    pivot_longer(cols = -!!predictor_to_visualize, names_to = "Concentration.Class", values_to = "Probability")

  ggplot(predicted_probs_long, aes(x = .data[[predictor_to_visualize]], y = Probability, color = Concentration.Class)) +
    geom_line(linewidth = 1) +
    labs(
      title = paste("Predicted Probabilities vs.", predictor_to_visualize),
      x = predictor_to_visualize,
      y = "Predicted Probability"
    ) +
    theme_bw()
}

# Interaction
anova_interaction_output <- anova(model_3, model_interaction_1, model_interaction_2, model_interaction_3, model_interaction_4)

aic_interaction_values <- AIC(model_3, model_interaction_1, model_interaction_2, model_interaction_3, model_interaction_4)
bic_interaction_values <- BIC(model_3, model_interaction_1, model_interaction_2, model_interaction_3, model_interaction_4)

best_interactive_model <- model_interaction_4

evaluate_interaction_model <- function(model, test_data) {
  predicted_classes_interaction_test <- predict(model, newdata = test_data)
  accuracy_interaction_test <- mean(predicted_classes_interaction_test == test_data$Concentration.Class)
  confusion_matrix_interaction <- table(predicted_classes_interaction_test, test_data$Concentration.Class)
  return(list(accuracy = accuracy_interaction_test, confusion_matrix = confusion_matrix_interaction))
}

plot_interaction_predictions <- function(model, test_data) {
  predicted_classes_interaction_test <- predict(model, newdata = test_data)
  ggplot(test_data, aes(x = measurement_count, fill = predicted_classes_interaction_test)) +
    geom_density(alpha = 0.5) +
    facet_wrap(~ Concentration.Class) +
    labs(title = "Predicted vs. Actual Concentration by Measurement Count (Interaction Model)",
         x = "Measurement Count",
         y = "Density",
         fill = "Predicted Class") +
    theme_bw()
}

plot_interaction_effects <- function(model, test_data) {
  predictor_x <- "measurement_count"
  predictor_color <- "speed_sum"

  measurement_range <- seq(min(test_data[[predictor_x]], na.rm = TRUE),
                           max(test_data[[predictor_x]], na.rm = TRUE),
                           length.out = 50)

  speed_sum_range <- seq(min(test_data[[predictor_color]], na.rm = TRUE),
                         max(test_data[[predictor_color]], na.rm = TRUE),
                         length.out = 3)

  new_data_predict_interaction <- expand.grid(
    measurement_count = measurement_range,
    speed_sum = speed_sum_range,
    buoy_count = mean(test_data$buoy_count, na.rm = TRUE)
  )

  predicted_probabilities_interaction <- predict(model, newdata = new_data_predict_interaction, type = "probs")

  predicted_probs_long_interaction <- predicted_probabilities_interaction %>%
    as_tibble() %>%
    mutate(measurement_count = new_data_predict_interaction$measurement_count,
           speed_sum = new_data_predict_interaction$speed_sum) %>%
    pivot_longer(cols = -c(measurement_count, speed_sum), names_to = "Concentration.Class", values_to = "Probability")

  ggplot(predicted_probs_long_interaction,
         aes(x = measurement_count, y = Probability, color = Concentration.Class)) +
    geom_line(linewidth = 1) +
    facet_wrap(~ speed_sum, labeller = label_bquote(speed_sum == .(round(speed_sum, 2)))) +
    labs(
      title = paste("Predicted Probabilities vs. Measurement Count by Speed Sum"),
      x = "Measurement Count",
      y = "Predicted Probability",
      color = "Concentration Class"
    ) +
    theme_bw()
}

# transformation
plot_predictor_histogram <- function(data, predictor) {
  if (grepl("log\\(", predictor, ignore.case = TRUE)) {
    original_predictor <- gsub("log\\((.*)\\)", "\\1", predictor, ignore.case = TRUE)
    x_data <- data %>% mutate(!!predictor := log(.data[[original_predictor]]))
  } else {
    x_data <- data
  }
  ggplot(x_data, aes(x = .data[[predictor]])) +
    geom_histogram(bins = 30, fill = "blue", color = "black") +
    labs(
      title = paste("Histogram of", predictor),
      x = predictor,
      y = "Frequency"
    ) +
    theme_bw()
}

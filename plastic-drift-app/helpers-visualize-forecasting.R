library(ggplot2)
library(dplyr)
library(nnet)
library(tidyr)

linked_data <- read.csv("datasources/currents_with_microplastics.csv")

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

# Store the models in a list so we can refer to them programmatically.
model_list <- list(model_1 = model_1, model_2 = model_2, model_3 = model_3, model_4 = model_4, model_5 = model_5, model_6 = model_6)

anova_output <- anova(model_1, model_2, model_3, model_4, model_5, model_6)

aic_values <- AIC(model_1, model_2, model_3, model_4, model_5, model_6)
bic_values <- BIC(model_1, model_2, model_3, model_4, model_5, model_6)

get_predictors <- function(model) {
  formula_str <- as.character(formula(model))
  predictors_part <- formula_str[3]
  predictors <- trimws(strsplit(predictors_part, "+", fixed = TRUE)[[1]])
  return(predictors)
}

# Visualize predictors on a specific model
generate_prediction_data <- function(model, test_data, predictor_to_visualize) {
  # 1. Choose a Predictor to Visualize Against: predictor_to_visualize
  predictor_to_visualize <- predictor_to_visualize

  # 2. Create a Range of Values
  predictor_range <- seq(min(test_data[[predictor_to_visualize]], na.rm = TRUE),
                           max(test_data[[predictor_to_visualize]], na.rm = TRUE),
                           length.out = 100)

  # 3. Create a New Data Frame for Prediction
  # Hold other predictors at their mean
  other_predictors <- setdiff(names(test_data), c("Concentration.Class", predictor_to_visualize))
  new_data_predict <- data.frame(predictor_value = predictor_range)

  for (predictor in other_predictors) {
    new_data_predict[[predictor]] <- mean(test_data[[predictor]], na.rm = TRUE)
  }
  colnames(new_data_predict)[colnames(new_data_predict) == "predictor_value"] <- predictor_to_visualize
  return(new_data_predict)
}

# Function to create prediction plot for a given predictor
create_prediction_plot <- function(model, new_data_predict, predictor_to_visualize) {
  # 4. Predict Probabilities
  predicted_probabilities <- predict(model, newdata = new_data_predict, type = "probs")

  # Convert to a long format for ggplot2
  predicted_probs_long <- predicted_probabilities %>%
    as_tibble() %>%
    mutate(!!predictor_to_visualize := new_data_predict[[predictor_to_visualize]]) %>%
    pivot_longer(cols = -!!predictor_to_visualize, names_to = "Concentration.Class", values_to = "Probability")

  # 5. Visualize the Predicted Probabilities
  ggplot(predicted_probs_long, aes(x = .data[[predictor_to_visualize]], y = Probability, color = Concentration.Class)) +
    geom_line(linewidth = 1) +
    labs(
      title = paste("Predicted Probabilities vs.", predictor_to_visualize),
      x = predictor_to_visualize,
      y = "Predicted Probability"
    ) +
    theme_bw()
}
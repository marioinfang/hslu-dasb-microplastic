library(arrow)
library(shiny)
library(ggplot2)
library(viridis)
library(dplyr)
library(nnet)
library(tidyr)

source("helpers-visualize-forecasting.R")
source("helpers-visualize-attributes.R")
source("helpers-visualize-correlations.R")
source("helpers-visualize-buoyes.R")

currents_and_microplastics <- read_parquet("datasources/currents_with_microplastics.parquet")
regions <- c(
  "North Atlantic", "Mediterranean", "North Pacific",
  "Northern North Atlantic", "Mid Atlantic (Upper South America)",
  "South Atlantic"
)
attributes_to_correlate <- c("measurement_count", "speed_sum", "speed_avg", "ve_avg", "vn_avg", "buoy_count")

all_correlation_coefficients <- reactiveVal(NULL)
{
  correlation_data <- calculate_all_correlations(currents_and_microplastics, regions, attributes_to_correlate)
  all_correlation_coefficients(correlation_data) # Store the calculated coefficients
}
currents_by_buoy_time <- read_parquet("datasources/currents_by_buoy_time.parquet")

shinyServer(function(input, output) {
  output$global_plot <- renderPlot({
    plot_ocean_measurements_global(
      selected_attribute = input$selected_var_global,
      show_microplastic = input$show_microplastic,
      title = paste("Global View -", input$selected_var_global),
      data = currents_and_microplastics
    )
  })

  filtered_region_data <- eventReactive(input$plot_button, {
    filter_region(currents_and_microplastics, input$selected_region)
  })

  output$regional_plot <- renderPlot({
    req(input$plot_button)
    region_data <- filtered_region_data()
    plot_ocean_measurements_regional(
      selected_attribute = input$selected_var_regional,
      title = paste(input$selected_region, "-", input$selected_var_regional),
      data = region_data,
    )
  })

  output$correlation_results <- renderPrint({
    req(input$plot_button)
    region_data <- filtered_region_data()
    show_correlation(region_data, input$selected_var_regional)
  })

  #####################################################################
  #################### CORRELATION ####################################
  #####################################################################

  output$selected_region_correlation_plot <- renderPlot({
    req(input$plot_button)
    selected_region <- input$selected_region
    correlation_data <- all_correlation_coefficients()
    region_correlation <- correlation_data[[selected_region]]

    if (!is.null(region_correlation) && nrow(region_correlation) > 0) {
      ggplot(region_correlation, aes(x = Attribute, y = Correlation)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = round(Correlation, 2)), vjust = ifelse(region_correlation$Correlation > 0, -0.3, 1.3), size = 3) +
        labs(title = selected_region, x = "Current Attribute", y = "Correlation") +
        theme_minimal()
    } else {
      ggplot() +
        annotate("text", x = 1, y = 1, label = paste("No correlation data for", selected_region), size = 4) +
        theme_void()
    }
  })

  output$all_regions_barplot <- renderPlot({
    correlation_data_list <- all_correlation_coefficients()
    if (is.null(correlation_data_list)) {
      return(ggplot() +
        annotate("text", x = 1, y = 1, label = "Calculating correlations...", size = 4) +
        theme_void())
    }

    all_regions_df <- bind_rows(correlation_data_list, .id = "Region")

    ggplot(all_regions_df, aes(x = Attribute, y = Correlation, fill = Region)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = round(Correlation, 2), group = Region),
        position = position_dodge(width = 0.9),
        hjust = ifelse(all_regions_df$Correlation > 0, -0.1, 1.1),
        vjust = 0.5,
        angle = 90,
        size = 3
      ) +
      labs(
        title = "Correlation of Attributes with Microplastic Density by Region",
        x = "Current Attribute",
        y = "Correlation Coefficient"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  output$sample_buoys <- renderPlot(
    plot_sample_buoys(currents_by_buoy_time, input$nr_of_buoys)
  )

  output$largest_buoys <- renderPlot({
    plot_largest_buoys(currents_by_buoy_time, input$nr_of_buoys)
  })

  ############################################################
  ####################### MODELLING ##########################
  ############################################################

  output$boxplot_forecasting <- renderPlot({
    selected_predictor <- input$predictor
    ggplot(train_data, aes(x = Concentration.Class, y = .data[[selected_predictor]], fill = Concentration.Class)) +
      geom_boxplot() +
      labs(title = paste(input$predictor, "by Concentration Class (Training)"), y = input$predictor)
  })
  output$summary_table <- renderTable({
    train_data %>%
      group_by(Concentration.Class) %>%
      summarise(
        mean_speed_avg = mean(speed_avg, na.rm = TRUE),
        mean_speed_sum = mean(speed_sum, na.rm = TRUE),
        mean_ve_avg = mean(ve_avg, na.rm = TRUE),
        mean_vn_avg = mean(vn_avg, na.rm = TRUE),
        mean_buoy_count = mean(buoy_count, na.rm = TRUE),
        mean_measurement_count = mean(measurement_count, na.rm = TRUE),
        mean_lon = mean(lon, na.rm = TRUE),
        mean_lat = mean(lat, na.rm = TRUE),
        n = n()
      )
  })

  output$model_definitions <- renderPrint({
    model_texts <- c(
      paste("model_1: Predictors = ", paste(get_predictors(model_1), collapse = ", ")),
      paste("model_2: Predictors = ", paste(get_predictors(model_2), collapse = ", ")),
      paste("model_3: Predictors = ", paste(get_predictors(model_3), collapse = ", ")),
      paste("model_4: Predictors = ", paste(get_predictors(model_4), collapse = ", ")),
      paste("model_5: Predictors = ", paste(get_predictors(model_5), collapse = ", ")),
      paste("model_6: Predictors = ", paste(get_predictors(model_6), collapse = ", "))
    )
    cat(paste(model_texts, collapse = "\n\n"), sep = "")
  })


  output$anova_table <- renderTable({
    anova_df <- as.data.frame(anova_output)

    anova_df$Model <- rownames(anova_df)
    rownames(anova_df) <- NULL

    anova_df <- anova_df %>%
      select(Model, everything())

    return(anova_df)
  })

  output$aic_table <- renderTable({
    data.frame(
      Model = 1:6,
      AIC = aic_values$AIC
    )
  })
  output$bic_table <- renderTable({
    data.frame(
      Model = 1:6,
      BIC = bic_values$BIC
    )
  })

  output$confusion_matrix_test <- renderTable({
    predicted_classes_test <- predict(model_3, newdata = test_data)
    confusion_matrix <- table(predicted_classes_test, test_data$Concentration.Class)
    as.data.frame.matrix(confusion_matrix) # Convert to data frame for tableOutput
  })

  output$accuracy_test <- renderPrint({
    predicted_classes_test <- predict(model_3, newdata = test_data)
    accuracy_test <- mean(predicted_classes_test == test_data$Concentration.Class)
    cat(paste("Accuracy of Model 3 on Test Data:", round(accuracy_test, 3), "\n"))
  })

  output$prediction_density_plot_test <- renderPlot({
    predicted_classes_test <- predict(model_3, newdata = test_data)
    ggplot(test_data, aes(x = measurement_count, fill = predicted_classes_test)) +
      geom_density(alpha = 0.5) +
      facet_wrap(~Concentration.Class) +
      labs(
        title = "Predicted vs. Actual Concentration by Measurement Count (Test Data)",
        x = "Measurement Count",
        y = "Density",
        fill = "Predicted Class"
      )
  })
  output$model3_prediction_plot <- renderPlot({
    selected_predictor <- input$selected_predictor_model3
    new_data_predict <- generate_prediction_data(model_3, test_data, selected_predictor)
    create_prediction_plot(model_3, new_data_predict, selected_predictor)
  })

  output$interaction_model_definitions <- renderPrint({
    model_texts <- c(
      paste("model_interaction_1: Predictors = ", paste(get_predictors(model_interaction_1), collapse = ", ")),
      paste("model_interaction_2: Predictors = ", paste(get_predictors(model_interaction_2), collapse = ", ")),
      paste("model_interaction_3: Predictors = ", paste(get_predictors(model_interaction_3), collapse = ", ")),
      paste("model_interaction_4: Predictors = ", paste(get_predictors(model_interaction_4), collapse = ", "))
    )
    cat(paste(model_texts, collapse = "\n\n"), sep = "")
  })

  output$anova_interaction_table <- renderTable({
    anova_df <- as.data.frame(anova_interaction_output)
    anova_df$Model <- rownames(anova_df)
    rownames(anova_df) <- NULL
    anova_df <- anova_df %>% select(Model, everything())
    return(anova_df)
  })

  output$aic_interaction_table <- renderTable({
    data.frame(
      Model = c("model_3", "model_interaction_1", "model_interaction_2", "model_interaction_3", "model_interaction_4"),
      AIC = aic_interaction_values$AIC
    )
  })

  output$bic_interaction_table <- renderTable({
    data.frame(
      Model = c("model_3", "model_interaction_1", "model_interaction_2", "model_interaction_3", "model_interaction_4"),
      BIC = bic_interaction_values$BIC
    )
  })

  output$interaction_model_evaluation <- renderPrint({
    evaluation_results <- evaluate_interaction_model(best_interactive_model, test_data)
    cat(paste("Accuracy of Best Interaction Model on Test Data:", round(evaluation_results$accuracy, 3), "\n"))
  })

  output$confusion_matrix_interaction_test <- renderTable({
    evaluation_results <- evaluate_interaction_model(best_interactive_model, test_data)
    as.data.frame.matrix(evaluation_results$confusion_matrix)
  })

  output$interaction_prediction_plot <- renderPlot({
    plot_interaction_predictions(best_interactive_model, test_data)
  })

  output$interaction_plot <- renderPlot({
    plot_interaction_effects(best_interactive_model, test_data)
  })

  output$predictor_histogram <- renderPlot({
    selected_predictor <- input$selected_predictor_hist
    plot_predictor_histogram(train_data, selected_predictor)
  })

  output$transformed_model_evaluation <- renderPrint({
    predicted_classes_transformed_test <- predict(model_transformed_extended, newdata = test_data)
    accuracy_transformed_test <- mean(predicted_classes_transformed_test == test_data$Concentration.Class)
    cat(paste("Accuracy of Transformed Model on Test Data:", round(accuracy_transformed_test, 3), "\n"))
  })

  output$transformed_model_prediction_plot <- renderPlot({
    predicted_classes_transformed_test <- predict(model_transformed_extended, newdata = test_data)
    ggplot(test_data, aes(x = measurement_count, fill = predicted_classes_transformed_test)) +
      geom_density(alpha = 0.5) +
      facet_wrap(~Concentration.Class) +
      labs(
        title = "Predicted vs. Actual Concentration by Measurement Count (Transformed Model)",
        x = "Measurement Count",
        y = "Density",
        fill = "Predicted Class"
      )
  })

  output$transformed_model_confusion_matrix <- renderTable({
    predicted_classes_transformed_test <- predict(model_transformed_extended, newdata = test_data)
    confusion_matrix_transformed <- table(predicted_classes_transformed_test, test_data$Concentration.Class)
    as.data.frame.matrix(confusion_matrix_transformed)
  })

  output$combined_model_definitions <- renderPrint({
    model_texts <- c(
      paste("model_transformed_extended: Predictors = ", paste(get_predictors(model_transformed_extended), collapse = ", ")) # added model
    )
    cat(paste(model_texts, collapse = "\n\n"), sep = "")
  })
})

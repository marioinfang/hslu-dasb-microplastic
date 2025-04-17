library(shiny)
library(ggplot2)
library(viridis)
library(dplyr)
source("helpers-visualize-attributes.R")
source("helpers-visualize-correlations.R")

currents_and_microplastics <- read.csv("datasources/currents_with_microplastics.csv")
regions <- c("North Atlantic", "Mediterranean", "North Pacific",
             "Northern North Atlantic", "Mid Atlantic (Upper South America)",
             "South Atlantic")
attributes_to_correlate <- c("measurement_count", "speed_sum", "speed_avg", "ve_avg", "vn_avg", "buoy_count")

all_correlation_coefficients <- reactiveVal(NULL)
{
  correlation_data <- calculate_all_correlations(currents_and_microplastics, regions, attributes_to_correlate)
  all_correlation_coefficients(correlation_data) # Store the calculated coefficients
}

shinyServer(function(input, output) {
  output$global_plot <- renderPlot({
    plot_ocean_measurements_global(
      fill_var = input$selected_var_global,
      title = paste("Global View -", input$selected_var_global),
      fill_label = input$selected_var_global,
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
      data = region_data,
      fill_var = input$selected_var_regional,
      title = paste(input$selected_region, "-", input$selected_var_regional),
      fill_label = input$selected_var_regional
    )
  })

  output$correlation_results <- renderPrint({
    req(input$plot_button)
    region_data <- filtered_region_data()
    show_correlation(region_data, input$selected_var_regional)
  })

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
      return(ggplot() + annotate("text", x = 1, y = 1, label = "Calculating correlations...", size = 4) + theme_void())
    }

    all_regions_df <- bind_rows(correlation_data_list, .id = "Region")

    ggplot(all_regions_df, aes(x = Attribute, y = Correlation, fill = Region)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = round(Correlation, 2), group = Region),
                position = position_dodge(width = 0.9),
                hjust = ifelse(all_regions_df$Correlation > 0, -0.1, 1.1), 
                vjust = 0.5,
                angle = 90,
                size = 3) +
      labs(title = "Correlation of Attributes with Microplastic Density by Region",
           x = "Current Attribute",
           y = "Correlation Coefficient") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
})

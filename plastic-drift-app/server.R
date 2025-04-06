library(shiny)
library(ggplot2)
library(viridis)
library(dplyr)
source("helpers-visualize-attributes.R")

currents_and_microplastics <- read.csv("datasources/currents_with_microplastics.csv")

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
})

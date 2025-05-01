library(shiny)

shinyUI(fluidPage(
  titlePanel("Microplastics & Ocean Currents Explorer"),
  tabsetPanel(
    tabPanel(
      "Global & Regional View",
      h3("Global Visualization of Microplastics and Ocean Currents"),
      sidebarLayout(
        sidebarPanel(
          selectInput("selected_var_global", "Select Global Attribute:",
            choices = list(
              "Measurement Count" = "measurement_count",
              "Speed Sum" = "speed_sum",
              "Speed Average" = "speed_avg",
              "Ve Average" = "ve_avg",
              "Vn Average" = "vn_avg",
              "Buoy Count" = "buoy_count"
            ),
            selected = "measurement_count"
          ),
          checkboxInput("show_microplastic", "Show Microplastic", value = TRUE)
        ),
        mainPanel(
          plotOutput("global_plot")
        )
      ),
      br(),
      h4("Correlation of All Attributes by Region"),
      plotOutput("all_regions_barplot"),
      br(),
      h3("Regional Correlation View & Analysis"),
      sidebarLayout(
        sidebarPanel(
          selectInput("selected_var_regional", "Select Regional Attribute:",
            choices = list(
              "Measurement Count" = "measurement_count",
              "Speed Sum" = "speed_sum",
              "Speed Average" = "speed_avg",
              "Ve Average" = "ve_avg",
              "Vn Average" = "vn_avg",
              "Buoy Count" = "buoy_count"
            ),
            selected = "measurement_count"
          ),
          selectInput("selected_region", "Select Region:",
            choices = c(
              "North Atlantic", "Mediterranean", "North Pacific",
              "Northern North Atlantic", "Mid Atlantic (Upper South America)",
              "South Atlantic"
            )
          ),
          actionButton("plot_button", "Show Map & Correlation")
        ),
        mainPanel(
          plotOutput("regional_plot"),
          verbatimTextOutput("correlation_results")
        )
      ),
      br(),
      plotOutput("selected_region_correlation_plot"),
    ),
    tabPanel(
      "Individual Buoys",
      h3("Individual Buoys"),
      sidebarLayout(
        sidebarPanel(
          sliderInput("nr_of_buoys", "Number of Buoys:",
            min = 1, max = 1000, value = 30
          )
        ),
        mainPanel(
          h4("Sample Buoys"),
          plotOutput("sample_buoys"),
          h4("Largest Buoys"),
          plotOutput("largest_buoys")
        )
      )
    ),
    tabPanel(
      "Modelling",
      h3("Distribution on the training data"),
      tableOutput("summary_table"),
      h3("Modelling on the Northern Pacific - Predictors"),
      sidebarLayout(
        sidebarPanel(
          selectInput("predictor", "Select Predictor Variable:",
            choices = c(
              "speed_avg", "speed_sum", "ve_avg", "vn_avg",
              "buoy_count", "measurement_count", "lon", "lat"
            ),
            selected = "speed_avg"
          )
        ),
        mainPanel(
          plotOutput("boxplot_forecasting")
        )
      ),
      h3("Model Comparison"),
      verbatimTextOutput("model_definitions"),
      fluidRow(
        column(
          6,
          h4("ANOVA Comparison of Models"),
          tableOutput("anova_table")
        ),
        column(
          3,
          h4("AIC Values"),
          tableOutput("aic_table")
        ),
        column(
          3,
          h4("BIC Values"),
          tableOutput("bic_table")
        )
      ),
      h3("Linear Model Evaluation"),
      selectInput("selected_model", "Choose a Model:",
            choices = c("Model 3" = "model_3", "Model 5" = "model_5"),
            selected = "model_3"),
      fluidRow(
        column(
          6,
          h4("Confusion Matrix"),
          tableOutput("confusion_matrix_test")
        ),
        column(
          6,
          h4("Test Set Accuracy"),
          verbatimTextOutput("accuracy_test")
        )
      ),
      h4("Predicted vs. Actual Concentration"),
      plotOutput("prediction_density_plot_test"),
      h4("Predicted Probability Plot"),
      sidebarLayout(
  sidebarPanel(
    selectInput("selected_predictor", "Select Predictor:",
                choices = c("measurement_count", "speed_sum", "buoy_count", "speed_avg"),
                selected = "measurement_count")
  ),
  mainPanel(
    plotOutput("model_prediction_plot")
  )
      ),
      h3("Interaction Models"),
      verbatimTextOutput("interaction_model_definitions"),
      fluidRow(
        column(
          6,
          h4("ANOVA Comparison of Interaction Models"),
          tableOutput("anova_interaction_table")
        ),
        column(
          3,
          h4("AIC Values (Interaction)"),
          tableOutput("aic_interaction_table")
        ),
        column(
          3,
          h4("BIC Values (Interaction)"),
          tableOutput("bic_interaction_table")
        )
      ),
      h3("Best Interaction Model Evaluation"),
      fluidRow(
        column(
          6,
          h4("Confusion Matrix (Interaction)"),
          tableOutput("confusion_matrix_interaction_test")
        ),
        column(
          6,
          h4("Test Set Accuracy (Interaction)"),
          verbatimTextOutput("interaction_model_evaluation")
        )
      ),
      h4("Predicted vs. Actual Concentration (Interaction Model)"),
      plotOutput("interaction_prediction_plot"),
      h4("Interaction Effect Plot"),
      plotOutput("interaction_plot"),
      h3("Transformation"),
      sidebarLayout(
        sidebarPanel(
          selectInput("selected_predictor_hist", "Select Predictor:",
            choices = c("measurement_count", "log(measurement_count)", "speed_sum", "log(speed_sum)", "buoy_count", "log(buoy_count)"),
            selected = "measurement_count"
          )
        ),
        mainPanel(
          plotOutput("predictor_histogram")
        )
      ),
      h3("Transformed Model Evaluation"),
      verbatimTextOutput("combined_model_definitions"),
      fluidRow(
        column(
          6,
          h4("Transformed Model Accuracy"),
          verbatimTextOutput("transformed_model_evaluation")
        ),
        column(
          6,
          h4("Transformed Model Confusion Matrix"),
          tableOutput("transformed_model_confusion_matrix")
        )
      ),
      fluidRow(
        column(
          6,
          h4("ANOVA Comparison of Models"),
          tableOutput("anova_all_table")
        ),
        column(
          3,
          h4("AIC Values"),
          tableOutput("aic_all_table")
        ),
        column(
          3,
          h4("BIC Values"),
          tableOutput("bic_all_table")
        )),
      h4("Predicted vs. Actual Concentration (Transformed Model)"),
      plotOutput("transformed_model_prediction_plot")
    ),
    tabPanel(
      "Buoys movement",
      h3("Movement analyse of buoys"),
      h4("Top"),
      sliderInput("nr_of_buoys_for_movement_top", "Number of Buoys:",
                  min = 1, max = 40, value = 10
      ),
      plotOutput("top_buoys_movement"),
      br(),
      h4("Bottom"),
      plotOutput("bottom_buoys_movement"),
    ),
    tabPanel(
      "PLACEHOLDER",
      h3("More cool stuff coming soon!")
    ),
  ),
))

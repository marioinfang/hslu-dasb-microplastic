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
          )
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
      h5("Based on the model comparisons, the boxplots and domain knowledge, the predictors used in model 3 will be picked and evaluated."),
      h3("Linear Model 3 Evaluation"),
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
      h4("Model 3: Predicted Probability Plot"),
      sidebarLayout(
        sidebarPanel(
          selectInput("selected_predictor_model3", "Select Predictor:",
            choices = c("measurement_count", "speed_sum", "buoy_count"),
            selected = "measurement_count"
          )
        ),
        mainPanel(
          plotOutput("model3_prediction_plot")
        )
      )
    ),
    tabPanel(
      "PLACEHOLDER",
      h3("More cool stuff coming soon!")
    ),
  ),
))

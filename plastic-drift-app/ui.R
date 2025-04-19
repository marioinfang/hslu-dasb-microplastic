library(shiny)

shinyUI(fluidPage(
  titlePanel("Microplastics & Ocean Currents Explorer"),

  tabsetPanel(
    tabPanel("Global & Regional View", 
      h3("Global Visualization of Microplastics and Ocean Currents"),
      sidebarLayout(
        sidebarPanel(
          selectInput("selected_var_global", "Select Global Attribute:", 
                      choices = list("Measurement Count" = "measurement_count",
                                     "Speed Sum" = "speed_sum",
                                     "Speed Average" = "speed_avg",
                                     "Ve Average" = "ve_avg",
                                     "Vn Average" = "vn_avg",
                                     "Buoy Count" = "buoy_count"),
                      selected = "measurement_count")
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
                      choices = list("Measurement Count" = "measurement_count",
                                     "Speed Sum" = "speed_sum",
                                     "Speed Average" = "speed_avg",
                                     "Ve Average" = "ve_avg",
                                     "Vn Average" = "vn_avg",
                                     "Buoy Count" = "buoy_count"),
                      selected = "measurement_count"),
          selectInput("selected_region", "Select Region:", 
                      choices = c("North Atlantic", "Mediterranean", "North Pacific",
                                  "Northern North Atlantic", "Mid Atlantic (Upper South America)",
                                  "South Atlantic")),
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
    tabPanel("Individual Buoys", 
      h3("Individual Buoys"),
      sidebarLayout(
        sidebarPanel(
          sliderInput("nr_of_buoys", "Number of Buoys:",
                      min = 1, max = 1000, value = 30)
        ),
        mainPanel(
          h4("Sample Buoys"),
          plotOutput("sample_buoys"),
          h4("Largest Buoys"),
          plotOutput("largest_buoys")
        )
      )
    ),
    tabPanel("PLACEHOLDER", 
      h3("More cool stuff coming soon!")
    )
  )
))

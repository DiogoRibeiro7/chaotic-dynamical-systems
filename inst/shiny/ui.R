# Interactive Extreme Value Explorer
# A Shiny application for exploring extreme value analysis in chaotic dynamical systems

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(chaoticds)

ui <- dashboardPage(
  dashboardHeader(title = "Chaotic Systems: Extreme Value Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Simulation", tabName = "simulation", icon = icon("chart-line")),
      menuItem("Threshold Analysis", tabName = "threshold", icon = icon("ruler")),
      menuItem("Extremal Index", tabName = "extremal", icon = icon("calculator")),
      menuItem("Cluster Analysis", tabName = "clusters", icon = icon("project-diagram")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Simulation tab
      tabItem(tabName = "simulation",
        fluidRow(
          box(title = "Simulation Parameters", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("map_type", "Map Type:",
                       choices = list("Logistic Map" = "logistic", "Hénon Map" = "henon"),
                       selected = "logistic"),
            
            conditionalPanel(
              condition = "input.map_type == 'logistic'",
              numericInput("n_obs", "Number of Observations:", value = 1000, min = 100, max = 10000, step = 100),
              numericInput("r_param", "r Parameter:", value = 3.8, min = 0.1, max = 4.0, step = 0.1),
              numericInput("x0_param", "Initial Value x0:", value = 0.2, min = 0.0, max = 1.0, step = 0.1)
            ),
            
            conditionalPanel(
              condition = "input.map_type == 'henon'",
              numericInput("n_obs_henon", "Number of Observations:", value = 1000, min = 100, max = 10000, step = 100),
              numericInput("a_param", "a Parameter:", value = 1.4, min = 0.1, max = 2.0, step = 0.1),
              numericInput("b_param", "b Parameter:", value = 0.3, min = 0.1, max = 1.0, step = 0.1)
            ),
            
            actionButton("simulate", "Generate Data", class = "btn-primary"),
            br(), br(),
            downloadButton("download_data", "Download Data", class = "btn-success")
          ),
          
          box(title = "Time Series Plot", status = "primary", solidHeader = TRUE, width = 8,
            plotlyOutput("time_series_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(title = "Data Summary", status = "info", solidHeader = TRUE, width = 6,
            verbatimTextOutput("data_summary")
          ),
          
          box(title = "Histogram", status = "info", solidHeader = TRUE, width = 6,
            plotlyOutput("histogram_plot", height = "300px")
          )
        )
      ),
      
      # Threshold Analysis tab
      tabItem(tabName = "threshold",
        fluidRow(
          box(title = "Threshold Selection", status = "primary", solidHeader = TRUE, width = 4,
            sliderInput("threshold_quantile", "Threshold (Quantile):", 
                       min = 0.8, max = 0.99, value = 0.95, step = 0.01),
            numericInput("threshold_value", "Or Specify Value:", value = NULL),
            br(),
            checkboxInput("show_exceedances", "Show Exceedances", value = TRUE),
            br(),
            h5("Diagnostic Plots:"),
            checkboxInput("show_mrl", "Mean Residual Life Plot", value = TRUE),
            actionButton("update_threshold", "Update Analysis", class = "btn-primary")
          ),
          
          box(title = "Threshold Diagnostics", status = "primary", solidHeader = TRUE, width = 8,
            plotlyOutput("threshold_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(title = "Exceedance Statistics", status = "info", solidHeader = TRUE, width = 6,
            verbatimTextOutput("exceedance_stats")
          ),
          
          box(title = "MRL Plot", status = "info", solidHeader = TRUE, width = 6,
            conditionalPanel(
              condition = "input.show_mrl",
              plotOutput("mrl_plot", height = "300px")
            )
          )
        )
      ),
      
      # Extremal Index tab
      tabItem(tabName = "extremal",
        fluidRow(
          box(title = "Extremal Index Estimation", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("ei_method", "Estimation Method:",
                       choices = list("Runs" = "runs", "Intervals" = "intervals"),
                       selected = "runs"),
            
            conditionalPanel(
              condition = "input.ei_method == 'runs'",
              numericInput("run_length", "Run Length:", value = 3, min = 1, max = 10, step = 1)
            ),
            
            numericInput("n_bootstrap", "Bootstrap Samples:", value = 100, min = 10, max = 1000, step = 10),
            checkboxInput("compute_ci", "Compute Confidence Interval", value = TRUE),
            br(),
            actionButton("estimate_ei", "Estimate Extremal Index", class = "btn-primary")
          ),
          
          box(title = "Extremal Index Results", status = "primary", solidHeader = TRUE, width = 8,
            verbatimTextOutput("ei_results"),
            br(),
            plotlyOutput("ei_plot", height = "300px")
          )
        )
      ),
      
      # Cluster Analysis tab
      tabItem(tabName = "clusters",
        fluidRow(
          box(title = "Cluster Analysis Parameters", status = "primary", solidHeader = TRUE, width = 4,
            numericInput("cluster_run_length", "Run Length for Clustering:", value = 3, min = 1, max = 10, step = 1),
            br(),
            actionButton("analyze_clusters", "Analyze Clusters", class = "btn-primary")
          ),
          
          box(title = "Cluster Size Distribution", status = "primary", solidHeader = TRUE, width = 8,
            plotlyOutput("cluster_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(title = "Cluster Statistics", status = "info", solidHeader = TRUE, width = 12,
            verbatimTextOutput("cluster_stats")
          )
        )
      ),
      
      # About tab
      tabItem(tabName = "about",
        fluidRow(
          box(title = "About This Application", status = "primary", solidHeader = TRUE, width = 12,
            h3("Interactive Extreme Value Explorer"),
            p("This Shiny application provides an interactive interface for exploring extreme value analysis 
              in chaotic dynamical systems using the chaoticds R package."),
            
            h4("Features:"),
            tags$ul(
              tags$li("Simulate logistic and Hénon maps with customizable parameters"),
              tags$li("Interactive threshold selection and diagnostic plots"),
              tags$li("Extremal index estimation using multiple methods"),
              tags$li("Cluster analysis of extreme events"),
              tags$li("Real-time visualization with plotly")
            ),
            
            h4("Usage:"),
            tags$ol(
              tags$li("Start by generating simulation data in the 'Simulation' tab"),
              tags$li("Explore threshold selection in the 'Threshold Analysis' tab"),
              tags$li("Estimate extremal index in the 'Extremal Index' tab"),
              tags$li("Analyze cluster patterns in the 'Cluster Analysis' tab")
            ),
            
            h4("About the chaoticds Package:"),
            p("The chaoticds package provides tools for extreme value analysis of chaotic dynamical systems,
              including simulation utilities, threshold diagnostics, extremal index estimation, and cluster analysis."),
            
            hr(),
            p("Built with Shiny, shinydashboard, ggplot2, and plotly.", 
              style = "font-style: italic; color: gray;")
          )
        )
      )
    )
  )
)
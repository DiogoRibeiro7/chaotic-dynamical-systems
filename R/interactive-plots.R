#' Interactive visualization utilities
#'
#' Functions for creating interactive plots and dashboards for exploring
#' chaotic dynamical systems and extreme value analysis.
#'
#' @name interactive_plots
NULL

#' Interactive attractor plot
#'
#' Create an interactive 3D plot of a chaotic attractor using plotly.
#'
#' @param data Data frame with columns for coordinates (x, y, z optional)
#' @param color_by Character name of variable to color points by
#' @param title Character plot title
#' @param point_size Numeric size of points
#'
#' @return plotly object
#' @export
interactive_attractor <- function(data, color_by = NULL, title = "Chaotic Attractor", 
                                  point_size = 2) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("plotly package is required for interactive plots")
  }
  
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that("x" %in% names(data), "y" %in% names(data))
  
  if (ncol(data) >= 3 && "z" %in% names(data)) {
    # 3D plot
    p <- plotly::plot_ly(data, x = ~x, y = ~y, z = ~z, 
                        type = "scatter3d", mode = "markers",
                        marker = list(size = point_size))
    
    if (!is.null(color_by) && color_by %in% names(data)) {
      p <- p %>% plotly::add_markers(color = data[[color_by]], 
                                   colorscale = "Viridis")
    }
  } else {
    # 2D plot
    p <- plotly::plot_ly(data, x = ~x, y = ~y, 
                        type = "scatter", mode = "markers",
                        marker = list(size = point_size))
    
    if (!is.null(color_by) && color_by %in% names(data)) {
      p <- p %>% plotly::add_markers(color = data[[color_by]], 
                                   colorscale = "Viridis")
    }
  }
  
  p %>% plotly::layout(title = title,
                      scene = list(aspectmode = "cube"))
}

#' Interactive time series plot with extreme events
#'
#' Create an interactive time series plot highlighting extreme events.
#'
#' @param data Numeric vector or data frame with time series
#' @param time_index Numeric vector of time indices (optional)
#' @param threshold Numeric threshold for highlighting extremes
#' @param title Character plot title
#'
#' @return plotly object
#' @export
interactive_timeseries <- function(data, time_index = NULL, threshold = NULL, 
                                   title = "Time Series with Extremes") {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("plotly package is required for interactive plots")
  }
  
  if (is.vector(data)) {
    if (is.null(time_index)) {
      time_index <- seq_along(data)
    }
    df <- data.frame(time = time_index, value = data)
  } else {
    df <- data
    if (!"time" %in% names(df)) {
      df$time <- seq_len(nrow(df))
    }
  }
  
  p <- plotly::plot_ly(df, x = ~time, y = ~value, type = "scatter", mode = "lines",
                      name = "Series")
  
  if (!is.null(threshold)) {
    # Add threshold line
    p <- p %>% plotly::add_lines(y = threshold, name = "Threshold",
                               line = list(color = "red", dash = "dash"))
    
    # Highlight exceedances
    exceedances <- df[df$value > threshold, ]
    if (nrow(exceedances) > 0) {
      p <- p %>% plotly::add_markers(data = exceedances, x = ~time, y = ~value,
                                   name = "Exceedances", 
                                   marker = list(color = "red", size = 6))
    }
  }
  
  p %>% plotly::layout(title = title,
                      xaxis = list(title = "Time"),
                      yaxis = list(title = "Value"))
}

#' Launch interactive Shiny dashboard
#'
#' Launch an interactive dashboard for exploring chaotic systems and extremes.
#'
#' @param launch Logical whether to launch the app immediately
#' @param port Integer port number for the Shiny app
#'
#' @return Shiny app object
#' @export
launch_chaos_dashboard <- function(launch = TRUE, port = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("shiny package is required for interactive dashboard")
  }
  if (!requireNamespace("shinydashboard", quietly = TRUE)) {
    stop("shinydashboard package is required for interactive dashboard")  
  }
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("DT package is required for interactive tables")
  }
  
  # Define UI
  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "Chaotic Dynamics Explorer"),
    
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Simulation", tabName = "simulation", icon = shiny::icon("chart-line")),
        shinydashboard::menuItem("Extreme Analysis", tabName = "extremes", icon = shiny::icon("mountain")),
        shinydashboard::menuItem("Diagnostics", tabName = "diagnostics", icon = shiny::icon("search"))
      )
    ),
    
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        # Simulation tab
        shinydashboard::tabItem(tabName = "simulation",
          shiny::fluidRow(
            shinydashboard::box(
              title = "Simulation Parameters", status = "primary", solidHeader = TRUE, width = 4,
              shiny::selectInput("map_type", "Map Type:", 
                               choices = list("Logistic" = "logistic", "Hénon" = "henon")),
              shiny::conditionalPanel(
                condition = "input.map_type == 'logistic'",
                shiny::numericInput("r", "Growth rate (r):", value = 3.8, min = 0, max = 4, step = 0.1),
                shiny::numericInput("x0", "Initial condition (x0):", value = 0.2, min = 0, max = 1, step = 0.01)
              ),
              shiny::conditionalPanel(
                condition = "input.map_type == 'henon'",
                shiny::numericInput("a", "Parameter a:", value = 1.4, min = 0, max = 2, step = 0.1),
                shiny::numericInput("b", "Parameter b:", value = 0.3, min = 0, max = 1, step = 0.1),
                shiny::numericInput("x0_henon", "Initial x:", value = 0, min = -2, max = 2, step = 0.1),
                shiny::numericInput("y0_henon", "Initial y:", value = 0, min = -2, max = 2, step = 0.1)
              ),
              shiny::numericInput("n_points", "Number of points:", value = 1000, min = 100, max = 10000),
              shiny::actionButton("simulate", "Simulate", class = "btn-primary")
            ),
            shinydashboard::box(
              title = "Attractor", status = "info", solidHeader = TRUE, width = 8,
              plotly::plotlyOutput("attractor_plot", height = "400px")
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(
              title = "Time Series", status = "info", solidHeader = TRUE, width = 12,
              plotly::plotlyOutput("timeseries_plot", height = "300px")
            )
          )
        ),
        
        # Extreme Analysis tab
        shinydashboard::tabItem(tabName = "extremes",
          shiny::fluidRow(
            shinydashboard::box(
              title = "Extreme Value Parameters", status = "warning", solidHeader = TRUE, width = 4,
              shiny::numericInput("threshold_q", "Threshold quantile:", value = 0.95, min = 0.8, max = 0.99, step = 0.01),
              shiny::numericInput("block_size", "Block size:", value = 50, min = 10, max = 200),
              shiny::actionButton("analyze_extremes", "Analyze Extremes", class = "btn-warning")
            ),
            shinydashboard::box(
              title = "Extreme Value Analysis", status = "success", solidHeader = TRUE, width = 8,
              DT::dataTableOutput("extremes_table")
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(
              title = "Threshold Diagnostics", status = "success", solidHeader = TRUE, width = 6,
              plotly::plotlyOutput("threshold_plot")
            ),
            shinydashboard::box(
              title = "Extremal Index", status = "success", solidHeader = TRUE, width = 6,
              shiny::verbatimTextOutput("extremal_index")
            )
          )
        ),
        
        # Diagnostics tab  
        shinydashboard::tabItem(tabName = "diagnostics",
          shiny::fluidRow(
            shinydashboard::box(
              title = "Model Diagnostics", status = "danger", solidHeader = TRUE, width = 12,
              shiny::h4("Additional diagnostic plots and statistics will be shown here"),
              shiny::p("This section would include QQ plots, residual analysis, and model validation metrics.")
            )
          )
        )
      )
    )
  )
  
  # Define server logic
  server <- function(input, output, session) {
    # Reactive values to store simulation data
    sim_data <- shiny::reactiveValues(data = NULL, raw_data = NULL)
    
    # Simulation
    shiny::observeEvent(input$simulate, {
      if (input$map_type == "logistic") {
        raw <- simulate_logistic_map(input$n_points, input$r, input$x0)
        sim_data$raw_data <- raw
        sim_data$data <- data.frame(x = raw, time = seq_along(raw))
      } else {
        raw <- simulate_henon_map(input$n_points, input$a, input$b, input$x0_henon, input$y0_henon)
        sim_data$raw_data <- raw
        sim_data$data <- raw
      }
    })
    
    # Attractor plot
    output$attractor_plot <- plotly::renderPlotly({
      if (is.null(sim_data$data)) return(NULL)
      
      if (input$map_type == "logistic") {
        # For logistic map, show delayed embedding
        if (nrow(sim_data$data) > 1) {
          x_lag <- c(NA, sim_data$data$x[-nrow(sim_data$data)])
          df_embed <- data.frame(x = sim_data$data$x[-1], y = x_lag[-1])
          interactive_attractor(df_embed, title = "Logistic Map (Delayed Embedding)")
        }
      } else {
        interactive_attractor(sim_data$data, title = "Hénon Attractor")
      }
    })
    
    # Time series plot
    output$timeseries_plot <- plotly::renderPlotly({
      if (is.null(sim_data$data)) return(NULL)
      
      if (input$map_type == "logistic") {
        interactive_timeseries(sim_data$data$x, title = "Logistic Map Time Series")
      } else {
        interactive_timeseries(sim_data$data$x, title = "Hénon Map x-component")
      }
    })
    
    # Extreme value analysis
    shiny::observeEvent(input$analyze_extremes, {
      if (is.null(sim_data$raw_data)) return()
      
      if (input$map_type == "logistic") {
        data_vec <- sim_data$raw_data
      } else {
        data_vec <- sim_data$raw_data$x
      }
      
      threshold <- quantile(data_vec, input$threshold_q)
      exc <- exceedances(data_vec, threshold)
      bm <- block_maxima(data_vec, input$block_size)
      
      output$extremes_table <- DT::renderDataTable({
        data.frame(
          Metric = c("Threshold", "Number of exceedances", "Mean exceedance", 
                    "Number of block maxima", "Mean block maximum"),
          Value = c(round(threshold, 4), length(exc), round(mean(exc), 4),
                   length(bm), round(mean(bm), 4))
        )
      }, options = list(dom = 't'))
      
      # Extremal index estimation
      output$extremal_index <- shiny::renderText({
        theta_runs <- extremal_index_runs(data_vec, threshold, run_length = 3)
        theta_intervals <- extremal_index_intervals(data_vec, threshold)
        
        paste0("Extremal Index Estimates:\n",
               "Runs method: ", round(theta_runs, 4), "\n",
               "Intervals method: ", round(theta_intervals, 4))
      })
    })
  }
  
  app <- shiny::shinyApp(ui = ui, server = server)
  
  if (launch) {
    if (is.null(port)) {
      shiny::runApp(app)
    } else {
      shiny::runApp(app, port = port)
    }
  }
  
  return(app)
}
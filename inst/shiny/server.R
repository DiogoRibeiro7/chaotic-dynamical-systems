# Server logic for Interactive Extreme Value Explorer

library(shiny)
library(ggplot2)
library(plotly)
library(chaoticds)

server <- function(input, output, session) {
  # Reactive values to store data
  values <- reactiveValues(
    sim_data = NULL,
    threshold = NULL,
    exceedances = NULL,
    extremal_index = NULL,
    cluster_sizes = NULL
  )
  
  # Simulate data when button is clicked
  observeEvent(input$simulate, {
    if (input$map_type == "logistic") {
      values$sim_data <- simulate_logistic_map(input$n_obs, input$r_param, input$x0_param)
    } else if (input$map_type == "henon") {
      henon_data <- simulate_henon_map(input$n_obs_henon, input$a_param, input$b_param)
      values$sim_data <- henon_data$x  # Use x component for analysis
    }
    
    # Reset derived values
    values$threshold <- NULL
    values$exceedances <- NULL
    values$extremal_index <- NULL
    values$cluster_sizes <- NULL
    
    showNotification("Data generated successfully!", type = "success")
  })
  
  # Time series plot
  output$time_series_plot <- renderPlotly({
    if (is.null(values$sim_data)) {
      p <- ggplot() + 
        geom_text(aes(x = 0.5, y = 0.5, label = "Generate data to see plot"), size = 6) +
        theme_void()
      return(ggplotly(p))
    }
    
    df <- data.frame(
      time = 1:length(values$sim_data),
      value = values$sim_data
    )
    
    p <- ggplot(df, aes(x = time, y = value)) +
      geom_line(alpha = 0.7, color = "steelblue") +
      labs(title = "Time Series", x = "Time", y = "Value") +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(title = list(text = "Time Series", font = list(size = 16)))
  })
  
  # Data summary
  output$data_summary <- renderText({
    if (is.null(values$sim_data)) {
      return("No data generated yet.")
    }
    
    paste(
      "Data Summary:",
      sprintf("Length: %d", length(values$sim_data)),
      sprintf("Mean: %.4f", mean(values$sim_data)),
      sprintf("SD: %.4f", sd(values$sim_data)),
      sprintf("Min: %.4f", min(values$sim_data)),
      sprintf("Max: %.4f", max(values$sim_data)),
      sprintf("Range: %.4f", diff(range(values$sim_data))),
      sep = "\n"
    )
  })
  
  # Histogram
  output$histogram_plot <- renderPlotly({
    if (is.null(values$sim_data)) {
      p <- ggplot() + 
        geom_text(aes(x = 0.5, y = 0.5, label = "Generate data to see histogram"), size = 5) +
        theme_void()
      return(ggplotly(p))
    }
    
    df <- data.frame(value = values$sim_data)
    
    p <- ggplot(df, aes(x = value)) +
      geom_histogram(bins = 30, fill = "lightblue", alpha = 0.7, color = "black") +
      labs(title = "Distribution", x = "Value", y = "Frequency") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Update threshold analysis
  observeEvent(input$update_threshold, {
    if (is.null(values$sim_data)) {
      showNotification("Please generate data first!", type = "warning")
      return()
    }
    
    # Calculate threshold
    if (!is.null(input$threshold_value) && !is.na(input$threshold_value)) {
      values$threshold <- input$threshold_value
    } else {
      values$threshold <- quantile(values$sim_data, input$threshold_quantile)
    }
    
    # Calculate exceedances
    tryCatch({
      values$exceedances <- exceedances(values$sim_data, values$threshold)
    }, error = function(e) {
      showNotification(paste("Error calculating exceedances:", e$message), type = "error")
      values$exceedances <- numeric(0)
    })
    
    showNotification("Threshold analysis updated!", type = "success")
  })
  
  # Threshold plot
  output$threshold_plot <- renderPlotly({
    if (is.null(values$sim_data)) {
      p <- ggplot() + 
        geom_text(aes(x = 0.5, y = 0.5, label = "Generate data and update threshold"), size = 5) +
        theme_void()
      return(ggplotly(p))
    }
    
    df <- data.frame(
      time = 1:length(values$sim_data),
      value = values$sim_data
    )
    
    p <- ggplot(df, aes(x = time, y = value)) +
      geom_line(alpha = 0.6, color = "gray") +
      labs(title = "Time Series with Threshold", x = "Time", y = "Value") +
      theme_minimal()
    
    # Add threshold line if available
    if (!is.null(values$threshold)) {
      p <- p + geom_hline(yintercept = values$threshold, color = "red", linetype = "dashed", size = 1)
      
      # Highlight exceedances if requested
      if (input$show_exceedances && !is.null(values$exceedances) && length(values$exceedances) > 0) {
        exceed_indices <- which(values$sim_data > values$threshold)
        if (length(exceed_indices) > 0) {
          exceed_df <- data.frame(
            time = exceed_indices,
            value = values$sim_data[exceed_indices]
          )
          p <- p + geom_point(data = exceed_df, aes(x = time, y = value), 
                             color = "red", size = 1, alpha = 0.8)
        }
      }
    }
    
    ggplotly(p)
  })
  
  # Exceedance statistics
  output$exceedance_stats <- renderText({
    if (is.null(values$sim_data) || is.null(values$threshold)) {
      return("Update threshold analysis to see statistics.")
    }
    
    n_exceed <- length(values$exceedances)
    exceed_rate <- n_exceed / length(values$sim_data)
    
    paste(
      "Exceedance Statistics:",
      sprintf("Threshold: %.4f", values$threshold),
      sprintf("Number of exceedances: %d", n_exceed),
      sprintf("Exceedance rate: %.4f", exceed_rate),
      if (n_exceed > 0) sprintf("Mean excess: %.4f", mean(values$exceedances)) else "Mean excess: N/A",
      sep = "\n"
    )
  })
  
  # MRL plot
  output$mrl_plot <- renderPlot({
    if (is.null(values$sim_data)) {
      plot.new()
      text(0.5, 0.5, "Generate data first", cex = 1.5)
      return()
    }
    
    tryCatch({
      mrl_plot(values$sim_data)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
    })
  })
  
  # Estimate extremal index
  observeEvent(input$estimate_ei, {
    if (is.null(values$sim_data) || is.null(values$threshold)) {
      showNotification("Please generate data and set threshold first!", type = "warning")
      return()
    }
    
    tryCatch({
      if (input$ei_method == "runs") {
        values$extremal_index <- extremal_index_runs(values$sim_data, values$threshold, input$run_length)
      } else {
        values$extremal_index <- extremal_index_intervals(values$sim_data, values$threshold)
      }
      showNotification("Extremal index estimated!", type = "success")
    }, error = function(e) {
      showNotification(paste("Error estimating extremal index:", e$message), type = "error")
      values$extremal_index <- NA
    })
  })
  
  # Extremal index results
  output$ei_results <- renderText({
    if (is.null(values$extremal_index)) {
      return("Estimate extremal index to see results.")
    }
    
    paste(
      "Extremal Index Results:",
      sprintf("Method: %s", input$ei_method),
      if (input$ei_method == "runs") sprintf("Run length: %d", input$run_length) else "",
      sprintf("Estimate: %.4f", values$extremal_index),
      "",
      "Interpretation:",
      "θ ≈ 1: Independent extremes",
      "θ < 1: Clustered extremes",
      sep = "\n"
    )
  })
  
  # Extremal index visualization
  output$ei_plot <- renderPlotly({
    if (is.null(values$extremal_index)) {
      p <- ggplot() + 
        geom_text(aes(x = 0.5, y = 0.5, label = "Estimate extremal index"), size = 5) +
        theme_void()
      return(ggplotly(p))
    }
    
    # Simple visualization of the extremal index value
    df <- data.frame(
      method = "Estimate",
      value = values$extremal_index
    )
    
    p <- ggplot(df, aes(x = method, y = value)) +
      geom_col(fill = "steelblue", alpha = 0.7) +
      geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
      labs(title = "Extremal Index Estimate", y = "θ", x = "") +
      ylim(0, 1.1) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Analyze clusters
  observeEvent(input$analyze_clusters, {
    if (is.null(values$sim_data) || is.null(values$threshold)) {
      showNotification("Please generate data and set threshold first!", type = "warning")
      return()
    }
    
    tryCatch({
      values$cluster_sizes <- cluster_sizes(values$sim_data, values$threshold, input$cluster_run_length)
      showNotification("Cluster analysis completed!", type = "success")
    }, error = function(e) {
      showNotification(paste("Error in cluster analysis:", e$message), type = "error")
      values$cluster_sizes <- integer(0)
    })
  })
  
  # Cluster plot
  output$cluster_plot <- renderPlotly({
    if (is.null(values$cluster_sizes)) {
      p <- ggplot() + 
        geom_text(aes(x = 0.5, y = 0.5, label = "Analyze clusters first"), size = 5) +
        theme_void()
      return(ggplotly(p))
    }
    
    if (length(values$cluster_sizes) == 0) {
      p <- ggplot() + 
        geom_text(aes(x = 0.5, y = 0.5, label = "No clusters found"), size = 5) +
        theme_void()
      return(ggplotly(p))
    }
    
    df <- data.frame(cluster_size = values$cluster_sizes)
    
    p <- ggplot(df, aes(x = factor(cluster_size))) +
      geom_bar(fill = "lightcoral", alpha = 0.7, color = "black") +
      labs(title = "Cluster Size Distribution", x = "Cluster Size", y = "Frequency") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Cluster statistics
  output$cluster_stats <- renderText({
    if (is.null(values$cluster_sizes)) {
      return("Analyze clusters to see statistics.")
    }
    
    if (length(values$cluster_sizes) == 0) {
      return("No clusters found with current parameters.")
    }
    
    cluster_summary_stats <- cluster_summary(values$cluster_sizes)
    
    paste(
      "Cluster Statistics:",
      sprintf("Number of clusters: %d", length(values$cluster_sizes)),
      sprintf("Mean cluster size: %.2f", cluster_summary_stats["mean_size"]),
      sprintf("Variance of cluster sizes: %.2f", cluster_summary_stats["var_size"]),
      sprintf("Largest cluster: %d", max(values$cluster_sizes)),
      sprintf("Smallest cluster: %d", min(values$cluster_sizes)),
      sep = "\n"
    )
  })
  
  # Download handler for data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("chaotic_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(values$sim_data)) {
        write.csv(data.frame(time = 1:length(values$sim_data), value = values$sim_data), 
                  file, row.names = FALSE)
      }
    }
  )
}
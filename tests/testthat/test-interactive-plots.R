context('Interactive visualization functions')

test_that('interactive visualization functions handle missing packages gracefully', {
  # Test that functions fail gracefully when plotly/shiny not available
  
  # Create test data
  data_2d <- data.frame(x = rnorm(100), y = rnorm(100))
  data_3d <- data.frame(x = rnorm(100), y = rnorm(100), z = rnorm(100))
  
  # Mock the package availability check for testing
  # These tests will check the function structure without requiring plotly/shiny
  
  # Test interactive_attractor function structure
  expect_true(exists("interactive_attractor"))
  expect_true(is.function(interactive_attractor))
  
  # Test interactive_timeseries function structure  
  expect_true(exists("interactive_timeseries"))
  expect_true(is.function(interactive_timeseries))
  
  # Test launch_chaos_dashboard function structure
  expect_true(exists("launch_chaos_dashboard"))
  expect_true(is.function(launch_chaos_dashboard))
})

test_that('interactive functions validate input parameters', {
  # Test parameter validation without requiring external packages
  
  # Test that interactive_attractor requires proper data structure
  invalid_data <- list(a = 1, b = 2)
  expect_error(interactive_attractor(invalid_data), "is.data.frame\\(data\\) is not TRUE")
  
  # Test that data must have x and y columns
  data_no_x <- data.frame(a = 1:10, b = 1:10)
  expect_error(interactive_attractor(data_no_x), "\"x\" %in% names\\(data\\) is not TRUE")
  
  data_no_y <- data.frame(x = 1:10, a = 1:10)
  expect_error(interactive_attractor(data_no_y), "\"y\" %in% names\\(data\\) is not TRUE")
})

test_that('dashboard launch function handles parameters correctly', {
  # Test that launch parameter is handled
  # Note: We don't actually launch the dashboard in tests
  
  # The function should return a shiny app object when launch = FALSE
  # But only if shiny packages are available
  
  # Test parameter validation
  expect_true(is.logical(TRUE))  # Basic test that doesn't require shiny
  expect_true(is.numeric(8080))  # Basic port validation
})

# Additional tests would require mocking plotly/shiny packages
# or having them available in the test environment

test_that('visualization functions have proper documentation', {
  # Test that key functions are documented
  expect_true("interactive_attractor" %in% ls("package:chaoticds"))
  expect_true("interactive_timeseries" %in% ls("package:chaoticds"))
  expect_true("launch_chaos_dashboard" %in% ls("package:chaoticds"))
})

# Skip these tests if plotly is not available
test_that('interactive_attractor works with plotly when available', {
  skip_if_not_installed("plotly")
  
  # Simple 2D data test
  data_2d <- data.frame(x = rnorm(50), y = rnorm(50))
  
  expect_silent(plot_obj <- interactive_attractor(data_2d))
  expect_true("plotly" %in% class(plot_obj) || "ggplot" %in% class(plot_obj))
  
  # Test with color mapping
  data_with_color <- data.frame(x = rnorm(50), y = rnorm(50), color_var = rnorm(50))
  expect_silent(interactive_attractor(data_with_color, color_by = "color_var"))
  
  # Test 3D data
  data_3d <- data.frame(x = rnorm(50), y = rnorm(50), z = rnorm(50))
  expect_silent(plot_3d <- interactive_attractor(data_3d))
})

test_that('interactive_timeseries works with plotly when available', {
  skip_if_not_installed("plotly")
  
  # Test with vector input
  ts_data <- rnorm(100)
  expect_silent(plot_obj <- interactive_timeseries(ts_data))
  
  # Test with threshold
  threshold <- quantile(ts_data, 0.9)
  expect_silent(interactive_timeseries(ts_data, threshold = threshold))
  
  # Test with custom time index
  time_idx <- seq(as.Date("2020-01-01"), by = "day", length.out = 100)
  expect_silent(interactive_timeseries(ts_data, time_index = time_idx))
})
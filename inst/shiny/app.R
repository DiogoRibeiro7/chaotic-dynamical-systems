# Launch script for the Interactive Extreme Value Explorer

# Check and install required packages
required_packages <- c("shiny", "shinydashboard", "ggplot2", "plotly")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load the chaoticds package
library(chaoticds)

# Launch the Shiny app
shiny::runApp(system.file("shiny", package = "chaoticds"), 
              launch.browser = TRUE)
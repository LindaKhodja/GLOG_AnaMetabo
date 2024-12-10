# This script sets up the global environment and launches the AnaMetabo™ Shiny application.

# List of required packages
required_packages <- c("shiny", "igraph", "visNetwork", "memoise", "DT", "xml2", "shinydashboard", "RColorBrewer", "future", "promises")

# Install any missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load necessary libraries
library(shiny)
library(shinydashboard)
library(visNetwork)
library(xml2)
library(RColorBrewer)
library(DT)
library(memoise)
library(igraph)
library(future)
library(promises)

# Load utility functions
source("utils.R")

# Load the UI (User Interface) components
source("ui.R")

# Load the server logic
source("server.R")

# Launch the Shiny app
shinyApp(ui = ui, server = server)

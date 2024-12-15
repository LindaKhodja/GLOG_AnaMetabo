# This script launches the AnaMetabo™ Shiny application.

# Load utility functions
source("utils.R")

# Load the UI (User Interface) components
source("ui.R")

# Load the server logic
source("server.R")

# Launch the Shiny app
shinyApp(ui = ui, server = server)


# app.R
# Minimal launcher — sources ui.R and server.R then starts the app.
# Shiny automatically picks up ui.R and server.R in the same folder,
# but this file lets you also run shiny::runApp("inst/shiny") directly.

source("ui.R")
source("server.R")
shiny::shinyApp(ui = ui, server = server)

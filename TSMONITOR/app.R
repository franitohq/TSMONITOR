
# app.R
source("TSM_ui.R")     # Load the UI function
source("TSM_server.R") # Load the server function

# Run the app
shinyApp(ui = ui, server = server)


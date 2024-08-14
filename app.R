source("ui.R")
source("server.R")
source("testing.R")

# moved to ./R/data_prep.R
# ###################################################
# # Global variables
# ###################################################

#############################################################################
# Per RShiny Instructions, auto-load supplementary functions from ./R folder
# and HTML content from ./www foldera
# https://shiny.rstudio.com/articles/app-formats.html
#
# data_prep.R
# support_functions.R
#############################################################################




# Run the application 



app <- shinyApp(ui = app_ui, server = app_server)

runApp()
library(shiny)
library(bs4Dash)
library(shinyjs)
library(auth0)


source("ui/ui.R")
source("server/server.R")


options(shiny.port = 8080)
shinyApp(ui = ui, server = server)
  



library(shiny)

ui2 <- fluidPage(
  titlePanel("Hello World"),
  tags$style(HTML("
      .btn-wrapper {
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
      }
    ")),
  div(class = "btn-wrapper",
      actionButton("helloButton", "Hello World")
      )
)

server2 <- function(input, output) {
  #nothing
}
 

app2 <- shinyApp(ui = ui2, server = server2)

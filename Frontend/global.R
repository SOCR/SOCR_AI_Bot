a0_info <- auth0::auth0_info()

add_mathjax <- function() {
  tagList(
    tags$head(
      tags$script(
        "window.MathJax = {
          tex: {
            inlineMath: [['$', '$']],
            displayMath: [['$$', '$$']]
          },
          startup: {
            typeset: true
          }
        };"
      ),
      tags$script(src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"),
      # Add observer for streaming updates
      tags$script("
        $(document).on('shiny:value', function(event) {
          if (event.target.id === 'current_message' || event.target.id === 'previous_messages') {
            if (window.MathJax) {
              MathJax.typeset();
            }
          }
        });
      ")
    )
  )
}
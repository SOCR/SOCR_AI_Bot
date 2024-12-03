a0_info <- auth0::auth0_info()


# MathJax implementation for rendering equations
add_mathjax <- function() {
  tagList(
    # Configure MathJax
    tags$head(
      tags$script(HTML(
        "window.MathJax = {
          tex: {
            inlineMath: [['$', '$']],
            displayMath: [['$$', '$$']]
          },
          svg: {
            fontCache: 'global'
          },
          startup: {
            typeset: false  // Changed to false to prevent initial typeset
          }
        };"
      )),
      # Load MathJax
      tags$script(src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js"),  # Using SVG output
      # Observer for dynamic updates
      tags$script(HTML("
        $(document).ready(function() {
          // Function to render equations
          function renderMathJax() {
            if (typeof MathJax !== 'undefined') {
              MathJax.typesetPromise().catch(function(err) {
                console.log('MathJax error:', err);
              });
            }
          }

          // Listen for Shiny value changes
          $(document).on('shiny:value', function(event) {
            if (event.target.id === 'current_message' || event.target.id === 'previous_messages') {
              // Wait a brief moment for content to be fully inserted
              setTimeout(renderMathJax, 100);
            }
          });

          // Initial render
          renderMathJax();
        });
      "))
    )
  )
}

# Katex implementation for math rendering (faster than mathjax)
add_katex <- function() {
  tagList(
    tags$head(
      # KaTeX CSS
      tags$link(
        rel = "stylesheet",
        href = "https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.css",
        integrity = "sha384-n8MVd4RsNIU0tAv4ct0nTaAbDJwPJzDEaqSD1odI+WdtXRGWt2kTvGFasHpSy3SV",
        crossorigin = "anonymous"
      ),
      
      # KaTeX JS
      tags$script(
        src = "https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.js",
        integrity = "sha384-XjKyOOlGwcjNTAIQHIpgOno0Hl1YQqzUOEleOLALmuqehneUG+vnGctmUb0ZY0l8",
        crossorigin = "anonymous"
      ),
      
      # Auto-render extension
      tags$script(
        src = "https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/contrib/auto-render.min.js",
        integrity = "sha384-+VBxd3r6XgURycqtZ117nYw44OOcIax56Z4dCRWbxyPt0Koah1uHoK0o4+/RRE05",
        crossorigin = "anonymous"
      ),
      
      # Initialize auto-render
      tags$script(HTML("
        document.addEventListener('DOMContentLoaded', function() {
          function renderMath() {
            renderMathInElement(document.body, {
              delimiters: [
                {left: '$$', right: '$$', display: true},
                {left: '$', right: '$', display: false}
              ],
              throwOnError: false,
              strict: false
            });
          }
          
          // Initial render
          renderMath();
          
          // Listen for Shiny updates
          $(document).on('shiny:value', function(event) {
            if (event.target.id === 'current_message' || event.target.id === 'previous_messages') {
              setTimeout(renderMath, 100);
            }
          });
        });
      "))
    )
  )
}
source("R/data_prep.R")
source("R/palmAI.R")
source("R/support_functions.R")

library(shiny)
library(golem)
library(RCurl)
library(json64)
library(plotly)
library(biomartr)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(shinyBS)
library(reticulate)
library(httr)
library(jsonlite)


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd

golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )
  
  tags$head(
    favicon(
      ico = "icon",
      rel = "shortcut icon",
      resources_path = "www",
      ext = "png"
    ),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "AI Bot"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "AIBot")
}


#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE.
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' @param file Location of the config file
#'
#' @noRd
get_golem_config <- function(
    value,
    config = Sys.getenv(
      "GOLEM_CONFIG_ACTIVE",
      Sys.getenv(
        "R_CONFIG_ACTIVE",
        "default"
      )
    ),
    use_parent = TRUE,
    # Modify this if your config file is somewhere else
    file = app_sys("golem-config.yml")
) {
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}

demo_questions <- c(
  'Example questions:' = "Example questions:",
  'Lookup R package' = "List popular R packages for time-series forecast.",
  'Explain concepts' = "What is Moran's I in spatial statistics?",
  'Find statistic method' = "What statistics test should I use to examine the 
  correlation of two categorical variable?",
  'How to, situation' = "How do you do regression when there are
predictor variables that are highly correlated?",
  'How to, methods' = "How do you choose k in k-means clustering?",
  'How to, evaluation' = "How do you assess linear regression models?",
  'How to, outliers' = "How to deal with outliers?",
  'How to, interpretation' = "What does P value = 0.02 mean in ANOVA?",
  'Vague question, rejected' = "How does k-means clustering work?",
  'Vague question, w/ context' = "How does k-means clustering work in statistics?"
)

demo_model_choose <- c('Choose Model: (Default: GPT-4o-mini)',
                       'GPT-4o-mini',
                       'Gemini 1.0 Pro')

demo_synth_prompts <- c('Example Text Prompts:' = "Example themes:",
                        'Medical note' = "Draft a clinical note for an Alzheimer's disease patient",
                        'Social Essay' = "Expected health disparities and racial inequality in the US in 2030")

img_prompts <- c('Example Image Prompts:' = "Example image prompts:",
                 'Healthy Control' = "2D sagittal MRI brain image of Healthy Asymptomatic Control",
                 'Patients' = "2D sagittal MRI brain image of Alzheimer's disease")







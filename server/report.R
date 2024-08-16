
observeEvent(input$submit_button, {
  choices <- 1:length(logs$code_history)
  names(choices) <- paste0("Chunk #", choices)
  updateSelectInput(
    inputId = "selected_chunk_report",
    label = "Chunks to include (Use backspace to delete):",
    selected = "All chunks without errors",
    choices = c(
      "All chunks",
      "All chunks without errors",
      choices
    )
  )
  
})

# collect all RMarkdown chunks
Rmd_total <- reactive({
  
  Rmd_script <- ""
  
  # if first chunk
  Rmd_script <- paste0(
    Rmd_script,   # Get the data from the params list-----------
    
    "\nDeveloped by [SOCR Team](https://www.socr.umich.edu/people/) utilizing 
      many open [CRAN](https://cran.r-project.org/) science resources, including 
      [OpenAI API and Models](https://cran.rstudio.com/web/packages/openai/index.html).\n",
    
    "The default model used in the AI Bot is \"", language_model, "\". ",
    "However, other models can be chosen from the suite of [AI models](https://beta.openai.com/docs/models).\n",
    
    "\nAI Bot website: [https://socr.umich.edu/HTML5/](https://socr.umich.edu/HTML5/). \n ",
    "\nSource code: [GitHub](https://github.com/SOCR). \n "
  )
  
  # if the first chunk & data is uploaded,
  # insert script for reading data
  if (input$select_data == uploaded_data) {
    
    # Read file
    file_name <- input$user_file$name
    if(user_data()$file_type == "read_excel") {
      txt <- paste0(
        "# install.packages(readxl)\nlibrary(readxl)\ndf <- read_excel(\"",
        file_name,
        "\")"
      )
      
    }
    if (user_data()$file_type == "read.csv") {
      txt <- paste0(
        "df <- read.csv(\"",
        file_name,
        "\")"
      )
    }
    if (user_data()$file_type == "read.table") {
      txt <- paste0(
        "df <- read.table(\"",
        file_name,
        "\", sep = \"\t\", header = TRUE)"
      )
    }
    
    Rmd_script <- paste0(
      "\n### 0. Read File\n",
      "```{R, eval = FALSE}\n",
      txt,
      "\n```\n"
    )
  }
  
  #------------------Add selected chunks
  if("All chunks" %in% input$selected_chunk_report) {
    ix <- 1:length(logs$code_history)
  } else if("All chunks without errors" %in% input$selected_chunk_report) {
    ix <- c()
    for (i in 1:length(logs$code_history)) {
      if(!logs$code_history[[i]]$error) {
        ix <- c(ix, i)
      }
    }
  } else {  # selected
    ix <- as.integer(input$selected_chunk_report)
  }
  
  for (i in ix) {
    Rmd_script <- paste0(Rmd_script, "\n", logs$code_history[[i]]$rmd)
  }
  return(Rmd_script)
})



# Markdown chunk for the current request
Rmd_chunk <- reactive({
  req(openAI_response()$cmd)
  req(openAI_prompt())
  
  Rmd_script <- ""
  Rmd_script <- paste0(
    Rmd_script,
    # Get the data from the params list for every chunk-----------
    # Do not change this without changing the output$Rmd_source function
    # This chunk is removed for local knitting.
    "```{R, echo = FALSE}\n",
    "df <- params$df\n",
    "```\n"
  )
  
  # User request----------------------
  Rmd_script  <- paste0(
    Rmd_script,
    "\n### ",
    counter$requests,
    ". ",
    paste(
      #remove pre-inserted commands
      gsub(
        paste0(
          "\n|",
          pre_text,
          "|",
          after_text,
          ".*"
        ),
        "",
        openAI_prompt()
      ),
      collapse = " "
    ),
    paste(
      "\n Sampling temperature:",
      sample_temp()
    ),
    "\n"
  )
  
  # R Markdown code chunk----------------------
  # if there are errors when running the code, stop, do not run
  if (code_error() == TRUE) {
    Rmd_script <- paste0(
      Rmd_script,
      "```{R, eval = FALSE}"
    )
  } else {
    Rmd_script <- paste0(
      Rmd_script,
      "```{R}"
    )
  }
  
  cmd <- openAI_response()$cmd
  # remove empty line
  if(nchar(cmd[1]) == 0) {
    cmd <- cmd[-1]
  }
  
  if(input$continue) {
    cmd <- c(logs$last_code, "\n#-----------------", cmd)
  }
  
  # Add R code
  Rmd_script <- paste0(
    Rmd_script,
    paste(
      cmd,
      collapse = "\n"
    ),
    "\n```\n"
  )
  
  # indicate error
  if (code_error()) {
    Rmd_script <- paste0(
      Rmd_script,
      "** Error **  \n"
    )
  }
  
  return(Rmd_script)
})

output$html_report <- renderUI({
  req(openAI_response()$cmd)
  tagList(
    downloadButton(
      outputId = "report",
      label = "Report"
    ),
    tippy::tippy_this(
      "report",
      "Download a HTML report for this session.",
      theme = "light-border"
    )
  )
})

output$rmd_chunk_output <- renderText({
  req(Rmd_chunk())
  Rmd_total()
})

# Markdown report from DataExplorer package; does not work
#  output$eda_report <- downloadHandler(
# For PDF output, change this to "report.pdf"
#    filename = "DataExplorer_report.html",
#    content = function(file) {
#      DataExplorer::create_report(
#        iris,
#        output_file = file,
#        output_dir = file.path(file),
#        knit_root_dir = file.path(file)
#      )
#    }
#  )

# Markdown report
output$Rmd_source <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = "AIBot.Rmd",
  content = function(file) {
    Rmd_script <- paste0(
      "---\n",
      "title: \"SOCR AI Bot report\"\n",
      "author: \"SOCR AI Bot, powered by ChatGPT, OpenAI, RTutor, and CRAN\"\n",
      "date: \"",
      date(), "\"\n",
      "output: html_document\n",
      "---\n",
      # this chunk is not needed when they download the Rmd and knit locally
      gsub(
        "```\\{R, echo = FALSE\\}\ndf <- params\\$df\n```\n",
        "", 
        Rmd_total()
      )
    )
    writeLines(Rmd_script, file)
  }
)

# Markdown report
output$report <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = "AIBot_report.html",
  content = function(file) {
    withProgress(message = "Generating Report ...", {
      incProgress(0.2)
      
      tempReport <- file.path(tempdir(), "report.Rmd")
      # tempReport
      tempReport <- gsub("\\", "/", tempReport, fixed = TRUE)
      
      req(openAI_response()$cmd)
      req(openAI_prompt())
      
      #RMarkdown file's Header
      Rmd_script <- paste0(
        "---\n",
        "title: \"SOCR AI Bot report\"\n",
        "author: \"AI Bot v.",  release,
        ", powered by ChatGPT, OpenAI, RTutor, and CRAN\"\n",
        "date: \"",
        date(), "\"\n",
        "output: html_document\n",
        "params:\n",
        "  df:\n",
        "printcode:\n",
        "  label: \"Display Code\"\n",
        "  value: TRUE\n",
        "  input: checkbox\n",
        "---\n"
      )
      
      Rmd_script <- paste0(
        Rmd_script,
        "\n\n### "
      )
      
      # R Markdown code chunk----------------------
      
      # Add R code
      Rmd_script <- paste(
        Rmd_script,
        Rmd_total()
      )
      
      write(
        Rmd_script,
        file = tempReport,
        append = FALSE
      )
      
      # Set up parameters to pass to Rmd document
      params <- list(df = iris) # dummy
      
      # if uploaded, use that data
      req(input$select_data)
      if (input$select_data != no_data) {
        params <- list(
          df = current_data()
        )
      }
      
      req(params)
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(
        input = tempReport, # markdown_location,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    })
  }
)


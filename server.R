source("global.R")
source("testing.R")

############################## APP ###################################################################
# Define server logic required to draw a histogram
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 

use_condaenv(condaenv = "pytorch_env", required = TRUE)
py_libs <- import("rag_bot_inference_engine")

app_server <- function(input, output, session) {
  #____________________________________________________________________________
  # 1.  General UI, observers, etc.
  #____________________________________________________________________________
  # increase max input file size
  options(shiny.maxRequestSize = 10 * 1024^2) # 10MB
  
  pdf(NULL) #otherwise, base R plots sometimes do not show.
  
  # load demo data when clicked
  observeEvent(input$demo_prompt, {
    req(input$select_data)
    if (input$demo_prompt != demos_mpg[1]) {
      updateTextInput(
        session,
        "input_text",
        value = input$demo_prompt
      )
    } else { # if not mpg data, reset
      updateTextInput(
        session,
        "input_text",
        value = "",
        placeholder =
          "Upload a file or use demo data. Then just ask questions or request analyses in plain English. For general questions, briefly explain the data first, especially the relevant columns. See examples above. If unsuccessful, try again with the same request or ask differently. Code works correctly some of the time. To use voice input, click Settings."
      )
    }
  })
  
  observeEvent(input$user_file, {
    updateSelectInput(
      session,
      "select_data",
      selected = uploaded_data
    )
  }, ignoreInit = TRUE, once = TRUE)
  
  
  observe({
    shinyjs::hideElement(id = "load_message")
  })
  
  #____________________________________________________________________________
  # 2. Voice narration
  #____________________________________________________________________________
  # had to use this. Otherwise, the checkbox returns to false
  # when the popup is closed and opened again.
  use_voice <- reactive({
    use_voice <- FALSE #default
    tem <- is.null(input$use_voice_button)
    if(!is.null(input$use_voice)) {
      use_voice <- input$use_voice
    }
    return(use_voice)
  })
  
  # Use voice input?
  # output$use_heyshiny <- renderUI({
  #   req(use_voice())
  #   tagList(
  #     heyshiny::useHeyshiny(language = "en-US"), # configure the heyshiny
  #     heyshiny::speechInput(
  #       inputId = "hey_cmd",
  #       command = "hey cox *msg"  # hey cox is more sensitive than 'hi bot'
  #     ), # set the input
  #   )
  # })
  
  # read the speech input
  observeEvent(input$hey_cmd, {
    speech <- input$hey_cmd
    # message(speech)
    showNotification(speech)
    
    if (input$tabs == "Home")    {
      if (grepl("^continue", speech)) {
        
        speech <- paste0(
          input$input_text, # current prompt
          ". ",  # add . and space.
          gsub("^continue", "", speech) # remove the continue
        )
      }
      
      updateTextInput(
        session,
        "input_text",
        value = speech
      )
    } else if (input$tabs == "Ask") {
      
      speech <- paste0(
        input$input_text, # current prompt
        ". ",  # add . and space.
        gsub("^continue", "", speech) # remove the continue
      )
      updateTextInput(
        session,
        "ask_question",
        value = speech
      )
      
    }
    
  })
  
  #____________________________________________________________________________
  #  3. Loading data
  #____________________________________________________________________________
  
  # uploaded data
  user_data <- reactive({
    req(input$user_file)
    in_file <- input$user_file
    in_file <- in_file$datapath
    req(!is.null(in_file))
    
    isolate({
      file_type <- "read_excel"
      # Excel file ---------------
      if(grepl("xls$|xlsx$", in_file, ignore.case = TRUE)) {
        df <- readxl::read_excel(in_file)
        df <- as.data.frame(df)
      } else {
        #CSV --------------------
        df <- read.csv(in_file)
        file_type <- "read.csv"
        # Tab-delimited file ----------
        if (ncol(df) == 2) {
          df <- read.table(
            in_file,
            sep = "\t",
            header = TRUE
          )
          file_type <- "read.table"
        }
      }
      # clean column names
      df <- df %>% janitor::clean_names()
      return(
        list(
          df = df,
          file_type = file_type
        )
      )
    })
  })
  
  # showing the current dataset. Warning if no data is uploaded.
  output$selected_dataset <- renderText({
    req(input$submit_button)
    # when submit is clicked, but no data is uploaded.
    
    if(input$select_data == uploaded_data) {
      if(is.null(input$user_file)) {
        txt <- "No file uploaded! Please Reset and upload your data first."
      } else {
        txt <- "Dataset: uploaded."
      }
    } else {
      txt <- paste0("Dataset: ", input$select_data)
    }
    
    return(txt)
  })
  
  output$data_upload_ui <- renderUI({
    
    # Hide this input box after the first run.
    req(input$submit_button == 0)
    
    fileInput(
      inputId = "user_file",
      label = "File Upload",
      accept = c(
        "text/csv",
        "text/comma-separated-values",
        "text/tab-separated-values",
        "text/plain",
        ".csv",
        ".tsv",
        ".txt",
        ".xls",
        ".xlsx"
      )
    )
  })
  
  output$demo_data_ui <- renderUI({
    
    # Hide this input box after the first run.
    req(input$submit_button == 0)
    
    selectInput(
      inputId = "select_data",
      label = "Data",
      choices = datasets,
      selected = "attitude",
      multiple = FALSE,
      selectize = FALSE
    )
    
  })
  
  output$prompt_ui <- renderUI({
    req(input$select_data)
    # hide after data is uploaded
    req(is.null(input$user_file))
    
    if (input$select_data == "mpg") {
      selectInput(
        inputId = "demo_prompt",
        choices = demos_mpg,
        label = "Example requests:"
      )
    } else if (input$select_data == no_data) {
      selectInput(
        inputId = "demo_prompt",
        choices = demos_no_data,
        label = "Example requests:"
      )
    } else if (input$select_data == "diamonds") {
      selectInput(
        inputId = "demo_prompt",
        choices = demos_diamond,
        label = "Example requests:"
      )
    } else if (input$select_data == rna_seq) {
      selectInput(
        inputId = "demo_prompt",
        choices = demos_rna_seq,
        label = "Example requests:"
      )
    }
    else {
      return(NULL)
    }
  })
  
  #____________________________________________________________________________
  # 4. API key management
  #____________________________________________________________________________
  # pop up modal for Settings
  observeEvent(input$api_button, {
    shiny::showModal(
      shiny::modalDialog(
        size = "l", easyClose=TRUE,
        footer = modalButton("Confirm"),
        tagList(
          fluidRow(
            column(
              width = 4,
              sliderInput(
                inputId = "temperature",
                label = "Sampling temperature",
                min = 0,
                max = 1,
                value = sample_temp(),
                step = .1,
                round = FALSE,
                width = "100%"
              )
            ),
            column(
              width = 8,
              p("This important parameter controls the AI's behavior in choosing 
              among possible answers. A higher sampling temperature tells the AI 
              to take more risks, producing more diverse and creative 
              solutions when the same request is repeated. A lower  temperature
              (such as 0) results in more conservative and well-defined solutions, 
              but less variety when repeated.
              "),
            )
          ),
          hr(),
          h4("Use your own API key"),
          h5("We pay a small fee to use the AI for every request.
            If you use this AI Bot regularly, 
            please take a few minutes to create your own API key: "),
          
          tags$ul(
            tags$li(
              "Create a personal account at",
              a(
                "OpenAI.",
                href = "https://openai.com/api/",
                target = "_blank"
              )
            ),
            tags$li("After logging in, click \"Personal\" from top right."),
            tags$li(
              "Click \"Manage Account\" and then \"Billing\",
                where you can add \"Payment methods\" and set \"Usage 
                limits\". $5 per month is more than enough."
            ),
            tags$li(
              "Click \"API keys\" to create a new key, 
                which can be copied and pasted below."
            ),
          ),
          textInput(
            inputId = "api_key_openAI",
            label = h5("Paste your API key from OpenAI:"),
            value = NULL,
            placeholder = "sk-..... (51 characters)"
          ),
          textInput(
            inputId = "api_key_gemini",
            label = h5("Paste your API key from Google Gemini:"),
            value = NULL,
            placeholder = "..... (39 characters)"
          ),
          uiOutput("valid_key"),
          uiOutput("save_api_ui"),
          verbatimTextOutput("session_api_source_openAI"),
          verbatimTextOutput("session_api_source_gemini"),
          hr(),
          
          fluidRow(
            column(
              width = 6,
              checkboxInput(
                inputId = "use_voice",
                label = strong("Enable voice narration"),
                value = use_voice()
              )
            ),
            column(
              width = 6,
              # this causes the use_voice() to refresh twice,
              # triggering the permission seeking in Chrome.
              # Don't know why, but this works. I'm a stable genius.
              actionButton("use_voice_button", strong("Seek mic permission"))
            )
          ),
          h5("First select the checkbox and then seek 
          permission to use the microphone. Your browser should have a popup 
          window. Otherwise, check the both ends of the URL bar for a 
          blocked icon, which
          could be clicked to grant permission. If successful, you will see 
          a red dot on top of the tab in Chrome.
          Voice narration can be used in both the Main and the 
          Ask Me Anything tabs by just saying \"Hey Cox ...\" 
          in honor of the statistician David Cox.     
          If not satisfied, try again to overwrite. 
          To continue, say \"Hey Cox Continue ...\""),
        ),
        hr(),
        fluidRow(
          column(
            width = 4,
            checkboxInput(
              inputId = "numeric_as_factor",
              label = strong("Treat as factors"),
              value = convert_to_factor()
            ),
            tippy::tippy_this(
              elementId = "numeric_as_factor",
              tooltip = "Treat the columns that looks like a category 
              as a category. This applies to columns that contain numbers
              but have very few unique values. ",
              theme = "light-border"
            )
          ),
          column(
            width = 4,
            numericInput(
              inputId = "max_levels_factor",
              label = "Max levels",
              value = max_levels_factor(),
              min = 5,
              max = 50,
              step = 1
            ),
            tippy::tippy_this(
              elementId = "max_levels_factor",
              tooltip = "To convert a numeric column as category, 
              the column must have no more than this number of unique values.",
              theme = "light-border"
            )
          ),
          column(
            width = 4,
            numericInput(
              inputId = "max_proptortion_factor",
              label = "Max proportion",
              value = max_proptortion_factor(),
              min = 0.05,
              max = 0.5,
              step = 0.1
            ),
            tippy::tippy_this(
              elementId = "max_proptortion_factor",
              tooltip = "To convert a numeric column as category, 
              the number of unique values in a column must not exceed 
              more this proportion of the total number of rows.",
              theme = "light-border"
            )
          )
        ),
        h5("Some columns contains numbers but should be treated 
        as categorical values or factors. For example, we sometimes 
        use 1 to label success and 0 for failure.
        If this is selected, using the default setting, a column 
        is treated as categories when the number of unique values 
        is less than or equal to 12, and less than 10% of the total rows."
        ),
        hr(),
        fluidRow(
          column(
            width = 4,
            checkboxInput(
              inputId = "contribute_data",
              label = "Help us enhance AI Bot",
              value = contribute_data()
            )
          ),
          column(
            width = 8,
            h5("Save your requests and the structure of your data 
            such as column names and data types, not the data itself. 
            We can learn from users about creative ways to use AI. 
            And we can try to improve unsuccessful attempts. ")
          )
        ),
        
      )
    )
  })
  # api key for the session
  api_key_session <- reactive({
    
    api_key_openAI <- api_key_global_openAI
    api_key_gemini <- api_key_global_gemini
    
    session_key_source <- key_source
    
    if(!is.null(input$api_key_openAI)) {
      key1 <- input$api_key_openAI
      key1 <- clean_api_key(key1)
      if (validate_api_key_openAI(key1)) {
        api_key_openAI <- key1
        session_key_source <- "pasted!"
      }
    }
    
    if(!is.null(input$api_key_gemini)){
      key2 <- input$api_key_gemini
      if(validate_api_key_gemini(key2)){
        api_key_gemini <- key2
        session_key_source <- "pasted!"
      }
    }
    
    return(
      list(
        api_key_openAI = api_key_openAI,
        api_key_gemini = api_key_gemini,
        key_source = session_key_source
      )
    )
  })
  
  output$session_api_source_openAI <- renderText({
    txt <- api_key_session()$api_key_openAI
    
    # The following is essential for correctly getting the 
    # environment variable on Linux
    tem <- Sys.getenv("OPEN_API_KEY")
    paste0(
      "Current OpenAI API key: ",
      substr(txt, 1, 4),
      ".....",
      substr(txt, nchar(txt) - 4, nchar(txt)),
      " (",
      api_key_session()$key_source,
      ")"
    )
  })
  
  output$session_api_source_gemini <- renderText({
    txt <- api_key_session()$api_key_gemini
    
    paste0(
      "Current Gemini API key: ",
      substr(txt, 1, 4),
      ".....",
      substr(txt, nchar(txt) - 4, nchar(txt)),
      " (",
      api_key_session()$key_source,
      ")"
    )
  })
  
  
  output$save_api_ui <- renderUI({
    req(input$api_key_openAI)
    
    # only show this when running locally.
    req(!file.exists(on_server))
    req(validate_api_key_openAI(input$api_key_openAI))
    
    tagList(
      actionButton(
        inputId = "save_api_button",
        label = "Save key file for next time."
      ),
      tippy::tippy_this(
        elementId = "save_api_button",
        tooltip = "Save to a local file, 
        so that you do not have to copy and paste next time.",
        theme = "light-border"
      )
    )
  })
  
  output$valid_key <- renderUI({
    req(input$api_key_openAI)
    
    if(validate_api_key_openAI(input$api_key_openAI)) {
      h4(
        "Key looks good. Just close this window.",
        style = "color:blue"
      )
    } else {
      h4(
        "That does not look like a valid key!",
        style = "color:red"
      )
    }
  })
  
  # only save the key, if the app is running locally.
  observeEvent(input$save_api_button, {
    req(input$save_api_button)
    req(input$api_key_openAI)
    writeLines(input$api_key_openAI, "api_key_openAI.txt")
    writeLines(input$api_key_gemini, "api_key_gemini.txt")
  })
  
  # only save the key, if the app is running locally.
  observeEvent(input$submit_button, {
    # if too short, do not send.
    if (nchar(input$input_text) < min_query_length) {
      showNotification(
        paste(
          "Request is too short! Should be more than ", 
          min_query_length, 
          " characters."
        ),
        duration = 10
      )
    }
    # if too short, do not send. 
    if (nchar(input$input_text) > max_query_length) {
      showNotification(
        paste(
          "Request is too long! Should be less than ", 
          max_query_length, 
          " characters."
        ),
        duration = 10
      )
    }
  })
  
  
  #____________________________________________________________________________
  # 5. Send API Request, handle API errors
  #____________________________________________________________________________
  
  sample_temp <- reactive({
    temperature <- default_temperature #default
    if (!is.null(input$temperature)) {
      temperature <- input$temperature
    }
    return(temperature)
  })
  
  openAI_prompt <- reactive({
    req(input$submit_button)
    req(input$select_data)
    prep_input(input$input_text, input$select_data, current_data())
  })
  
  openAI_response <- reactive({
    req(input$submit_button)
    
    isolate({  # so that it will not respond to text, until submitted
      req(input$input_text)
      prepared_request <- openAI_prompt()
      req(prepared_request)
      
      # when submit is clicked, but no data is uploaded.
      if(input$select_data == uploaded_data) {
        req(user_data())
      }
      
      shinybusy::show_modal_spinner(
        spin = "semipolar",
        text = paste0("... Please wait, processing ...\n",
                      prepared_request # sample(jokes, 1)
        ),
        color = "#000000"
      )
      
      start_time <- Sys.time()
      
      
      #START HERE#
      
      #print(prepared_request)
      #print(api_key_session()$api_key_openAI)
      
      
      chat <- function(input_message){
        user_input <- list(list(role = "user", content = input_message))
        base_url <- "https://api.openai.com/v1/chat/completions"
        api_key_openAI <- api_key_session()$api_key_openAI
        body <- list(model = language_model,
                     messages = user_input,
                     max_tokens = 200,
                     temperature = sample_temp())
        req <- request(base_url)
        resp <-
          req |>
          req_auth_bearer_token(token = api_key_openAI) |>
          req_headers("Content-Type" = "application/json") |>
          req_body_json(body) |>
          req_retry(max_tries = 4) |>
          req_throttle(rate = 15) |>
          req_perform()
        
        openai_chat_response <- resp |> resp_body_json(simplifyVector = TRUE)
        #print(openai_chat_response) #shows more details of the result
        #print(openai_chat_response$choices$message$content)
        return(openai_chat_response)
      }
      
      #response <- chat(prepared_request)
      #print(response)
      
      
      #print(response$choices$message$content) #actual result
      
      
      # Send to openAI
      tryCatch(
        response <- chat(prepared_request),
        error = function(e) {
          # remove spinner, show message for 5s, & reload
          shinybusy::remove_modal_spinner()
          shiny::showModal(api_error_modal)
          Sys.sleep(8)
          session$reload()
          
          list(
            error_value = -1,
            message = capture.output(print(e$message)),
            error_status = TRUE
          )
        }
        
      )
      
      error_api <- FALSE
      # if error returns true, otherwise 
      #  that slot does not exist, returning false.
      # or be NULL
      error_api <- tryCatch(
        !is.null(response$error_status),
        error = function(e) {
          return(TRUE)
        }
      )
      
      error_message <- NULL
      if(error_api) {
        cmd <- NULL
        response <- NULL
        error_message <- response$message
      } else {
        cmd <- response$choices$message$content
      }
      
      api_time <- difftime(
        Sys.time(),
        start_time,
        units = "secs"
      )[[1]]
      
      if(0) {
        # if more than 10 requests, slow down. Only on the server.
        if(counter$requests > 20 && file.exists(on_server)) {
          Sys.sleep(counter$requests / 5 + runif(1, 0, 5))
        }
        if(counter$requests > 50 && file.exists(on_server)) {
          Sys.sleep(counter$requests / 10 + runif(1, 0, 10))
        }
      }
      
      if(counter$requests > 100 && file.exists(on_server)) {
        Sys.sleep(counter$requests / 40 + runif(1, 0, 40))
      }
      
      shinybusy::remove_modal_spinner()
      
      # update usage via global reactive value
      counter$tokens <- counter$tokens + response$usage$completion_tokens
      counter$requests <- counter$requests + 1
      counter$time <- round(api_time, 0)
      counter$tokens_current <- response$usage$completion_tokens
      
      return(
        list(
          cmd = cmd,
          response = response,
          time = round(api_time, 0),
          error = error_api,
          error_message = error_message
        )
      )
    })
  })
  
  # a modal shows api connection error
  api_error_modal <- shiny::modalDialog(
    title = "API connection error!",
    # tags$h4(response$message, style = "color:red"),
    tags$h4("Is the API key is correct?", style = "color:red"),
    tags$h4("How about the WiFi?", style = "color:red"),
    tags$h4("Maybe the openAI.com website is taking forever to respond.", style = "color:red"),    
    tags$h5("If you keep having trouble, send us an email.", style = "color:red"),
    tags$h4(
      "Auto-reset ...", 
      style = "color:blue; text-align:right"
    ),
    easyClose = TRUE,
    size = "s"
  )
  
  
  # show a warning message when reached 10c, 20c, 30c ...
  observeEvent(input$submit_button, {
    req(file.exists(on_server))
    req(!openAI_response()$error)
    
    cost_session <-  round(counter$tokens * 2e-3, 0)
    if (cost_session %% 20  == 0 & cost_session != 0) {
      shiny::showModal(
        shiny::modalDialog(
          size = "s",
          easyClose	= TRUE,
          h4(
            paste0(
              "Cumulative API Cost reached ",
              cost_session,
              "¢"
            )
          ),
          h4("Slow down. Please try to use your own API key.")
        )
      )
    }
  })
  
  output$openAI <- renderText({
    req(openAI_response()$cmd)
    res <- logs$raw
    # Replace multiple newlines with just one.
    #res <- gsub("\n+", "\n", res)
    # Replace empty lines,  [ ]{0, }--> zero or more space
    #res <- gsub("^[ ]{0, }\n", "", res)
    res <- gsub("```", "", res)
    
  })
  
  # Defining & initializing the reactiveValues object
  logs <- reactiveValues(
    id = 0, # 1, 2, 3, id for code chunk
    code = "", # cumulative code
    raw = "",  # cumulative original code for print out
    last_code = "", # last code for Rmarkdown
    code_history = list() # keep all code chunks
  )
  
  observeEvent(input$submit_button, {
    logs$id <- logs$id + 1
    # if not continue
    if(!input$continue) {
      logs$code <- openAI_response()$cmd
      
      logs$raw <- openAI_response()$response$choices$message$content
      # remove one or more blank lines in the beginning.
      logs$raw <- gsub("^\n+", "", logs$raw)
      
      logs$last_code <- ""
      
    } else { # if continue
      logs$last_code <- logs$code  # last code
      logs$code <- paste(
        logs$code,
        "\n",
        "#-------------------------------",
        openAI_response()$cmd
      )
      logs$raw <- paste(
        logs$raw,
        "\n\n#-------------------------\n",
        gsub("^\n+", "", openAI_response()$response$choices[1, 1])
      )
    }
    
    # A list holds current request
    current_code <- list(
      id = logs$id,
      code = logs$code,
      raw = logs$raw, # for print
      prompt = input$input_text,
      error = code_error(),
      rmd = Rmd_chunk()
    )
    
    logs$code_history <- append(logs$code_history, list(current_code))
    
    choices <- 1:length(logs$code_history)
    names(choices) <- paste0("Chunk #", choices)
    # update chunk choices
    updateSelectInput(
      inputId = "selected_chunk",
      label = "AI generated code:",
      choices = choices,
      selected = logs$id
    )
    
    # turn off continue button
    updateCheckboxInput(
      session = session,
      inputId = "continue",
      label = "Continue from this chunk",
      value = FALSE
    )
  })
  
  # change code when past code is selected.
  observeEvent(input$selected_chunk, {
    #req(run_result())
    
    id <- as.integer(input$selected_chunk)
    logs$code <- logs$code_history[[id]]$code
    logs$raw <- logs$code_history[[id]]$raw
    updateTextInput(
      session,
      "input_text",
      value = logs$code_history[[id]]$prompt
    )
    
    
  })
  
  output$usage <- renderText({
    req(input$submit_button != 0 || input$ask_button != 0)
    
    paste0(
      "R",
      counter$requests, ":  ",
      counter$tokens_current,
      " tokens, ",
      counter$time,
      " second(s)"
    )
  })
  
  output$total_cost <- renderText({
    if(input$submit_button == 0 & input$ask_button == 0) {
      return("The generative-AI Bot uses OpenAI, which charges 2¢ per 1000 tokens/words 
      from user's accounts. Non-UMich users 
      should use your own accounts by entering their KEYs in the \'Settings\'.")
    } else {
      #req(openAI_response()$cmd)
      paste0(
        "Cumulative API Cost: ",
        sprintf("%5.1f", counter$tokens * 2e-3),
        "¢"
      )
    }
  })
  
  output$temperature <- renderText({
    req(openAI_response()$cmd)
    
    paste0(
      "Temperature: ",
      sample_temp()
    )
  })
  
  output$retry_on_error <- renderText({
    req(code_error())
    if(code_error()) {
      "Error! Try again or change the request."
    }
    
  })
  # Defining & initializing the reactiveValues object
  counter <- reactiveValues(
    tokens = 0, # cumulative tokens
    requests = 0, # cumulative requests    
    tokens_current = 0,  # tokens for current query
    time = 0 # response time for current
  )
  
  
  #____________________________________________________________________________
  # 6. Run the code, shows plots, code, and errors
  #____________________________________________________________________________
  
  # stores the results after running the generated code.
  # return error indicator and message
  
  # Note that the code is run three times!!!!!
  
  # Sometimes returns NULL, even when code runs fine. Especially when
  # a base R plot is generated.
  run_result <- reactive({
    req(logs$code)
    req(input$submit_button != 0)
    withProgress(message = "Running the code ...", {
      incProgress(0.4)
      tryCatch(
        eval(
          parse(
            text = clean_cmd(
              logs$code,
              input$select_data
            )
          )
        ),
        error = function(e) {
          list(
            error_value = -1,
            message = capture.output(print(e$message)),
            error_status = TRUE
          )
        }
      )
    })
  })
  
  # Error when run the generated code?
  code_error <- reactive({
    error_status <- FALSE
    req(input$submit_button != 0)
    
    # if error returns true, otherwise 
    #  that slot does not exist, returning false.
    # or be NULL
    try(  # if you do not 'try', the entire app quits! :-)
      if (is.list(run_result())) {
        req(!is.null(names(run_result())[1]))
        if (names(run_result())[1] == "error_value") {
          error_status <- TRUE
        }
      }
    )
    return(error_status)
  })
  
  
  output$error_message <- renderUI({
    req(!is.null(code_error()))
    if(code_error()) {
      h4(paste("Error!", run_result()$message), style = "color:red")
    } else {
      return(NULL)
    }
    
  })
  
  # just capture the screen output
  output$console_output <- renderText({
    req(!code_error())
    req(logs$code)
    out <- ""
    withProgress(message = "Running the code for console...", {
      incProgress(0.4)
      try(
        out <- capture.output(eval(
          parse(
            text = clean_cmd(logs$code, input$select_data)
          )
        )
        )
      )
      
      # This works most of the time, but not when cat is used.
      #out <- capture.output(
      #    run_result()
      #)
      paste(out, collapse = "\n")
    })
  })
  
  # base R plots can not be auto generated from the run_results() object
  # must run the code again.
  output$result_plot <- renderPlot({
    req(!code_error())
    req(logs$code)
    withProgress(message = "Generating a plot ...", {
      incProgress(0.4)
      try(
        eval(
          parse(
            text =  clean_cmd(logs$code, input$select_data)
          )
        )
      )
    })
  })
  
  output$result_plotly <- plotly::renderPlotly({
    req(!code_error())
    req(
      is_interactive_plot() ||   # natively interactive
        turned_on(input$make_ggplot_interactive)
    )
    
    g <- run_result()
    # still errors some times, when the returned list is not a plot
    if(is.character(g) || is.data.frame(g) || is.numeric(g)) {
      return(NULL)
    } else {
      return(g)
    }
    
  })
  
  output$plot_ui <- renderUI({
    req(input$submit_button)
    
    if (code_error() || input$submit_button == 0) {
      return()
    } else if (
      is_interactive_plot() ||   # natively interactive
      turned_on(input$make_ggplot_interactive) # converted
    ){
      plotly::plotlyOutput("result_plotly")
    } else {
      plotOutput("result_plot")
    }
  })
  
  # reset to FALSE after each submission
  observeEvent(input$submit_button, {
    updateCheckboxInput(
      session = session,
      inputId = "make_ggplot_interactive",
      label = "Make it interactive!",
      value = FALSE
    )
  })
  
  observeEvent(input$submit_button, {
    # hide it by default
    shinyjs::hideElement(id = "make_ggplot_interactive")
    req(!code_error())
    req(logs$code)
    txt <- paste(openAI_response()$cmd, collapse = " ")
    
    if (grepl("ggplot", txt) && # if  ggplot2, and it is 
        !is_interactive_plot() && #not already an interactive plot, show
        # if there are too many data points, don't do the interactive
        !(dim(current_data())[1] > max_data_points && grepl("geom_point|geom_jitter", txt))
    ) {
      shinyjs::showElement(id = "make_ggplot_interactive")
    }
  })
  
  is_interactive_plot <- reactive({
    # only true if the plot is interactive, natively.
    req(input$submit_button)
    req(logs$code)
    req(!code_error())
    if (
      grepl(
        "plotly|plot_ly|ggplotly",
        paste(logs$code, collapse = " ")
      )
    ) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  
  output$tips_interactive <- renderUI({
    req(input$submit_button)
    req(openAI_response()$cmd)
    if(is_interactive_plot() ||   # natively interactive
       turned_on (input$make_ggplot_interactive)
    ) {
      tagList(
        p("Interactive plot. Mouse over to see values. Select a region to zoom. 
        Click on the legends to deselect a group. 
        Double click a category to hide all others. 
        Use the menu on the top right for other functions."
        )
      )
    }
  })
  
  rna_seq_data <- reactive({
    req(input$select_data == rna_seq)
    
    df <- read.csv(app_sys("app", "www", "GSE37704.csv"))
    return(df)
  })
  
  
  # had to use this. Otherwise, the checkbox returns to false
  # when the popup is closed and opened again.
  convert_to_factor <- reactive({
    convert <- TRUE #default
    if (!is.null(input$numeric_as_factor)) {
      convert <- input$numeric_as_factor
    }
    return(convert)
  })
  
  max_proptortion_factor <- reactive({
    max_proptortion <- TRUE #default
    if(!is.null(input$max_proptortion_factor)) {
      max_proptortion <- input$max_proptortion_factor
    }
    if(max_proptortion < 0.05) {
      max_proptortion <- 0.05
    }
    if(max_proptortion > 0.5) {
      max_proptortion <- 0.5
    }
    return(max_proptortion)
  })
  
  
  max_levels_factor <- reactive({
    max_levels_1 <- max_levels #default
    if (!is.null(input$max_levels_factor)) {
      max_levels_1 <- input$max_levels_factor
    }
    if (max_levels_1 < 2) {
      max_levels_1 <- 2
    }
    if (max_levels_1 > 100) {
      max_levels_1 <- 100
    }
    return(max_levels_1)
  })
  
  # The current data, just for showing.
  current_data <- reactive({
    req(input$select_data)
    
    if(input$select_data == uploaded_data) {
      eval(parse(text = paste0("df <- user_data()$df")))
    } else if(input$select_data == no_data){
      df <- NULL #as.data.frame("No data selected or uploaded.")
    } else if(input$select_data == rna_seq){
      df <- rna_seq_data()
    } else {
      # otherwise built-in data is unavailable when running from the R package.
      library(conflicted)  
      library(tidyverse)
      conflict_prefer("complete", "RCurl")
      conflict_prefer("filter", "plotly")
      conflict_prefer("layout", "plotly")
      conflict_prefer("lag", "stats")
      conflict_prefer("flatten", "jsonlite")
      conflict_prefer("filter", "stats")
      conflict_prefer("validate", "shiny")
      #library(tidyverse)
      eval(parse(text = paste0("df <- ", input$select_data)))
    }
    
    df <- convert_ts_to_df(df)
    
    if (convert_to_factor()) {
      df <- numeric_to_factor(
        df,
        max_levels_factor(),
        max_proptortion_factor()
      )
    }
    
    # if the first column looks like id?
    if(
      length(unique(df[, 1])) == nrow(df) &&  # all unique
      is.character(df[, 1])  # first column is character
    ) {
      row.names(df) <- df[, 1]
      df <- df[, -1]
    }
    
    # sometimes no row is left after processing.
    if(is.null(df)) { # no_data
      return(NULL)
    } else if(nrow(df) == 0) {
      return(NULL)
    } else { # there are data in the dataframe
      #print(df)
      return(df)
    }
  })
  
  
  # The data, after running the chunk
  data_afterwards <- reactive({
    req(input$select_data)
    req(current_data())
    
    if (input$submit_button == 0) {
      return(current_data())
    }
    
    df <- current_data()
    # This updates the data by running the entire code one more time.
    if(input$submit_button != 0) {
      if (code_error() == FALSE && !is.null(logs$code)) {
        withProgress(message = "Updating values ...", {
          incProgress(0.4)
          try(
            eval(
              parse(
                text = clean_cmd(logs$code, input$select_data)
              )
            ),
          )
        })
      }
    }
    
    # sometimes no row is left after processing.
    if(is.null(df)) { # no_data
      return(NULL)
    } else if(nrow(df) == 0) {
      return(NULL)
    } else { # there are data in the dataframe
      return(df)
    }
  })
  
  
  output$data_table_DT <- DT::renderDataTable({
    req(data_afterwards())
    DT::datatable(
      data_afterwards(),
      options = list(
        lengthMenu = c(5, 20, 50, 100),
        pageLength = 1000,
        dom = 'ftp',
        scrollX = "400px"
      ),
      rownames = TRUE
    )
  })
  
  output$data_table <- renderTable({
    req(data_afterwards())
    
    data_afterwards()[
      1:min(20, nrow(data_afterwards())),
    ]
  })
  
  output$data_size <- renderText({
    req(!is.null(data_afterwards()))
    paste("Current data tensor (", input$select_data, ") dimensions: ",
          dim(data_afterwards())[1], "rows X ",
          dim(data_afterwards())[2], "columns"
    )
  })
  output$data_structure <- renderPrint({
    req(!is.null(data_afterwards()))
    str(data_afterwards())
  })
  
  output$data_summary <- renderText({
    req(!is.null(data_afterwards()))
    paste(
      capture.output(
        summary(data_afterwards())
      ),
      collapse = "\n"
    )
  })
  
  
  
  #____________________________________________________________________________
  # 7. Logs and Reports
  #____________________________________________________________________________
  
  source("server/report.R", local = TRUE)
  
  #______________________________________________________________________________
  #  8. Server rebooting every 24 hours; this gives a warning
  #______________________________________________________________________________
  
  # returns hour and minutes
  time_var <- reactive({
    input$submit_button
    min <- format(Sys.time(), "%M")
    hr <- format(Sys.time(), "%H")
    return(list(
      min = as.integer(min),
      hr = as.integer(hr)
    ))
  })
  
  
  output$timer_ui <- renderUI({
    if (
      time_var()$min >= 56 &&
      time_var()$hr %% 24 == 23 &&  # time_var()$hr %% 8 == 7 &&
      file.exists(on_server)
    ) {
      h4(
        paste(
          "Server rebooting in a few minutes. ",
          " Download your files. Reload this site after being 
          disconnected at the top of the hour."
        ),
        style = "color:red"
      )
      
    }
  })
  
  
  #______________________________________________________________________________
  # 9. ASK (Q and A)
  #______________________________________________________________________________
  
  source("server/ask.R", local = TRUE)
  
  #______________________________________________________________________________
  # 10. Exploratory Data Analysis (EDA)
  #______________________________________________________________________________
  
  source("server/EDA.R", local = TRUE)
  
  #______________________________________________________________________________
  # 11. Synthetic Text Generation
  #______________________________________________________________________________
  
  source("server/syth_text.R", local = TRUE)
  
  #______________________________________________________________________________
  # 12. Synthetic 2D Image Generation
  #______________________________________________________________________________
  
  source("server/syth_image.R", local = TRUE)
  
  #______________________________________________________________________________
  # 12. Synthetic 2D Image Generation
  #______________________________________________________________________________
  
  source("server/ai_assistant.R", local = TRUE)
  
  #______________________________________________________________________________
  #  13. Miscellaneous
  #______________________________________________________________________________
  
  source("server/about.R", local = TRUE)  
  
  #______________________________________________________________________________
  #  14. TESTING
  #______________________________________________________________________________
  
  observeEvent(input$tabs, {
    if (input$tabs == "TESTING") {
      ###put something
    }
  })
  
  observeEvent(input$save_feedbck, {
    req(input$save_feedbck)
    feedback_len <- nchar(input$user_feedback)
    if (feedback_len < 5) {
      showNotification("Feedback is too short.")
    } else  if (feedback_len > 2000) {
      showNotification("Feedback is too long.")
    } else {
      showNotification("Thank you for your feedback!")
      
      try(
        save_comments(
          date = Sys.Date(),
          time = format(Sys.time(), "%H:%M:%S"),
          comments = input$user_feedback,
          helpfulness = input$helpfulness,
          experience = input$experience
        )
      )
    }
    
    # clear the comments after submitted. 
    # This prevents users from submitting the same request twice.
    updateTextInput(
      session,
      "user_feedback",
      value = "",
      placeholder = "Any questions? Suggestions? Improvements? Things you like or dislike?" 
    )
    
    
  })
  
  observe({
    shinyjs::toggle(id = "user_feedback", condition = input$Comments)
    shinyjs::toggle(id = "save_feedbck", condition = input$Comments)
    shinyjs::toggle(id = "helpfulness", condition = input$Comments)
    shinyjs::toggle(id = "experience", condition = input$Comments)
    
  })
}
# load demo data when clicked
observeEvent(input$img_prompt,{
  if(input$img_prompt != img_prompts[1]) {
    updateTextInput(
      session,
      "image_prompt",
      value = input$img_prompt   # demo_prompt
    )
  } else { # if not mpg data, reset
    updateTextInput(
      session,
      "image_prompt",
      value = "",
      placeholder = "Provide a prompt for synthetic IMAGE generation, see examples. 
        For audio commands, enable voice narration in \"Settings\"."
    )
  }
})

output$synth_image <- renderPlotly({
  req(input$image_button)
  
  isolate({
    req(input$image_prompt) 
    
    #----------------------------Prep question
    txt <- input$image_prompt
    
    # force to within 280 characters
    if (nchar(txt) > max_char_question) {
      txt <- substr(txt, 1, max_char_question)
      showNotification(
        paste("Only the first", max_char_question, " characters will be used."),
        duration = 10
      )
    }
    
    # If the last character is not a stop, add it.
    # Otherwise, GPT3 will add a sentence.
    
    # The following 5 lines were generated by ChatGPT!!!!!
    # Check if the last character is not a period
    if (substr(txt, nchar(txt), nchar(txt)) != ".") {
      # If the last character is not a period, add it to the end
      txt <- paste(txt, ".", sep = "")
    }
    
    # prepared_request <- paste(
    #   "If the next question is not related to statistics or data science 
    #  say 'Statistics only!' ",
    #   txt
    # )
    prepared_request <- txt
    
    #----------------------------Send request
    shinybusy::show_modal_spinner(
      spin = "semipolar",
      text = paste("... Please wait, processing ..." # sample(jokes, 1)
      ),
      color = "#000000"
    )
    
    start_time <- Sys.time()
    
    # Send to openAI
    # synthImages <- create_image("2D sagittal MRI brain image of Alzheimer's disease",
    #                             n=1, size="1024x1024",
    #                             response_format="b64_json")  # response_format="url"
    tryCatch(
      response <- openai::create_image(n=1,
                                       prompt = prepared_request, size="256x256",
                                       openai_api_key = api_key_session()$api_key_openAI,
                                       response_format="b64_json"
      ),
      error = function(e) {
        # remove spinner, show message for 5s, & reload
        shinybusy::remove_modal_spinner()
        shiny::showModal(api_error_modal)
        Sys.sleep(5)
        session$reload()
        
        list(
          error_value = -1,
          message = capture.output(print(e$message)),
          error_status = TRUE
        )
      }
    )
    
    convBase64JSON2Img <- function(txt) {
      raw <- base64Decode(txt, mode="raw")
      # Handle PNG format
      if (all(as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)) == raw[1:8])) {
        img <- png::readPNG(raw)
      } 
      # Handle JPEG format
      else if (all(as.raw(c(0xff, 0xd8, 0xff, 0xd9)) == raw[c(1:2, length(raw)-(1:0))])) { 
        img <- jpeg::readJPEG(raw)
      } 
      # Currently no other formats are interpreted, but other formats can be added below
      else stop("No Appropriate Image Format Interpreter Available ...")
      return(img)
    }
    
    vars <- c(1:length(response$data[1]$b64_json))
    plots <- lapply(vars, function(var) {
      raw = convBase64JSON2Img(response$data[1]$b64_json[var])
      plot_ly(z=~(255*raw), type="image", 
              name=paste0(input$image_prompt, " ", var)) %>%
        layout(title = paste0(input$image_prompt, " ", var),
               xaxis = list(title = "", showticklabels = FALSE),
               yaxis = list(title = "", showticklabels = FALSE))
    })
    ################# ------------------------------------------------------
    # ans <- subplot(plots, nrows = 1) %>% layout(title="Simulated 2D Alzheimer's disease scans")
    
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
    if (error_api) {
      cmd <- NULL
      response <- NULL
      error_message <- response$message
    } else {
      cmd <- response$choices[1, 1]
    }
    
    api_time <- difftime(
      Sys.time(),
      start_time,
      units = "secs"
    )[[1]]
    
    # if more than 10 requests, slow down. Only on the server.
    if (counter$requests > 20 && file.exists(on_server)) {
      Sys.sleep(counter$requests / 5 + runif(1, 0, 5))
    }
    if (counter$requests > 50 && file.exists(on_server)) {
      Sys.sleep(counter$requests / 10 + runif(1, 0, 10))
    }
    if (counter$requests > 100 && file.exists(on_server)) {
      Sys.sleep(counter$requests / 40 + runif(1, 0, 40))
    }
    
    shinybusy::remove_modal_spinner()
    
    # update usage via global reactive value
    counter$tokens <- counter$tokens + response$usage$completion_tokens
    counter$requests <- counter$requests + 1
    counter$time <- round(api_time, 0)
    counter$tokens_current <- response$usage$completion_tokens
    
    # return(ans)
    subplot(plots, nrows = 1) %>% 
      layout(title=paste0("Simulated 2D image: ", input$image_prompt))
    
  })
  
})
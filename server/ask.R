# load demo data when clicked
observeEvent(input$demo_question,{
  if(input$demo_question != demo_questions[1]) {
    updateTextInput(
      session,
      "ask_question",
      value = input$demo_question
    )
  } else { # if not mpg data, reset
    updateTextInput(
      session,
      "ask_question",
      value = "",
      placeholder = "Ask the AI Bot any question, see the list of examples.\n
        You can enable voice narration in \"Settings\" to provide verbal commands."
    )
  }
})

# ADD A WARNING WHEN NOT SELECTING ANY MODEL
# observeEvent(input$model_choose,{
#   if(input$model_choose == demo_model_choose[1]){
#     updateTextInput(
#       session,
#       "ask_question",
#       value = "",
#       placeholder = "YOU ARE CURRENTLY NOT SELECTING A MODEL."
#     )
#   }
# })


# Initialize chat history as a reactive value
chat_history <- reactiveVal("")

output$answer <- renderUI({
  req(input$ask_button)
  
  isolate({
    req(input$ask_question)
    
    # ---------------------------- Prep question
    txt <- input$ask_question
    max_char_question <- 280  # Example; set your max character limit here
    
    if (nchar(txt) > max_char_question) {
      txt <- substr(txt, 1, max_char_question)
      showNotification(
        paste("Only the first", max_char_question, " characters will be used."),
        duration = 10
      )
    }
    
    if (substr(txt, nchar(txt), nchar(txt)) != ".") {
      txt <- paste(txt, ".", sep = "")
    }
    
    prepared_request <- txt
    
    shinybusy::show_modal_spinner(
      spin = "semipolar",
      text = paste0("... Please wait, processing ...\n\n", prepared_request),
      color = "#000000"
    )
    
    start_time <- Sys.time()
    
    ans <- NULL
    
    # OpenAI or Google API Logic
    if (input$model_choose == demo_model_choose[1] || input$model_choose == demo_model_choose[2]) {
      chat <- function(input_message) {
        user_input <- list(list(role = "user", content = input_message))
        base_url <- "https://api.openai.com/v1/chat/completions"
        api_key_openAI <- api_key_session()$api_key_openAI
        body <- list(
          model = language_model,
          messages = user_input,
          max_tokens = 200,
          temperature = sample_temp()
        )
        req <- request(base_url)
        resp <- req |>
          req_auth_bearer_token(token = api_key_openAI) |>
          req_headers("Content-Type" = "application/json") |>
          req_body_json(body) |>
          req_retry(max_tries = 4) |>
          req_throttle(rate = 15) |>
          req_perform()
        
        openai_chat_response <- resp |> resp_body_json(simplifyVector = TRUE)
        return(openai_chat_response)
      }
      
      tryCatch(
        response <- chat(prepared_request),
        error = function(e) {
          shinybusy::remove_modal_spinner()
          showNotification("API Error occurred.", type = "error")
          return(NULL)
        }
      )
      
      ans <- if (!is.null(response)) response$choices$message$content else "No response available."
      
    } else if (input$model_choose == demo_model_choose[3]) {
      chat <- function(input_message) {
        api_key_gemini <- api_key_session()$api_key_gemini
        base_url <- "https://generativelanguage.googleapis.com/v1beta/models"
        model1 <- "/gemini-pro:generateContent"
        model2 <- "/gemini-pro-vision:generateContent"
        
        if (is.null(input$user_image)) {
          base_url <- paste0(base_url, model1)
          input_list <- list(
            role = "user",
            parts = list(list(text = input_message))
          )
        } else {
          base_url <- paste0(base_url, model2)
          image_type <- input$user_image$type
          image_path <- input$user_image$datapath
          image_data <- base64enc::dataURI(file = image_path, mime = image_type)
          image_base64 <- sub("data:.*?base64,", "", image_data)
          
          input_list <- list(
            role = "user",
            parts = list(
              list(text = input_message),
              list(
                inline_data = list(
                  mime_type = image_type,
                  data = image_base64
                )
              )
            )
          )
        }
        
        response <- POST(
          url = base_url,
          query = list(key = api_key_gemini),
          content_type_json(),
          encode = "json",
          body = list(
            contents = input_list,
            generationConfig = list(
              temperature = 0.5,
              maxOutputTokens = 1024
            )
          )
        )
        return(response)
      }
      
      tryCatch(
        response <- chat(prepared_request),
        error = function(e) {
          shinybusy::remove_modal_spinner()
          showNotification("Google API Error occurred.", type = "error")
          return(NULL)
        }
      )
      
      candidates <- if (!is.null(content(response)$candidates)) content(response)$candidates else list()
      ans <- if (length(candidates) > 0) candidates[[1]]$content$parts[[1]]$text[1] else "No response available."
    }
    
    shinybusy::remove_modal_spinner()
    
    # Concatenate the new Q&A to the chat history
    current_history <- chat_history()
    qq <- paste("Q:", prepared_request)
    aa <- paste("A:", ans)
    new_history <- paste(qq, aa, sep = "\n")
    new_history <- paste(new_history, current_history, sep = "\n\n")
    chat_history(new_history)
    
    HTML(paste0("<pre>", chat_history(), "</pre>"))
    #return(chat_history())
  })
})

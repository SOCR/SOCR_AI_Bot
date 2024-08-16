
jscode <- 'var container = document.getElementById("chat-container");
if (container) {
  var elements = container.getElementsByClassName("user-message");
  if (elements.length > 1) {
    var lastElement = elements[elements.length - 1];
    lastElement.scrollIntoView({
      behavior: "smooth"
    });
  }
}'

historyALL <- reactiveValues(df = data.frame() , val = character(0))

# On click of send button
observeEvent(input$submit, {
  
  if (nchar(trimws(input$prompt)) > 0) {
    
    # Spinner
    w <- Waiter$new(id = "chat-history",
                    html = spin_3(),
                    color = transparent(.5))
    w$show()
    
    # Response
    # chatGPT <- chatGPT_R(input$apiKey, input$prompt, input$model)
    # chatGPT<- socrragGPT(input$prompt)
    chatGPT <- py_libs$generate_response(input$api_key_openAI, input$model,  input$prompt)
    print(chatGPT)
    historyALL$val <- chatGPT
    history <- data.frame(users = c("Human", "AI"),
                          content = c(input$prompt, markdown::mark_html(text=chatGPT)),
                          stringsAsFactors = FALSE)
    historyALL$df <- rbind(historyALL$df, history)
    updateTextInput(session, "prompt", value = "")
    
    # Conversation Interface
    output$chatThread <- renderUI({
      conversations <- lapply(seq_len(nrow(historyALL$df)), function(x) {
        tags$div(class = ifelse(historyALL$df[x, "users"] == "Human",
                                "user-message",
                                "bot-message"),
                 HTML(paste0(ifelse(historyALL$df[x, "users"] == "Human",
                                    "<div class='img-wrapper'></div>",
                                    "<div class='img-wrapper'><img src='socr_logo.png' class='img-wrapper2'></div>"),
                             historyALL$df[x, "content"])))
      })
      do.call(tagList, conversations)
    })
    
    w$hide()
    execute_at_next_input(runjs(jscode))
    
  }
  
})

observeEvent(input$remove_chatThread, {
  output$chatThread <- renderUI({return(NULL)})
  historyALL$df <- NULL
  updateTextInput(session, "prompt", value = "")
})

observe({
  req(input$clipbtn)
  CopyButtonUpdate(session,
                   id = "clipbtn",
                   label = "Copy",
                   icon = icon("clipboard"),
                   text = as.character(historyALL$val))
  
})
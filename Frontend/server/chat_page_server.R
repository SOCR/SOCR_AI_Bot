chat_server <- function(input, output, session) {
  # Initialize database connection
  db_conn <- dbConnect(RSQLite::SQLite(), "chat_history.db")
  
  # Initialize reactive values for message management
  messages <- reactiveVal(list())
  current_message <- reactiveVal(NULL)
  
  # Helper function to safely convert markdown to HTML
  # safe_markdown_to_html <- function(text) {
  #   tryCatch({
  #     html <- commonmark::markdown_html(text)
  #     html <- gsub("^<p>|</p>$", "", html)
  #     return(html)
  #   }, error = function(e) {
  #     return(htmltools::htmlEscape(text))
  #   })
  # }
  
  safe_markdown_to_html <- function(text) {
    tryCatch({
      # Convert markdown but preserve math delimiters
      html <- commonmark::markdown_html(text)
      
      # Remove wrapping paragraph tags if present
      html <- gsub("^<p>|</p>$", "", html)
      
      return(HTML(html))
    }, error = function(e) {
      return(htmltools::htmlEscape(text))
    })
  }
  
  # Render previous messages from database
  output$previous_messages <- renderUI({
    # Trigger update when messages change
    session$userData$rv$message_update
    
    req(session$userData$current_chat_id())
    cat("chat id changed to: ", session$userData$current_chat_id(),"\n")
    
    messages <- dbGetQuery(db_conn, "
      SELECT role, content, created_at
      FROM messages
      WHERE chat_id = ?
      ORDER BY created_at ASC
    ", params = list(session$userData$current_chat_id()))
    
    if (nrow(messages) == 0) {
      return(
        div(
          class = "text-center text-muted mt-4",
          icon("comments"),
          h4("Start a new conversation"),
          p("Type a message below to begin chatting")
        )
      )
    }
    
    tagList(
      lapply(1:nrow(messages), function(i) {
        msg <- messages[i, ]
        is_user <- msg$role == "user"
        print(msg$content)
        div(
          class = paste("chat-message", if(is_user) "user" else "assistant"),
          if (!is_user) div(class = "assistant-name", "Assistant"),
          if (is_user) {
            htmltools::htmlEscape(msg$content)
          } else {
            HTML(safe_markdown_to_html(msg$content))
          },
          div(
            class = "message-timestamp",
            format(as.POSIXct(msg$created_at), "%I:%M %p")
          )
        )
      })
    )
  })
  
  # Render current streaming message
  output$current_message <- renderUI({
    msg <- current_message()
    
    if (is.null(msg)) {
      return(NULL)
    }
    
    div(
      class = "chat-message assistant",
      div(class = "assistant-name", "Assistant"),
      HTML(safe_markdown_to_html(msg$content))
    )
  })
  
  # Initialize WebSocket connection
  ws <- websocket::WebSocket$new("ws://127.0.0.1:8000/ws")
  
  # Handle incoming WebSocket messages
  ws$onMessage(function(event) {
    data <- jsonlite::fromJSON(event$data)
    
    isolate({
      if (!exists("partial_message", envir = .GlobalEnv)) {
        assign("partial_message", list(role = "assistant", content = ""), envir = .GlobalEnv)
      }
      partial_message <- get("partial_message", envir = .GlobalEnv)
      
      if (!is.null(data$content)) {
        if (data$content == "END_OF_MESSAGE") {
          # Store completed message in database
          dbExecute(db_conn, "
            INSERT INTO messages (message_id, chat_id, role, content, created_at)
            VALUES (?, ?, ?, ?, datetime('now'))
          ", params = list(
            UUIDgenerate(),
            session$userData$current_chat_id(),
            "assistant",
            partial_message$content
          ))
          
          # Update reactive values
          session$userData$rv$message_update <- session$userData$rv$message_update + 1
          session$userData$rv$chat_list_update <- session$userData$rv$chat_list_update + 1
          
          # Clear current streaming message
          current_message(NULL)
          
          # Reset buffer
          assign("partial_message", list(role = "assistant", content = ""), envir = .GlobalEnv)
          
          # Scroll to bottom
          runjs("
            const chatDiv = document.getElementById('chat_container');
            if(chatDiv) {
              chatDiv.scrollTop = chatDiv.scrollHeight;
            }
          ")
        } else {
          # Update partial message with new content
          partial_message$content <- paste0(partial_message$content, data$content)
          assign("partial_message", partial_message, envir = .GlobalEnv)
          
          # Update current streaming message
          current_message(partial_message)
        }
      }
    })
  })
  
  # Handle WebSocket errors
  ws$onError(function(event) {
    showNotification("WebSocket connection error.", type = "error")
  })
  
  # Handle WebSocket closure
  ws$onClose(function(event) {
    showNotification("WebSocket connection closed.", type = "error")
  })
  
  # Handle user message submission
  observeEvent(input$send, {
    req(input$user_input, session$userData$current_chat_id())
    message <- trimws(input$user_input)
    if (message == "") return()
    
    tryCatch({
      # Insert user message into database
      dbExecute(db_conn, "
        INSERT INTO messages (message_id, chat_id, role, content, created_at)
        VALUES (?, ?, ?, ?, datetime('now'))
      ", params = list(
        UUIDgenerate(),
        session$userData$current_chat_id(),
        "user",
        message
      ))
      
      # Update reactive values
      session$userData$rv$message_update <- session$userData$rv$message_update + 1
      session$userData$rv$chat_list_update <- session$userData$rv$chat_list_update + 1
      
      # Clear input
      updateTextAreaInput(session, "user_input", value = "")
      
      # Send message to WebSocket server
      ws$send(jsonlite::toJSON(list(
        chat_id = jsonlite::unbox(session$userData$current_chat_id()),
        query = jsonlite::unbox(message)
      )))
      
      # Update chat title if needed
      chat_title <- dbGetQuery(db_conn, "SELECT title FROM chats WHERE chat_id = ?",
                               params = list(session$userData$current_chat_id()))$title
      
      if (chat_title == "New Chat") {
        new_title <- substr(message, 1, 25)
        if (nchar(message) > 25) new_title <- paste0(new_title, "...")
        
        dbExecute(db_conn, "
          UPDATE chats
          SET title = ?
          WHERE chat_id = ?
        ", params = list(new_title, session$userData$current_chat_id()))
      }
      
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
    })
  })
  
  
  
  # Server code for button toggling
  observeEvent(input$send, {
    shinyjs::hide(id = "send")
    shinyjs::show(id = "stop_gen")
  })
  
  observeEvent(input$stop_gen, {
    shinyjs::show(id = "send")
    shinyjs::hide(id = "stop_gen")
  })
  
  # When generation completes
  observe({
    # Your generation complete condition here
    shinyjs::show(id = "send")
    shinyjs::hide(id = "stop_gen")
  })
  # Handle stop generation
  observeEvent(input$stop_gen, {
    # Add your stop generation logic here
  })
  
  # Clean up when session ends
  session$onSessionEnded(function() {
    ws$close()
    dbDisconnect(db_conn)
  })
  
}
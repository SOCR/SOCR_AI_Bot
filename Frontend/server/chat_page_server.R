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
  
  # First, modify your safe_markdown_to_html function to include copy buttons
  safe_markdown_to_html <- function(text) {
    tryCatch({
      # First convert markdown to HTML
      html <- commonmark::markdown_html(text)
      
      # Add copy button only to code blocks
      html <- gsub(
        '<pre><code class="language-([^"]*)">(.*?)</code></pre>',
        '<div class="code-wrapper">
         <div class="code-header">
           <button class="code-copy-btn" data-clipboard-text="\\2">
             <i class="fas fa-copy"></i> Copy code
           </button>
         </div>
         <pre><code class="language-\\1">\\2</code></pre>
       </div>',
        html
      )
      
      # Remove wrapping paragraph tags if present
      html <- gsub("^<p>|</p>$", "", html)
      return(html)
    }, error = function(e) {
      return(htmltools::htmlEscape(text))
    })
  }
  
  # Modify your message rendering in previous_messages
  output$previous_messages <- renderUI({
    print("previous message was called")
    session$userData$rv$message_update
    req(session$userData$current_chat_id())
    
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
        
        div(
          class = paste("chat-message", if(is_user) "user" else "assistant"),
          
          if (!is_user) div(class = "assistant-name", "Assistant"),
          
          if (is_user) {
            htmltools::htmlEscape(msg$content)
          } else {
            tagList(
              # Message content
              HTML(safe_markdown_to_html(msg$content)),
              
              # Copy full response button at bottom
              div(
                class = "response-footer",
                tags$button(
                  class = "copy-response-btn",
                  `data-clipboard-text` = msg$content,
                  icon("copy"), 
                  "Copy response"
                )
              )
            )
          },
          
          div(
            class = "message-timestamp",
            format(as.POSIXct(msg$created_at), "%I:%M %p")
          )
        )
      })
    )
  })
  
  # Similarly modify current_message output
  output$current_message <- renderUI({
    msg <- current_message()
    
    if (is.null(msg)) {
      return(NULL)
    }
    
    div(
      class = "chat-message assistant",
      style = "position: relative;",
      div(class = "assistant-name", "Assistant"),
      tagList(
        HTML(safe_markdown_to_html(msg$content)),
        tags$button(
          class = "copy-response-btn",
          `data-clipboard-text` = msg$content,
          "Copy response"
        )
      )
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
      
      # Handle status messages from backend
      if (!is.null(data$type) && data$type == "status") {
        if (data$content %in% c("stopped", "complete")) {
          print("Processing completion/stop")  # Debug log
          # Save partial message if it exists
          if (nchar(partial_message$content) > 0) {
            dbExecute(db_conn, "
            INSERT INTO messages (message_id, chat_id, role, content, created_at)
            VALUES (?, ?, ?, ?, datetime('now'))
          ", params = list(
            UUIDgenerate(),
            session$userData$current_chat_id(),
            "assistant",
            paste0(partial_message$content, 
                   if(data$content == "stopped") "\n\n[Generation stopped]" else "")
          ))
            
            # Update reactive values
            session$userData$rv$message_update <- session$userData$rv$message_update + 1
            session$userData$rv$chat_list_update <- session$userData$rv$chat_list_update + 1
            
            # Clear current streaming message
            current_message(NULL)
            
            # Reset buffer
            assign("partial_message", list(role = "assistant", content = ""), envir = .GlobalEnv)
          }
          
          # Reset UI
          # Use session$sendCustomMessage to update UI
          session$sendCustomMessage("toggleButtons", list(
            show = "send",
            hide = "stop_gen"
          ))
          
          # Scroll to bottom
          session$sendCustomMessage("scrollChat", TRUE)
        }
        return()
      }
      
      # Handle normal content streaming
      if (!is.null(data$content) && !is.null(data$role) && data$role == "ai") {
        # Update partial message with new content
        partial_message$content <- paste0(partial_message$content, data$content)
        assign("partial_message", partial_message, envir = .GlobalEnv)
        
        # Update current streaming message
        current_message(partial_message)
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
      
      if (chat_title == "(New Chat)") {
        new_title <- format_chat_title(message)
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
    # shinyjs::hide(id = "send")
    shinyjs::show(id = "stop_gen")
  })
  
  observeEvent(input$stop_gen, {
    # shinyjs::show(id = "send")
    shinyjs::hide(id = "stop_gen")
  })
  
  # When generation completes
  observe({
    # Your generation complete condition here
    # shinyjs::show(id = "send")
    shinyjs::hide(id = "stop_gen")
  })
  # Handle stop generation
  # Handle stop generation
  observeEvent(input$stop_gen, {
    # Send stop signal to WebSocket server
    ws$send(jsonlite::toJSON(list(
      type = jsonlite::unbox("stop"),
      chat_id = jsonlite::unbox(session$userData$current_chat_id())
    )))
    
    # Update UI immediately
    # shinyjs::show(id = "send")
    shinyjs::hide(id = "stop_gen")
  })
  
  # Clean up when session ends
  session$onSessionEnded(function() {
    ws$close()
    dbDisconnect(db_conn)
  })
  
}


# Util functions 

format_chat_title <- function(message) {
  # Remove excess whitespace and line breaks
  message <- trimws(gsub("\\s+", " ", message))
  
  # Remove common prefixes that might not be meaningful
  message <- gsub("^(hey|hi|hello|please|can you|could you|I want to|I need|help me)\\s+", "", 
                  message, ignore.case = TRUE)
  
  # Remove code blocks and special characters
  message <- gsub("```.*?```", "", message)  # Remove code blocks
  message <- gsub("[^[:alnum:][:space:]?!.]", "", message)  # Keep only alphanumeric, spaces, and basic punctuation
  
  # If message starts with a question mark after cleaning, remove it
  message <- gsub("^\\?+\\s*", "", message)
  
  # Capitalize first letter of each word, but preserve common terms
  words <- strsplit(tolower(message), " ")[[1]]
  keep_lowercase <- c("a", "an", "the", "in", "on", "at", "to", "for", "of", "and", "or", "but")
  words <- sapply(1:length(words), function(i) {
    if (i == 1 || !words[i] %in% keep_lowercase) {
      tools::toTitleCase(words[i])
    } else {
      words[i]
    }
  })
  message <- paste(words, collapse = " ")
  
  # Truncate while preserving whole words
  if (nchar(message) > 35) {
    words <- strsplit(message, " ")[[1]]
    title <- ""
    for (word in words) {
      if (nchar(paste0(title, word)) > 32) {  # 37 to leave room for "..."
        title <- paste0(trimws(title), "...")
        break
      }
      title <- paste(title, word)
    }
    message <- title
  }
  
  # Add question mark if the original message was a question
  if (grepl("\\?", message)) {
    message <- gsub("\\?+\\s*$", "", message)  # Remove existing question marks at end
    message <- paste0(message, "?")
  }
  
  return(trimws(message))
}
view_all_chat_server <- function(input, output, session) {
  # In server.R
  db_conn <- dbConnect(RSQLite::SQLite(), "chat_history.db")
  auth0_user <- session$userData$auth0_info
  
  
  # Chat search reactive value
  chat_search <- reactiveVal("")
  
  # Update search value when input changes
  observeEvent(input$chat_search, {
    chat_search(input$chat_search)
  })
  
  
  output$chat_count_text <- renderUI({
    
    session$userData$rv$chat_list_update
    # Get total chat count from database
    chat_count <- dbGetQuery(db_conn, "
    SELECT COUNT(*) as count
    FROM chats
    WHERE user_id = ?
  ", params = list(auth0_user$sub))$count
    
    sprintf("You have %d previous chats with DSPA Assistant", chat_count)
  })
  
  
  
  # Render chat list
  output$chat_list <- renderUI({
    # Trigger updates
    session$userData$rv$chat_list_update
    search_query <- chat_search()
    
    # Get chats from database
    chats <- dbGetQuery(db_conn, "
    SELECT 
      c.chat_id,
      c.title,
      MAX(COALESCE(m.created_at, c.created_at)) as last_message
    FROM chats c
    LEFT JOIN messages m ON c.chat_id = m.chat_id
    WHERE c.user_id = ?
    GROUP BY c.chat_id
    ORDER BY last_message DESC
  ", params = list(auth0_user$sub))
    
    # Filter based on search
    if (!is.null(search_query) && nchar(search_query) > 0) {
      chats <- chats[grepl(search_query, chats$title, ignore.case = TRUE), ]
    }
    
    # If no chats found
    if (nrow(chats) == 0) {
      return(
        div(
          class = "text-center text-muted p-4",
          icon("comments"),
          h4("No chats found"),
          if (!is.null(search_query) && nchar(search_query) > 0) {
            p("Try a different search term")
          } else {
            p("Start a new chat to begin")
          }
        )
      )
    }
    
    # Format timestamps
    format_time_ago <- function(timestamp) {
      if (is.null(timestamp)) return("Never")
      
      diff <- difftime(Sys.time(), as.POSIXct(timestamp), units = "secs")
      
      if (diff < 60) {
        return("Just now")
      } else if (diff < 3600) {
        mins <- floor(diff / 60)
        return(paste(mins, "minutes ago"))
      } else if (diff < 86400) {
        hours <- floor(diff / 3600)
        return(paste(hours, "hours ago"))
      } else {
        days <- floor(diff / 86400)
        return(paste(days, "days ago"))
      }
    }
    
    # In your output$chat_list renderUI function:
    tagList(
      lapply(1:nrow(chats), function(i) {
        chat <- chats[i, ]
        div(
          class = "history-list-item",
          onclick = sprintf(
            "Shiny.setInputValue('selected_chat', '%s', {priority: 'event'})",
            chat$chat_id
          ),
          div(
            class = "history-item-content",
            div(class = "history-item-title", chat$title),
            div(
              class = "history-item-info",
              span(paste("Last message", format_time_ago(chat$last_message))),
              span("•"),
              span("in @DSPA Assistant")
            )
          ),
          div(
            class = "history-item-actions",
            tags$button(
              class = "history-delete-btn",
              # Add onclick with stopPropagation
              onclick = sprintf(
                "event.stopPropagation(); Shiny.setInputValue('delete_chat', '%s', {priority: 'event'})",
                chat$chat_id
              ),
              icon("trash")
            )
          )
        )
      })
    )
    
    
  })
  
  # Handle chat deletion
  observeEvent(input$delete_chat, {
    print(input$delete_chat)
    req(input$delete_chat)

    isolate({
      # Delete chat and its messages
      dbWithTransaction(db_conn, {
        dbExecute(db_conn, "DELETE FROM messages WHERE chat_id = ?",
                  params = list(input$delete_chat))
        dbExecute(db_conn, "DELETE FROM chats WHERE chat_id = ?",
                  params = list(input$delete_chat))
      })

      # Update UI
      session$userData$rv$chat_list_update <- session$userData$rv$chat_list_update + 1
    })


    showNotification("Chat deleted", type = "message")
  })
  
}
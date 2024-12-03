# server.R
library(auth0)
library(DBI)
library(RSQLite)
library(reticulate)
library(here)
library(uuid)
library(shinyjs)
library(rintrojs)
library(bs4Dash)
library(DT)


source("server/chat_page_server.R")
source("server/view_all_chat_page_server.R")

# DB initialization function remains the same
init_db <- function() {
  con <- dbConnect(RSQLite::SQLite(), "chat_history.db")
  
  # Create chats table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS chats (
      chat_id TEXT PRIMARY KEY,
      user_id TEXT NOT NULL,
      title TEXT DEFAULT '(New Chat)',
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      model TEXT DEFAULT 'GPT-3.5',
      temperature REAL DEFAULT 0.7
    );
  ")
  
  # Create messages table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS messages (
      message_id TEXT PRIMARY KEY,
      chat_id TEXT NOT NULL,
      role TEXT NOT NULL,
      content TEXT NOT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (chat_id) REFERENCES chats(chat_id) ON DELETE CASCADE
    );
  ")
  
  # Create user_settings table
  dbExecute(con, "
  CREATE TABLE IF NOT EXISTS user_settings (
    user_id TEXT PRIMARY KEY,
    default_model TEXT DEFAULT 'gpt-3.5-turbo',
    temperature REAL DEFAULT 0.7,
    dark_mode BOOLEAN DEFAULT 0,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  );
")
  

  
  dbDisconnect(con)
}

server <- auth0_server(function(input, output, session) {
  
  init_db()
  # Initialize database connection
  db_conn <- dbConnect(RSQLite::SQLite(), "chat_history.db")
  
  # Get user info
  auth0_user <- session$userData$auth0_info
  
  # Create reactive values for triggering updates
  session$userData$rv <- reactiveValues(
    message_update = 0,
    chat_list_update = 0
  )
  
  # Current chat state
  session$userData$current_chat_id <- reactiveVal(NULL)
  
  # User UI
  output$user <- renderUser({
    dashboardUser(
      image = auth0_user$picture,
      title = auth0_user$name,
      subtitle = NULL,  
      status = NULL,    
      fluidRow(
        dashboardUserItem(
          width = 12,
          div(
            class = "user-menu-items",
            # Settings button - changed to actionButton
            actionButton(
              "show_settings_modal",  # Changed ID
              div(
                class = "menu-item",
                icon("gear"),
                span("Settings")
              ),
              class = "btn-menu"
            ),
            tags$hr(class = "menu-divider"),
            logoutButton(
              class = "btn-menu",
              label = div(
                class = "menu-item",
                icon("sign-out-alt"),
                span("Log out")
              )
            )
          )
        )
      )
    )
  })
  
  # Recent chats list
  # output$recent_chats_list <- renderUI({
  #   # Trigger update when chat list changes
  #   session$userData$rv$chat_list_update
  # 
  #   chats <- dbGetQuery(db_conn, "
  #     SELECT
  #       chat_id,
  #       COALESCE(title, 'Untitled Chat') as title,
  #       created_at,
  #       (SELECT COUNT(*) FROM messages m WHERE m.chat_id = c.chat_id) as msg_count
  #     FROM chats c
  #     WHERE user_id = ?
  #     ORDER BY created_at DESC
  #     LIMIT 5
  #   ", params = list(auth0_user$sub))
  # 
  #   if (nrow(chats) == 0) {
  #     return(div(
  #       style = "padding: 10px;",
  #       "No recent chats"
  #     ))
  #   }
  # 
  #   tagList(
  #     lapply(1:nrow(chats), function(i) {
  #       chat <- chats[i, ]
  #       div(
  #         class = "recent-chat-item",
  #         style = paste0(
  #           "padding: 8px 20px; cursor: pointer; ",
  #           if(chat$chat_id == session$userData$current_chat_id()) "background: rgba(0,0,0,0.1);" else ""
  #         ),
  #         onclick = sprintf("Shiny.setInputValue('select_chat', '%s');", chat$chat_id),
  #         icon("comments"),
  #         span(
  #           style = "margin-left: 8px;",
  #           paste0(chat$title, " (", chat$msg_count, ")")
  #         )
  #       )
  #     })
  #   )
  # })
  # 
  # # Handle chat selection
  # observeEvent(input$select_chat, {
  #   req(input$select_chat)
  #   cat("Selecting chat:", input$select_chat, "\n")
  #   session$userData$current_chat_id(input$select_chat)
  #   updateTabItems(session, "sidebar", selected = "chat")
  # })
  
  
  
  observe({
    # Initialize if not already set
    if (is.null(session$userData$rv$chat_list_update)) {
      session$userData$rv <- reactiveValues(
        chat_list_update = 0,
        message_update = 0
      )
    }
  }, priority = 1000)  # High priority ensures this runs early
  
  # Recent chats list output
  output$recent_chats_list <- renderUI({
    
    session$userData$rv$chat_list_update
    # print("recent_chats_list was called")
    # Get recent chats from database
    chats <- dbGetQuery(db_conn, "
      SELECT chat_id, title
      FROM chats
      WHERE user_id = ?
      ORDER BY created_at DESC
      LIMIT 10
    ", params = list(session$userData$auth0_info$sub))

    if (nrow(chats) == 0) {
      # Return empty message if no chats exist
      return(div(
        style = "color: #666; font-size: 0.9rem; padding: 8px 12px;",
        "No chats yet"
      ))
    }

    tagList(
      lapply(1:nrow(chats), function(i) {
        chat <- chats[i, ]
        is_active <- !is.null(session$userData$current_chat_id()) &&
          chat$chat_id == session$userData$current_chat_id()

        div(
          class = paste("chat-item", if(is_active) "active" else ""),
          id = paste0("chat_", chat$chat_id),
          onclick = sprintf("Shiny.setInputValue('selected_chat', '%s')", chat$chat_id),
          icon("comments"),
          div(class = "chat-title", chat$title)
        )
      })
    )
    # print("recent_chats_list finished")
  })
  #
  # # Handle chat selection
  observeEvent(input$selected_chat, {
    session$userData$current_chat_id(input$selected_chat)
    updateTabItems(session, "sidebar", selected = "chat")
  })
  # 
  # # Chat history table
  # output$chat_history_table <- renderDataTable({
  #   # Trigger update when chat list changes
  #   session$userData$rv$chat_list_update
  #   
  #   chats <- dbGetQuery(db_conn, "
  #     SELECT 
  #       c.chat_id,
  #       COALESCE(c.title, 'Untitled Chat') as title,
  #       c.created_at,
  #       COUNT(m.message_id) as message_count,
  #       MAX(m.created_at) as last_message,
  #       c.model
  #     FROM chats c
  #     LEFT JOIN messages m ON c.chat_id = m.chat_id
  #     WHERE c.user_id = ?
  #     GROUP BY c.chat_id
  #     ORDER BY MAX(COALESCE(m.created_at, c.created_at)) DESC
  #   ", params = list(auth0_user$sub))
  #   
  #   if (!is.null(input$date_filter)) {
  #     chats <- chats[as.Date(chats$created_at) >= input$date_filter[1] & 
  #                      as.Date(chats$created_at) <= input$date_filter[2], ]
  #   }
  #   
  #   if (!is.null(input$search_filter) && nchar(input$search_filter) > 0) {
  #     chats <- chats[grepl(input$search_filter, chats$title, ignore.case = TRUE), ]
  #   }
  #   
  #   datatable(
  #     data = chats,  # Keep all columns for selection
  #     options = list(
  #       pageLength = 10,
  #       order = list(list(3, 'desc')),
  #       dom = 'frtip'
  #     ),
  #     selection = 'single',
  #     rownames = FALSE
  #   ) %>%
  #     formatDate('created_at', method = 'toLocaleString') %>%
  #     formatDate('last_message', method = 'toLocaleString')
  # })
  # 
  # # Handle table selection
  # observeEvent(input$chat_history_table_rows_selected, {
  #   req(input$chat_history_table_rows_selected)
  #   
  #   chats <- dbGetQuery(db_conn, "
  #     SELECT chat_id
  #     FROM chats
  #     WHERE user_id = ?
  #     ORDER BY created_at DESC
  #   ", params = list(auth0_user$sub))
  #   
  #   selected_row <- input$chat_history_table_rows_selected
  #   if (length(selected_row) > 0 && selected_row <= nrow(chats)) {
  #     selected_chat_id <- chats$chat_id[selected_row]
  #     cat("Selected chat from table:", selected_chat_id, "\n")
  #     session$userData$current_chat_id(selected_chat_id)
  #     updateTabItems(session, "sidebar", selected = "chat")
  #   }
  # })
  
  
  chat_server(input, output, session)
  view_all_chat_server(input, output, session)
  
  # Handle new chat creation  
  observeEvent(input$new_chat, {
    
    print("new chat was called")
    # First check if there's already a "New Chat" with no messages
    existing_new_chat <- dbGetQuery(db_conn, "
    SELECT c.chat_id 
    FROM chats c
    LEFT JOIN messages m ON c.chat_id = m.chat_id
    WHERE c.user_id = ? 
    AND c.title = '(New Chat)'
    AND m.message_id IS NULL
    LIMIT 1
  ", params = list(session$userData$auth0_info$sub))
    
    if (nrow(existing_new_chat) > 0) {
      # If empty "New Chat" exists, switch to it
      session$userData$current_chat_id(existing_new_chat$chat_id)
      updateTabItems(session, "sidebar", selected = "chat")
    } else {
      # If no empty "New Chat" exists, create new one
      new_chat_id <- UUIDgenerate()
      
      dbExecute(db_conn, "
      INSERT INTO chats (chat_id, user_id, title, created_at)
      VALUES (?, ?, ?, datetime('now'))
    ", params = list(
      new_chat_id,
      session$userData$auth0_info$sub,
      "(New Chat)"
    ))
      
      session$userData$current_chat_id(new_chat_id)
      session$userData$rv$chat_list_update <- session$userData$rv$chat_list_update + 1
      updateTabItems(session, "sidebar", selected = "chat")
    }
  })
  
  
  # observeEvent(input$sidebar, {
  #   if (input$sidebar == "chat") {
  #     new_chat_id <- UUIDgenerate()
  #     # cat("Selecting chat:", input$select_chat, "\n")
  #     cat("New chat created:", new_chat_id,"\n")
  #     
  #     tryCatch({
  #       dbExecute(db_conn, "
  #         INSERT INTO chats (chat_id, user_id, title, model, temperature)
  #         VALUES (?, ?, ?, ?, ?)
  #       ", params = list(
  #         new_chat_id,
  #         auth0_user$sub,
  #         "New Chat",
  #         input$model %||% "GPT-3.5",
  #         input$temperature %||% 0.7
  #       ))
  #       
  #       session$userData$current_chat_id(new_chat_id)
  #       session$userData$rv$chat_list_update <- session$userData$rv$chat_list_update + 1
  #       
  #     }, error = function(e) {
  #       cat("Error creating new chat:", conditionMessage(e), "\n")
  #     })
  #   }
  # })
  
  
  # In server.R
  
  
  # Navigation observers for settings modal
  observeEvent(input$nav_general, {
    shinyjs::hide("section_model")
    shinyjs::hide("section_data")
    shinyjs::show("section_general")
    
    # Update active states
    removeClass(selector = ".sidebar-btn", class = "active")
    addClass(id = "nav_general", class = "active")
  })
  
  observeEvent(input$nav_model, {
    shinyjs::hide("section_general")
    shinyjs::hide("section_data")
    shinyjs::show("section_model")
    
    # Update active states
    removeClass(selector = ".sidebar-btn", class = "active")
    addClass(id = "nav_model", class = "active")
  })
  
  observeEvent(input$nav_data, {
    shinyjs::hide("section_general")
    shinyjs::hide("section_model")
    shinyjs::show("section_data")
    
    # Update active states
    removeClass(selector = ".sidebar-btn", class = "active")
    addClass(id = "nav_data", class = "active")
  })
  
  # Show settings modal
  # In show settings modal observer
  observeEvent(input$show_settings_modal, {
    # Get user settings
    user_settings <- dbGetQuery(db_conn, "
    SELECT * FROM user_settings WHERE user_id = ?
  ", params = list(auth0_user$sub))
    
    # If user has settings, update the UI inputs
    if (nrow(user_settings) > 0) {
      updatePickerInput(session, "default_model", selected = user_settings$default_model)
      updateSliderInput(session, "default_temperature", value = user_settings$temperature)
      updateMaterialSwitch(session, "dark_mode", value = as.logical(user_settings$dark_mode))
    }
    
    showModal(settingsModalUI())
  })
  
  # Close modal
  observeEvent(input$close_settings, {
    removeModal()
  })
  
  # Save settings
  # Save settings
  observeEvent(input$save_settings, {
    # First check if user has settings
    existing_settings <- dbGetQuery(db_conn, "
    SELECT user_id FROM user_settings WHERE user_id = ?
  ", params = list(auth0_user$sub))
    
    if (nrow(existing_settings) == 0) {
      # Insert new user settings
      dbExecute(db_conn, "
      INSERT INTO user_settings (user_id, default_model, temperature, dark_mode)
      VALUES (?, ?, ?, ?)
    ", params = list(
      auth0_user$sub,
      input$default_model,
      input$default_temperature,
      as.integer(input$dark_mode)
    ))
    } else {
      # Update existing settings
      dbExecute(db_conn, "
      UPDATE user_settings 
      SET 
        default_model = ?,
        temperature = ?,
        dark_mode = ?,
        updated_at = CURRENT_TIMESTAMP
      WHERE user_id = ?
    ", params = list(
      input$default_model,
      input$default_temperature,
      as.integer(input$dark_mode),
      auth0_user$sub
    ))
    }
    
    showNotification("Settings saved", type = "message")
    removeModal()
  })
  
  # Handle chat history clear
  observeEvent(input$clear_history, {
    showModal(
      bs4Modal(
        id = "confirm_clear",
        title = "Confirm Clear History",
        body = "Are you sure you want to clear all chat history? This cannot be undone.",
        footer = tagList(
          actionButton("cancel_clear", "Cancel", class = "btn-secondary"),
          actionButton("confirm_clear", "Clear All", class = "btn-danger")
        )
      )
    )
  })
  
  # Export chat history
  output$export_chats <- downloadHandler(
    filename = function() {
      paste("chat-history-", Sys.Date(), ".json", sep="")
    },
    content = function(file) {
      chats <- dbGetQuery(db_conn, "
      SELECT c.*, m.* 
      FROM chats c 
      LEFT JOIN messages m ON c.chat_id = m.chat_id 
      WHERE c.user_id = ?
    ", params = list(auth0_user$sub))
      
      write_json(chats, file)
    }
  )
  
  
  observeEvent(input$view_all_chats, {
    updateTabItems(session, "sidebar", selected = "all_chats")
  })
  
  # Clean up
  session$onSessionEnded(function() {
    dbDisconnect(db_conn)
  })
  
}, info = a0_info)



# utils 


# Make sure to add to your library imports
library(shinyWidgets)
settingsModalUI <- function() {
  # Initial active section is "General"
  modalDialog(
    title = NULL,  # Remove default title for custom styling
    div(
      class = "clean-settings-container",
      # Custom Title with Save button
      div(
        class = "settings-header",
        h3("Settings", class = "settings-title"),
        div(
          class = "settings-header-actions",
          actionButton(
            "save_settings",
            "Save changes",
            class = "btn-primary btn-sm save-settings-btn"
          ),
          tags$button(
            onclick = "Shiny.setInputValue('close_settings', true, {priority: 'event'})",
            class = "close-btn",
            icon("times")
          )
        )
      ),
      # Layout container
      div(
        class = "settings-layout",
        # Sidebar
        div(
          class = "settings-sidebar",
          # Sidebar items
          actionButton(
            "nav_general",
            div(
              class = "sidebar-item active",
              icon("gear"),
              span("General")
            ),
            class = "sidebar-btn active"
          ),
          actionButton(
            "nav_model",
            div(
              class = "sidebar-item",
              icon("sliders-h"),
              span("Model Settings")
            ),
            class = "sidebar-btn"
          ),
          actionButton(
            "nav_data",
            div(
              class = "sidebar-item",
              icon("database"),
              span("Data Management")
            ),
            class = "sidebar-btn"
          )
        ),
        # Content sections
        div(
          class = "settings-content",
          # General Section
          div(
            id = "section_general",
            class = "settings-section active",
            style = "display: block;",
            div(
              class = "setting-row",
              span("Theme", class = "setting-label"),
              div(
                class = "setting-control",
                pickerInput(
                  "theme_setting",
                  NULL,
                  choices = c("System", "Light", "Dark"),
                  selected = "System",
                  width = "140px"
                )
              )
            ),
            div(
              class = "setting-row",
              materialSwitch(
                inputId = "dark_mode",
                label = "Dark Mode",
                value = FALSE,
                status = "primary",
                right = TRUE
              )
            )
          ),
          # Model Settings Section
          div(
            id = "section_model",
            class = "settings-section",
            style = "display: none;",
            div(
              class = "setting-row",
              span("Default Model", class = "setting-label"),
              div(
                class = "setting-control",
                pickerInput(
                  "default_model",
                  NULL,
                  choices = c(
                    "GPT-3.5" = "gpt-3.5-turbo",
                    "GPT-4" = "gpt-4",
                    "Claude" = "claude-3"
                  ),
                  selected = "gpt-3.5-turbo",
                  width = "140px"
                )
              )
            ),
            div(
              class = "setting-row",
              span("Default Temperature", class = "setting-label"),
              div(
                class = "setting-control",
                sliderInput(
                  "default_temperature",
                  NULL,
                  min = 0, max = 1,
                  value = 0.7,
                  step = 0.1,
                  width = "200px"
                )
              )
            )
          ),
          # Data Management Section
          div(
            id = "section_data",
            class = "settings-section",
            style = "display: none;",
            div(
              class = "setting-row border-top",
              span("Clear History", class = "setting-label"),
              div(
                class = "setting-control",
                actionButton(
                  "clear_history",
                  "Delete all chats",
                  class = "btn-danger btn-sm"
                )
              )
            )
          )
        )
      )
    ),
    size = "m",
    easyClose = TRUE
  )
}
library(bs4Dash)
library(auth0)
library(here)
library(waiter)
library(rintrojs)
library(DT)
library(websocket)
library(markdown)
library(commonmark)
library(htmltools)
library(RSQLite)
library(uuid)
library(shinyjs)

source("global.R")

ui <- auth0_ui(
  bs4DashPage(
    dark = NULL,
    help = NULL,
    fullscreen = TRUE,
    # header = NULL,
    # Header section remains the same
    header = bs4DashNavbar(
      title = dashboardBrand(
        title = "DSPA AI assistant",
        # color = "white",
        href = "#",
        image = "logo.png",
        opacity = 0.8
      ),
      # title = "DSPA AI assistant",
      border = FALSE,
      compact = TRUE,
      sidebarIcon = shiny::icon("sliders"),
      fixed = TRUE,
      rightUi = userOutput("user")
    ),
    
    

    # Sidebar section remains the same
    dashboardSidebar(
      status = "gray",
      # skin = "dark",
      width = "40vh",
      # bs4SidebarUserPanel("DSPA AI assistant", image = NULL),
      minified =  FALSE,
      flat = TRUE,
      elevation = 0,
      title = "DSPA ",
      collapsed = FALSE,
      opacity = 1,
      sidebarMenu(
        id = "sidebar",
        # Hidden chat tab for functionality
        menuItem(
          "Chat",
          tabName = "chat",
          selected = TRUE
        ),
        menuItem(
          "All Chats",
          tabName = "all_chats",
          icon = icon("list")
        ),
        
     
        
        div(
          # class = "p-3",
          tags$a(
            id = "new_chat",
            href = "#",
            class = "new-chat-link",
            icon("pen-to-square", class = "new-chat-icon"),  # This is cleaner than layered icons
            span("Start new chat", class = "new-chat-text"),
            icon("arrow-right", class = "new-chat-arrow")
          )
        ),
        
        # Recent chats section with heading
        div(
          # class = "p-3",
          h4("Recent Chats", class = "text-muted", style = "font-size: 0.9rem; margin-top: 15px; padding: 0 0 0 8px"),
          uiOutput("recent_chats_list"),
          # View All link
          tags$a(
            href = "#",
            onclick = "Shiny.setInputValue('view_all_chats', Math.random())",
            style = "display: block; padding: 8px 12px; color: #007bff; font-size: 0.9rem; text-decoration: none;",
            icon("arrow-right"), 
            "View all chats"
          )
        ),
        
        
        tags$hr(style = "border-top: 1px solid rgba(255,255,255,0.1); margin: 10px 15px;")
        
      )
    ),
    
    dashboardBody(
      useShinyjs(),  # Add this line
      # add_mathjax(),
      add_katex(),
      tags$head(
        
        # custom css
        tags$link(rel = "stylesheet", type = "text/css", href = "global.css"),
        
        # Third-party CSS
        tags$link(href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@400;500;600&family=Source+Serif+Pro:wght@400;600&display=swap", rel = "stylesheet"),
        tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github.min.css"),
        
        #custom javascript
        tags$script(src = "main.js"),
        
        # Third-party JavaScript
        tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.8/clipboard.min.js"),
        tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"),
        tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/python.min.js"),


      ),
      
      tabItems(
        # Chat tab
        tabItem(
          tabName = "chat",
          # Main full-width container
          div(
            style = "
                background-color: white; 
                height: calc(100vh - 50px);  /* Subtract header height */
                width: 100%;
                display: flex;
                flex-direction: column;
              ",
                      # Messages container with automatic height
                      div(
                        id = "chat_container",
                        style = "
                  flex-grow: 1;
                  overflow-y: auto;
                    ",
              # Previous messages from database
              div(id = "previous_messages", 
                  uiOutput("previous_messages")),
              # Current streaming message
              div(id = "current_message",
                  uiOutput("current_message"))
            ),
            # Input container fixed at bottom
            div(
              class = "input-container",
              # Main input area
              # div(
              #   class = "input-group",
              
              
                textAreaInput(
                  "user_input",
                  label = NULL,
                  placeholder = "Message DSPA AI assistant",
                  width = "100%",
                  resize = "none",
                  rows = 1
                # )
              ),
              # Bottom toolbar
              div(
                class = "input-toolbar",
                style = "display: flex; justify-content: space-between; align-items: center;",
                # Left side - attach and model
                div(
                  style = "display: flex; gap: 12px; align-items: center;",
                  actionButton(
                    "upload_file",
                    label = NULL,
                    icon = icon("paperclip"),
                    class = "btn-light btn-sm",
                    style = "padding: 4px 8px; border: none;"
                  ),
                  selectInput(
                    "chat_model",
                    label = NULL,
                    choices = c("GPT-3.5" = "gpt-3.5-turbo", 
                                "GPT-4" = "gpt-4",
                                "Claude" = "claude-3"),
                    width = "120px",
                    selectize = FALSE
                  )
                ),
                # Right side - send/stop button
                div(
                  id = "button_container",
                  style = "position: relative;",
                  actionButton(
                    "send",
                    label = NULL,
                    icon = icon("arrow-up"),
                    class = "btn-primary btn-sm",
                    style = "height: 32px; width: 32px; border-radius: 16px;"
                  ),
                  actionButton(
                    "stop_gen",
                    label = NULL,
                    icon = icon("stop"),
                    class = "btn-danger btn-sm",
                    style = "height: 32px; width: 32px; border-radius: 16px;  position: absolute; right: 0; top: 0;"
                  )
                )
              )
            )
          )
        ),
        
        # Settings tab (unchanged)
        # tabItem(
        #   tabName = "model_settings",
        #   fluidRow(
        #     column(
        #       width = 12,
        #       box(
        #         width = NULL,
        #         title = "Model Configuration",
        #         status = "primary",
        #         selectInput(
        #           "model",
        #           "Select Model",
        #           choices = c("GPT-3.5", "GPT-4", "Claude"),
        #           selected = "GPT-3.5"
        #         ),
        #         sliderInput(
        #           "temperature",
        #           "Temperature",
        #           min = 0,
        #           max = 1,
        #           value = 0.7,
        #           step = 0.1
        #         )
        #       )
        #     )
        #   )
        # ),
        
        # All chats history tab (unchanged)
        # Replace the current all_chats tabItem with:
        # Replace the current all_chats tabItem with:
        # Update the all_chats tabItem in ui.R
        tabItem(
          tabName = "all_chats",
          div(
            class = "history-container",
            # Search and header section
            div(
              class = "history-header",
              h2("Your chat history", class = "history-title"),
              
              div(
                class = "history-search-container",
                div(
                  class = "input-group",
                  div(
                    class = "input-group-prepend",
                    span(class = "input-group-text", icon("search"))
                  ),
                  tags$input(
                    type = "text",
                    id = "chat_search",
                    class = "form-control",
                    placeholder = "Search your chats..."
                  )
                ),
                uiOutput("chat_count_text")
                
              )
              
              
              
              
            ),
            # Chat list container
            div(
              class = "history-list-container",
              uiOutput("chat_list")
            )
          )
        )
        
        
        
      )
    )
  ),
  info = a0_info
)
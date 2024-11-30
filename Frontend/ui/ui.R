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

source(here("global.R"))

ui <- auth0_ui(
  bs4DashPage(
    dark = FALSE,
    help = FALSE,
    fullscreen = TRUE,
    # header = NULL,
    # Header section remains the same
    header = bs4DashNavbar(
      # title = dashboardBrand(
      #   title = "DSPA AI assistant",
      #   color = "white",
      #   href = "#",
      #   image = "logo.png",
      #   opacity = 0.8
      # ),
      title = "DSPA AI assistant",
      border = FALSE,
      compact = TRUE,
      sidebarIcon = shiny::icon("sliders"),
      fixed = TRUE,
      rightUi = userOutput("user")
    ),
    
    
    # dashboardHeader(title = "Basic dashboard",rightUi = userOutput("user")),
    
    
    # Sidebar section remains the same
    dashboardSidebar(
      status = "gray",
      # skin = "dark",
      width = "35vh",
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
        
        # New Chat button
        div(
          class = "p-3",
          actionButton(
            "new_chat",
            span(icon("plus"), "New Chat"),
            width = "100%",
            class = "btn-light",
            style = "text-align: left; padding: 10px 15px;"
          )
        ),
        
        # Recent chats section with heading
        div(
          class = "p-3",
          h6("Recent Chats", class = "text-muted", style = "font-size: 0.8rem; margin-bottom: 15px;"),
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
        
        
        tags$hr(style = "border-top: 1px solid rgba(255,255,255,0.1); margin: 10px 15px;"),
        
        menuItem(
          "Settings",
          tabName = "settings",
          icon = icon("gear"),
          menuSubItem(
            text = "Model Settings",
            tabName = "model_settings",
            icon = icon("sliders-h")
          )
        )
      )
    ),
    
    dashboardBody(
      useShinyjs(),  # Add this line
      # add_mathjax(),
      tags$head(
        tags$script("
          $(document).ready(function() {
            const textarea = document.getElementById('user_input');
            
            function autoResize() {
              textarea.style.height = 'auto';
              const maxHeight = 200;
              const scrollHeight = textarea.scrollHeight;
              textarea.style.height = Math.min(scrollHeight, maxHeight) + 'px';
              
              if (scrollHeight > maxHeight) {
                textarea.style.overflowY = 'auto';
              } else {
                textarea.style.overflowY = 'hidden';
              }
            }
            
            $('#user_input').on('input', function() { 
              autoResize();
            });
            
            // Initial resize
            autoResize();
          });
        "),
        tags$link(rel = "stylesheet", type = "text/css", href = "global.css"),
        tags$style(HTML("
          /* Container Styles */
           body .wrapper .content-wrapper > .content {
              padding: 0 !important;
           }
            .container-fluid{
              padding: 0 !important;
            }
            
            .main-sidebar {
              background-color: #f4f6f7 !important;
            }
            
            /* Also update sidebar-dark and nav-sidebar to ensure color consistency */
            .sidebar-dark-primary .nav-sidebar > .nav-item > .nav-link.active,
            .sidebar-light-primary .nav-sidebar > .nav-item > .nav-link.active {
              background-color: rgba(0,0,0,0.05) !important;
              color: #000 !important;
            }
            
            /* Change text color for better visibility on light background */
            .nav-sidebar .nav-item .nav-link {
              color: #000 !important;
            }
            
            /* Change hover state */
            .nav-sidebar .nav-item .nav-link:hover {
              background-color: rgba(0,0,0,0.05) !important;
            }
          .input-container {
            position: relative;
            bottom: 0;
            left: 0;
            right: 0;
            background: #e4eded;
            padding: 15px;
            max-width: 800px;  /* or your preferred width */
            left: 50%;
            transform: translateX(-50%);
            border-radius: 15px;
          }
          
          /* Ensure chat container doesn't overlap with input */
          #chat_container {
            flex-grow: 1;
            overflow-y: auto;
            padding-bottom: 120px; /* Space for input container */
          }
          
          /* Input Area Styles */
          .input-group {
            border: 1px solid #e0e0e0;
            border-radius: 8px;
            background: white;
          }
          
          #user_input {
            border: none;
            min-height: 24px;
            max-height: 200px;
            padding: 12px 16px;
            line-height: 1.5;
            border-radius: 8px;
          }
          
          /* Toolbar Styles */
          .input-toolbar {
            margin-top: 4px;
          }
          
          .input-toolbar select {
            border: none;
            background: transparent;
            font-size: 0.9rem;
            color: #666;
            cursor: pointer;
            padding: 4px 8px;
          }
          
          /* Button Styles */
          #button_container {
            display: inline-block;
          }
          
          #send, #stop_gen {
            padding: 0;
            display: flex;
            align-items: center;
            justify-content: center;
          }
          
          #stop_gen {
            width: 100%;
            height: 100%;
            z-index: 1000;
            display: none;
          }
          
          #stop_gen.showing {
            display: block;
          }
          
          /* Hide Menu Items */
          [data-value='chat'], [data-value='all_chats'] {
            display: none !important;
          }
          
          /* Chat Items in Sidebar */
          .chat-item {
            padding: 3px 12px;
            margin-bottom: 1px;
            border-radius: 5px;
            cursor: pointer;
            transition: background-color 0.2s;
            display: flex;
            align-items: center;
            gap: 10px;
          }
          
          .chat-item:hover {
            background-color: rgba(0,0,0,0.05);
          }
          
          .chat-item.active {
            background-color: rgba(0,123,255,0.1);
          }
          
          .chat-title {
            font-size: 0.9rem;
            white-space: nowrap;
            overflow: hidden;
            text-overflow: ellipsis;
          }
          
          /* Chat Messages - Base Styles */
        /* Chat Container */
#previous_messages, #current_message {
  max-width: 900px;  /* or your preferred width */
  margin: 0 auto;    /* center the container */
}

/* Chat Messages - Base Styles */
.chat-message {
  margin: 10px 0;
  padding: 10px 15px;
  max-width: 100%;
  word-wrap: break-word;
}

/* User Messages */
.user {
  margin-left: auto;
  margin-right: 0;
  background: #007bff;
  color: white;
  border-radius: 15px;
  max-width: 80%;
  width: fit-content;
  padding: 15px 20px;
}

/* Assistant Messages */
.assistant {
  margin: 0;
  width: 100%;
  background: white;
  color: #1a1a1a;
  border-radius: 0;
  border-bottom: 1px solid #f0f0f0;
  padding: 20px;
}

.assistant:last-child {
  border-bottom: none;
}

/* Message Components */
.message-timestamp {
  font-size: 0.8em;
  margin-top: 5px;
  text-align: right;
}

.user .message-timestamp {
  color: rgba(255,255,255,0.7);
}

.assistant .message-timestamp {
  color: rgba(0,0,0,0.5);
}

.assistant-name {
  font-weight: bold;
  margin-bottom: 5px;
  color: #1a1a1a;
}

/* Code Blocks */
.chat-message.assistant pre {
  background: #f1f3f4;
  border-radius: 4px;
  padding: 10px;
  overflow-x: auto;
  margin: 10px 0;
}

.chat-message.assistant code {
  background: #f1f3f4;
  padding: 2px 4px;
  border-radius: 3px;
  font-family: monospace;
}  
        "))
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
              div(
                class = "input-group",
                textAreaInput(
                  "user_input",
                  label = NULL,
                  placeholder = "Message DSPA AI assistant",
                  width = "100%",
                  resize = "none",
                  rows = 1
                )
              ),
              # Bottom toolbar
              div(
                class = "input-toolbar",
                style = "display: flex; justify-content: space-between; align-items: center; padding-top: 8px;",
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
                    style = "height: 32px; width: 32px; border-radius: 16px; display: none; position: absolute; right: 0; top: 0;"
                  )
                )
              )
            )
          )
        ),
        
        # Settings tab (unchanged)
        tabItem(
          tabName = "model_settings",
          fluidRow(
            column(
              width = 12,
              box(
                width = NULL,
                title = "Model Configuration",
                status = "primary",
                selectInput(
                  "model",
                  "Select Model",
                  choices = c("GPT-3.5", "GPT-4", "Claude"),
                  selected = "GPT-3.5"
                ),
                sliderInput(
                  "temperature",
                  "Temperature",
                  min = 0,
                  max = 1,
                  value = 0.7,
                  step = 0.1
                )
              )
            )
          )
        ),
        
        # All chats history tab (unchanged)
        tabItem(
          tabName = "all_chats",
          fluidRow(
            column(
              width = 12,
              box(
                width = NULL,
                title = "Chat History",
                status = "primary",
                div(
                  class = "chat-history-filters mb-3",
                  dateRangeInput(
                    "date_filter",
                    "Date Range",
                    start = Sys.Date() - 30,
                    end = Sys.Date()
                  ),
                  textInput(
                    "search_filter",
                    "Search",
                    placeholder = "Search in chat history..."
                  )
                ),
                dataTableOutput("chat_history_table")
              )
            )
          )
        )
      )
    )
  ),
  info = a0_info
)
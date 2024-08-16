source("global.R")

###################################### UI ###################################
# Initialize AI Bot UI defaulting to drawing a plot_ly() graph
#############################################################################
#' Application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @import shiny
#' @noRd
#' 

css <- sass(sass_file("www/chat.scss"))
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


app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    navbarPage("SOCR AI Bot", id = "tabs",
               theme = shinytheme("cerulean"),
               ## navbarMenu(span("Basic",title="Short description  for the tab" )),
               tabPanel(id="Basic", "Basic",
                        bsTooltip("Basic", title="Test Title", trigger = "hover"),
                        # navbarMenu(span("Basic",title="Short description  for the tab" )),
                        # span("Basic", title = "Basic", 
                        #      `data-toggle`="tooltip"),
                        # bsTooltip("Basic", "left"),
                        div(id = "load_message",
                            h2("Interrogate your data using AI"),
                            h3("Still being tested and improved. Loading ... ...")
                        ),
                        # uiOutput("use_heyshiny"), # For voice control
                        # move notifications and progress bar to the center of screen
                        tags$head(tags$style(HTML(".shiny-notification {
                  position:fixed;
                  top: calc(20%);
                  left: calc(50%);
                  }
                  "
                        ))),
                        
                        # Sidebar with a slider input for number of bins
                        sidebarLayout(
                          sidebarPanel(#uiOutput("timer_ui"),
                            fluidRow(column(width = 6, textOutput("selected_dataset")),
                                     column(width = 6,
                                            p(HTML("<div align=\"right\"> <A HREF=\"javascript:history.go(0)\">Reset</A></div>")))
                            ),
                            fluidRow(column(width = 6, uiOutput("demo_data_ui")),
                                     column(width = 6, uiOutput("data_upload_ui"))
                            ),
                            
                            uiOutput("prompt_ui"),
                            
                            tags$style(type = "text/css", "textarea {width:100%}"),
                            tags$textarea(
                              id = "input_text",
                              placeholder = NULL,
                              rows = 8, ""
                            ),
                            
                            fluidRow(
                              column(
                                width = 6,
                                actionButton("submit_button", strong("Submit")),
                                tags$head(tags$style(
                                  "#submit_button{font-size: 16px;color: red}"
                                )),
                                tippy::tippy_this(
                                  "submit_button",
                                  "ChatGPT can return different results for the same request.",
                                  theme = "light-border"
                                )
                              ),
                              column(
                                width = 6,
                                actionButton("api_button", strong("Settings")),
                                tags$head(tags$style(
                                  "#api_button{font-size: 16px;color: blue}"
                                )),
                                tippy::tippy_this(
                                  "api_button",
                                  "Some functions require personal AI-service Keys!",
                                  theme = "light-border"
                                )
                              )
                            ),
                            br(),
                            textOutput("usage"),
                            textOutput("total_cost"),
                            textOutput("temperature"),
                            br(),
                            textOutput("retry_on_error"),
                            # checkboxInput("Comments", "Help improve AI Bot"),
                            tags$style(type = "text/css", "textarea {width:100%}"),
                            tags$textarea(
                              id = "user_feedback",
                              placeholder = "Any questions? Suggestions? Things you like, don't like? Leave your email if you want to hear back from us.",
                              rows = 4,
                              ""
                            ),
                            radioButtons("helpfulness", "How useful is AI Bot?",
                                         c(
                                           "Not at all",
                                           "Slightly",
                                           "Helpful",
                                           "Extremely"
                                         ),
                                         selected = "Slightly"
                            ),
                            radioButtons("experience", "Your experience with R:",
                                         c(
                                           "None",
                                           "Beginner",
                                           "Intermediate",
                                           "Advanced"
                                         ),
                                         selected = "Beginner"
                            ),
                            actionButton("save_feedbck", "Save Feedback")
                          ),
                          
                          ###############################################################################
                          # Main
                          ###############################################################################
                          
                          mainPanel(
                            shinyjs::useShinyjs(),
                            conditionalPanel(
                              condition = "input.submit_button == 0",
                              uiOutput("AIBot_version_main"),
                              fluidRow(
                                column(
                                  width = 3,
                                  img(
                                    src = "https://wiki.socr.umich.edu/images/thumb/5/5e/SOCR_UMich_2020a.png/1025px-SOCR_UMich_2020a.png",
                                    width = "120",
                                    height = "108"
                                  ),
                                  align = 'left'
                                ),
                                column(
                                  width = 9,
                                  h4(
                                    "SOCR AI Bot provides a helpful human-machine interface.  
                                    It utilizes SOCR and various generative-AI (e.g., OpenAI) models
                                    trained on billions of pieces of information,
                                    thousands of books, millions of code repositories, and
                                    and innumerable web pages, written in dozens languages. 
                                    English is the default language, but other languages may also work."),
                                  align = 'left')
                              ),
                              hr(),
                              fluidRow(
                                h6(
                                  "The AI Bot first three tabs (synthetic text, code, and image generation)
                                    require users to input their private KEYs (in \"SETTINGS\") for external generative AI model (GAIM)
                                    services, such as OpenAI, PaLM, etc. Without importing their private keys
                                    these functions will not work, although the remaining SOCR AI Bot functions
                                    will be fully operational even withiut AI service keys."),
                                align = 'left'),
                              hr(),
                              # h3("Instructions:"),
                              accordion(
                                id = "BothInstructions",
                                accordionItem(
                                  title = "Instructions",
                                  status = "danger",
                                  collapsed = TRUE,
                                  tags$ul(
                                    tags$li(
                                      "Try simple experiments first before gradually adding complexity, 
                  specifying alternative plots, or chosing different models." #, style = "color:blue"
                                    ),
                                    tags$li(
                                      "AI Bot is a prototype release expanding OpenAI API, RTutor, SOCRAT, and other resources. 
                                       Its intended for simple and quick demonstrations, not heavy research, complicated studies,
                                       or high-throughput analytics."
                                    ),
                                    tags$li(
                                      "When using generative-AI functionality (e.g., to synthetically generate
                                       text, images of software code), you have to use your own personal OpenAI
                                       unique 51-character key."
                                    ),
                                    tags$li(
                                      "Experiment with parameter settings, e.g., increasing the \"Temperature\"
                                      setting may drive more aggressive AI predictions."
                                    ),
                                    tags$li(
                                      HTML(paste("Preprocess and prep the test data prior to loading it in the AI Bot. You can use ", 
                                                 tags$a(href="https://socr.umich.edu/HTML5/SOCRAT/", 
                                                        "SOCRAT's data wrangling functionality"),
                                                 "for preprocessing. AI Bot is intended as a hands-on 
                                                   demonstration using classical flat data-frame/tabular data 
                                                   files with rows and columns representing cases and features.",
                                                 sep = " "))
                                    ),
                                    tags$li(
                                      HTML(paste("", 
                                                 tags$a(href="https://socr.umich.edu/docs/uploads/2023/SOCR_Dataset_Gapminder.csv", 
                                                        "An example dataset for testing data uploading is available here"),
                                                 ".",
                                                 sep = ""))
                                    ),
                                    
                                    tags$li(
                                      "Confirm proper data type specifications: numeric columns and
                                      categories (factors or characters). The AI Bot can convert data types 
                                      subject to proper instructions, e.g., \"Convert \'variable1\' as 
                                      numeric\", or \"Convert \'variable2\' as factor\"."
                                    ),
                                    tags$li(
                                      "When outputting Rmd/HTML/PDF reports, the Bot prefixes all OpenAI requests with
                                      \"Generate R code\" and append by \"Use the df data frame\".
                                      Note that the default \"attitude\" dataset is numeric\". 
                                      If you have no data to plot, choose \"No data\" from the Data dropdown."
                                    ),
                                    tags$li(
                                      "Your data is processed locally on you rclient machine and SOCR server, 
                                      it's not sent to OpenAI cloud service. To ask generic questions without
                                      mentioning column names, briefly describe your data, especially 
                                      the relevant columns, just like when providing data-dictionary to an 
                                      expert consultant."
                                    ),
                                    tags$li(
                                      "Each chunk of code is run independently using the selected/uploaded data.
                                      If you want to build upon the current code,
                                      select the \"Continue from this chunk\" checkbox.
                                      Your current R code will be inserted and executed prior to your next 
                                      chunk. This is especially important for data wrangling when you 
                                      remove rows, add columns, or when first transforming and then modeling the data. 
                                      You can always go back to any previous chunks and continue from there."
                                    )
                                  )
                                )
                              )
                            ),
                            hr(),
                            conditionalPanel(
                              condition = "input.submit_button != 0",
                              fluidRow(
                                column(
                                  width = 4,
                                  selectInput(
                                    inputId = "selected_chunk",
                                    label = "AI generated code:",
                                    selected = NULL,
                                    choices = NULL
                                  )
                                ),
                                column(
                                  width = 4,
                                  style = "margin-top: 10px;",
                                  checkboxInput(
                                    inputId = "continue",
                                    label = "Continue from this chunk",
                                    value = FALSE
                                  ),
                                  tippy::tippy_this(
                                    "continue",
                                    "If selected, the current R scripts will be kept in the next questions. We build upon the code chunk.",
                                    theme = "light-border"
                                  )
                                )
                              ),
                              verbatimTextOutput("openAI"),
                              uiOutput("error_message"),
                              h4("Results:"),
                              
                              # shows error message in local machine, but not on the server
                              verbatimTextOutput("console_output"),
                              uiOutput("plot_ui"),
                              checkboxInput(
                                inputId = "make_ggplot_interactive",
                                label = NULL,
                                value = FALSE
                              ),
                              br(),
                              uiOutput("tips_interactive"),
                              hr(),
                            ),
                            verbatimTextOutput("data_structure"),
                            tableOutput("data_table")
                            
                            
                          )
                        )
               ), #tabPanel
               
               ## Synth Text Generation
               tabPanel(
                 title = "Synth Text",
                 value = "Synth Text",
                 img(
                   src = "https://wiki.socr.umich.edu/images/thumb/5/5e/SOCR_UMich_2020a.png/1025px-SOCR_UMich_2020a.png",
                   width = "200",
                   height = "200"
                 ),
                 br(), br(),
                 fluidRow(
                   column(
                     width = 7,
                     tags$style(type = "text/css", "textarea {width:100%}"),
                     tags$textarea(
                       id = "text_prompt",    # ask_question",
                       placeholder = NULL,
                       rows = 2,
                       ""
                     )
                   ),
                   column(
                     width = 5,
                     selectInput(
                       inputId = "demo_synth_prompt",
                       choices = demo_synth_prompts,   # demo_questions,
                       label = "Enter a prompt theme for synthetic text generation"
                     )
                   )
                 ),
                 
                 actionButton("prompt_button", strong("Generate Synth Text")),  # "ask_button"
                 br(),
                 hr(),
                 wellPanel(textOutput("synth_text")),  #"answer"
                 tags$head(
                   tags$style(
                     "#synth_text{
              color: blue;
              font-size: 16px
            }"
                   )
                 )
               ),
               
               ## Synth Image Generation
               tabPanel(
                 title = "Synth Images",
                 value = "Synth Images",
                 img(
                   src = "https://wiki.socr.umich.edu/images/thumb/5/5e/SOCR_UMich_2020a.png/1025px-SOCR_UMich_2020a.png",
                   width = "200",
                   height = "200"
                 ),
                 br(), br(),
                 fluidRow(
                   column(
                     width = 7,
                     tags$style(type = "text/css", "textarea {width:100%}"),
                     tags$textarea(
                       id = "image_prompt",    # text_prompt",
                       placeholder = NULL,
                       rows = 2,
                       ""
                     )
                   ),
                   column(
                     width = 5,
                     selectInput(
                       inputId = "img_prompt",
                       choices = img_prompts,   # demo_questions,
                       label = "Enter a prompt for synthetic IMAGE generation"
                     )
                   )
                 ),
                 
                 actionButton("image_button", strong("Generate Synth Image")),  # "ask_button"
                 br(),
                 hr(),
                 # have to use plotlyOutput() per
                 # https://stackoverflow.com/questions/57085342/renderplotly-does-not-work-despite-not-having-any-errors
                 wellPanel(plotlyOutput("synth_image"))  # ,  #"answer"
                 # tags$head(
                 #   tags$style(
                 #     "#synth_text{
                 #       color: blue;
                 #       font-size: 16px
                 #     }"
                 #   )
                 # )
               ),
               
               
               ## Data Tab 
               tabPanel(
                 title = "Data",
                 value = "Data",
                 textOutput("data_size"),
                 DT::dataTableOutput("data_table_DT")
               ),
               tabPanel(
                 title = "Report",
                 value = "Report",
                 br(),
                 selectInput(
                   inputId = "selected_chunk_report",
                   label = "Code chunks to include:",
                   selected = NULL,
                   choices = NULL,
                   multiple = TRUE
                 ),
                 fluidRow(
                   column(
                     width = 6,
                     uiOutput("html_report")
                   ),
                   column(
                     width = 6,
                     downloadButton(
                       outputId = "Rmd_source",
                       label = "RMarkdown"
                     ),
                     tippy::tippy_this(
                       "Rmd_source",
                       "Download a R Markdown source file.",
                       theme = "light-border"
                     )
                   )
                 ),
                 br(),
                 verbatimTextOutput("rmd_chunk_output")
               ),
               
               tabPanel(
                 title = "EDA",
                 value = "EDA",
                 tabsetPanel(
                   tabPanel(
                     title = "Basic",
                     verbatimTextOutput("data_summary")
                   ),
                   tabPanel(
                     title = "Summary",
                     verbatimTextOutput("dfSummary"),
                     h4(
                       "Generated by the ",
                       a(
                         "summarytools",
                         href="https://cran.r-project.org/web/packages/summarytools/vignettes/introduction.html",
                         target = "_blank"
                       ),
                       "package using the command:summarytools::dfSummary(df)."
                     )
                   ),
                   tabPanel(
                     title = "Table1",
                     uiOutput("table1_inputs"),
                     verbatimTextOutput("table1"),
                     h4(
                       "Generated by the CreateTableOne() function in the",
                       a(
                         "tableone",
                         href="https://cran.r-project.org/web/packages/tableone/vignettes/introduction.html",
                         target = "_blank"
                       ),
                       "package."
                     )
                   ),
                   tabPanel(
                     title = "Categorical",
                     h4(
                       "Categorical plots generated by \'plot_bar()\' in the package",
                       a(
                         "DataExplorer",
                         href="https://cran.r-project.org/web/packages/DataExplorer/vignettes/dataexplorer-intro.html",
                         target = "_blank"
                       ),
                       "."
                     ),
                     plotOutput("distribution_category")
                   ),
                   tabPanel(
                     title = "Numerical",
                     h4(
                       "Quantile-quantile plots, using \'plot_qq()\' in package ",
                       a(
                         "DataExplorer",
                         href="https://cran.r-project.org/web/packages/DataExplorer/vignettes/dataexplorer-intro.html",
                         target = "_blank"
                       ),
                       "."
                     ),
                     plotOutput("qq_numeric"),
                     h4(
                       "Histogram plots of several variables using \'plot_histogram()\' in package ",
                       a(
                         "DataExplorer",
                         href="https://cran.r-project.org/web/packages/DataExplorer/vignettes/dataexplorer-intro.html",
                         target = "_blank"
                       ),
                       "."
                     ),
                     plotOutput("distribution_numeric"),
                     
                   ),
                   tabPanel(
                     title = "Correlation",
                     h4(
                       "Correlation plot using \'corr_plot()\' in package ",
                       a(
                         "corrplot",
                         href="https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html",
                         target = "_blank"
                       ),
                       "."
                     ),
                     plotOutput("corr_map")
                   ),
                   tabPanel(
                     title = "GGpairs",
                     uiOutput("ggpairs_inputs"),
                     h4(""),
                     h4(
                       "Please be patient while the pairs plot is generated by \'ggpairs()\' in package",
                       a(
                         "GGally",
                         href="https://cran.r-project.org/web/packages/GGally/index.html",
                         target = "_blank"
                       ),
                       "."
                     ),
                     plotOutput("ggpairs")
                   )
                 )
               ),
               
               tabPanel(
                 title = "Ask",
                 value = "Ask",
                 img(
                   src = "https://wiki.socr.umich.edu/images/thumb/5/5e/SOCR_UMich_2020a.png/1025px-SOCR_UMich_2020a.png",
                   width = "344",
                   height = "309"
                 ),
                 br(), br(),
                 fluidRow(
                   column(
                     width = 7,
                     tags$style(type = "text/css", "textarea {width:100%}"),
                     tags$textarea(
                       id = "ask_question",
                       placeholder = NULL,
                       rows = 4,
                       ""
                     )
                   ),
                   column(
                     width = 5,
                     selectInput(
                       inputId = "model_choose",
                       choices = demo_model_choose,
                       label = NULL
                     )
                   ),
                   column(
                     width = 5,
                     selectInput(
                       inputId = "demo_question",
                       choices = demo_questions,
                       label = NULL
                     )
                   ),
                   br(),br(),br(),br(),
                   column(width = 7,
                          fileInput(
                            inputId = "user_image",
                            label = "Upload Image",
                            accept = c(
                              "image/png",
                              "image/jpeg"
                            )
                          )
                   )
                 ),
                 actionButton("ask_button", strong("Ask AI Bot")),
                 br(),
                 hr(),
                 wellPanel(uiOutput("answer")),
                 tags$head(
                   tags$style(
                     "#answer{
              color: purple;
              font-size: 16px
            }"
                   )
                 )
               ),
               tabPanel(
                 useWaiter(),
                 useShinyjs(),
                 use_copy(),
                 tags$head(tags$style(css)),
                 title = "Ai assistant",
                 value = "Ai assistant",
                 # img(
                 #   src = "https://wiki.socr.umich.edu/images/thumb/5/5e/SOCR_UMich_2020a.png/1025px-SOCR_UMich_2020a.png",
                 #   width = "200",
                 #   height = "200"
                 # ),
                 # br(), br(),
                 # fluidRow(
                   # column(
                     # width = 12,
                     tags$div(
                       id = "chat-container",
                       tags$div(
                         id = "chat-header",
                         # tags$img(src = "TnUa864.png", alt = "AI Profile Picture"),
                         tags$h3("SOCR AI Assistant")
                       ),
                       tags$div(
                         id = "chat-history",
                         uiOutput("chatThread"),
                       ),
                       tags$div(
                         id = "chat-input",
                         tags$form(
                           column(12,textAreaInput(inputId = "prompt", label="", placeholder = "Type your message here...", width = "100%")),
                           fluidRow(
                             tags$div(style = "margin-left: 1.5em;",
                                      actionButton(inputId = "submit",
                                                   label = "Send",
                                                   icon = icon("paper-plane")),
                                      actionButton(inputId = "remove_chatThread",
                                                   label = "Clear History",
                                                   icon = icon("trash-can")),
                                      CopyButton("clipbtn",
                                                 label = "Copy",
                                                 icon = icon("clipboard"),
                                                 text = ""),
                                      selectInput("model", "Model", choices = c("gpt-3.5-turbo", "gpt-4"), selected = "gpt-3.5-turbo")
                                      
                             ))
                         ))
                     ),
                   # ),
                   # column(
                   #   width = 0,
                   #   selectInput(
                   #     inputId = "demo_synth_prompt",
                   #     choices = demo_synth_prompts,   # demo_questions,
                   #     label = "Enter a prompt theme for synthetic text generation"
                   #   )
                   # )
                 # ),
                 
                 # actionButton("prompt_button", strong("Generate Synth Text")),  # "ask_button"
            br(),
                 hr(),
            #      wellPanel(textOutput("synth_text")),  #"answer"
            #      tags$head(
            #        tags$style(
            #          "#synth_text{
            #   color: blue;
            #   font-size: 16px
            # }"
            #        )
            #      )
               ),
               
               tabPanel(
                 title = "About",
                 value = "About",
                 uiOutput("AIBot_version"),
                 p("The SOCR AI Bot uses existing ",
                   a(
                     "SOCR/DSPA computational libraries",
                     href = "https://dspa2.predictive.space/",
                     target = "_blank"
                   ),
                   " and Generative artificial intelligence (gen-AI) interfaces, such as ",
                   a(
                     "OpenAI's",
                     href = "https://openai.com/",
                     target = "_blank"
                   ),
                   " powerful ",
                   language_model,
                   "language model, ",
                   " to translate natural language (human text/audio commands) to 
                   synthetically generate R code, draft text, simulate 2D images,
                   as well as, model, visualize, and analyze data.",
                   "You can request specific types of data analysis, or
                   use thematic text prompts to generate synthetic images or text.",
                   "Upload a data file (CSV, TSV/tab-delimited text files, and Excel) 
                   and just analyze it using plain human commands.
                   The results can be quickly downloaded as Rmd source-code or HTML reports."
                 ),
                 p("The first three tabs of the AI Bot (synthetic text, code, and image generation)
                    require users to input their private KEYs (in \"SETTINGS\") for external generative AI model (GAIM)
                    services, such as OpenAI, PaLM, etc. Without importing their private keys
                    these functions will not work, although the remaining SOCR AI Bot functions
                    will be fully operational even withiut AI service keys."),
                 p("The AI Bot comes with absolutely NO WARRANTY! Some scripts may yield incorrect results.
                    Please use the auto-generated code as a starting 
                    point for further refinement and validation.
                    The SOCR website and the 
                    source code (extending DSPA, OpenAI API, RTutor, and other CRAN libraries) 
                    is CC BY-NC 3.0 licensed and freely available for academic and 
                    non-profit organizations only. 
                    For commercial use beyond testing please contact ",
                   a(
                     "statistics@umich.edu.",
                     href = "mailto:statistics@umich.edu?Subject=SOCR AI Bot"
                   )
                 ),
                 
                 hr(),
                 p("The SOCR IA Bot project is developed by the ",
                   a(
                     "SOCR Team",
                     href = "https://socr.umich.edu/people/",
                     target = "_blank"
                   ),
                   " using existing open-science comunity-supported resources. 
                   All user feedback is valuable, please contact us via ",
                   a(
                     "SOCR Contact.",
                     href = "http://www.socr.umich.edu/html/SOCR_Contact.html",
                     target = "_blank"
                   ),
                   "The AI Bot source code is available at ",
                   a("GitHub,",
                     href = "https://github.com/SOCR"
                   ),
                   a(" RTutor ",
                     href = "https://github.com/gexijin/RTutor",
                     target = "_blank"
                   ), " and ",
                   a("CRAN.",
                     href = "https://cran.r-project.org/",
                     target = "_blank"
                   )
                 ),
                 
                 hr(),
                 accordion(
                   id = "Frequently_Asked_Questions",
                   accordionItem(
                     title = "Frequently Asked Questions",
                     status = "danger",
                     collapsed = TRUE,
                     tags$ul(
                       tags$li(HTML(paste("<b>What is the ",  
                                          tags$a(href="https://rcompute.nursing.umich.edu/SOCR_AI_Bot/", 
                                                 "SOCR AI Bot"), "?</b>", "<br /> &emsp;",
                                          "An artificial intelligence (AI)-based RShiny app facilitating 
                                   human-machine interactions, data-interrogation, modeling and analytics.
                                   Following a data upload or selection (or preloaded sets), users can ask
                                   questions about or analyze the data with text or verbal commands. 
                                   The app generates and runs R code to answer research questions 
                                   using exploratory (graphical) or analytical (quantitative) reports.",
                                          sep = ""))),
                       tags$li(HTML(paste("<b>How does AI Bot work?</b>", "<br /> &emsp;",
                                          "User requests are sent to SOCR RShiny server, and OpenAI’s AI service.
                                The returned objects include draft R code, text, images, and data that is
                                cleaned up and manipulated within the RShiny app, along with appropriate messages.
                                Multiple requests are logged to produce (on-demand) an R Markdown file, which can be
                                knitted into an HTML report. This enables record keeping, scientific
                                reproducibility, and independent validation.",
                                          sep = ""))),
                       tags$li(HTML(paste("<b>Can people without R coding experience use AI Bot for data analysis?</b>",
                                          "<br /> &emsp;",
                                          "Some prior coding experience may be useful as AI-generated code may be incomplete. 
                                    AI Bot may be used for quick EDA.",
                                          sep = ""))),
                       tags$li(HTML(paste("<b>Who is this App for?</b>",
                                          "<br /> &emsp;",
                                          "The primary goal is to help people with some R experience to learn
                                   R or be more productive. AI Bot can be used to quickly speed up the
                                   coding process using R. It gives you a draft code to test and
                                   refine. Be wary of bugs and errors.",
                                          sep = ""))),
                       tags$li(HTML(paste("<b>How do you make sure the results are correct?</b>",
                                          "<br /> &emsp;",
                                          "Try to word your question differently. And try
                                  the same request several times. A higher temperature parameter will give
                                  diverse choices. Then users can double-check to see
                                  if you get the same results from different runs.",
                                          sep = ""))),
                       tags$li(HTML(paste("<b>Use AI Bot to complete R coding homeworks?</b>",
                                          "<br /> &emsp;",
                                          "This is an example of an inappropriate (unethical) AI use. ",
                                          sep = ""))),
                       tags$li(HTML(paste("<b>Can for-profit organizations and private companies use AI Bot?</b>",
                                          "<br /> &emsp;",
                                          "No. It can be tried as a demo. AI Both website
                                  and source code are freely available for non-profit organizations
                                  only and distributed using the CC NC 3.0 license. ",
                                          sep = ""))),
                       tags$li(HTML(paste("<b>Can you run AI Both locally?</b>",
                                          "<br /> &emsp;",
                                          "Yes, download the R package and install the source code locally.
                                  You still will need to obtain an API key from OpenAI.",
                                          sep = ""))),
                       tags$li(HTML(paste("<b>Why do I get different results with the same request?</b>",
                                          "<br /> &emsp;",
                                          "Just like two human experts often generate different predictions,
                                  AI systems generate stochastic results with natural variability,
                                  the extent of the variation is controlled by hyper-parameters,
                                  e.g.,  \"temperature\" defined in the \"Settings\".",
                                          sep = ""))),
                       tags$li(HTML(paste("<b>Are there costs associated with SOCR AI Bot & OpenAI’s use?</b>",
                                          "<br /> &emsp;",
                                          "Generative-AI services like OpenAI service costs ~$0.01 to ~$0.1, for about 10 to 50 requests.<br />
                                    All SOCR Resources, including the AI Bot, are free to use. <br />
                                    The SOCR AI Bot has a monthly usage limit for Generative-AI, 
                                    which is restricted to University of Michigan
                                    users. Exhausting the limit (exceeding the monthly cap) will
                                    stop servicing queries until the next month resets.
                                    Outside University of Michigan users should enter and use their own API keys. ",
                                          sep = ""))),
                       tags$li(HTML(paste("<b>Can AI replace human experts?</b>",
                                          "<br /> &emsp;",
                                          "No. AI Bot intends to complement experts and optimize efficiency. ",
                                          sep = ""))),
                       tags$li(HTML(paste("<b>How to write effectively AI prompts to solicit rational generative responses?</b>",
                                          "<br /> &emsp;",
                                          "Write brief, concise, informative, and to the point queries.",
                                          sep = ""))),
                       tags$li(HTML(paste("<b>Can I install or load an additional R package inside AI generated code?</b>",
                                          "<br /> &emsp;",
                                          "No, there are over 20K R packages, over 600 of these are already
                                        included as dependencies in the SOCR AI Bot.  In addition, the SOCR
                                        RCompute server already has a large number of packages installed. ",
                                          sep = ""))),
                       tags$li(HTML(paste("<b>Is there a limit on the max size of data uploads?</b>",
                                          "<br /> &emsp;",
                                          "Yes, limit all data uploads to 10MB, otherwise subset your data.
                                        Upload it to the site to get the code, which can be run locally on your
                                        client machine. Alternatively, download the AI Bot R package, and run it locally. ",
                                          sep = ""))),
                       tags$li(HTML(paste("<b>What to make of errors, busy server notices, or unresponsive website reports?</b>",
                                          "<br /> &emsp;",
                                          "Try starting the AI Bot in a new browser window or restart the entire browser.",
                                          sep = ""))),
                       tags$li(HTML(paste("<b>What to do if voice commands do not work?</b>",
                                          "<br /> &emsp;",
                                          "By default, voice commands are disabled. <br />
                                        When voice commands are enabled, but not functioning properly, 
                                        check that your browser allows access to your microphone from the AI Bot website.
                                        Make sure you access the site using ",
                                          a("https://rcompute.nursing.umich.edu/SOCR_AI_Bot/ ",
                                            href = "https://rcompute.nursing.umich.edu/SOCR_AI_Bot/"
                                          ),
                                          ". With (insecure) \'http\' protocol, microphone access is automatically
                                                   blocked in many browsers. Make sure mic is working and speak closer
                                        to the mic and confirm that only one browser tab uses the mic. ",
                                          sep = "")))
                     )
                   )
                 ),
                 
                 hr(),
                 accordion(
                   id = "Update_log",
                   accordionItem(
                     title = "Update log",
                     status = "danger",
                     collapsed = TRUE,
                     tags$ul(
                       tags$li("V2.5 5/22/2024. fixed datasets, organized frontend & backend structure"),
                       tags$li("V2.4 4/28/2024. updated UI/UX, integrated Google Gemini its related contents"),
                       tags$li("V2.3 3/13/2024. updated API invocation syntax to adopt the latest OpenAI interface"),
                       tags$li("V2.0 7/05/2023. updated UI, fixed model reference problems, updated content and appearance"),
                       tags$li("V1.5 2/23/2023. updated UX, disabled voice commands, paired down on expensive models"),
                       tags$li("V1.4 1/26/2023. additional features"),
                       tags$li("V1.3 1/23/2023. correction of typos and inconsistencies"),
                       tags$li("V1.2 1/23/2023. enhancements"),
                       tags$li("V1.1 1/22/2023. initial deployment")
                     )
                   )
                 ),
                 
                 hr(),
                 
                 # accordion(
                 #   id = "Package List",
                 #   accordionItem(
                 #     title = "Package List",
                 #     status = "danger",
                 #     collapsed = TRUE,
                 #     tags$h4( uiOutput("package_list")
                 #     )
                 #   )
                 # ),
                 # 
                 uiOutput("package_list"),
                 
                 hr(),
                 
                 # accordion(
                 #   id = "Session Info",
                 #   accordionItem(
                 #     title = "Session Info",
                 #     status = "danger",
                 #     collapsed = TRUE,
                 #     tags$div(uiOutput("session_info"))
                 #   )
                 # )
                 
                 hr(),
                 # uiOutput("session_info")
                 accordion(
                   id = "sess_info",
                   accordionItem(
                     title = "Session Information",
                     status = "danger",
                     collapsed = TRUE,
                     tags$ul(
                       tags$li(uiOutput("session_info"))
                     )
                   )
                 )
               ),
               
               tabPanel(
                 title = "TESTING",
                 value = "TESTING",
                 p("This is a testing tab for development purposes.")
               ),
    ),
    
    #tags$head(includeHTML(app_sys("app", "www", "ga.html")))
    #,tags$head(includeScript(app_sys("app", "www", "ga.js")))
    #,
    # ----------------------- Footer ----------------------- #
    tags$footer(
      div(shinyUI(bootstrapPage(div(
        # include The SOCR footer HTML
        includeHTML("./www/SOCR_footer.html")
      )))),
      div(paste("AI Bot Version", release),
          align = 'center'
      ),
    )
  )
}

output$AIBot_version <- renderUI({
  h4(paste("AI Bot Version", release))
})

output$AIBot_version_main <- renderUI({
  tagList(
    h3(paste("SOCR AI Bot (v.", release, ")")),
    h4(paste("Statistics Online Computational Resource"))
  )
})

output$package_list <- renderUI({
  all <- .packages(all.available = TRUE)
  all <- sapply(
    all,
    function(x) paste(x, paste0(packageVersion(x), collapse = "."))
  )
  all <- unname(all)
  #all <- c("", all)
  
  selectInput(
    inputId = "installed_packages",
    label = paste0(
      "List of installed AI Bot Server packages ( ",
      length(all),
      " total)"
    ),
    width = '50%',
    choices = all,
    selected = NULL
  )
})

output$session_info <- renderUI({
  i <- c("<h4>R session info </h4>")
  i <- c(i, capture.output(sessionInfo()))
  HTML(paste(i, collapse = "<br/>"))
})


contribute_data <- reactive({
  save_info <- TRUE #default
  if(!is.null(input$contribute_data)) {
    save_info <- input$contribute_data
  }
  return(save_info)
})

# save user data when allowed
observeEvent(input$submit_button, {
  req(openAI_prompt())
  req(logs$code)
  
  if(contribute_data()) {
    # remove user data, only keep column names and data type
    txt <- capture.output(str(current_data(), vec.len = 0))
    txt <- gsub(" levels .*$", " levels", txt)
    try(
      save_data(
        date = Sys.Date(),
        time = format(Sys.time(), "%H:%M:%S"),
        request = openAI_prompt(),
        code = logs$code,
        error_status = code_error(),  # 1 --> error!  0 --> no error, success!!
        data_str = paste(txt, collapse = "\n"),
        dataset = input$select_data,
        session = session$token,
        filename = ifelse(is.null(input$user_file[1, 1]), " ", input$user_file[1, 1]),
        filesize = ifelse(is.null(input$user_file[1, 2]), " ", input$user_file[1, 2]),
        chunk = counter$requests,
        api_time = counter$time,
        tokens = counter$tokens_current
      )
    )
  }
})

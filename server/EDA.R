output$dfSummary <- renderText({
  req(current_data())
  res <- capture.output(summarytools::dfSummary(current_data()))
  res <- paste(res, collapse = "\n")
  return(res)
})

output$table1_inputs <- renderUI({
  req(ggpairs_data())
  df <- ggpairs_data()
  selectInput(
    inputId = "table1_strata",
    label = "Select a category for strata",
    choices = colnames(df)[!sapply(df, is.numeric)],
    multiple = FALSE
  )
})

output$table1 <- renderText({
  req(ggpairs_data())
  df <- ggpairs_data()
  req(input$table1_strata)
  options(width = 3000)
  withProgress(message = "Calculating table1 ...", {
    incProgress(0.3)
    ix <- match(input$table1_strata, colnames(df))
    res <- capture.output(
      tableone::CreateTableOne(
        vars = colnames(df)[-ix],
        data = df,
        strata = input$table1_strata
      )
    )
    res <- paste(res, collapse = "\n")
  })
  return(res)
})

output$distribution_category <- renderPlot({
  withProgress(message = "Barplots of categorical variables ...", {
    incProgress(0.3)
    DataExplorer::plot_bar(current_data())
  })
}
# ,
# width = 800,
# height = 800
)

output$distribution_numeric <- renderPlot({
  withProgress(message = "Creating histograms ...", {
    incProgress(0.3)
    DataExplorer::plot_histogram(current_data())
  })
})

output$qq_numeric <- renderPlot({
  withProgress(message = "Generating QQ plots ...", {
    incProgress(0.3)
    DataExplorer::plot_qq(current_data())
  })
})

output$corr_map <- renderPlot({
  withProgress(message = "Generating correlation map ...", {
    incProgress(0.3)
    #GGally::ggpairs(current_data())
    df <- current_data()
    df <- df[, sapply(df, is.numeric)]
    M <- cor(df)
    testRes <- corrplot::cor.mtest(df, conf.level = 0.95)
    corrplot::corrplot(
      M,
      p.mat = testRes$p,
      method = 'circle',
      type = 'lower',
      insig = 'blank',
      addCoef.col = 'black',
      number.cex = 0.8,
      order = 'AOE',
      diag = FALSE
    )
  })
})

ggpairs_data <- reactive({
  df <- current_data()
  cat_variables <- colnames(df)[!sapply(df, is.numeric)]
  # ggpairs does not tolerate variables with too many levels
  for (v in cat_variables) {
    counts <- sort(table(df[, v]), decreasing = TRUE)
    # more than 12 levels?
    if (length(counts) > max_levels) {
      # if the top 12 levels represent more than 30% of the observations
      if (sum(counts[1:max_levels]) / dim(df)[1] > 0.30) {
        
        df[, v] <- unlist(
          sapply(
            1:dim(df)[1],
            function(x) {
              if (df[x, v] %in% names(counts)[1:max_levels]) {
                return(df[x, v])
              } else {
                return("Other")
              }
            }
          )
        )
      } else {
        # too many levels, remove this column. Likely names
        df <- df[, !(colnames(df) %in% v)]
      }
    }
  }
  return(df)
})

output$ggpairs_inputs <- renderUI({
  req(ggpairs_data())
  df <- ggpairs_data()
  selected <- colnames(df)
  if(length(selected) > 3) {
    selected <- sample(selected, 3)
  }
  tagList(
    fluidRow(
      column(
        width = 6,
        selectInput(
          inputId = "ggpairs_variables",
          label = "Select variables",
          choices = colnames(df),
          multiple = TRUE,
          selected = selected
        )
        
      ),
      column(
        width = 6,
        selectInput(
          inputId = "ggpairs_variables_color",
          label = "Select a category for coloring",
          choices = colnames(df)[!sapply(df, is.numeric)],
          multiple = FALSE
        )
      )
    )
  )
  
})

output$ggpairs <- renderPlot({
  req(input$ggpairs_variables)
  #req(input$ggpairs_variables_color)
  req(length(input$ggpairs_variables) > 0)
  req(ggpairs_data())
  
  withProgress(message = "Running ggpairs ...", {
    incProgress(0.3)
    df <- as.data.frame(ggpairs_data())
    if(input$ggpairs_variables_color != "") {
      GGally::ggpairs(
        df[, input$ggpairs_variables],
        mapping = aes(
          color = df[, input$ggpairs_variables_color],
          alpha = 0.5
        )
      )
      
    } else {  # no color
      GGally::ggpairs(
        df[, input$ggpairs_variables]
      )
    }
  })
}
# ,
# width = 1200,
# height = 1200
)

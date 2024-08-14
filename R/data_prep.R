###################################################
# prepare a list of available R data sets
###################################################

###################################################
# Global variables
###################################################

###################################################
# https://platform.openai.com/docs/model-index-for-researchers
###################################################
library(xts)


release <- "2.5" # SOCR AI Bot
uploaded_data <- "User Upload" # used for drop down
no_data <- "No data (examples)" # no data is uploaded or selected
rna_seq <- "RNA-Seq"  # RNA-Seq read counts
min_query_length <- 5  # minimum # of characters
max_query_length <- 300 # max # of characters
# language_model <- "code-davinci-002" # this is the free model; paid model is "text-davinci-003"
# language_model <- "text-davinci-003" # this is the payed model (too expensive)
language_model <- "gpt-3.5-turbo" # this is the newest model, March 2023

default_temperature <- 0.1
pre_text <- "Generate R code. "
after_text <- " Use the df data frame. "
max_char_question <- 280 # max n. of characters in the Q&A
max_levels <- 12 # max number of levels in categorical variable for EDA, ggairs
max_data_points <- 10000  # max number of data points for interactive plot
# if a column is numeric but only have a few unique values, treat as categorical
unique_ratio <- 0.1   # number of unique values / total # of rows
sqlitePath <- "./data/usage_data.db" # folder to store the user queries, generated R code, and running results
sqltable <- "usage"

# if this file exists, run on the server, otherwise run locally.
# this is used to change app behavior.
on_server <- "on_server.txt"


#' Move an element to the front of a vector
#'
#' The response from GPT3 sometimes contains strings that are not R commands.
#'
#' @param v is the vector
#' @param e is the element
#'
#' @return Returns a reordered vector
move_front <- function(v, e){
  ix <- which(v == e)
  
  # if found, move to the beginning.
  if(length(ix) != 0) {
    v <- v[-ix]
    v <- c(e, v)
  }
  return(v)
}


convert_ts_to_df <- function(dat){
  if(is.ts(dat)){
    datx <- as.xts(dat)
    return(data.frame(date = index(datx), coredata(datx)))
  }
  else{
    return(dat)
  }
}


# prepare a list of available data sets.
datasets <- as.character(data()$results[, 3])   # 4] # name of datasets

datasets <- gsub(" .*", "", datasets) #however, changes name of the dataset
datasets <- gsub("ability.cov", "ability.cov$cov", datasets) #exception
datasets <- sort(datasets)

# append a dummy value, used when user upload their data.
datasets <- c(datasets, uploaded_data)
# move it to 2nd place
datasets <- move_front(datasets, uploaded_data)

# append a dummy value, used when user do not use any data
datasets <- c(datasets, rna_seq)
# move it to 2nd place
datasets <- move_front(datasets, rna_seq)

# append a dummy value, used when user do not use any data
datasets <- c(datasets, no_data)
# move it to 2nd place
datasets <- move_front(datasets, no_data)




# demo requests for mpg dataset
demos_mpg <- c(
  ' ... ' = 'Example requests',
  
  'Boxplot, ggplot2' = "Use ggplot2 to create a boxplot of hwy vs. class. 
 Color by class. 
 Add jitter.",
  
  'Distribution (D): Numers' = 'Show me the distribution of hwy.',
  "D: normality" = "Is cty normally distributed?",
  'D: Categories' = 'Show me the distribution of class as a barchart.',
  "D: Categories, pie" = "Create an pie chart based on  class.",
  
  'Relationship (R): Numbers-numbers' = "Show me the relationship between hwy and cty.",
  "R: Refined scatter plot" = "Use ggplot2. Plot hwy vs. cty, colored by class. 
Change shape by drv. Change size by displ. 
Change x label to 'Highway mpg'. 
Change y label to 'City mpg'. 
Change background to white. 
Increase font for labels to 15. 
Remove all grids.",
  'R: Correlation coefficient' = "Calculate the correlation coefficient of cty vs hwy. Repeat that after log transformation. Collect these results and show them.",
  'R: Numbers-categories' = "Show me the relationship between hwy and class.",
  "R: Numbers-categories, density" = "Only keep 4, 6, and 8 cylinders. Create a density plot of cty, colored by year. Split into panels with one column  by cyl.",
  'R: Numbers-categories, violin' = "Create a violin plot of cty vs. class. Color by class. Add data points with jitter.",
  
  "R: Category-category" = "Are drv and cyl independent?",
  "R: Category-category, plot" = "Plot the combinations of drv and cyl.",
  "R: Category-category, mosaic" = "Plot the combinations of drv and cyl as a mosaic plot.",
  
  "Multivariate (M): Correlation heatmap" = "Create a correlation map of all the columns that contain numbers.",
  "M: Hierarchical clustering" = "Conduct hierarchical clustering. ",
  'M: ANOVA' = "Conduct ANOVA of log-transformed hwy by class and drv.",
  'M: Regression' = "Build a regression model of hwy based on cyl, displ, drv, and class. 
Give me diagnostic plots.",
  "M: Neural network" = "Build a neural network model to predict  
hwy based on displ, cyl, and class.   
Use the nnet package. Plot the distribution of residuals.",
  
  "Data analysis" = "Calculate average cty by year and class. Then use ggplot2 to create a barplot of average mpg by class, colored by year. The bars for different years should be side by side.",
  "Convert data types" = "Convert cyl as numeric and calculate its correlation coefficient with hwy.",
  "Data processing" = "hwy and cty represent miles per gallon (MPG) on the highway and in the city, respectively. 
Only keep cars more efficient than 15 MPG, but less than 40, on the highway. 
Add 0.5 to city MPG for correction. 
Perform log transformation on city MPG. 
Raise highway MPG to the second power. 
Calculate correlation coefficient of  the two transformed variables.",
  "Data wrangling, base R" = "The dataset contains information about cars. Remove everything after \"(\" in trans column. Remove cars labeled as 2seater in class. Define a new column called ratio by dividing hwy by cty. Use ggplot2 to plot ratio vs. hwy. Color by class. Change marker type by trans. Change marker size by cyl.",
  "Data wrangling, dplyr" = "The dataset contains various about cars. First, use dplyr to prepare the data. Remove everything after \"(\" in trans column. Remove cars labeled as 2seater in class. Define a new column called ratio by dividing hwy by cty. Sort the cars by ratio, highest first. Second, use ggplot2 and the new data to plot ratio vs. hwy. Color by class. Change marker type by trans. Change marker size by cyl.",
  'Interactive plots' = "Plot hwy vs. displ group by cyl. Make it interactive with ggplotly.",
  
  'Chinese, 中文' = "按class画hwy的箱线图。按class更改颜色。添加抖动。",
  'Spanish, En español' = "Cree un diagrama de caja de hwy por class. Cambia de color por class. Añade nerviosismo.",
  'Japanese, 日本語' = "classごとに hwy の箱ひげ図を作成します。classごとに色を変えます。ジッターを追加します。",
  'German, deutsche Sprache' = "Verwenden Sie ggplot2, um einen Boxplot von hwy nach class zu erstellen. Farbe um class ändern. Jitter-Punkte hinzufügen",
  'French, Français' = "Créez une boîte à moustaches de hwy par class. Changer de couleur par class. Ajoutez des points de gigue.",
  'Italian, Italiano' = "Crea un boxplot di hwy di class. Varia colore di class. Aggiungi punti di jitter.",
  'Hindi, हिन्दी भाषा' = "class द्वारा hwy का बॉक्सप्लॉट बनाने के लिए ggplot2 का उपयोग करें। class द्वारा भिन्न रंग। जिटर पॉइंट जोड़ें।"
)

# demo requests when no dataset is selected.
demos_no_data <- c(
  'Random numbers' = "Generate 100 random numbers. Plot their distribution.",
  'Hierarchical tree' = "Provide a demo for hierarchical clustering tree.",
  'Heat map' = "Create a heatmap with hierarchical clustering tree.",
  "Ridge regression" = "Provide a demo for ridge regression.",
  'PCA' = "Create a matrix with 100 columns and 20 rows. Fill it with random numbers from the normal distribution. 
The mean is 1 for the first 10 rows, but 3 for the rest. 
Conduct PCA. Plot using the first two principal components.",
  'Map' = "Create an world map. ",
  'Map, US' = "Create a US map.",
  'Bioinformatics, genes' = "Use the biomaRt package to retrieve all human genes on Chr.Y.",
  'Bioinformatics, position' = "Use the biomaRt package to retrieve the gene symbol, the start and end position of all human genes on Chr.Y.",
  'Bioinformatics, length' = "Use the biomaRt package to retrieve the gene symbol, the start and end position of all human genes on Chr.Y. Calculate the length as the absolute difference between the start and end positions. Create a density plot of the length after log10 transformation.",
  'Financial, stocks' = "Retrieve and plot the stock price of Apple in 2022. Add 20 day moving average."
)


demos_diamond <- c(
  'Distribution, cut' = "Plot the distribution of cut using a pie chart.",
  'Distribution, price' = "Plot the distribution of price after log transformation.",
  'Scatter, price vs. carat' = "Plot price vs. carat. Change color by clarity. ",
  'Combinations' = "Plot the combinations of cut and clarity.",
  'Modelling' = "Remove diamonds larger than 3 carat. 
Build a model of price vs. carat. 
Create a violin plot of the residuals by clarity. 
Limit ploting to -10000 to 10000."
  
)

demos_rna_seq <- c(
  'Total Counts' = "Plot the column sums.",
  'Boxplot, raw' = "Create a bloxplot of all columns.",
  'Boxplot, log' = "Add 1 to all numbers and then conduct log transformation. Create a bloxplot of all columns.",
  'Heatmap of variable genes' = "Each row represents a gene. Each column is a sample. Remove genes with sum less than 10. Add 1 to all numbers. Log transform using base 2. Rank genes by standard deviations in descending order. Convert data as matrix. Subtract row means from all rows. Create a heatmap of the top 50 genes using red and green colors.",
  'DESeq2' = "Each row represents a gene. Each column is a sample. Remove rows with sum less than 10. Then use DESeq2 to identify differentially expressed genes. The first three columns are control samples. The last three are mutant. Show me the numbers of up and down-regulated genes based on FDR < 0.05 and log fold change > 1 or less than -1."
)

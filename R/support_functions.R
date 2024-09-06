###################################################
# Supporting functions
###################################################

#' Clean up API key character
#'
#' The response from GPT3 sometimes contains strings that are not R commands.
#'
#' @param api_key is a character string
#'
#' @return Returns a string with api key.


clean_api_key <- function(api_key) {
  # remove spaces
  api_key <- gsub(" ", "", api_key)
  return(api_key)
}


#' Validate API key character
#'
#' The response from GPT3 sometimes contains strings that are not R commands.
#'
#' @param api_key is a character string
#'
#' @return Returns TRUE or FALSE
validate_api_key_openAI <- function(api_key) {
  valid <- TRUE
  # if 51 characters, use the one in the file
  if (nchar(api_key) == 0) {
    valid <- FALSE
  }
  return(valid)
}

validate_api_key_gemini <- function(api_key) {
  valid <- TRUE
  # if 51 characters, use the one in the file
  if (nchar(api_key) != 39) {
    valid <- FALSE
  }
  return(valid)
}

###################################################
# get API key from environment variable
###################################################
api_key_global_openAI <- Sys.getenv("OPEN_API_KEY")

key_source <- "from OS environment variable."
# If there is an key file in the current folder, use that instead.
if (file.exists(file.path(getwd(), "api_key_openAI.txt"))) {
  api_key_file_openAI <- readLines(file.path(getwd(), "api_key_openAI.txt"))
  api_key_openAI <- clean_api_key(api_key_file_openAI)
  
  # if valid, replace with file
  if(validate_api_key_openAI(api_key_file_openAI)) {
    api_key_global_openAI <- api_key_file_openAI
    key_source <- "from file."
  }
}

if (file.exists(file.path(getwd(), "api_key_gemini.txt"))) {
  api_key_file_gemini <- readLines(file.path(getwd(), "api_key_gemini.txt"))
  api_key_gemini <- api_key_file_gemini
  
  # if valid, replace with file
  if(validate_api_key_gemini(api_key_file_gemini)) {
    api_key_global_gemini <- api_key_file_gemini
    key_source <- "from file."
  }
}


#' Returns true only defined and has a value of true
#'
#' This is used when some the input variables are not defined globally,
#' But could be turned on. if you use if(input$selected), it will
#' give an error.
#'
#' @param x
#'
#' @return Returns TRUE or FALSE
turned_on <- function(x) {
  
  # length = 0 when NULL, length = 3 if vector
  if (length(x) != 1) {
    return(FALSE)
  } else {
    
    # contain logical value?
    if (!is.logical(x)) {
      return(FALSE)
    } else {
      # return the logical value.
      return(x)
    }
  }
}


#' Returns a data frame with some numeric columns with fewer levels 
#' converted as factors
#'
#'
#' @param df a data frame
#' @param max_levels_factor  max levels, defaults to max_levels
#' @param max_proportion_factor max proportion
#'
#' @return Returns a data frame
numeric_to_factor <- function(df, max_levels_factor, max_proptortion_factor) {
  # some columns looks like numbers but have few levels
  # convert these to factors
  convert_index <- sapply(
    df,
    function(x) {
      if (
        is.numeric(x) &&
        # if there are few unique values compared to total values
        length(unique(x)) / length(x) < max_proptortion_factor &&
        length(unique(x)) <= max_levels_factor  # less than 12 unique values
        # relcassify numeric variable as categorical
      ) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  )
  
  convert_var <- colnames(df)[convert_index]
  for (var in convert_var) {
    eval(
      parse(   #df$cyl <- as.factor(df$cyl)
        text = paste0("df$", var, " <- as.factor(df$", var, ")")
      )
    )
  }
  return(df)
  
}


#' Prepare User input.
#'
#' The response from GPT3 sometimes contains strings that are not R commands.
#'
#' @param txt A string that stores the user input.
#' @param selected_data Name of the dataset.
#' @param df the data frame
#'
#' @return Returns a cleaned up version, so that it could be sent to GPT.
prep_input <- function(txt, selected_data, df) {
  
  if(is.null(txt) || is.null(selected_data)) {
    return(NULL)
  } 
  # if too short, do not send. 
  if(nchar(txt) < min_query_length || nchar(txt) > max_query_length) {
    return(NULL)
  }
  
  # remove extra space at the end.
  txt <- gsub(" *$|\n*$", "", txt)
  # some times it is like " \n "
  txt <- gsub(" *$|\n*$", "", txt)
  # if last character is not a period. Add it. Otherwise, 
  # Davinci will try to complete a sentence.
  if (!grepl("\\.$", txt)) {
    txt <- paste(txt, ".", sep = "")
  }
  
  if (!is.null(selected_data)) {
    if (selected_data != no_data) {
      
      data_info <- ""
      
      numeric_index <- sapply(
        df,
        function(x) {
          if (is.numeric(x)) {
            return(TRUE)
          } else {
            return(FALSE)
          }
        }
      )
      
      numeric_var <- colnames(df)[numeric_index]
      none_numeric_var <- colnames(df)[!numeric_index]
      
      # variables mentioned in request
      relevant_var <- sapply(
        colnames(df),
        function(x) {
          # hwy. class
          grepl(
            paste0(
              " ", # proceeding space
              x,
              "[ |\\.|,]" # ending space, comma, or period
            ),
            txt
          )
        }
      )
      
      relevant_var <- colnames(df)[relevant_var]
      
      if (length(relevant_var) > 0) {
        
        # numeric variables-----------------------------
        relevant_var_numeric <- intersect(relevant_var, numeric_var)
        if (length(relevant_var_numeric) == 1) {
          data_info <- paste0(
            data_info,
            "Note that ",
            relevant_var_numeric,
            " is a numeric variable. "
          )
        } else if (length(relevant_var_numeric) > 1) {
          data_info <- paste0(
            data_info,
            "Note that ",
            paste0(
              relevant_var_numeric[1:(length(relevant_var_numeric) - 1)],
              collapse = ", "
            ),
            " and ",
            relevant_var_numeric[length(relevant_var_numeric)],
            " are numeric variables. "
          )
        }
        
        # Categorical variables-----------------------------
        all_relevant_var_categorical <- intersect(
          relevant_var,
          none_numeric_var
        )
        
        for (relevant_var_categorical in all_relevant_var_categorical) {
          ix <- match(relevant_var_categorical, colnames(df))
          factor_levels <- sort(table(df[, ix]), decreasing = TRUE)
          factor_levels <- names(factor_levels)
          
          # have more than 6 levels?
          many_levels <- FALSE
          
          if (length(factor_levels) > 6) {
            many_levels <- TRUE
            factor_levels <- factor_levels[1:6]
          }
          
          last_level <- factor_levels[length(factor_levels)]
          factor_levels <- factor_levels[-1 * length(factor_levels)]
          tem <- paste0(
            factor_levels,
            collapse = "', '"
          )
          if (!many_levels) { # less than 6 levels
            factor_levels <- paste0("'", tem, "', and '", last_level, "'")
          } else { # more than 6 levels
            factor_levels <- paste0(
              "'",
              tem,
              "', '",
              last_level,
              "', etc"
            )
          }
          
          data_info <- paste0(
            data_info,
            "The column ",
            relevant_var_categorical,
            " contains a categorical variable with these levels: ",
            factor_levels,
            ". "
          )
        }
      }
      
      txt <- paste(txt, after_text)
      # if user is not trying to convert data
      if (!grepl("Convert |convert ", txt)) {
        txt <- paste(txt, data_info)
      }
    }
  }
  
  txt <- paste(pre_text, txt)
  # replace newline with space.
  txt <- gsub("\n", " ", txt)
  
  return(txt)
}


#' Clean up R commands generated by GPT
#'
#' The response from GPT3 sometimes contains strings that are not R commands.
#'
#' @param cmd A string that stores the completion from GPT3
#' @param selected_data, name of the selected dataset. 
#' @return Returns a cleaned up version, so that it could be executed as R command.
clean_cmd <- function(cmd, selected_data) {
  req(cmd)
  # simple way to check
  if(grepl("That model is currently overloaded with other requests.|Error:", cmd)) {
    return(NULL)
  }
  # Use cat to converts \n to newline
  # use capture.output to get the string
  cmd <- capture.output(
    cat(cmd)
  )
  
  #cmd is a vector. Each element is a line.
  
  # sometimes it returns RMarkdown code.
  cmd <- gsub("```", "", cmd)
  
  # remove empty lines
  cmd <- cmd[cmd != ""]
  
  # replace install.packages by "#install.packages"
  cmd <- gsub("install.packages", "#install.packages", cmd)
  
  if (selected_data != no_data) {
    cmd <- c("df <- as.data.frame(current_data())", cmd)
  }
  
  return(cmd)
  
}

#' Creates a SQLite database file for collecting user data
#' 
#' The data file should be stored in the ../../data folder inside 
#' the container. From outside in the RTutor_server folder, 
#' it is in data folder.
#'  Only works on local machines. Not on linux.
#' @return nothing
create_usage_db <- function() {
  # if db does not exist, create one
  if(!file.exists(sqlitePath)) {
    db <- RSQLite::dbConnect(RSQLite::SQLite(), gsub(".*/", "", sqlitePath))
    txt <- sprintf(
      paste0(
        "CREATE TABLE ",
        sqltable,
        "(\n",
        "date DATE NOT NULL,
        time TIME NOT NULL,
        request varchar(5000),
        code varchar(5000),
        error int ,
        data_str varchar(5000))"
      )
    )
    # Submit the update query and disconnect
    RSQLite::dbExecute(db, txt)
    RSQLite::dbDisconnect(db)
  }
}

# To create a database under Ubuntu
# sudo apt update
# sudo apt install sqlite3
# cd ~/Rtutor_server/data
# sudo  sqlite3 usage_data.db


# CREATE TABLE usage (
#        date DATE NOT NULL,
#        time TIME NOT NULL,
#        request varchar(5000),
#        code varchar(5000),
#        error int,
#        data_str varchar(5000),
#       dataset varchar(100));

# sudo chmod a+w usage_data.db

# note that error column, 1 means error, 0 means no error, success.


#' Saves user queries, code, and error status
#' 
#'
#' @param date Date in the format of "2023-01-04"
#' @param time Time "13:05:12"
#' @param request, user request
#' @param code AI generated code
#' @param error status, TRUE, error
#' @param chunk, id, from 1, 2, ...
#' @param api_time  time in seconds for API response
#' @param tokens  total completion tokens
#' @param filename name of the uploaded file
#' @param filesize size
#' 
#' @return nothing
save_data <- function(
    date,
    time,
    request,
    code,
    error_status,
    data_str,
    dataset,
    session,
    filename,
    filesize,
    chunk,
    api_time,
    tokens
) {
  # if db does not exist, create one
  if (file.exists(sqlitePath)) {
    # Connect to the database
    db <- RSQLite::dbConnect(RSQLite::SQLite(), sqlitePath, flags = RSQLite::SQLITE_RW)
    # Construct the update query by looping over the data fields
    txt <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      sqltable,
      "date, time, request, code, error, data_str, dataset, session, filename, filesize, chunk, api_time, tokens",
      paste(
        c(
          as.character(date),
          as.character(time),
          clean_txt(request),
          clean_txt(code),
          as.integer(error_status),
          clean_txt(data_str),
          dataset,
          session,
          filename,
          filesize,
          chunk,
          api_time,
          tokens
        ),
        collapse = "', '"
      )
    )
    # Submit the update query and disconnect
    try(
      RSQLite::dbExecute(db, txt)
    )
    RSQLite::dbDisconnect(db)
  }
}

#' Clean up text strings for inserting into SQL
#' 
#'
#' @param x a string that can contain ' or "
#'
#' @return nothing
clean_txt <- function(x) {
  return(gsub("\'|\"", "", x))
}



# SQLite command to create feedbck table

# "CREATE TABLE feedback (
#        date DATE NOT NULL,
#        time TIME NOT NULL,
#        helpfulness varchar(50),
#        experience varchar(50),
#        comments varchar(5000)); "


#' Save user feedback
#' 
#'
#' @param date Date in the format of "2023-01-04"
#' @param time Time "13:05:12"
#' @param comments, user request
#' @param helpfulness rating
#' @param experience  R experience
#'
#' @return nothing
save_comments <- function(date, time, comments, helpfulness, experience) {
  # if db does not exist, create one
  if (file.exists(sqlitePath)) {
    # Connect to the database
    db <- RSQLite::dbConnect(RSQLite::SQLite(), sqlitePath, flags = RSQLite::SQLITE_RW)
    # Construct the update query by looping over the data fields
    txt <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      "feedback",
      "date, time, comments, helpfulness, experience",
      paste(
        c(
          as.character(date),
          as.character(time),
          clean_txt(comments),
          helpfulness,
          experience
        ),
        collapse = "', '"
      )
    )
    # Submit the update query and disconnect
    try(
      RSQLite::dbExecute(db, txt)
    )
    RSQLite::dbDisconnect(db)
  }
}


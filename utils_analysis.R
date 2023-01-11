###-----------------------------------------------------------------------------
### DAF FUNCTIONS
###-----------------------------------------------------------------------------

load_entry <- function(daf_row){
  #' Load an entry from the DAF.
  #' 
  #' This function replaces load.entry (from descriptive_analysis)
  #' 
  
  entry <- as.list(daf_row)
  # load disaggregate variables as vector:
  disaggregations <- c(str_split(str_remove_all(entry$disaggregations, " "), ";", simplify = T))
  entry$disaggregate.variables <- disaggregations[disaggregations != ""]
  
  # comments - add two lines to them if necessary
  entry$comments <- ifelse(is.na(entry$comments), "", paste0("\n\n", entry$comments))
  # label - if NA, take it from tool.survey
  entry$label <- ifelse(is.na(entry$label), get.label(entry$variable), entry$label)
  # func - set using the q.type from tool.survey
  if(is.na(entry$func)){
    var_type <- entry$var_type
    entry$func <- case_when(
      isna(var_type) ~ "select_one",     # taking select_one by default - but perhaps count is better?
      var_type %in% c("integer", "numeric", "decimal", "calculate") ~ "numeric",
      var_type == "text" ~ "count",
      TRUE ~ var_type                    # all other cases - will be select_one or select_multiple most likely
      )
    warning("Missing parameter 'func' in one of the entries (variable: ", entry$variable, ")\tWill be set to ", entry$func,"\n")
  }
  # admin - stop if NA
  if(is.na(entry$admin)) stop("Missing parameter 'admin' in one of the entries (variable: ", entry$variable, ")")
  
  # parse the 'calculation' column:
  if(!is.na(entry$calculation)){
    # omit_na is especially always True for "numeric", it is False only if matches the pattern "include_na"
    entry$omit_na <- entry$func == "numeric" | str_detect(entry$calculation, "include[_-]na", negate = T)
    # join is False by default, True only if the pattern is detected in this column
    entry$join <- str_detect(entry$calculation, "join")
    # add_total is False by default, True only if the pattern is detected in this column
    entry$add_total <- str_detect(entry$calculation, "add[_-]total")
  }else{
    # default values
    entry$omit_na <- TRUE
    entry$join <- FALSE
    entry$add_total <- FALSE
  }
  
  return(entry)
}

convert_cols_with_daf <- function(df, omit_na = T){
  #' brand new function for conversions... 
  #' But it's not the best. It's better to use convert_cols_with_daf.R which is in tabular analysis v2
  
  converted <- c()
  # filter the daf using the data that was entered 
  daf <- daf %>% filter(variable %in% colnames(df))
  return(df)
}

###-----------------------------------------------------------------------------
### ANALYSIS
###-----------------------------------------------------------------------------

convert.col.type <- function(df, col, omit_na = T){
  #' Convert the type of a specified column of Kobo data.
  #'
  #' @description  The provided dataframe is assumed to contain kobo data.
  #' The type of `col` is taken from `tool.survey`, so `col` must be present in the `name` column of `tool.survey`.
  #'
  #' If `col` type is "select_one", this function finds a list of choice labels for the specified question name.
  #' In this case the output is a factor vector. Levels for this factor are taken from the `label_colname` column in `tool.choices`.
  #'
  #' @details
  #' For 'integer'-type questions, the result is a numeric vector. There will be problems with "calculate" columns, however!
  #'
  #' For 'date' questions results are converted from numeric to date and then returned as character.
  #'
  #' For 'select_one' questions, the result is a factor, with levels equal to a list of choice labels from `tool.choices`,
  #' and including NA if `omit_na` is FALSE.
  #'
  #' For columns containing 'select_multiple' choices, the result is a factor with `levels=c(0, 1)` or `c(0, 1, NA)` if `omit_na` is FALSE
  #'
  #' Other question types are left as they are.
  #'
  #' @param df Dataframe (containing kobo data) from which to extract a column.
  #' @param col Name of the column from `df` which should be converted
  #' @param omit_na This flag should be set if NA values should be skipped (not included as level).
  #' Otherwise NA values are included as levels and will be used for calculating num_samples.
  #' @returns a vector containing the converted values of column `col`.
  if((col %in% tool.survey$name)){
    if(get.type(col) == "select_one"){
      choices <- tool.choices %>% filter(list_name==get.choice.list.from.name(col)) %>%
                                  select(name, `label_colname`) %>%
                                  rename(label = `label_colname`)
      d <- data.frame(col = as.character(df[[col]])) %>%
                      left_join(choices, by=c("col"="name"))
      if(omit_na){
        return(factor(d$label, levels = choices$label, exclude = NA))
      } else  {
        return(factor(d$label, levels = append(choices$label, NA), exclude = NULL))
      }      
    }
    else if (get.type(col)=="integer" | get.type(col)=="decimal" | get.type(col)=="numeric") return(as.numeric(df[[col]]))
    else if (get.type(col)=="date") return(as.character(as.Date(convertToDateTime(as.numeric(df[[col]])))))
    else return(df[[col]])
  } else if (str_detect(col, "/")){
    # branch: column name present in data but not in tool.survey
    # meaning it's one of select_multiple options and should contain a "/"
    return(factor(as.numeric(df[[col]]), levels=c(0, 1), exclude = NA))
  }
  return(df[[col]])
}


convert.cols.check.dap <- function(df, dap) {
  #' Convert types of columns in data and check DAP.
  #'
  #' @description This function is quite obsolete. Tabular analysis v2 doesn't use this one at all.
  #' 
  #' The provided dataframe is assumed to contain Kobo data.

    # - convert "select_one" columns (names -> labels) to factor
    # - convert "integer" and "decimal" columns to numeric
    # - convert "date" columns to Date
    # - convert "select_multiple" columns to factor

    converted <- c()
    
    # filter the dap using the data that was entered 

    dap <- dap %>% filter(variable %in% colnames(df))
    
    if(nrow(dap) == 0){
      cat("\nThere was nothing to convert - no questions from data are found in DAP.")
      return(df)
    }

    for(r in 1:nrow(dap)){
        entry <- load_entry(dap[r,])
        col <- entry$variable

        cat("Converting",col," ")
        # check if variables exist in data
        if(!col %in% colnames(df)) {
          stop(paste("Variable", col, "not found in data!"))
        }
          
        # if(!col %in% tool.survey$name){
        #     warning(paste("Variable", col, "not found in tool.survey!\n"))
        # }
        # if(is.na(entry$func)){
        #   warning("Missing parameter func in row ", r, " (variable ",entry$variable,")\n")
        #   next
        # }

        q <- tool.survey[tool.survey$name == col,]
        if(!is.na(entry$calculation)){
            if(str_detect(entry$calculation, "include_na")){
                if(!is.na(q$relevant))   stop(paste0("Issue with entry #", r, " (", col,"): flag include_na cannot be set if question has relevancy!"))
                if(entry$func == "mean") stop(paste0("Issue with entry #", r, " (", col,"): flag include_na cannot be set if func == mean!"))
            }
        }

        # check and convert disagg variable :)
        if(!all(is.na(entry$disaggregate.variables))){
            for(disagg.var in entry$disaggregate.variables){
                if(!disagg.var %in% colnames(df)){          
                  warning(paste("Disaggregation variable", disagg.var, "not found in data! Skipping.\n"))
                  next
                  }
                if(!disagg.var %in% tool.survey$name) warning(paste("Disaggregation variable", disagg.var, "not found in tool.survey!\n"))
                if(!disagg.var %in% converted){
                    df[[disagg.var]] <- convert.col.type(df, disagg.var)
                    converted <- append(converted, disagg.var)
                }
            }
        }

        if(col %in% converted) next

        if(entry$func == "select_multiple"){
            if(get.type(col) != "select_multiple")
                stop(paste0("Issue with entry #", r, " (", col,"): func is 'select_multiple', but question type is ", get.type(col)))

            choice_cols <- colnames(df)[str_starts(colnames(df), paste0(col, "/"))]
            for(ccol in choice_cols){
                df[[ccol]] <- convert.col.type(df, ccol, entry$omit_na)
                df <- df %>% rename_with(~str_replace(ccol, "/", "___"), ccol)
                converted <- append(converted, ccol)
            }
            if(!entry$omit_na){
                # create a new NA column
                na_colname <- paste0(col,"___NA")
                df[[na_colname]] <- factor(ifelse(is.na(df[[col]]), 1, 0))
                df <- df %>% relocate(na_colname, .after = !!sym(col))
            }
        }else {
          if(!col %in% tool.survey$name){
              if(entry$func == "select_one") df[[col]] <- as.factor(df[[col]])
              else if(entry$func == "mean") df[[col]] <- as.numeric(df[[col]])
            }
            df[[col]] <- convert.col.type(df, col, entry$omit_na)
            converted <- append(converted, col)
        }
        cat("... done.\n")
    }
    cat("\nAll conversions done!")
    return(df)
}

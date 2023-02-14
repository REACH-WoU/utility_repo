
# ------------------------------------------------------------------------------------------
# SAVING RESPONSES/REQUESTS
# ------------------------------------------------------------------------------------------

# styles
style.col.blue <- createStyle(fgFill="#CCE5FF", valign="top",
                              border="TopBottomLeftRight", borderColour="#000000", wrapText=T)
style.col.green <- createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000",
                              valign="top", fontSize = 10, fontName = "Arial Narrow", wrapText=T)
style.col.green.bold <- createStyle(textDecoration="bold", fgFill="#E5FFCC", valign="top",
                              border="TopBottomLeftRight", borderColour="#000000",
                              fontSize = 10, fontName = "Arial Narrow", wrapText=T)

# ------------------------------------------------------------------------------------------

save.other.requests <- function(df, wb_name, use_template = F){

  if(use_template) wb <- loadWorkbook("resources/other_requests_template.xlsx")
  else wb <- createWorkbook()
  addWorksheet(wb, "Sheet2", zoom = 90)
  writeData(wb = wb, x = df, sheet = "Sheet2", startRow = 1,
            headerStyle = createStyle(textDecoration="bold", border = "Bottom", fontName = "Arial"))

    response_cols_ind <- which(str_starts(colnames(df), "response"))
  for(i in response_cols_ind){
    addStyle(wb, "Sheet2", style = createStyle(fontSize = 10, fontName = "Arial Narrow", wrapText = T),
             rows = 1:nrow(df)+1, cols=i)
    setColWidths(wb, "Sheet2", cols = i, widths = 30)
  }
  addStyle(wb, "Sheet2", style = createStyle(fontSize = 10, fontName = "Arial Narrow", wrapText = T),
           rows = 1:nrow(df)+1, cols=which(colnames(df) == "choices.label"))
  addStyle(wb, "Sheet2", style = createStyle(fontSize = 11, wrapText = T),
           rows = 1:nrow(df)+1, cols=which(colnames(df) == "full.label"))

  setColWidths(wb, "Sheet2", cols = 1, widths = 5)
  setColWidths(wb, "Sheet2", cols = 2:which(colnames(df) == "choices.label")-1, widths = "auto")
  setColWidths(wb, "Sheet2", cols = which(colnames(df) == "choices.label"), widths = 50)
  setColWidths(wb, "Sheet2", cols = which(colnames(df) == "full.label"), widths = 30)
  setColWidths(wb, "Sheet2", cols = (ncol(df)-4):(ncol(df)), widths = 35)

  addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df)-2, stack = T)
  addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df)-1, stack = T)
  addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df), stack = T)
  addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df)-2, stack = T)
  addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df)-1, stack = T)
  addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df), stack = T)

  filename <- paste0(dir.requests, wb_name, ".xlsx")
  saveWorkbook(wb, filename, overwrite=TRUE)

}

save.trans.requests <- function(df, wb_name, blue_cols = NULL, use_template = F){

    if(use_template) wb <- loadWorkbook("resources/trans_requests_template.xlsx")
    else wb <- createWorkbook()
    
    # remove the useless "EXISTING" column, and the word 'other':
    if(length(df %>% select(starts_with("EXISTING")) %>% names) > 0){
      df <- df %>% select(-starts_with("EXISTING"))
      colnames(df) <- gsub("other ", "", colnames(df))
    }
    df <- df %>% select(-starts_with("ref")) %>% select(-starts_with("choices"))
    
    addWorksheet(wb, "Sheet2")
    writeData(wb = wb, x = df, sheet = "Sheet2", startRow = 1)

    setColWidths(wb, "Sheet2", cols = 1, widths = 5)
    setColWidths(wb, "Sheet2", cols = 2:ncol(df), widths = "auto")

    response_cols_ind <- which(str_starts(colnames(df), "response"))
    for(i in append(response_cols_ind, 1)){
        addStyle(wb, "Sheet2", style = createStyle(fontSize = 10, fontName = "Arial Narrow", wrapText = T),
                 rows = 1:nrow(df)+1, cols=i)
        setColWidths(wb, "Sheet2", cols = i, widths = 30)
    }
    for (col in blue_cols) {
        i <- grep(paste0('^',col,'$'), colnames(df))
        if(length(i) == 0) stop(paste(col,"not found in df!"))
        addStyle(wb, "Sheet2", style = style.col.blue, rows = 1:(nrow(df)+1), cols = i, stack = T)
        setColWidths(wb, "Sheet2", cols = which(colnames(df) == col), widths = 20)
    }

    addStyle(wb, "Sheet2", style = createStyle(textDecoration="bold", valign = "bottom"), rows = 1, cols=1:ncol(df), stack = T)

    addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = which(str_starts(colnames(df), "TRUE")), stack = T)
    addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = which(str_starts(colnames(df), "TRUE")), stack = T)
    addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = which(str_starts(colnames(df), "INVALID")), stack = T)
    addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = which(str_starts(colnames(df), "INVALID")), stack = T)

    filename <- paste0(dir.requests, wb_name, ".xlsx")
    saveWorkbook(wb, filename, overwrite=TRUE)
}


create.follow.up.requests <- function(checks.df, wb_name){
    use.color <- function(check.id){
        return(str_starts(check.id, "0"))
    }
    # define styles
    style.col.green <- createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000")
    style.col.green.first <- createStyle(textDecoration="bold", fgFill="#E5FFCC",
                                         border="TopBottomLeftRight", borderColour="#000000", wrapText=F)
    col.style <- createStyle(textDecoration="bold", fgFill="#CECECE",halign="center",
                             border="TopBottomLeftRight", borderColour="#000000")
    # arrange cleaning.log so that colors are properly assigned later
    cl <- checks.df %>%
        arrange(variable) %>%
        group_modify(~ rbind(
            filter(.x, !use.color(check.id)) %>% arrange(check.id, uuid),
            filter(.x, use.color(check.id)) %>% arrange(check.id)))
    cl <- cl %>% arrange(match(check.id, str_sort(unique(cl$check.id), numeric=T)))
    # save follow-up requests
    wb <- createWorkbook()
    addWorksheet(wb, "Follow-up", zoom = 90)
    writeData(wb = wb, x = cl, sheet = "Follow-up", startRow = 1)

    addStyle(wb, "Follow-up", style = style.col.green, rows = 1:(nrow(cl)+1), cols=which(colnames(cl)=="explanation"))
    addStyle(wb, "Follow-up", style = style.col.green, rows = 1:(nrow(cl)+1), cols=which(colnames(cl)=="invalid"))
    addStyle(wb, "Follow-up", style = style.col.green, rows = 1:(nrow(cl)+1), cols=which(colnames(cl)=="new.value"))
    addStyle(wb, "Follow-up", style = style.col.green.first, rows = 1, cols=which(colnames(cl)=="explanation"))
    addStyle(wb, "Follow-up", style = style.col.green.first, rows = 1, cols=which(colnames(cl)=="new.value"))
    addStyle(wb, "Follow-up", style = style.col.green.first, rows = 1, cols=which(colnames(cl)=="invalid"))


    setColWidths(wb, "Follow-up", cols=1:ncol(cl), widths="auto")
    # setColWidths(wb, "Follow-up", cols=ncol(cl)-1, widths=50)

    setColWidths(wb, "Follow-up", cols=which(colnames(cl)=="issue"), widths=50)
    addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(nrow(cl)+1), cols=which(colnames(cl)=="issue"))

    addStyle(wb, "Follow-up", style = col.style, rows = 1, cols=1:ncol(cl))

    col.id <- which(colnames(cl)=="old.value")
    if(nrow(cl) > 0){
      random.color <- ""
      for (r in 2:nrow(cl)){
        if((!use.color(as.character(cl[r, "check.id"])) &
            as.character(cl[r, "uuid"])==as.character(cl[r-1, "uuid"]) &
            as.character(cl[r, "check.id"])==as.character(cl[r-1, "check.id"])) |
           (use.color(as.character(cl[r, "check.id"])) &
            as.character(cl[r, "check.id"])==as.character(cl[r-1, "check.id"]))){
          if (random.color == "") random.color <- randomColor(1, luminosity = "light")
          addStyle(wb, "Follow-up", style = createStyle(fgFill=random.color, wrapText=T),
                   rows = r:(r+1), cols=col.id)
        } else random.color=""
      }
    }

    filename <- paste0(dir.requests, wb_name)
    filename <- ifelse(str_ends(filename, "\\.xlsx", T), paste0(filename, ".xlsx"), filename)
    saveWorkbook(wb, filename, overwrite = TRUE)
}

# simply an alias:
save.follow.up.requests <- function(checks.df, wb_name) create.follow.up.requests(checks.df, wb_name)

# ------------------------------------------------------------------------------------------
# LOADING REQUESTS/RESPONSES FILES
# ------------------------------------------------------------------------------------------

load.requests <- function(dir, filename.pattern, sheet=NULL, validate=FALSE){
  #' Load 'request' logs from specified directory.
  #'
  #' Searches `dir` to find XLSX files with names that start with a match for `filename.pattern`.
  #' NB: This pattern-matching is case-insensitive. If files contain the classic "TRUE", "EXISTING" or "INVALID" (TEI) columns,
  #' these will be renamed to "true.v", "existing.v", and "invalid.v" respectively and optionally validated for errors.
  #'
  #' @param dir Directory which should be searched for files.
  #' @param filename.pattern String with a regex pattern which will be passed to `list.files` to match files,
  #' however: '^' (string start) is added at the start of the pattern, and ".*\\.xlsx" is added at the end,
  #' so effectively files that will be loaded must be XLSX and have names that start with the provided pattern.
  #'
  #' @param sheet Optional parameter passed to `read_xlsx`, defaults to NULL (first sheet of an Excel workbook)
  #' @param validate Should the file be validated (make sure that only one of TEI columns is filled.)

  file.type = str_squish(str_replace_all(filename.pattern, "[^a-zA-Z]+"," "))
  filenames <- list.files(dir, recursive=FALSE, full.names=TRUE, ignore.case = TRUE,
                          pattern=paste0("^",filename.pattern,".*\\.xlsx"))
  if (length(filenames) == 0){
    warning(paste("Files with",file.type,"requests not found!"))
    return(tibble())
  } else {
    cat(paste("Loading",length(filenames),file.type,"files:\n"),paste(filenames, collapse = "\n "),"\n")
    res <- data.frame()
    for (filename in filenames){
      # load file
      other <- read_xlsx(filename, col_types = "text", trim_ws = T, sheet = sheet)
      if (filename==filenames[1]) res <- other
      else{
        if(ncol(res)!=ncol(other)) warning("Number of columns differs between files! Check them to make sure everything is correct, please!")
        res <- bind_rows(res, other)
      }

    }
    # rename: TRUE -> true.v, EXISTING -> existing.v, INVALID -> invalid.v
    c_tei_cols <- c("true", "existing", "invalid")
    for(c in c_tei_cols) colnames(res)[str_starts(colnames(res), str_to_upper(c))] <- paste0(c,'.v')

    if(validate){
      c_tei_cols <- paste0(c_tei_cols, ".v")
      if(all(c_tei_cols %in% colnames(res))){
        res <- res %>% mutate(check = rowSums(is.na(select(res, all_of(c_tei_cols)))))
        check.res <- res %>% select(c(
          any_of(c("uuid","ref.type","check"))))
        check.missing <- check.res %>% filter(check == 3)
        if(nrow(check.missing)){
          warning(paste0("Missing entries:\n", paste0(" ", unlist(check.missing[,1]) , collapse = "\n")))
        }
        if("ref.type" %in% colnames(check.res)){
          check.res <- check.res %>%
            filter(check == 0 | (check == 1 & ref.type != "select_multiple"))  # select_multiple can have 2 columns selected
        }else{
          check.res <- filter(check.res, check != 2)
        }
        if(nrow(check.res)>0) {
          warning(paste0("Multiple columns selected:\n", paste0(" ", unlist(check.res[,1]) , collapse = "\n")))
        }
      }else{
        stop("One or more of 'true', 'existing', 'invalid' columns not found in requests files.")
      }
    }
    return(res)
  }
}


# ------------------------------------------------------------------------------------------
# CLEANING LOG FUNCTIONS
# ------------------------------------------------------------------------------------------

# important constant vector
CL_COLS <- c("uuid", "loop_index", "variable", "old.value", "new.value", "issue")

# ----------------
# GENERAL RECODING
# ------------------------------------------------------------------------------

recode.set.NA.if <- function(data, variables, code, issue, ignore_case = T){
    #' Recode a question by setting variables to NA if they are equal to a given value (code).
    #'
    #' @note DO NOT use this function for select_multiple questions. Instead use `recode.multiple.set.NA`
    #'
    #' @param data Dataframe containing records which will be affected.
    #' @param variables Vector of strings (or a single string) containing the names of the variables.
    #' @param code Vector of strings (or a single string) which will be changed to NA.
    #' @param issue String with explanation used for the cleaning log entry.
    #' @param ignore_case Whether `code` should be matched case-insensitively. Defaults to True.
    #'
    #' @returns Dataframe containing cleaning log entries constructed from `data`.
    #'
    #' @usage `recode.set.NA.if(data = filter(raw.main, condition),
    #'  variables = c("question1", "question2"),
    #'   code = "999", issue = "explanation")`
    #'
    
  # TODO filter variables to only include those in data (and produce warnings)
  
    clog <- tibble()
    for(variable in variables){
        if(ignore_case) data1 <- data %>% filter(str_to_lower(!!sym(variable)) %in% str_to_lower(code))
        else data1 <- data %>% filter(!!sym(variable) %in% code)
        cl <- data1 %>% mutate(variable = variable, old.value = !!sym(variable), new.value = NA,
                              issue = issue) %>% select(any_of(CL_COLS))
        clog <- rbind(clog, cl)
    }
    return(clog)
}

recode.set.NA.regex <- function(data, variables, pattern, issue){
  #' Recode a question by setting variables to NA if they are matching a regex pattern.
  #' 
  #' A more powerful version of the function `recode.set.NA.if`.
  #' This function is also useful also if you need to simply set some variables to NA - you can put ".*" as the `pattern`.
  #'
  #' @note DO NOT use this function for select_multiple questions. Instead use `recode.multiple.set.NA`
  #'
  #' @param data Dataframe containing records which will be affected.
  #' @param variables Vector of strings (or a single string) containing the names of the variables.
  #' @param pattern Regex pattern which will be used to find entries that will be turned to NA.
  #' @param issue String with explanation used for the cleaning log entry.
  #'
  #' @returns Dataframe containing cleaning log entries constructed from `data`.
  #'
  #' @usage `recode.set.NA.regex(data = filter(raw.main, condition),
  #'  variables = c("question1", "question2"),
  #'   pattern = "birth_certificates?", issue = "explanation")`
  #'
  clog <- tibble()
  for(variable in variables){
    data1 <- data %>% filter(str_detect(!!sym(variable), pattern = pattern))
    cl <- data1 %>% mutate(variable = variable, old.value = !!sym(variable), new.value = NA,
                           issue = issue) %>% select(any_of(CL_COLS))
    clog <- rbind(clog, cl)
  }
  return(clog)
}

recode.set.NA <- function(data, variables, issue){
  #' Set the given variables for the given entries to NA.
  #'
  #' Small utility function that calls `recode.set.value.regex` with parameter `pattern = ".*"`
  recode.set.NA.regex(data, variables, ".*", issue)
}

recode.set.value.regex <- function(data, variables, pattern, new.value, issue, affect_na = FALSE){
  #' Recode a question by setting variables to some new value if they are matching a regex pattern.
  #' 
  #' @note it's better to not use this function for select_multiple questions. Instead use `recode.multiple.set.choices`
  #' 
  #' @param data Dataframe containing records which will be affected.
  #' @param variables Vector of strings (or a single string) containing the names of the variables.
  #' @param pattern Regex pattern which will be used to match entries.
  #' @param issue String with explanation used for the cleaning log entry.
  #' @param affect_na Whether this function should also change NA values to `new.value`. Defaults to False (NA entries will be skipped)
  #'
  #' @returns Dataframe containing cleaning log entries constructed from `data`.
  #'
  #' @usage `recode.set.value.regex(data = filter(raw.main, condition),
  #'  variables = c("question1", "question2"),
  #'   pattern = "birth_certificates?", issue = "explanation")`
  #'
  clog <- tibble()
  for(variable in variables){
    if(affect_na) data1 <- data %>% filter(str_detect(!!sym(variable), pattern = pattern) | is.na(!!sym(variable)))
    else data1 <- data %>% filter(str_detect(!!sym(variable), pattern = pattern))
    data1 <- data1 %>% filter(!!sym(variable) %!=na% new.value)
    cl <- data1 %>% mutate(variable = variable, old.value = !!sym(variable), new.value = new.value,
                           issue = issue) %>% select(any_of(CL_COLS))
    clog <- rbind(clog, cl)
  }
  return(clog)
}

# ----------------
# RECODING SELECT_MULTIPLES
# ------------------------------------------------------------------------------

recode.multiple.set.NA <- function(data, variable, issue, other_var_name = NULL){
    #' Recode select_multiple responses: set to NA.
    #'
    #' Changes all 1s and 0s to NA in choice columns, sets cumulative variable and _other text answers to NA.
    #'
    #' @param data Dataframe containing records which will be affected.
    #' @param variable String containing the name of the select_multiple variable.
    #' @param issue String with explanation used for the cleaning log entry.
    #'
    #' @returns Dataframe containing cleaning log entries constructed from `data`.
    #'
    #' @usage `recode.multiple.set.NA(data = filter(raw.main, condition), variable = "question_name", issue = "explanation")`

    ccols <- colnames(data)[str_starts(colnames(data), paste0(variable, "/"))]

    # filter out cases that already are NA
    data <- data %>% filter(!if_all(all_of(ccols), ~is.na(.)))
    if(nrow(data)>0){
        cl_cummulative <- data %>% select(any_of(c("uuid", "loop_index", variable))) %>%
            mutate(variable = variable, old.value = !!sym(variable), new.value = NA, issue = issue) %>%
            select(any_of(CL_COLS))

        cl_choices <- data.frame()
        for(col in ccols){
            df <- data %>% filter(!is.na(!!sym(col)))
            if(nrow(df)>0){
                cl <- df %>%
                    mutate(variable = col, old.value = !!sym(col), new.value = NA, issue = issue) %>%
                    select(any_of(CL_COLS))

                cl_choices <- rbind(cl_choices, cl)
                # remove text from text other response
                if(str_ends(col, "/other")){
                  other_var_name <- ifelse(is.null(other_var_name), paste0(variable, "_other"), other_var_name)
                  cl_other_text <- df %>% filter(!is.na(!!sym(other_var_name))) %>% 
                    mutate(variable = other_var_name, old.value = !!sym(other_var_name),
                    new.value = NA, issue = issue) %>%
                    select(any_of(CL_COLS))
                  
                  cl_choices <- rbind(cl_choices, cl_other_text) 
                }
            }
        }
        return(rbind(cl_cummulative, cl_choices))

    }
    return(data.frame())

}

recode.multiple.set.choice <- function(data, variable, choice, issue, other_var_name = NULL){
    #' Recode select_multiple responses: set answer to one particular choice.
    #' 
    #' [obsolete] Use recode.multiple.set.choices instead
    #'
    #' Changes all 1s to 0 in choice columns (except for `choice`) and sets cumulative variable to be equal to `choice`.
    #' Additionally, all NAs will be changed too, and the _other variable will be set to NA (if it is not the chosen option).
    #'
    #' @param data Dataframe containing records which will be affected.
    #' @param variable String containing the name of the select_multiple variable.
    #' @param choice String containing the choice that must be a valid option for this variable.
    #' @param issue String with explanation used for the cleaning log entry.
    #'
    #' @returns Dataframe containing cleaning log entries constructed from `data`.
    #'
    #' @usage `recode.multiple.set.choice(data = filter(raw.main, condition), variable = "question_name", choice = "option", issue = "explanation")`
    #'
  warning(" recode.multiple.set.choice is obsolete! Please use recode.multiple.set.choices instead!\n\t...unless you are trying to change some NA values - in this case I hope you know what you're doing :)")
  choice_column <- paste0(variable,"/",choice)
    if(!choice_column %in% colnames(data)) stop(paste("Column",choice_column,"not present in data!"))
    # filter out cases that already have only this choice selected
    data <- data %>% filter(!!sym(variable) %!=na% choice)
    if(nrow(data) > 0){
        cl_cummulative <- data %>% select(any_of(c("uuid", "loop_index", variable))) %>%
            rename(old.value = !!sym(variable)) %>%
            mutate(variable = variable, new.value = choice, issue = issue)

        cl_choices <- data %>%
            mutate(variable = choice_column, old.value = !!sym(choice_column), new.value = "1", issue = issue) %>%
            select(any_of(CL_COLS))

        # set all other choices columns to 0
        cols <- colnames(data)[str_starts(colnames(data), paste0(variable, "/")) &
                                   !(str_ends(colnames(data), choice))]
        for(col in cols){
            df <- data %>% filter(!!sym(col) %!=na% "0")
            if(nrow(df>0)){
                cl <- df %>%
                    mutate(variable = col, old.value = !!sym(col), new.value = "0", issue = issue) %>%
                    select(any_of(CL_COLS))

                cl_choices <- rbind(cl_choices, cl)
            }
        }
        if(choice != "other"){
          other_var_name <- ifelse(is.null(other_var_name), paste0(variable, "_other"), other_var_name)
          cl_other <- recode.set.NA(data, other_var_name, issue)
          return(rbind(cl_cummulative, cl_choices, cl_other))
        } else return(rbind(cl_cummulative, cl_choices))

    }
    return(data.frame())
}

recode.multiple.set.choices <- function(data, variable, choices, issue, other_var_name = NULL){
  #' Recode select_multiple responses: set answer to one particular choice.
  #'
  #' This function affects choice columns: setting 1 for `choices` and 0 everywhere else.
  #' For the cumulative variable, the valye will be more or less equal to `paste(choices, collapse=" ")` 
  #' (to be precise, the exact order of choices will be the same as the one in `tool.choices`) ... 
  #' Additionally, the _other variable will be set to NA if it is not one of the `choices`
  #'
  #' @param data Dataframe containing records which will be affected.
  #' @param variable String containing the name of the select_multiple variable.
  #' @param choices Vector of strings (or a single string) containing the choices that will be added. They must be valid options for this variable.
  #' @param issue String with explanation used for the cleaning log entry.
  #' 
  #' @note this function does not change entries where variable was prevoiusly NA
  #' # TODO: allow for this^
  #'
  #' @returns Dataframe containing cleaning log entries constructed from `data`.
  #'
  #' @usage `recode.multiple.set.choices(data = filter(raw.main, condition), variable = "question_name", choices = "option", issue = "explanation")`
  #'
  
  choice_columns <- paste0(variable,"/",choices)
  if(any(!choice_columns %in% colnames(data))) stop(paste("Columns",paste(choice_columns, collapse = ", "),"not present in data!"))
  
  # find the new value for the cumulative variable (get the proper order from tool.choices)
  newvalue <- tool.choices %>% filter(list_name == get.choice.list.from.name(variable) & name %in% choices) %>% 
    pull(name) %>% paste(collapse = " ")
  anychoice_pattern <- paste0("(",choices,")", collapse = "|")
  
  # filter out NA and cases that already have only these choices selected
  data <- data %>% filter(!!sym(variable) != newvalue)
  
  if(nrow(data) > 0){
    cl_cummulative <- data %>% select(any_of(c("uuid", "loop_index", variable))) %>%
      rename(old.value = !!sym(variable)) %>%
      mutate(variable = variable, new.value = newvalue, issue = issue)
    
    # set all other choices columns to 0
    cols <- colnames(data)[str_starts(colnames(data), paste0(variable, "/")) &
                             !(str_ends(colnames(data), anychoice_pattern))]
    
    cl_choices <- rbind(recode.set.value.regex(data, choice_columns, "0", "1", issue), 
                        recode.set.value.regex(data, cols, "1", "0", issue))
    
    if(!"other" %in% choices) {
      # remove text from other responses
      other_var_name <- ifelse(is.null(other_var_name), paste0(variable, "_other"), other_var_name)
      cl_other <- recode.set.NA(data, other_var_name, issue)
      return(rbind(cl_cummulative, cl_choices, cl_other))
    } else{
      return(rbind(cl_cummulative, cl_choices))
    }
  }
  return(data.frame())
}

recode.multiple.add.choices <- function(data, variable, choices, issue){
  #' Recode select_multiple responses: add particular choices.
  #'
  #' Changes all 0s to 1s in choice columns specified by `choices`. Modifies cumulative variable too.
  #' 
  #' @note This function does not affect entries that have NA in `variable`.
  #'
  #' @param data Dataframe containing records which will be affected.
  #' @param variable String containing the name of the select_multiple variable.
  #' @param choices Vector of strings (or a single string) containing the choices that will be added. They must be valid options for this variable.
  #' @param issue String with explanation used for the cleaning log entry.
  #'
  #' @returns Dataframe containing cleaning log entries constructed from `data`.
  #'
  #' @usage `recode.multiple.add.choices(data = filter(raw.main, condition), variable = "question_name", choices = c("option1", "option2"), issue = "explanation")`
    
    choice_columns <- paste0(variable,"/",choices)
    if(any(!choice_columns %in% colnames(data))){
      stop(paste("\nColumn",choice_columns[!choice_columns %in% colnames(data)],"not present in data!"))
    }
    choices_pattern <- paste0("(",paste0(choices, collapse = ")|("), ")")
    choices_len <- str_length(paste0(choices, collapse = "")) + length(choices)
    # filter out cases that already have all choices selected
    data <- data %>%
      select(any_of(c("uuid", "loop_index", variable)), all_of(choice_columns)) %>% filter(!is.na(!!sym(variable))) %>%
        mutate(variable2 = str_squish(str_remove_all(!!sym(variable), choices_pattern))) %>%
        mutate(len_diff = str_length(!!sym(variable)) - str_length(variable2)) %>%
        filter(str_length(!!sym(variable)) - str_length(variable2) != choices_len)
    if(nrow(data) > 0){
      cl_cummulative <- data %>% select(any_of(c("uuid", "loop_index", variable, "variable2"))) %>%
          rename(old.value = !!sym(variable)) %>%
          mutate(variable = variable, new.value = str_squish(paste(variable2, paste0(choices, collapse = " "))), issue = issue) %>%
          select(-variable2)
      if(all(cl_cummulative$new.value %==na% cl_cummulative$old.value)) cl_cummulative <- data.frame()
      
      choice_columns <- paste0(variable,"/",choices)
      cl_choices <- recode.set.value.regex(data, choice_columns, "0", "1", issue)
      return(rbind(cl_cummulative, cl_choices))
    }
    return(data.frame())
}

recode.multiple.add.choice <- function(data, variable, choice, issue){
  #' [obsolete] use `recode.multiple.add.choices` instead
  #' 
  
  warning(" recode.multiple.add.choice is obsolete! Please use recode.multiple.add.choices instead.")
  
  choice_column <- paste0(variable,"/",choice)
  if(!choice_column %in% colnames(data)) stop(paste("Column",choice_column,"not present in data!"))
  # filter out cases that already have choice selected
  data <- data %>% filter(str_detect(!!sym(variable), choice, negate = T))
  if(nrow(data) > 0){
    cl_cummulative <- data %>% select(any_of(c("uuid", "loop_index", variable))) %>%
      rename(old.value = !!sym(variable)) %>%
      mutate(variable = variable, new.value = str_squish(paste(old.value, choice)), issue = issue)

    cl_choice <- data %>% select(any_of(c("uuid", "loop_index"))) %>%
      mutate(variable = choice_column, old.value = "0", new.value = "1", issue = issue)
    return(rbind(cl_cummulative, cl_choice))
  }
  return(data.frame())
}

recode.multiple.remove.choice <- function(data, variable, choice, issue){
  #' [obsolete] use `recode.multiple.remove.choices` instead
  #' 
  
  warning(" recode.multiple.remove.choice is obsolete! Please use recode.multiple.remove.choices instead.")
  
  choice_column <- paste0(variable,"/",choice)
  if(!choice_column %in% colnames(data)) stop(paste("Column",choice_column,"not present in data!"))
  # filter out cases that dont have the choice selected
  data <- data %>% filter(str_detect(!!sym(variable), choice))
  if(nrow(data) > 0){
    cl_cummulative <- data %>% select(any_of(c("uuid", "loop_index", variable))) %>%
      rename(old.value = !!sym(variable)) %>%
      mutate(variable = variable, new.value = str_squish(str_remove(old.value, choice)), issue = issue)

    cl_choice <- data %>% select(any_of(c("uuid", "loop_index"))) %>%
      mutate(variable = choice_column, old.value = "1", new.value = "0", issue = issue)
    return(rbind(cl_cummulative, cl_choice))
  }
  return(data.frame())
}

recode.multiple.remove.choices <- function(data, variable, choices, issue, other_var_name = NULL){
  #' Recode select_multiple responses: remove particular choices.
  #'
  #' Removes the relevant text from the cummulative column. Changes all 1s to 0 in choice columns specified by `choices`.
  #' Also, if one of the `choices` is "other", then the text variable (_other) will be changed to NA.
  #' 
  #' @note This function does not affect entries that have NA in `variable`.
  #'
  #' @param data Dataframe containing records which will be affected.
  #' @param variable String containing the name of the select_multiple variable.
  #' @param choices Vector of strings (or a single string) containing the choices that will be removed. They must be valid options for this variable.
  #' @param issue String with explanation used for the cleaning log entry.
  #'
  #' @returns Dataframe containing cleaning log entries constructed from `data`.
  #'
  #' @usage `recode.multiple.remove.choices(data = filter(raw.main, condition), variable = "question_name", choices = c("option1", "option2"), issue = "explanation")`
  #'
  
  choice_columns <- paste0(variable,"/",choices)
  if(any(!choice_columns %in% colnames(data))) stop(paste("Column",choice_columns[!choice_columns %in% colnames(data)],"were not found in data!"))
  
  # filter to include only rows that are not NA, and that have at least one of the choices selected
  anychoice_pattern <- paste0("(",choices,")", collapse = "|")
  data <- data %>% filter(!is.na(!!sym(variable)) & str_detect(!!sym(variable), anychoice_pattern))
  
  if(nrow(data) > 0){
    # remove the choices from cumulative column using a combined regex pattern
    cl_cummulative <- data %>% select(any_of(CL_COLS), all_of(variable)) %>% 
      rename(old.value = !!sym(variable)) %>% 
      mutate(variable = variable, new.value = str_squish(str_remove_all(old.value, anychoice_pattern)), issue = issue)
    
    cl_choices <- recode.set.value.regex(data, choice_columns, "1", "0", issue)
    
    if(!"other" %in% choices) { return(rbind(cl_cummulative, cl_choices))
    } else{
      other_var_name <- ifelse(is.null(other_var_name), paste0(variable, "_other"), other_var_name)
      cl_other <- recode.set.NA(data, other_var_name, issue)
      return(rbind(cl_cummulative, cl_choices, cl_other))
    }
  }
  return(data.frame())
  
}

# ----------------
# RECODING OTHERS (new functions!)
# ------------------------------------------------------------------------------

recode.others <- function(data, or.edited, orig_response_col = "response.uk", is.loop = F, print_debug = T){
  #' Create a cleaning log for recoding other responses
  #' 
  #' Use the filled out other_requests file to create cleaning.log.other.
  #' Run this function on every datasheet your data has (main, loop1, loop2, etc...)
  #' Unexpected behavior may occur if some variables from `or.edited` are not present in the data.
  #' @param data Dataframe containing Kobo data
  #' @param or.edited Dataframe containing the filled-out other_requests file. Needs to contain the standard TEI (True, Existing, Invalid) columns,
  #' as well as column `check`. Please use the ```load.requests``` function to load these xlsx files.
  #' @param orig_response_col The name of the column which stores the original (untranslated!) response for each question.
  #' @param is.loop Set this to True if the provided dataframe is a loop (it needs to contain the column loop_index).
  #' @param print_debug Whether debugging information will be printed to screen (about how many responses will be recoded/translated etc.).
  #' @returns Dataframe containing cleaning log entries covering recoding others, constructed from `data` and `or.edited`
  
  
  # a new thing: UNIQUI - universal unique identifier (either loop_index or uuid)
  # it will be used for matching records to or.edited entries :)
  if(!is.loop) {
    or.edited <- or.edited %>% select(-any_of("loop_index")) %>% mutate(uniqui = uuid)
    data <- data %>% mutate(uniqui = uuid)
  }else{
    if(!"loop_index" %in% colnames(or.edited)) stop("Parameter is.loop = TRUE, but column loop_index was not found in or.edited!")
    else{
      or.edited <- or.edited %>% mutate(uniqui = loop_index)
      data <- data %>% mutate(uniqui = loop_index)
    } 
  }
  
  # fix legacy naming
  if(!"existing.v" %in% colnames(or.edited)) {
    if("existing.other" %in% colnames(or.edited)) or.edited <- or.edited %>% rename_with(~gsub(".other", ".v", .), ends_with(".other"))
    else stop("Column 'existing.v' not found in or.edited!\n\tPlease check your requests file.")
  }
  # the column 'check' must be present in the or.edited (meaning you must use the validate = T option when loading requests)
  if(!"check" %in% colnames(or.edited)) stop("Column 'check' was not found in or.edited!\n\tPlease, use the `validate` option in load.requests.")

  # check for missing identifiers:
  if(any(!or.edited$uniqui %in% data$uniqui)){
    ids <- or.edited %>% distinct(uniqui) %>% pull
    missing_ids <- ids[!ids %in% data$uniqui]
    if(length(missing_ids) == length(ids)) stop("NONE of the identifiers from or.edited were found in data!")
    else{
      if(print_debug) warning("Identifiers from or.edited not found in data:\n\t", paste(missing_ids, collapse = ",\n\t"), "\n")
    } 
  } 
  
  # HANDLE SELECT_ONES:
  or.select_one <- or.edited %>% filter(ref.type == "select_one")
  if(print_debug) cat("Total number of select_one other responses:", nrow(or.select_one), "\n")
  s1_data <- data %>% filter(uniqui %in% or.select_one$uuid)
  if(nrow(s1_data) == 0) cl_select_one <- tibble()
  else cl_select_one <- recode.others_select_one(s1_data, or.select_one, orig_response_col, print_debug)

  # HANDLE SELECT_MULTIPLES:
  or.select_multiple <- or.edited %>% filter(ref.type == "select_multiple")
  if(print_debug) cat("Total number of select_multiple other responses:", nrow(or.select_multiple), "\n")
  sm_data <- data %>% filter(uniqui %in% or.select_multiple$uuid)
  if(nrow(sm_data) == 0)  cl_select_multiple <- tibble()
  else cl_select_multiple <- recode.others_select_multiple(sm_data, or.select_multiple, orig_response_col, print_debug)
  
  cl_all_others <- rbind(cl_select_one, cl_select_multiple) %>% filter(old.value %!=na% new.value)
  
  return(cl_all_others)
}

recode.others_select_one <- function(data, or.select_one, orig_response_col = "response.uk", print_debug = T){

  # invalid select_ones:
  or.select_one.remove <- filter(or.select_one, !is.na(invalid.v))
  if(print_debug) cat(paste("Number of invalid other select_one responses:", nrow(or.select_one.remove)), "\n")
  cl_s1_remove <- rbind(
    or.select_one.remove %>% 
      mutate(variable = name, old.value = !!sym(orig_response_col), new.value = NA, issue = "Invalid other response") %>% 
      select(any_of(CL_COLS)),
    or.select_one.remove %>% 
      mutate(variable = ref.name, old.value = "other", new.value = NA, issue = "Invalid other response") %>% 
      select(any_of(CL_COLS))
  )
  
  # recoding select_ones:
  or.select_one.recode <- or.select_one %>% filter(!is.na(existing.v)) %>% 
    mutate(list_name = get.choice.list.from.name(ref.name), existing.v = str_remove_all(existing.v, ";"))
  if(print_debug) cat(paste("Number of select_one responses to be recoded:", nrow(or.select_one.recode)), "\n")
  choices_lookup <- or.select_one.recode %>% select(existing.v, list_name) %>% rename(label = existing.v) %>% 
    left_join(tool.choices %>% rename(label = !!sym(label_colname), choice_name = name), by = c("label", "list_name")) %>% distinct()
  if(any(is.na(choices_lookup$choice_name))) {
    # TODO: replace stop with a warning, and automatically fix typos using agrep
    missing_names <- choices_lookup %>% filter(is.na(choice_name))
    stop("Choices not found in lists:\n\t", paste(missing_names$label, missing_names$list_name, sep = "\t - in list ", collapse = "\n\t"))
  }
  cl_s1_recode <- rbind(
    or.select_one.recode %>% mutate(variable = name, old.value = !!sym(orig_response_col), new.value = NA, issue = "Recoding other response") %>% 
      select(any_of(CL_COLS)),
    or.select_one.recode %>% rename(label = existing.v) %>% left_join(choices_lookup, by = c("list_name", "label")) %>% 
      mutate(variable = ref.name, old.value = "other", new.value = choice_name, issue = "Recoding other response") %>% select(any_of(CL_COLS))
  )
  
  # true select_ones:
  or.select_one.true <- filter(or.select_one, !is.na(true.v))
  if(print_debug) cat("Number of true other select_one responses:", nrow(or.select_one.true), "\n")
  cl_s1_true <- or.select_one.true %>% 
    mutate(variable = name, old.value = !!sym(orig_response_col), new.value = true.v, issue = "Translating other response") %>% 
    select(any_of(CL_COLS))

  return(rbind(cl_s1_true, cl_s1_remove, cl_s1_recode))
}

recode.others_select_multiple <- function(data, or.select_multiple, orig_response_col = "response.uk", print_debug = T){
  
  # invalid select_multiples:
  or.select_multiple.remove <- filter(or.select_multiple, !is.na(invalid.v))
  cl_sm_remove <- tibble()
  if (nrow(or.select_multiple.remove) > 0) {
    if(print_debug) cat(paste("Number of invalid select_multiple responses:", nrow(or.select_multiple.remove)), "\n")
    variables <- or.select_multiple.remove %>% select(ref.name, name) %>% distinct(ref.name, .keep_all = T)
    for(variable in variables %>% pull(ref.name)){
      thisvar_data <- data %>% filter(uniqui %in% (or.select_multiple.remove %>% filter(ref.name == variable) %>% pull(uniqui)))
      other_variable <- variables$name[which(variables$ref.name == variable)]
      # if the 'other' was the only one selected, change the entire question to NA:
      cl_only_other <- thisvar_data %>% filter(!!sym(variable) == "other") %>% 
        recode.multiple.set.NA(variable, "Invalid other response", other_var_name = other_variable)
      cl_notjust_other <- thisvar_data %>% filter(!!sym(variable) != "other") %>% 
        recode.multiple.remove.choices(variable, "other", "Invalid other response", other_var_name = other_variable)
      cl_sm_remove <- rbind(cl_sm_remove, cl_only_other, cl_notjust_other)
    }
  }
    
  # recoding select_multiples:
  or.select_multiple.recode <- or.select_multiple %>% filter(!is.na(existing.v)) %>% 
    mutate(list_name = get.choice.list.from.name(ref.name), existing.v = str_split(str_squish(existing.v), " *; *", simplify = T))
  
  if(nrow(or.select_multiple.recode) > 0){
    cl_sm_recode <- tibble()
    if(print_debug) cat(paste("Number of select_multiple responses to be recoded:", nrow(or.select_multiple.recode)), "\n")
    
    choices_lookup <- tool.choices %>% filter(list_name %in% or.select_multiple.recode$list_name) %>% 
       rename(label = !!sym(label_colname), choice_name = name)
    
    for (r in 1:nrow(or.select_multiple.recode)) {
      or.row <- or.select_multiple.recode[r,]
      data.row <- data %>% filter(uniqui == or.row$uniqui)
      
      if(nrow(data.row) == 0) {
        # uniqui was not found in data
        next
      }
      chosen_labels <- or.row$existing.v[or.row$existing.v != "" & !is.na(or.row$existing.v)]
      
      if(any(!chosen_labels %in% choices_lookup$label)) {
        # TODO: replace stop with a warning, and automatically fix typos
        stop("Choice '", paste(chosen_labels[!chosen_labels %in% choices_lookup$label], collapse = "' "), "' not found in list ", or.row$list_name)
      }
      
      choices <- choices_lookup %>% filter(list_name == or.row$list_name & label %in% chosen_labels) %>% pull(choice_name)
      
      # check if true.v is also not na
      if(!is.na(or.row$true.v)){
        # in this case, simply add new choices
        cl_sm_recode <- rbind(cl_sm_recode, or.row %>% mutate(variable = name, old.value = !!sym(orig_response_col), new.value = true.v,
                                                              issue = "Translating other response") %>% select(any_of(CL_COLS)))
        cl_sm_recode <- rbind(cl_sm_recode, 
                              recode.multiple.add.choices(data.row, or.row$ref.name, choices, "Recoding other response"))
      }else{
        # read the previous choices and set selection to previous + new
        old_choices <- data.row[[or.row$ref.name]] %>% str_split(" ", simplify = T)
        choices <- c(choices, old_choices[old_choices!="other"])
        cl_sm_recode <- rbind(cl_sm_recode, 
                              recode.multiple.set.choices(data.row, or.row$ref.name, choices, "Recoding other response", other_var_name = or.row$name))
      }
    }
  }
  
  # true select_multiples:
  or.select_multiple.true <- or.select_multiple %>% filter(!is.na(true.v) & check == 2)
  if(print_debug) cat(paste("Number of true select_multiple responses:", nrow(or.select_multiple.true)), "\n")
  cl_sm_true <- or.select_multiple.true %>% 
    mutate(variable = name, old.value = !!sym(orig_response_col), new.value = true.v, issue = "Translating other response") %>% 
    select(any_of(CL_COLS))

  return(rbind(cl_sm_true, cl_sm_remove, cl_sm_recode))

}

# ------------------------------------------------------------------------------

apply.changes <- function(data, clog, is.loop = F, print_debug = T){
  #' Apply changes to main data basing on a cleaning log.
  #'
  #' Outputs warnings if uuids, loop indexes or variables from `clog` are not found in `data`.
  #' Be aware: all values will be written to data as character.
  #' @param data Data (raw.main or raw.loop#)
  #' @param clog Cleaning log - dataframe containing columns uuid, variable, new.value, old.value
  #' @param is.loop Obsolete. This function automatically guesses whether the data contains column 'loop_index'. But there will be warnings produced just in case.
  #' @param print_debug If True (any by default), warnings related to value mismatches and informational messages about the number of changes will be printed.
  #'
  #' @returns Dataframe containing data with applied changes
    
    if(!is.loop && "loop_index" %in% colnames(data)){
      warning("Parameter is.loop is = False, but data contains column 'loop_index'. It will be assumed that this data is, actually, a loop!\n
              N.B.: for the future, you can use apply.changes without the is.loop parameter.\n")
      is.loop <- T
    }else if(is.loop && (!"loop_index" %in% colnames(data))) {
      stop("Parameter is.loop is = True, but data does not contain column 'loop_index'!\n
              N.B.: for the future, you can use apply.changes without the is.loop parameter.\n")
    }
      
    if(!is.loop && ("loop_index" %in% colnames(clog))){
      clog <- filter(clog, is.na(loop_index))
    }else if(is.loop)
      clog <- filter(clog, !is.na(loop_index))
    if(nrow(clog) == 0){
        warning("No changes to be applied (cleaning log empty).")
    }
    else{
        missinguuids <- c()
        missingloop_indexs <- c()
        missingvars <- c()
        changes_counter <- 0
        for (r in 1:nrow(clog)){
          variable <- as.character(clog$variable[r])
            if(!variable %in% colnames(data)) {
              missingvars <- append(missingvars, variable)
              next
            }
          if(is.loop){
            loop_index <- as.character(clog$loop_index[r])
            if(!loop_index %in% data$loop_index){
              missingloop_indexs <- append(missingloop_indexs, loop_index)
              next
            }
            if(print_debug && data[data$loop_index == loop_index, variable] %!=na% clog$old.value[r]){
              warning(paste0("Value in data is different than old.value in Cleaning log!\nloop_index: ", loop_index,
                             "\tVariable: ", variable,
                             "\tExpected: ", clog$old.value[r], "\t found: ", data[data$loop_index == loop_index, variable],
                             "\tReplacing with: ", clog$new.value[r]))
            }
            data[data$loop_index == loop_index, variable] <- as.character(clog$new.value[r])
            changes_counter <- changes_counter + 1
          }else {
            uuid <- as.character(clog$uuid[r])
            if(!uuid %in% data$uuid) {
              missinguuids <- append(missinguuids, uuid)
              next
            }
            if(print_debug && data[data$uuid == uuid, variable] %!=na% clog$old.value[r]){
              warning(paste0("Value in data is different than old.value in Cleaning log!\nUUID: ", uuid,
                             "\tVariable: ", variable,
                            "\tExpected: ", clog$old.value[r], "\t found: ", data[data$uuid == uuid, variable],
                            "\tReplacing with: ", clog$new.value[r]))
            }
            data[data$uuid == uuid, variable] <- as.character(clog$new.value[r])
            changes_counter <- changes_counter + 1
          }
        }
        if(print_debug && length(missinguuids > 0)) warning(paste0("uuids from cleaning log not found in data:\n\t", paste0(missinguuids, collapse = "\n\t")))
        if(print_debug && length(missingloop_indexs) > 0) warning(paste0("loop_indexes from cleaning log not found in data:\n\t", paste0(missingloop_indexs, collapse = "\n\t")))
        if(print_debug && length(missingvars > 0))  warning(paste0("variables from cleaning log not found in data:\n\t", paste0(missingvars, collapse = "\n\t")))
        if(print_debug) cat("\tMade", changes_counter, "changes to the data.\n")
    }
    return(data)
}

undo.changes <- function(data, clog){
  #' ever made changes to your data that you should not have done?
  #' 
  #' Simply provide the same cleaning log as the one that you used to apply the changes and voila...
  #' 
  #' this function will undo the changes and rewind your data to the previous state! as long as you didn't modify the cleaning log since you applied the changes!
  #' This function basically flips old.value and new.value around in the clog, and applies changes again.
  clog <- clog %>% mutate(temp = new.value) %>% mutate(new.value = old.value, old.value = temp)
  return(data %>% apply.changes(clog, print_debug = T))
}

make.logical.check.entry <- function(check, id, question.names, issue, cols_to_keep = c("today"), is.loop = F){
  #' Create a logical check DF
  #'
  #' this function replaces `add.to.cleaning.log`. The functionality is changed:
  #' no longer modifies a global environment variable, instead returns a dataframe.
  #'
  #' @param check Dataframe with data, filtered according to some flag. Must contain columns `uuid` and all columns in `question.names`
  #' @param id The identifier of this logical check.
  #' @param question.names List of relevant queston names for this logical check.
  #' @param cols_to_keep List of columns from raw.main to be included in result.
  #' @param is.loop Obsolete. This function automatically guesses whether the input data (`check`) contains column 'loop_index'. By setting this to True, you will end up with an empty 'loop_index' column if it does not exist in `check`
  #'
  #' @returns Dataframe containing at the least columns: `uuid`, `check.id`, `variable`, `issue`, `old.value`, `new.value`, `explanation`.
  #' This object can be later added to cleaning log.

  res <- data.frame()
  if(is.loop || "loop_index" %in% colnames(check)) {
      cols_to_keep <- append("loop_index", cols_to_keep)
  }
  for(q.n in question.names){
    new.entries <- check %>%
      mutate(variable = q.n, issue=issue,
             old.value =!!sym(q.n), new.value = NA, invalid = NA, explanation = NA)
    new.entries[["check.id"]] <- id
    new.entries <- new.entries %>%
      select(any_of(c(cols_to_keep, "uuid", "check.id", "variable", "issue",
                      "old.value", "new.value", "invalid", "explanation"))) %>%
      rename(survey.date=today) %>% relocate(uuid) %>%
      mutate_all(as.character)
    res <- rbind(res, new.entries)
  }
  if(is.loop & !("loop_index" %in% colnames(res))){
      # res$loop_index <- NA
      res <- res %>% mutate(loop_index = NA, .after = uuid)
    }
  return(res %>% arrange(uuid))
}

# OLD CLEANING LOG FUNCTIONS
# ------------------------------------------------------------------------------

add.to.cleaning.log.other.recode <- function(data, x){
  # [obsolete] use recode.others instead
  warning(" add.to.cleaning.log.other.recode is obsolete! Please use recode.others instead.")
  if(!"existing.other" %in% colnames(x)){
    x <- rename_with(x, ~gsub(".v",".other", .), ends_with(".v"))
  } # a bit of a dirty fix :)
  if (x$ref.type[1]=="select_one") res <- add.to.cleaning.log.other.recode.one(data, x)
  if (x$ref.type[1]=="select_multiple") res <- add.to.cleaning.log.other.recode.multiple(data, x)
  if (res == "err") cat("Errors while recoding other. Check the warnings!\t")
}

add.to.cleaning.log <- function(checks, check.id, question.names=c(), issue="", enumerator.code.col="Staff_Name"){
  #' [obsolete] replaced by make.logical.check.entry
  #' 
  warning(" add.to.cleaning.log is obsolete! Please use make.logical.check.entry instead.")
  for(q.n in question.names){
    new.entries <- checks %>% filter(flag) %>%
      mutate(variable = q.n,
             issue=issue,
             old.value =!!sym(q.n),
             new.value=NA,
             explanation =NA)
    new.entries[["check.id"]] <- check.id
    new.entries <- new.entries %>% select(any_of(c("today", "uuid", "country", "Reporting_organization",
                                                   enumerator.code.col, "check.id",
                                                   "variable", "issue", "old.value", "new.value", "explanation"))) %>%
      dplyr::rename(enumerator.code=enumerator.code.col, survey.date=today)
    cleaning.log.checks <<- arrange(rbind(cleaning.log.checks, new.entries),country, uuid)
  }
}

add.to.cleaning.log.other.remove <- function(data, x){
  # [obsolete] use recode.others instead
  warning(" add.to.cleaning.log.other.remove is obsolete! Please use recode.others instead.")
  issue <- "Invalid other response"
  old.response <- data %>% filter(uuid == x$uuid) %>% pull(!!sym(x$name))
  # remove text of the response
  df <- data.frame(uuid=x$uuid, variable=x$name, issue=issue,
                   old.value=old.response, new.value=NA)
  cleaning.log.other <<- rbind(cleaning.log.other, df)
  # remove relative entries
  if (x$ref.type[1]=="select_one"){
    old.value <- "other"
    df <- data.frame(uuid=x$uuid, variable=x$ref.name, issue=issue, old.value=old.value, new.value=NA)
    cleaning.log.other <<- rbind(cleaning.log.other, df)
  }
  if (x$ref.type[1]=="select_multiple"){
    old.value <- as.character(data[data$uuid==x$uuid[1], x$ref.name])
    l <- str_split(old.value, " ")[[1]]
    new.value <- paste(l[l!="other"], collapse=" ")
    new.value <- ifelse(new.value=="", NA, new.value)
    df <- data.frame(uuid=x$uuid, variable=x$ref.name, issue=issue, old.value=old.value, new.value=new.value)
    cleaning.log.other <<- rbind(cleaning.log.other, df)
    if (is.na(new.value)){
      # set all choices columns to NA
      cols <- colnames(data)[str_starts(colnames(data), paste0(x$ref.name, "/"))]
      oldvalues <- data %>% filter(uuid == x$uuid) %>%
        select(all_of(cols)) %>% unlist() %>% unname()
      df <- data.frame(uuid=x$uuid, variable=cols, issue=issue, old.value=oldvalues, new.value=NA)
      cleaning.log.other <<- rbind(cleaning.log.other, df)
    } else{
      df <- data.frame(uuid=x$uuid, variable=paste0(x$ref.name, "/other"), issue=issue,
                       old.value="1", new.value="0")
      cleaning.log.other <<- rbind(cleaning.log.other, df)
    }
  }
}

add.to.cleaning.log.trans.remove <- function(data, x){
  # [obsolete]
  issue <- "Invalid other response"
  # remove text of the response
  df <- data.frame(uuid=x$uuid, variable=x$name, issue=issue,
                   old.value=x$response.uk, new.value=NA)
  cleaning.log.trans <<- rbind(cleaning.log.trans, df)
}

add.to.cleaning.log.other.recode.one <- function(data, x){
  # [obsolete] use recode.others instead
  warning(" add.to.cleaning.log.other.recode is obsolete! Please use recode.others instead.")
  issue <- "Recoding other response"
  old.response <- data %>% filter(uuid == x$uuid) %>% pull(!!sym(x$name))
  # remove text of the response
  df <- data.frame(uuid=x$uuid, variable=x$name, issue=issue,
                   old.value=old.response, new.value=NA)

  cleaning.log.other <<- rbind(cleaning.log.other, df)
  # get list of choices from other response
  if (str_detect(x$existing.other, ";")) {
    choices <- str_trim(str_split(x$existing.other, ";")[[1]])
  } else {
    choices <- str_trim(str_split(x$existing.other, "\r\n")[[1]])
  }
  choices <- choices[choices!=""]
  if (length(choices)>1) {
    print(select(x, uuid, name))
    stop("More than one existing.option for a select_one question")
  }
  # recode choice
  choice <- choices[1]
  list.name <- filter(tool.survey, name==x$ref.name[1])$list_name
  new.code <- filter(tool.choices, list_name==list.name & !!sym(label_colname)==choice)
  if (nrow(new.code)!=1) {
    warning(paste0("Choice is not in the list. UUID: ", x$uuid,"; recode.into: ", choice))
    return("err")
  }
  else{
    df <- data.frame(uuid=x$uuid, variable=x$ref.name, issue=issue,
                     old.value="other", new.value=new.code$name)
    cleaning.log.other <<- rbind(cleaning.log.other, df)
    return("succ")
  }
}

add.to.cleaning.log.other.recode.multiple <- function(data, x){
  # [obsolete] use recode.others instead
  warning(" add.to.cleaning.log.other.recode is obsolete! Please use recode.others instead.")
  issue <- "Recoding other response"
  old.response <- data %>% filter(uuid == x$uuid) %>% pull(!!sym(x$name))
  # remove text of the response
  df <- data.frame(uuid=x$uuid, variable=x$name, issue=issue,
                   old.value=old.response, new.value=NA)
  cleaning.log.other <<- rbind(cleaning.log.other, df)
  # get list of choices from other response
  if (str_detect(x$existing.other, ";")) {
    choices <- str_trim(str_split(x$existing.other, ";")[[1]])
  } else {
    choices <- str_trim(str_split(x$existing.other, "\r\n")[[1]])
  }
  choices <- choices[choices!=""]
  # set variable/other to "0"
  df <- data.frame(uuid=x$uuid,  variable=paste0(x$ref.name, "/other"), issue=issue,
                   old.value="1", new.value="0")
  cleaning.log.other <<- rbind(cleaning.log.other, df)
  # get list of choices already selected
  old.value <- as.character(data[data$uuid==x$uuid[1], x$ref.name[1]])
  l <- str_split(old.value, " ")[[1]]
  l.cumulative <- l[l!="other"]
  # add to the cleaning log each choice in the other response
  for (choice in choices){
    # set corresponding variable to "1" if not already "1"
    list.name <- filter(tool.survey, name==x$ref.name[1])$list_name
    new.code <- filter(tool.choices, list_name==list.name & !!sym(label_colname)==choice)
    if (nrow(new.code)!=1){
      warning(paste0("Choice is not in the list. UUID: ", x$uuid,"; recode.into: ", choice))
      return("err")
    }
    variable.name <- paste0(x$ref.name, "/", new.code$name)
    if (variable.name %in% colnames(data)){
      old.boolean <- data[[variable.name]][data$uuid==x$uuid[1]]
    } else stop(paste("Column", variable.name,"not found in data"))
    if (old.boolean=="0"){
      df <- data.frame(uuid=x$uuid, variable=variable.name, issue=issue,
                       old.value=old.boolean, new.value="1")
      cleaning.log.other <<- rbind(cleaning.log.other, df)
    }
    l.cumulative <- unique(c(l.cumulative, new.code$name))
  }
  # update cumulative variable
  new.value <- str_squish(paste(sort(l.cumulative), collapse=" "))
  df <- data.frame(uuid=x$uuid, variable=x$ref.name, issue=issue,
                   old.value=old.value, new.value=new.value)
  cleaning.log.other <<- rbind(cleaning.log.other, df)
  return("succ")
}


# ------------------------------------------------------------------------------------------
# DELETION LOG FUNCTIONS
# ------------------------------------------------------------------------------------------

create.deletion.log <- function(data, col_enum, reason){
  #' Creates a deletion log for the provided data and reason.
  #'
  #' @param data Dataframe containing columns 'uuid' and `col_enum`. For example, this could be a subset of `raw.main`, filtered.
  #' @param col_enum Name of the column which contains the enumerator's id.
  #' @param reason This is a string describing the reason for removing a survey from data.
  #' @returns A dataframe containing a deletion log with columns `uuid`, `col_enum`, `reason`, OR an empty dataframe if `data` has 0 rows.
  
  if(!col_enum %in% colnames(data))
    stop(paste0("Enumerator column (", col_enum, ") not found in the data!\nDid you mean ", colnames(data)[agrep(col_enum, colnames(data), max.distance = 1, value = T)], "?"))
  
  if(nrow(data) > 0){
    # if it's a loop, then include the loop_index in the deletion log
    if("loop_index" %in% colnames(data))
      data <- data %>% select(uuid, loop_index, any_of(col_enum))
    else
      data <- data %>% select(uuid, any_of(col_enum))
    
    return(data %>% mutate(reason=reason))
  }else return(data.frame())
}


# ------------------------------------------------------------------------------------------
# FIND & TRANSLATE RESPONSES
# ------------------------------------------------------------------------------------------

find.responses <- function(data, questions.db, values_to="response.uk", is.loop = F){
  #' Look up a raw Kobo dataframe to find all responses to a given set of questions.
  #'
  #' The dataframe `questions.db` needs to contain a column `name` (like a subset of `tool.survey`) which will be used to look up `data`.
  #' The input `data` needs to contain a column "uuid", and all the columns specified in `questions.db`
  #' The vector containing found responses is stored in column specified by parameter `values_to`.
  #'
  #' Be warned: all responses will be converted to character.
  #' @param values_to Name of the column in which found responses will be stored.
  #' @returns A dataframe containing columns "uuid", "question.name", and the column specified by `values_to`. Additionally, "loop_index" if `is.loop` is TRUE.
  #' @example
  #' q.db <- data.frame(name = c("age", "occupation"))
  #' raw.data <- data.frame(age = c(21,32), occupation = c("cook", "train conductor"), uuid = c("abc","def"))
  #' find.responses(raw.data, q.db, "responses")

  if(nrow(questions.db) == 0){
      warning("questions.db is empty - returning an empty dataframe.")
      return(data.frame())
  }

  if(nrow(data) == 0){
      warning("data is empty - returning an empty dataframe.")
      return(data.frame())
  }

  if(!is.loop){
    data[["loop_index"]] <- NA
  }
  responses <- data %>%
      select(c("uuid", "loop_index", any_of(questions.db$name))) %>%
      pivot_longer(cols = any_of(questions.db$name),
                   names_to="question.name", values_to=values_to,
                   values_transform = as.character) %>%
      filter(!is.na(!!sym(values_to))) %>%
      select(uuid, loop_index, question.name, !!sym(values_to))

  if(is.loop){
    responses.j <- responses %>%
      left_join(questions.db, by=c("question.name"="name")) %>% dplyr::rename(name="question.name") %>%
      left_join(select(data, loop_index), by="loop_index")
    } else {
    responses.j <- responses %>%
        left_join(questions.db, by=c("question.name"="name")) %>% dplyr::rename(name="question.name") %>%
        left_join(select(data, uuid), by="uuid") 
    # relevant_colnames <- relevant_colnames[!relevant_colnames %in% c("loop_index")]
    }
  return(responses.j)
}

find.other.responses <- function(data, other.db, values_to = "response.uk", is.loop = F){
  #' Look up raw Kobo data using `other.db` to find other responses...
  #' 
  #' This is a small utility to make life easier for AOs: this function finds also the original response to the 'ref.name'
  #' which is the select_one or select_multiple question where 'other' was selected.
  #' (it will be 'other' for select_ones, more useful for select_multiples... )
  #' 
  
  other.responses <- find.responses(data, other.db, values_to, is.loop) %>% 
    mutate(uquest = paste0(uuid, ref.name))
  ref.responses <- find.responses(data %>% filter(uuid %in% other.responses$uuid), 
                                  other.db %>% mutate(name = ref.name), "ref.response") %>% 
    mutate(uquest = paste0(uuid, name)) %>% filter(uquest %in% other.responses$uquest)
  if(nrow(ref.responses) != nrow(other.responses)) stop("Something went wrong while searching for ref.responses :(")
  
  binded.responses <- other.responses %>% 
    left_join(ref.responses %>% select(uquest, ref.response), by = "uquest") %>% 
    select(-uquest) %>% relocate(ref.response, .after = ref.name)
  
  return(binded.responses)
}

translate.responses <- function(responses, values_from = "response.uk", source_lang = NULL, target_lang = "en"){

  #' Translate a vector from a given dataframe.
  #'
  #' The provided dataframe `responses` must contain the column `values_from` which will be used as input vector for the translation.
  #' Also outputs informative logs to file named "translate_info.csv". You may specify the target language using `target_lang` parameter (or keep it NULL - by default it will be autodected)
  #'
  #' @param respones Dataframe containing a column which shall be translated.
  #' @param values_from Name of the column from `responses` which shall be translated.
  #' @param source_lang String containing a two-letter language code. The input vector will be translated from this language. Defaults to NULL - autodetect language.
  #' @param target_lang A two-letter language code. Input vector will be translated into this language. Defaults to 'en' - translation to English
  #' @returns The same dataframe as `responses`, but with a new column, containing the translation.
  #' The column will be named according to the target language. By default, the output will be stored in column named 'response.en'
  info_df <- data.frame()
  responses_batch <- data.frame()
  temp_resp_whole <- data.frame()
  start_time <- Sys.time()
  # relevant_colnames <- c("uuid","loop_index","name", "ref.name","full.label","ref.type",
                         # "choices.label", values_from)

  # extract unique responses from the source dataframe
  responses <- responses %>% mutate(resp_lower = str_to_lower(!!sym(values_from)))
   
  input_vec <- responses %>% distinct(resp_lower) %>% pull(resp_lower)
  # cleaning up html leftovers:
  input_vec <- gsub("&#39;", "'", input_vec)
  # counts characters which will be translated
  char_counter <- sum(str_length(input_vec))
  # TODO: pause here, print the char_counter, and ask the user if the translation should go ahead
  if (char_counter > 200000){
    yes_no <- svDialogs::dlgInput("The number of characters exceeds 200,000. Please enter [YES] if you would like to proceed or [NO] to kill:", "YES or NO")$res
  } else{
    yes_no <- "YES"
  }
  batching <- svDialogs::dlgInput(paste0("How many batches would you like to split your translation (",char_counter," characters)? (please only integer)"), 0)$res
  batching <- as.numeric(batching)
  if(yes_no == "YES"){
    if(length(input_vec) > 0){
      col_name <- paste0("response.",target_lang)
      
      temp_resp <- tibble(input_vec)
      temp_resp[[col_name]] <- NA
      temp_resp <-  temp_resp[sample(1:nrow(temp_resp)),]
      ## create batches
      temp_resp_batches <- split(temp_resp, factor(sort(rank(row.names(temp_resp))%%batching)))
      progress.bar.title <- as.character(Sys.time())
      pb <- tcltk::tkProgressBar(progress.bar.title, "Number of batches executed", 0, batching, 0, width = 600)
      prog <- 1
      for (temp_resp_batch in temp_resp_batches){
        tcltk::setTkProgressBar(pb, prog, progress.bar.title, paste0("Number of batches executed: ", prog, " of ", batching,"\n",length(temp_resp_batch$input_vec)," responses will be translated to ",target_lang, "\nThis means ",sum(str_length(temp_resp_batch$input_vec))," utf-8 characters."))
        prog <- prog + 1
        # actual translation:
        result_vec <- NULL
        result_vec <- try(translateR::translate(content.vec = temp_resp_batch$input_vec,
                            microsoft.api.key = source("resources/microsoft.api.key_ukraine.R")$value,
                            microsoft.api.region = "switzerlandnorth",
                            source.lang = source_lang, target.lang = target_lang))
        if(inherits(result_vec,"try-error")) break
        # checking the results
        info_df <- rbind(info_df, data.frame(## DEBUGG IT HERE
          "input_responses_num" = length(temp_resp_batch$input_vec),
          "translated_characters_num" = sum(str_length(temp_resp_batch$input_vec)),
          "language_from" = ifelse(is.null(source_lang), "NULL", source_lang),
          "result_num" = length(result_vec),
          "time_elapsed" = as.numeric(Sys.time() - start_time),
          "date"=Sys.Date(),
          "status"=NA))
        if(is.null(result_vec)){
          warning("Error while translating responses: result_vec is NULL\n")
          info_df$status <- "error"
        }else{
          temp_resp_batch[[col_name]] <- gsub("&#39;", "'", result_vec)
          if(length(result_vec) == length(temp_resp_batch$input_vec)){ 
            info_df$status <- "success"
            # bind the translated and source dfs
            temp_resp_whole <- rbind(temp_resp_whole,temp_resp_batch)
          }else{
            info_df$status <- "partial success"
          }
        }
      }
      close(pb)
      if("partial success" %in% info_df$status){
        svDialogs::msgBox("translate.responses: finished - PARTIAL SUCCESS?")
      } else{
        svDialogs::msgBox("translate.responses: finished - SUCCESS")
      }
      responses <- responses %>% left_join(temp_resp_whole, by = c("resp_lower" = "input_vec")) 
    }else{
      warning("Nothing to be translated")
    }
  }
  # dump info about the results of translation
  log_filename <- "translate_info.csv"
  if(file.exists(log_filename)) write.table(info_df, file = log_filename, append = T, row.names = F, col.names = F, sep = ',')
  else write.table(info_df, file = log_filename, row.names = F, col.names = T, sep = ',')

  responses <- responses %>% select(-resp_lower)
  return(responses)
}


create.translate.requests <- function(questions.db, responses.j, response_colname = "response.en", is.loop = F){
  #' Format a dataframe containing responses to prepare for other/translate requests
  #' 
  #' Relocates columns and adds the TEI columns.
  #' 
  #' @param questions.db Dataframe containing questions (e.g. other.db or trans.db)
  #' @param responses.j Dataframe containing responses to any questions from `questions.db`
  #' @param response_colname String containing name of the column which has the relevant response (tanslated to English in most cases)
  #' @param is.loop unused

    relevant_colnames <- c("uuid", "loop_index", "name", "ref.name","full.label","ref.type","ref.response", "choices.label", "today")

    response_cols <- colnames(responses.j)[str_starts(colnames(responses.j), "response")]
    relevant_colnames <- append(relevant_colnames, response_cols)
    responses.j <- responses.j %>%
        select(any_of(relevant_colnames)) %>%
        relocate(all_of(response_cols), .after = last_col()) %>%
        mutate("TRUE other (provide a better translation if necessary)"=NA,
               "EXISTING other (copy the exact wording from the options in column choices.label)"=NA,
               "INVALID other (insert yes or leave blank)"=NA) %>%
        arrange(name, !!sym(response_cols[which(response_cols == response_colname)]))
    # if(!is.loop) {
    #     responses.j <- responses.j %>% select(-loop_index)
    #     }

    return(responses.j)
}

#-------------------------------------------------------------------------------
# misc functions for pulling and checking data
#-------------------------------------------------------------------------------

what.country <- function(id){
  #' [useless] Looks up raw.main to find to which country an id belongs to.
  #'
  #' @param id uuid to look up in `raw.main`
  #' @returns a string found in the `country` column of `raw.main`.
  return(raw.main %>% filter(uuid == id) %>% pull(country))
}

pull.raw <- function(uniquis, print_warnings = T){
    #' Pull records from raw data with the given uuids or loop_indexes.
    #' 
    #' A vector of either uuids or loop_indexes must be provided as the argument.
    #' If pulling by uuid, the dataframe used is `raw.main`.
    #' Otherwise, all of the loop_indexes must belong to the same loop (so all must start with the same string "loop#"),
    #' and the data is pulled from dataframe `raw.loop1`, or `raw.loop2`, etc...
    #' 
    #' @param uniquis Character vector of either uuids or loop_indexes
    #' @param print_warning Whether to print warnings about identifiers that were not found in data. This slows down this function a bit.
    #' @returns Dataframe: raw.main, or one of the raw.loops, filtered to only include the provided identifiers.

  if(all(is.na(uniquis)) & print_warnings) warning("Nothing to pull (provided uniquis vector is empty)")
  uniquis <- str_squish(uniquis)
  # check if we're dealing with loop_indexes or uuids:
  loop_num <- str_extract(str_extract(uniquis, "^loop\\d+_"), "\\d+")
  if(all(is.na(loop_num))){
    ### uuids
    find.missing.ids(raw.main, uniquis)
    return(raw.main %>% filter(uuid %in% uniquis))       
  } else{
    ### loop_indexes
    if(any(loop_num != loop_num[1])) stop("Provided loop indexes belong to different loops!")
    
    loop_to_pull <- switch (loop_num[1],                                 
      "1" = raw.loop1, 
      "2" = raw.loop2, 
      "3" = raw.loop3, 
      "4" = raw.loop4, 
      "5" = raw.loop5, 
      "6" = raw.loop6  
      # add additional loops if necessary
    )
    find.missing.ids(loop_to_pull, uniquis)
    return(loop_to_pull %>% filter(loop_index %in% uniquis))
  }
}

find.missing.ids <- function(data, uniquis, print_warnings = T){
  if(length(uniquis) == 0) return(character(0))
  if(any(str_starts(uniquis, "loop\\d"))){
    if(!"loop_index" %in% names(data)) stop("uniquis are loop indexes, but data does not contain column loop_index!")
    if(!all(str_starts(uniquis, "loop\\d")) & print_warnings) warning("not all provided identifiers are loop_indexes!")
    data$uniqui <- data$loop_index
  } else if ("uuid" %in% names(data)) {
    data$uniqui <- data$uuid
  } else stop("data does not contain column uuid!")

  if(any(!data$uniqui %in% uniquis)){
    missing_ids <- uniquis[!uniquis %in% data$uniqui]
    if(length(missing_ids) == length(uniquis) & print_warnings) warning("NONE of the identifiers were found in data!")
    else if(length(missing_ids) > 0 & print_warnings) warning("Identifiers not found in data:\n\t", paste(missing_ids, collapse = ",\n\t"))
    return(missing_ids)
  }
} 

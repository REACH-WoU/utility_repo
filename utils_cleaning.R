
# ------------------------------------------------------------------------------------------
# SAVING RESPONSES/REQUESTS
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
save.responses <- function(df, wb_name, or.submission=""){
  # TODO: upgrade this function to work on changing df sizes
  style.col.color <- createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000", 
                                 valign="top", wrapText=T)
  style.col.color.first <- createStyle(textDecoration="bold", fgFill="#E5FFCC", valign="top",
                                       border="TopBottomLeftRight", borderColour="#000000", wrapText=T)
  style.col.color.first2 <- createStyle(textDecoration="bold", fgFill="#CCE5FF", valign="top",
                                        border="TopBottomLeftRight", borderColour="#000000", wrapText=T)
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet1")
  writeData(wb = wb, x = df, sheet = "Sheet1", startRow = 1)
  addStyle(wb, "Sheet1", style = style.col.color, rows = 1:(nrow(df)+1), cols=10)
  addStyle(wb, "Sheet1", style = style.col.color, rows = 1:(nrow(df)+1), cols=11)
  addStyle(wb, "Sheet1", style = style.col.color, rows = 1:(nrow(df)+1), cols=12)
  setColWidths(wb, "Sheet1", cols=1, widths=35)
  setColWidths(wb, "Sheet1", cols=c(5, 7), widths=50)
  setColWidths(wb, "Sheet1", cols=c(8:9), widths=30)
  setColWidths(wb, "Sheet1", cols=c(2:4, 6), widths=20)
  setColWidths(wb, "Sheet1", cols=c(10:12), widths=40)
  addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=1)
  addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=2)
  addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=3)
  addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=4)
  addStyle(wb, "Sheet1", style = createStyle(wrapText=T, valign="top"), rows = 1:(nrow(df)+1), cols=5)
  addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=6)
  addStyle(wb, "Sheet1", style = createStyle(wrapText=T, valign="top"), rows = 1:(nrow(df)+1), cols=7)
  addStyle(wb, "Sheet1", style = createStyle(wrapText=T, valign="top"), rows = 1:(nrow(df)+1), cols=8)
  addStyle(wb, "Sheet1", style = createStyle(wrapText=T, valign="top"), rows = 1:(nrow(df)+1), cols=9)
  addStyle(wb, "Sheet1", style = createStyle(textDecoration="bold"), rows = 1, cols=1:ncol(df))
  addStyle(wb, "Sheet1", style = style.col.color.first, rows = 1, cols=10:12)
  modifyBaseFont(wb, fontSize = 10, fontColour = "black", fontName = "Calibri")
  filename <- paste0("output/checking/requests/", wb_name, ".xlsx")
  saveWorkbook(wb, filename, overwrite=TRUE)
}

save.trans.responses <- function(df, or.submission=""){
  for (countr in country_list){
    df1 <- df %>% filter(country == str_to_lower(countr))
    style.col.color <- createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000", 
                                   valign="top", wrapText=T)
    style.col.color.first <- createStyle(textDecoration="bold", fgFill="#E5FFCC", valign="top",
                                         border="TopBottomLeftRight", borderColour="#000000", wrapText=T)
    style.col.color.first2 <- createStyle(textDecoration="bold", fgFill="#CCE5FF", valign="top",
                                          border="TopBottomLeftRight", borderColour="#000000", wrapText=T)
    wb <- createWorkbook()
    addWorksheet(wb, "Sheet1")
    writeData(wb = wb, x = df, sheet = "Sheet1", startRow = 1)
    addStyle(wb, "Sheet1", style = style.col.color, rows = 1:(nrow(df)+1), cols=13-2)
    addStyle(wb, "Sheet1", style = style.col.color, rows = 1:(nrow(df)+1), cols=13-1)
    addStyle(wb, "Sheet1", style = style.col.color, rows = 1:(nrow(df)+1), cols=13)
    setColWidths(wb, "Sheet1", cols=1, widths=35)
    setColWidths(wb, "Sheet1", cols=c(5, 7), widths=50)
    setColWidths(wb, "Sheet1", cols=c(8:10), widths=30)
    setColWidths(wb, "Sheet1", cols=c(2:4, 6), widths=20)
    setColWidths(wb, "Sheet1", cols=c(11:13), widths=40)
    addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=1)
    addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=2)
    addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=3)
    addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=4)
    addStyle(wb, "Sheet1", style = createStyle(wrapText=T, valign="top"), rows = 1:(nrow(df)+1), cols=5)
    addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=6)
    for(i in 7:ncol(df)-3){
      addStyle(wb, "Sheet1", style = createStyle(wrapText=T, valign="top"), rows = 1:(nrow(df)+1), cols=i)
    }
    addStyle(wb, "Sheet1", style = createStyle(textDecoration="bold"), rows = 1, cols=1:ncol(df))
    addStyle(wb, "Sheet1", style = style.col.color.first, rows = 1, cols=11:ncol(df))
    modifyBaseFont(wb, fontSize = 10, fontColour = "black", fontName = "Calibri")
    filename <- paste0("output/checking/requests/",countr,"_translate_responses.xlsx")
    saveWorkbook(wb, filename, overwrite=TRUE)
    rm(df1)
  }
}

# ------------------------------------------------------------------------------------------
# OUTLIER SECTION
# ------------------------------------------------------------------------------------------
save.outlier.responses <- function(df, or.submission=""){
  for (i in country_list){
    df1 <- df %>% 
      filter(country == i)
    style.col.color <- createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000", 
                                   valign="top", wrapText=T)
    style.col.color.first <- createStyle(textDecoration="bold", fgFill="#E5FFCC", valign="top",
                                         border="TopBottomLeftRight", borderColour="#000000", wrapText=T)
    style.col.color.first2 <- createStyle(textDecoration="bold", fgFill="#CCE5FF", valign="top",
                                          border="TopBottomLeftRight", borderColour="#000000", wrapText=T)
    wb <- createWorkbook()
    addWorksheet(wb, "Sheet1")
    writeData(wb = wb, x = df1, sheet = "Sheet1", startRow = 1)
    addStyle(wb, "Sheet1", style = style.col.color, rows = 1:(nrow(df1)+1), cols=6)
    addStyle(wb, "Sheet1", style = style.col.color, rows = 1:(nrow(df1)+1), cols=7)
    setColWidths(wb, "Sheet1", cols=c(1:5), widths=35)
    setColWidths(wb, "Sheet1", cols=c(6:7), widths=40)
    addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df1)+1), cols=1)
    addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df1)+1), cols=2)
    addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df1)+1), cols=3)
    addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df1)+1), cols=4)
    addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df1)+1), cols=5)
    addStyle(wb, "Sheet1", style = createStyle(wrapText=T, valign="top"), rows = 1:(nrow(df1)+1), cols=6)
    addStyle(wb, "Sheet1", style = createStyle(wrapText=T, valign="top"), rows = 1:(nrow(df1)+1), cols=7)
    addStyle(wb, "Sheet1", style = createStyle(textDecoration="bold"), rows = 1, cols=1:ncol(df1))
    addStyle(wb, "Sheet1", style = style.col.color.first, rows = 1, cols=6:7)
    modifyBaseFont(wb, fontSize = 10, fontColour = "black", fontName = "Calibri")
    filename <- paste0("output/checking/outliers/",i,"_outliers_requests.xlsx")
    saveWorkbook(wb, filename, overwrite=TRUE)
    rm(df1)
  }
}

save.outlier.responses_msna <- function(df, or.submission=""){
    style.col.color <- createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000", 
                                   valign="top", wrapText=T)
    style.col.color.first <- createStyle(textDecoration="bold", fgFill="#E5FFCC", valign="top",
                                         border="TopBottomLeftRight", borderColour="#000000", wrapText=T)
    style.col.color.first2 <- createStyle(textDecoration="bold", fgFill="#CCE5FF", valign="top",
                                          border="TopBottomLeftRight", borderColour="#000000", wrapText=T)
    wb <- createWorkbook()
    addWorksheet(wb, "Sheet1")
    writeData(wb = wb, x = df, sheet = "Sheet1", startRow = 1)
    addStyle(wb, "Sheet1", style = style.col.color, rows = 1:(nrow(df)+1), cols=6)
    addStyle(wb, "Sheet1", style = style.col.color, rows = 1:(nrow(df)+1), cols=7)
    setColWidths(wb, "Sheet1", cols=c(1:5), widths=35)
    setColWidths(wb, "Sheet1", cols=c(6:7), widths=40)
    addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=1)
    addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=2)
    addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=3)
    addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=4)
    addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=5)
    addStyle(wb, "Sheet1", style = createStyle(wrapText=T, valign="top"), rows = 1:(nrow(df)+1), cols=6)
    addStyle(wb, "Sheet1", style = createStyle(wrapText=T, valign="top"), rows = 1:(nrow(df)+1), cols=7)
    addStyle(wb, "Sheet1", style = createStyle(textDecoration="bold"), rows = 1, cols=1:ncol(df))
    addStyle(wb, "Sheet1", style = style.col.color.first, rows = 1, cols=6:7)
    modifyBaseFont(wb, fontSize = 10, fontColour = "black", fontName = "Calibri")
    filename <- paste0("output/checking/outliers/outliers_requests.xlsx")
    saveWorkbook(wb, filename, overwrite=TRUE)
}

save.follow.up.requests <- function(cleaning.log, data){
  use.color <- function(check.id){
    return(str_starts(check.id, "0")) 
    # |  str_starts(check.id, "3") | str_starts(check.id, "4"))
  }
  # define styles
  style.col.color <- createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000")
  style.col.color.first <- createStyle(textDecoration="bold", fgFill="#E5FFCC",
                                       border="TopBottomLeftRight", borderColour="#000000", wrapText=F)
  col.style <- createStyle(textDecoration="bold", fgFill="#CECECE",halign="center",
                           border="TopBottomLeftRight", borderColour="#000000")
  # arrange cleaning.log so that colors are properly assigned later
  cleaning.log <- cleaning.log %>% 
    arrange(country) %>% 
    group_by(country) %>% 
    group_modify(~ rbind(
      filter(.x, !use.color(check.id)) %>% arrange(check.id, uuid),
      filter(.x, use.color(check.id)) %>% arrange(check.id)))
  # add missing columns
  cl <- cleaning.log %>% 
    left_join(select(data, uuid, `_submission_time`), by="uuid") %>% 
    rename(submission_time="_submission_time") %>% 
    select(uuid, submission_time, country, Reporting_organization, enumerator.code, check.id, 
           variable, issue, old.value, new.value) %>% 
    mutate(explanation=NA)
  cl <- cl %>% arrange(match(check.id, str_sort(unique(cl$check.id), numeric=T)))
  for(i in country_list){
    cl1 <- cl %>% 
      filter(country == i)
    # save follow-up requests
    wb <- createWorkbook()
    addWorksheet(wb, "Follow-up")
    writeData(wb = wb, x = cl1, sheet = "Follow-up", startRow = 1)
    
    addStyle(wb, "Follow-up", style = style.col.color, rows = 1:(nrow(cl1)+1), cols=10)
    addStyle(wb, "Follow-up", style = style.col.color, rows = 1:(nrow(cl1)+1), cols=11)
    
    setColWidths(wb, "Follow-up", cols=1:ncol(cl1), widths="auto")
    setColWidths(wb, "Follow-up", cols=7, widths=50)
    setColWidths(wb, "Follow-up", cols=8, widths=50)
    
    addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(nrow(cl1)+1), cols=7)
    addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(nrow(cl1)+1), cols=8)
    
    addStyle(wb, "Follow-up", style = col.style, rows = 1, cols=1:ncol(cl1))
    
    col.id <- which(colnames(cl1)=="old.value")
    if(nrow(cl1) > 0){
      random.color <- ""
      for (r in 2:nrow(cl1)){
        if((!use.color(as.character(cl1[r, "check.id"])) & 
            as.character(cl1[r, "uuid"])==as.character(cl1[r-1, "uuid"]) &
            as.character(cl1[r, "check.id"])==as.character(cl1[r-1, "check.id"])) |
           (use.color(as.character(cl1[r, "check.id"])) & 
            as.character(cl1[r, "country"])==as.character(cl1[r-1, "country"]) &
            as.character(cl1[r, "check.id"])==as.character(cl1[r-1, "check.id"]))){
          if (random.color == "") random.color <- randomColor(1, luminosity = "light")
          addStyle(wb, "Follow-up", style = createStyle(fgFill=random.color, wrapText=T), 
                   rows = r:(r+1), cols=col.id)
        } else random.color=""
      }
    }
    addStyle(wb, "Follow-up", style = style.col.color.first, rows = 1, cols=10)
    addStyle(wb, "Follow-up", style = style.col.color.first, rows = 1, cols=11)
    filename <- paste0("output/checking/requests/", str_to_lower(i) , "_follow_up_requests.xlsx")
    saveWorkbook(wb, filename, overwrite = TRUE)
    rm(cl1)
  }
}

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
  } else {
    cat(paste("Loading",length(filenames),file.type,"requests files:\n"),paste(filenames, collapse = "\n "),"\n")
    res <- data.frame()
    for (filename in filenames){
      # load file
      other <- read_xlsx(filename, col_types = "text", sheet = sheet)
      if (filename==filenames[1]) res <- other
      else res <- rbind(res, other)
    }
    c_tei_cols <- c("true", "existing", "invalid")
    for(c in c_tei_cols) colnames(res)[str_starts(colnames(res), str_to_upper(c))] <- paste0(c,'.v')
    if(validate){
      c_tei_cols <- paste0(c_tei_cols, ".v")
      if(all(c_tei_cols %in% colnames(res))){
        res <- res %>% mutate(check = rowSums(is.na(select(res, all_of(c_tei_cols)))))
        check.res <- res %>% select(c(
          any_of(c("uuid","ref.name","check")))) %>% filter(check!=2)
        if(nrow(check.res)>0) {
          warning(paste0("Missing entries or multiple columns selected:\n", paste0(" ", unlist(check.res[,1]) , collapse = "\n")))
        }
      }else{
        stop("One or more of 'true', 'existing', 'invalid' columns not found in requests files.")
      }
    }
    return(res)   
  }
}

load.edited <- function(dir.edited, file.type){
  #' Load logs from specified directory.
  #' 
  #' This function is superceded by load.requests

  
  # file.type should be one of the following: 
  valid_types = c("other","translate","follow_up","outliers")
  if(!(file.type %in% valid_types))
    warning("Unexpected file.type for load.edited")
  
  filenames <- list.files(dir.edited, recursive=FALSE, full.names=TRUE, ignore.case = TRUE,
                          pattern=paste0(".*",file.type,"_((responses)|(requests))(_edited)?.*\\.xlsx$"))
  if (length(filenames) == 0){
    warning(paste("Files with",file.type,"responses not found!"))
  } else {
    cat(paste("Loading",length(filenames),file.type,"logs:\n"),paste(filenames, collapse = "\n "),"\n")
    res <- data.frame()
    for (filename in filenames){
      # load file
      other <- read_xlsx(filename) %>% mutate(uuid=uuid, .before=1)
      if (filename==filenames[1]) res <- other
      else res <- rbind(res, other)
    }
    return(res)   
    
  }
}

load.logic.request <- function(dir.requests){
  logic.filenames <- list.files(dir.requests, pattern="follow_up_requests",
                                recursive=FALSE, full.names=TRUE)
  cat(paste("\nLoading",length(logic.filenames),"logic requests logs:\n"),paste(logic.filenames, collapse = "\n "),"\n")
  for (filename in logic.filenames){
    # load file
    trans <- read_xlsx(filename) %>% 
      mutate(uuid=uuid, .before=1)
    if (filename==logic.filenames[1]) res <- trans
    else res <- rbind(res, trans)
  }
  return(res)
}

load.outlier.edited <- function(dir.outlier.edited){
  logic.filenames <- list.files(dir.outlier.edited, pattern="outliers_responses.xlsx",
                                recursive=TRUE, full.names=TRUE)
  cat(paste("Loading",length(logic.filenames),"outlier logs:\n"),paste(logic.filenames, collapse = "\n "),"\n")
  res <- data.frame()
  for (filename in logic.filenames){
    # load file
    trans <- read_xlsx(filename) %>% 
      mutate(uuid=uuid, .before=1)
    if (filename==logic.filenames[1]) res <- trans
    else res <- rbind(res, trans)
  }
  return(res)
}

# ------------------------------------------------------------------------------------------
# CLEANING LOG FUNCTIONS
# ------------------------------------------------------------------------------------------

make.logical.check.entry <- function(check, id, question.names, issue, cols_to_keep = c("today")){
  #' Create a logical check DF
  #' 
  #' this function replaces `add.to.cleaning.log`. The functionality is changed: 
  #' no longer modifies a global environment variable, instead returns a dataframe.
  #' 
  #' @param check Dataframe the same as raw.main, but containing a column named `flag`
  #' @param id The identifier of this logical check. 
  #' @param question.names List of relevant queston names for this logical check.
  #' @param cols_to_keep List of columns from raw.main to be included in result.
  #' 
  #' @returns Dataframe containing at the least columns: `uuid`, `check.id`, `variable`, `issue`, `old.value`, `new.value`, `explanation`.
  #' This object can be later added to cleaning log.
  
  res <- data.frame()
  for(q.n in question.names){
    new.entries <- checks %>% filter(flag) %>% 
      mutate(variable = q.n, issue=issue, 
             old.value =!!sym(q.n), new.value = NA, explanation = NA)
    new.entries[["check.id"]] <- id
    new.entries <- new.entries %>% 
      select(any_of(c(cols_to_keep, "check.id", "variable", "issue",
                      "old.value", "new.value", "explanation"))) %>%
      dplyr::rename(survey.date=today)
    res <- rbind(res, new.entries)
  }
  return(res %>% arrange(uuid))
}

add.to.cleaning.log.other.recode <- function(data, x){
  if (x$ref.type[1]=="select_one") res <- add.to.cleaning.log.other.recode.one(x)
  if (x$ref.type[1]=="select_multiple") res <- add.to.cleaning.log.other.recode.multiple(data, x)
  if (res == "err") cat("Errors encountered while recoding other. Check the warnings!")
}

add.to.cleaning.log <- function(checks, check.id, question.names=c(), issue="", enumerator.code.col="Staff_Name"){
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
  issue <- "Invalid other response"
  # remove text of the response
  df <- data.frame(uuid=x$uuid, variable=x$name, issue=issue, 
                   old.value=x$response.uk, new.value=NA)
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
      df <- data.frame(uuid=x$uuid, variable=cols, issue=issue, old.value="0 or 1", new.value=NA)
      cleaning.log.other <<- rbind(cleaning.log.other, df)
    } else{
      df <- data.frame(uuid=x$uuid, variable=paste0(x$ref.name, "/other"), issue=issue,
                       old.value="1", new.value="0")
      cleaning.log.other <<- rbind(cleaning.log.other, df)
    }
  }
}

add.to.cleaning.log.trans.remove <- function(data, x){
  issue <- "Invalid other response"
  # remove text of the response
  df <- data.frame(uuid=x$uuid, variable=x$name, issue=issue, 
                   old.value=x$response.uk, new.value=NA)
  cleaning.log.trans <<- rbind(cleaning.log.trans, df)
}

add.to.cleaning.log.other.recode.one <- function(x){
  issue <- "Recoding other response"
  # remove text of the response
  df <- data.frame(uuid=x$uuid, variable=x$name, issue=issue,
                   old.value=x$response.uk, new.value=NA)
  
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
  issue <- "Recoding other response"
  # remove text of the response
  df <- data.frame(uuid=x$uuid, variable=x$name, issue=issue,
                   old.value=x$response.uk, new.value=NA)
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
    } else stop("Column not found")
    if (old.boolean=="0"){
      df <- data.frame(uuid=x$uuid, variable=variable.name, issue=issue,
                       old.value=old.boolean, new.value="1")
      cleaning.log.other <<- rbind(cleaning.log.other, df)
    }
    l.cumulative <- unique(c(l.cumulative, new.code$name))
  }
  # update cumulative variable
  new.value <- paste(sort(l.cumulative), collapse=" ")
  df <- data.frame(uuid=x$uuid, variable=x$ref.name, issue=issue,
                   old.value=old.value, new.value=new.value)
  cleaning.log.other <<- rbind(cleaning.log.other, df)
  return("succ")
}


# ------------------------------------------------------------------------------------------
# DELETION LOG FUNCTIONS
# ------------------------------------------------------------------------------------------

create.deletion.log <- function(ids, reason){
  #' Creates a deletion log for the provided ids and reason.
  #' 
  #' @param ids This is a vector of uuids, obtained for example from follow-ups with FPs.
  #' @param reason This is a string describing the reason for removing a survey from data.
  #' @returns A dataframe containing a deletion log with columns `uuid` and `reason`, OR an empty dataframe if `ids` is empty.
  if(length(ids) > 0)
    return(data.frame("uuid" = ids, "reason" = reason))
  else return(data.frame())
}

create.deletion.log.minors <- function(data){
  #' [obsolete] find all submissions run with minor age <18 and no legal guardian consent
  #'
  #' warning: Hardcoded column names for the purposes of PP.
  #' creates a deletion log AND DOES NOT DELETE THESE ROWS FROM DATA
  #' 
  #' @param data Raw data (`raw.main`)
  #' @returns A dataframe containing a deletion log with columns `uuid` and `reason`, OR an empty dataframe if no surveys with minors found.
  
  ids <- data[data$a4_2_resp_age < 18,]
  ids <- ids %>% 
    filter(!is.na(uuid)) %>% 
    mutate(delete = ifelse(is.na(Legal_guardian_consent),"yes",
                           ifelse(Legal_guardian_consent == "yes","no","yes"))) %>% 
    select(uuid,delete) %>% 
    filter(delete=="yes")
  
  ids <- ids$uuid
  return(create.deletion.log(ids=ids, reason="Survey done with a minor"))
}

create.deletion.log.too.fast <- function (data, ids){
  #' obsolete
  return(create.deletion.log(ids=ids, reason="Survey duration < 3 minutes"))
}

create.deletion.log.duplicates <- function(data, ids){
  #' obsolete - TO BE USED ONLY FOR SOFT DUPLICATES (NOT REGULAR UUID DUPLICATES)
  return(create.deletion.log(ids=ids, reason= "Surveys with only 10 or less diff columns"))
}

# ------------------------------------------------------------------------------------------

find.responses <- function(data, questions.db, values_to="response.uk", is.loop = F){
  #' looks up `data` using `questions.db` to find all responses
  #' 
  #' response vector is stored in column specified by `values_to`
  if(is.loop){
    if("loop1_index" %in% colnames(data)){
      data[["loop_index"]] <- data[["loop1_index"]]
    } else {
      data[["loop_index"]] <- data[["loop2_index"]]
    } 
  } else {    
    data[["loop_index"]] <- NA
  }
  responses <- data %>%
      select(c("uuid", "loop_index", any_of(questions.db$name))) %>% 
      pivot_longer(cols = any_of(questions.db$name), names_to="question.name", values_to=values_to) %>% 
      filter(!is.na(!!sym(values_to))) %>% 
      select(uuid,loop_index, question.name, !!sym(values_to))
  
  return(responses)
}

translate.responses <- function(responses, values_from = "response.uk", language_codes = 'uk', is.loop = F){

  info_df <- data.frame()
  start_time <- Sys.time()
  
  # counts characters which will be translated
  char_counter <- sum(str_length(responses[[values_from]]))
  
  if(nrow(responses) > 0){
    for (code in language_codes) {
      cat(nrow(responses),"responses will be translated from",code,"to English.\tThis means",char_counter,"utf-8 characters.\n")
      col_name <- paste0('response.en.from.',code)
      # cleaning up html leftovers:
      responses[[values_from]] <- gsub("&#39;", "'", responses[[values_from]])
      responses[[col_name]] <- NULL
      # actual translation:
      result_vec <- NULL
      result_vec <- translateR::translate(content.vec = responses[[values_from]],
                          google.api.key = source("resources/google.api.key_regional.R")$value,
                          source.lang = code, target.lang = "en")
      # checking the results
       info_df <- rbind(info_df, data.frame(
          "input_responses_num" = nrow(responses),
          "translated_characters_num" = char_counter, 
          "language_from" = code, 
          "result_num" = length(result_vec),
          "time_elapsed" = as.numeric(Sys.time() - start_time),
          "date"=Sys.Date()))
      if(is.null(result_vec)){
        warning("Error while translating responses: result_vec is NULL\n") 
        info_df$status <- "error"
      }else{
        responses[[col_name]] <- gsub("&#39;", "'", result_vec)
        if(length(result_vec) == nrow(responses)){
          cat("\ntranslate.responses: finished - SUCCESS!\n")
          info_df$status <- "success"
        }else{
          cat("\ntranslate.responses: finished - PARTIAL SUCCESS?\n")
          info_df$status <- "partial success"
        }
      }
    }
  }else{
    warning("Nothing to be translated")
  }
    # dump info about the results of translation
  write.table(info_df, file = "translate_info.csv", append = T, row.names = F, col.names = F, sep = ',')
  return(responses)
}
  
create.translate.requests <- function(data, questions.db, responses, is.loop = F, include_cols = "country"){
  
    relevant_colnames <- append(colnames(responses), 
                          c("name", "ref.name","full.label","ref.type", "choices.label", include_cols))
    tryCatch({
      if(is.loop){
        responses.j <- responses %>% 
          left_join(questions.db, by=c("question.name"="name")) %>% dplyr::rename(name="question.name") %>% 
          left_join(select(data, any_of(append(include_cols, "loop_index"))), by="loop_index")
      } else {
        responses.j <- responses %>% 
          left_join(questions.db, by=c("question.name"="name")) %>% dplyr::rename(name="question.name") %>% 
          left_join(select(data, any_of(append(include_cols, "uuid"))), by="uuid")
        # relevant_colnames <- relevant_colnames[!relevant_colnames %in% c("loop_index")]
      }
      response_cols <- colnames(responses.j)[str_starts(colnames(responses.j), "response")]
      responses.j <- responses.j %>% 
          select(any_of(relevant_colnames)) %>% 
          mutate("TRUE other (provide a better translation if necessary)"=NA,
                 "EXISTING other (copy the exact wording from the options in column choices.label)"=NA,
                 "INVALID other (insert yes or leave blank)"=NA) %>% 
          select(-c("loop_index")) %>%
          relocate(all_of(include_cols), .after = uuid) %>%
          relocate(all_of(response_cols), .after = choices.label) %>%
          arrange(name)
      
    }, error = function(err){
      warning("Error while saving responses.j (this is to be expected if result_vec is NULL)\n::",err)
    })

    return(responses.j)
}

#------------------------------------------------------------------------------------------------------------
what.country <- function(id){
  #' Looks up raw.main to find to which country an id belongs to.
  #' 
  #' @param id uuid to look up in `raw.main`
  #' @returns a string found in the `country` column of `raw.main`.
  return(raw.main %>% filter(uuid == id) %>% pull(country))
}


#------------------------------------------------------------------------------------------------------------
# utility operators & other legacy functions
#------------------------------------------------------------------------------------------------------------

"%==%" <- function(a, b) ifelse(!is.na(a), a==b, F)
"%!=%" <- function(a, b) ifelse(!is.na(a), a!=b, F)
"%_<_%" <- function(a, b) ifelse(!is.na(a), as.numeric(a)<b, F)
"%_<=_%" <- function(a, b) ifelse(!is.na(a), as.numeric(a)<=b, F)
"%_>_%" <- function(a, b) ifelse(!is.na(a), as.numeric(a)>b, F)
"%_>=_%" <- function(a, b) ifelse(!is.na(a), as.numeric(a)>=b, F)
"%!=na%" <- function(e1, e2) (e1 != e2 | (is.na(e1) & !is.na(e2)) | (is.na(e2) & !is.na(e1))) & !(is.na(e1) & is.na(e2))


# ------------------------------------------------------------------------------------------
is_all_numeric <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
}

# ------------------------------------------------------------------------------------------
add.to.fu.requests <- function(checks, check.id){
  new.entries <- filter(checks, flag) %>% mutate(check.id=check.id) %>% select(uuid, check.id)
  fu.requests <<- arrange(rbind(fu.requests, new.entries), uuid)
}

# ------------------------------------------------------------------------------------------
write_excel_pwd <- function(df, file, password){
  xlsx::write.xlsx2(df, file, row.names=F, password=password)
}

# ------------------------------------------------------------------------------------------
name2label_question <- function(col){
  if (str_detect(col, "/")) {
    q.name <- str_split(col, "/")[[1]][1]
    c.name <- paste0(tail(str_split(col, "/")[[1]], -1), collapse="/")
  } else {
    q.name <- col
    c.name <- NA
  }
  if (q.name %in% tool.survey$name){
    q <- tool.survey[tool.survey$name==q.name,]
    q.label <- q[label_colname]
    if (is.na(q.label) | q$q.type %in% c("note")) q.label <- q.name
    if (!is.na(c.name)){
      q.list_name=ifelse(q$list_name=="NA", NA, q$list_name)
      c.label <- tool.choices[tool.choices$list_name==q.list_name & tool.choices$name==c.name, label_colname]
    } else c.label <- NA
    label <- ifelse(is.na(c.label), q.label, paste0(q.label, "/", c.label))
  } else label <- q.name
  return(label)
}

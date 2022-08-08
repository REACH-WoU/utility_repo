source("./src/utils/kobo_utils.R")


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

# ------------------------------------------------------------------------------------------
save.trans.responses <- function(df, or.submission=""){
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
    filename <- paste0("output/checking/requests/",i,"_translate_responses.xlsx")
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

# ------------------------------------------------------------------------------------------
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


# ------------------------------------------------------------------------------------------
add.to.cleaning.log.trans.remove <- function(data, x){
  issue <- "Invalid other response"
  # remove text of the response
  df <- data.frame(uuid=x$uuid, variable=x$name, issue=issue, 
                   old.value=x$response.uk, new.value=NA)
  cleaning.log.trans <<- rbind(cleaning.log.trans, df)
}
# ------------------------------------------------------------------------------------------
add.to.cleaning.log.other.recode <- function(data, x){
  if (x$ref.type[1]=="select_one") add.to.cleaning.log.other.recode.one(x)
  if (x$ref.type[1]=="select_multiple") add.to.cleaning.log.other.recode.multiple(data, x)
}

# ------------------------------------------------------------------------------------------
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
  if (nrow(new.code)!=1) stop(paste0("Choice is not in the list. UUID: ", x$uuid,"; recode.into: ", choice))
  else{
    df <- data.frame(uuid=x$uuid, variable=x$ref.name, issue=issue,
                     old.value="Other_please_specify", new.value=new.code$name)
    cleaning.log.other <<- rbind(cleaning.log.other, df)
  }
}

# ------------------------------------------------------------------------------------------
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
  df <- data.frame(uuid=x$uuid,  variable=paste0(x$ref.name, "/Other_please_specify"), issue=issue,
                   old.value="1", new.value="0")
  cleaning.log.other <<- rbind(cleaning.log.other, df)
  # get list of choices already selected
  old.value <- as.character(data[data$uuid==x$uuid[1], x$ref.name[1]])
  l <- str_split(old.value, " ")[[1]]
  l.cumulative <- l[l!="Other_please_specify"]
  # add to the cleaning log each choice in the other response
  for (choice in choices){
    # set corresponding variable to "1" if not already "1"
    list.name <- filter(tool.survey, name==x$ref.name[1])$list_name
    new.code <- filter(tool.choices, list_name==list.name & !!sym(label_colname)==choice)
    if (nrow(new.code)!=1) stop(paste0("Choice is not in the list. UUID: ", x$uuid,"; recode.into: ", choice))
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
}

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

# ------------------------------------------------------------------------------------------
load.edited <- function(dir.edited, file.type){
  # function for loading responses or requests, doesn't matter if edited or not
  # file.type should be one of the following: 
  valid_types = c("other","translate","follow_up","outliers")
  if(!(file.type %in% valid_types))
    warning("Unexpected file.type for load.edited")
  
  filenames <- list.files(dir.edited,recursive=TRUE, full.names=TRUE, ignore.case = TRUE,
                          pattern=paste0(".*",file.type,"_((responses)|(requests))(_edited)?.*\\.xlsx$"))
  if (length(filenames) == 0){
    warning(paste("Files with",file.type,"responses not found!"))
  } else {
    cat(paste("Loading",length(filenames),file.type,"logs:"),filenames,"\n")
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



# ------------------------------------------------------------------------------------------
load.logic.request <- function(dir.logic.edited){
  logic.filenames <- list.files(dir.logic.edited, pattern="follow_up_requests.xlsx",
                                recursive=TRUE, full.names=TRUE)
  print(paste("Loading",length(logic.filenames),"logic requests logs"))
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
load.outlier.edited <- function(dir.outlier.edited){
  logic.filenames <- list.files(dir.outlier.edited, pattern="outliers_responses.xlsx",
                                recursive=TRUE, full.names=TRUE)
  print(paste("Loading",length(logic.filenames),"outlier logs"))
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
add.to.cleaning.log <- function(checks, check.id, question.names=c(), issue=""){
  for(q.n in question.names){
    new.entries <- checks %>% filter(flag) %>% 
      mutate(uuid=uuid,
             variable = q.n,
             issue=issue,
             old.value =!!sym(q.n),
             new.value=NA,
             explanation =NA)
    new.entries[["check.id"]] <- check.id 
    new.entries <- new.entries %>% select(today, uuid, country, Reporting_organization, Staff_Name, check.id, 
                                          variable,issue, old.value, new.value, explanation) %>%
      dplyr::rename(enumerator.code="Staff_Name", survey.date=today)
    cleaning.log.checks <<- arrange(rbind(cleaning.log.checks, new.entries),country, uuid)
  }
}

# ------------------------------------------------------------------------------------------
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
    filename <- paste0("output/checking/requests/",i,"_follow_up_requests.xlsx")
    saveWorkbook(wb, filename, overwrite = TRUE)
    rm(cl1)
  }
}

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
  #' find all submissions run with minor age <18 and no legal guardian consent
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

translate.responses <- function(data, questions.db, language_codes = 'uk', is.loop = F){
  if(is.loop){
    if("loop1_index" %in% colnames(data)){
      data[["loop_index"]] <- data[["loop1_index"]]
    } else {
      data[["loop_index"]] <- data[["loop2_index"]]
    } 
  } else {    
    data[["loop_index"]] <- NA
  }

  # counts characters which will be translated
  char_counter <- 0

  relevant_colnames <- c("uuid","loop_index","name", "ref.name","full.label","ref.type",
                         "choices.label", "response.uk")
  if(nrow(questions.db)>0){
    responses <- data[, c("uuid", "loop_inde x",all_of(questions.db$name))] %>% 
      pivot_longer(cols= all_of(questions.db$name), names_to="question.name", values_to="response.uk") %>% 
      filter(!is.na(response.uk)) %>% 
      select(uuid,loop_index, question.name, response.uk)
    char_counter <- char_counter + sum(str_length(responses$response.uk))
    if(nrow(responses) > 0){
      for (code in language_codes) {
        col_name <- paste0('response.en.from.',code)
        relevant_colnames <- append(relevant_colnames, col_name)  # this line may be bugged
        responses[[col_name]] <- gsub("&#39;", "'", translateR::translate(content.vec = responses$response.uk,
                                                                          google.api.key = source("resources/google.api.key_regional.R")$value,
                                                                          source.lang = code, target.lang = "en"))
      }
    }else{
      warning("Nothing to be translated")
    }
    if(is.loop){
      responses.j <- responses %>% 
        left_join(questions.db, by=c("question.name"="name")) %>% dplyr::rename(name="question.name") %>% 
        left_join(select(data, loop_index), by="loop_index")
    } else {
      responses.j <- responses %>% 
        left_join(questions.db, by=c("question.name"="name")) %>% dplyr::rename(name="question.name") %>% 
        left_join(select(data, uuid), by="uuid")
    }
    responses.j <- responses.j %>% 
        select(all_of(relevant_colnames)) %>%   # NOTE: this line will throw an error if there isn't anything to be translated
        mutate("TRUE other (provide a better translation if response.en is not correct)"=NA,
               "EXISTING other (copy the exact wording from the options in column G)"=NA,
               "INVALID other (insert yes or leave blank)"=NA) %>% 
        arrange(name)
    # dump info about char_counter
    write.csv(data.frame(
                        "translated_characters_num" = char_counter, 
                        "responses_num" = nrow(responses.j), 
                        "date"=Sys.Date()),
              file = "char_counter.csv", append = TRUE)
    return(responses.j)
  } 
}




#------------------------------------------------------------------------------------------------------------
# Logical and Outlier functions

"%==%" <- function(a, b) ifelse(!is.na(a), a==b, F)
"%!=%" <- function(a, b) ifelse(!is.na(a), a!=b, F)
"%_<_%" <- function(a, b) ifelse(!is.na(a), as.numeric(a)<b, F)
"%_<=_%" <- function(a, b) ifelse(!is.na(a), as.numeric(a)<=b, F)
"%_>_%" <- function(a, b) ifelse(!is.na(a), as.numeric(a)>b, F)
"%_>=_%" <- function(a, b) ifelse(!is.na(a), as.numeric(a)>=b, F)
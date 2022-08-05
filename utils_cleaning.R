###############################################################################

###############################################################################

get.ref.question <- function(x){
  x.1 <- str_split(x, "\\{")[[1]][2]
  return(str_split(x.1, "\\}")[[1]][1])
}


################################################################################
#To update with all the other variables that are needed

get.other.variables <- function(){
  ov <- tool.survey %>% 
    filter(type=="text" & 
             (str_detect(tolower(relevant), "other'") |
                name %in% c("Organisation_Name",
                            "Staff_Name",
                            "K008_Other",
                            "L002_referral_name",
                            "L004_referral_contact",
                            "L005_comments"))) %>%
    select("type", "name", label_colname, "relevant") %>% 
    mutate(ref.question=as.character(lapply(relevant, get.ref.question)))
  return(ov)
}

# ------------------------------------------------------------------------------------------
get.var.labels <- function() {
  var.labels <- tool.survey %>% 
    select(type, name, `label_colname`) %>% 
    rename(label=`label_colname`) %>% 
    left_join(get.other.variables() %>% select(name, ref.question), by="name")
  
  var.labels <- var.labels %>% 
    left_join(var.labels %>% select(name, label), by=c("ref.question"="name")) %>% 
    mutate(label.x=ifelse(is.na(label.x), name, label.x),
           label.full=ifelse(is.na(label.y), label.x, paste0(label.y, " / ", label.x))) %>%
    select(-c(label.x, label.y))
  return(var.labels)
}

# ------------------------------------------------------------------------------------------
get.choice.list.name <- function(x){
  x.1 <- str_split(x, " ")[[1]]
  if (length(x.1)==1) return(NA)
  else return(x.1[2])
}

# ------------------------------------------------------------------------------------------
get.q.type <- function(x) return(str_split(x, " ")[[1]][1])

# ------------------------------------------------------------------------------------------
get.select.db <- function(){
  # list of choices for each list_name (from TOOL_CHOICES)
  list.choices <- tool.choices %>% filter(!is.na(list_name)) %>% group_by(list_name) %>% 
    mutate(choices=paste(name, collapse=";\r\n"),
           choices.label=paste(!!sym(label_colname), collapse=";\r\n")) %>% 
    summarise(choices=choices[1], choices.label=choices.label[1])
  # list of choices for each question
  select.questions <- tool.survey %>% 
    rename(q.label=label_colname) %>% 
    select(type, name, q.label) %>% 
    mutate(q.type=as.character(lapply(type, get.q.type)),
           list_name=as.character(lapply(type, get.choice.list.name))) %>% 
    filter(list_name!="NA" & list_name!="group" & list_name!="repeat") %>% 
    left_join(list.choices, by="list_name") %>% 
    filter(!is.na(choices))
  return(select.questions)
}

# ------------------------------------------------------------------------------------------
get.other.db <- function(){
  select.questions <- get.select.db()
  
  # for each "other" question, get ref.question and list of choices
  df1 <- tool.survey %>% filter(str_ends(name, "_other"), type=="text") %>% 
    rename(label=label_colname) %>% 
    select("name", "label", "relevant") %>% 
    mutate(ref.name=as.character(lapply(relevant, get.ref.question))) %>% 
    left_join(select(select.questions, "name", "q.type", "q.label", "list_name", "choices", "choices.label"),
              by=c("ref.name"="name")) %>% 
    rename(ref.label=q.label, ref.type=q.type) %>% 
    mutate(full.label=paste0(ref.label, " - ", label)) %>%  
    select(name, ref.name, full.label, ref.type, choices, choices.label)
  
  return(df1)
}

# ------------------------------------------------------------------------------------------
get.trans.db <- function(){
  select.questions <- get.select.db()
  
  ## TO UPDATE THE LIST OF QUESTIONS TO BE TRANSLATED
  tool.survey_1 <- tool.survey %>% 
    filter(name %in% c("K008_Other",
                       "L005_comments"))
  
  df1 <- tool.survey_1 %>%
    rename(label=label_colname) %>% 
    select("name", "label", "relevant") %>% 
    mutate(ref.name=as.character(lapply(relevant, get.ref.question))) %>% 
    left_join(select(select.questions, "name", "q.type", "q.label", "list_name", "choices", "choices.label"),
              by=c("ref.name"="name")) %>% 
    rename(ref.label=q.label, ref.type=q.type) %>% 
    mutate(full.label=paste0(ref.label, " - ", label)) %>%  
    select(name, ref.name, full.label, ref.type, choices, choices.label)
  return(df1)
}

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
  rm(df1)
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
name2label_question <- function(tool.survey, tool.choices, col){
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

create.deletion.log.minors <- function(data){
  # Delete all submissions run with minor age <18
  # creates a deletion log based on surveys with minors AND DELETES THESE ROWS FROM DATA
  
  deletion.log.new <- data.frame()
  ids <- data[data$a4_2_resp_age < 18,]
  ids <- ids %>% 
    filter(!is.na(uuid)) %>% 
    mutate(delete = ifelse(is.na(Legal_guardian_consent),"yes",
                           ifelse(Legal_guardian_consent == "yes","no","yes"))) %>% 
    select(uuid,delete) %>% 
    filter(delete=="yes")
  
  ids <- ids$uuid
  # add to deletion log and remove from data
  if (length(ids) > 0) {
    # add to deletion log
    deletion.log.new <- rbind(deletion.log.new, data.frame(uuid=ids, reason= "Survey done with a minor"))
    # remove from data
    data <- data[!(data$uuid %in% ids), ]
  }
  return(deletion.log.new)
}

create.deletion.log.too.fast <- function (data, ids){
  # id's is a vector of uuids obtained from FP responses
  deletion.log.too.fast <- data.frame()
  # add to deletion log and remove from data
  if (length(ids) > 0) {
    # add to deletion log
    deletion.log.too.fast <- rbind(deletion.log.too.fast, data.frame(uuid=ids, reason= "Survey duration < 3 minutes"))
    # remove from data
    data <- data[!(data$uuid %in% ids), ]
  }
  return(deletion.log.too.fast)
}

create.deletion.log.duplicates <- function(data, ids){
  # id's is a vector of uuids obtained from FP responses
  deletion.log.duplicates <- data.frame()
  # add to deletion log and remove from data
  if (length(ids) > 0) {
    # add to deletion log
    deletion.log.duplicates <- rbind(deletion.log.duplicates, data.frame(uuid=ids, reason= "Surveys with only 10 or less diff columns"))
    # remove from data
    data <- data[!(data$uuid %in% ids), ]
  }
  return(deletion.log.duplicates)
}

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
  relevant_colnames <- c("uuid","loop_index","name", "ref.name","full.label","ref.type",
                         "choices.label", "response.uk")
  if(nrow(questions.db)>0){
    responses <- data[, c("uuid", "loop_index",all_of(questions.db$name))] %>% 
      pivot_longer(cols= all_of(questions.db$name), names_to="question.name", values_to="response.uk") %>% 
      filter(!is.na(response.uk)) %>% 
      select(uuid,loop_index, question.name, response.uk)
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
      if("loop1_index" %in% colnames(data)){
        responses.j <- responses %>% 
          left_join(questions.db, by=c("question.name"="name")) %>% dplyr::rename(name="question.name") %>% 
          left_join(select(data, loop1_index), by="loop1_index") %>% 
          select(all_of(relevant_colnames)) %>% 
          mutate("TRUE other (provide a better translation if response.en is not correct)"=NA,
                 "EXISTING other (copy the exact wording from the options in column G)"=NA,
                 "INVALID other (insert yes or leave blank)"=NA) %>% 
          arrange(name)
        return(responses.j)
      } else {
        responses.j <- responses %>% 
          left_join(questions.db, by=c("question.name"="name")) %>% dplyr::rename(name="question.name") %>% 
          left_join(select(data, loop2_index), by="loop2_index") %>% 
          select(all_of(relevant_colnames)) %>% 
          mutate("TRUE other (provide a better translation if response.en is not correct)"=NA,
                 "EXISTING other (copy the exact wording from the options in column G)"=NA,
                 "INVALID other (insert yes or leave blank)"=NA) %>% 
          arrange(name)
        return(responses.j)
      }
    }else{
      responses.j <- responses %>% 
        left_join(questions.db, by=c("question.name"="name")) %>% dplyr::rename(name="question.name") %>% 
        left_join(select(data, uuid), by="uuid") %>% 
        select(all_of(relevant_colnames)) %>% 
        mutate("TRUE other (provide a better translation if response.en is not correct)"=NA,
               "EXISTING other (copy the exact wording from the options in column G)"=NA,
               "INVALID other (insert yes or leave blank)"=NA) %>% 
        arrange(name)
      return(responses.j)
    }
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
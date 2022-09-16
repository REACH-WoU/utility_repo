
# ------------------------------------------------------------------------------------------
# CLEANING LOG FUNCTIONS
# ------------------------------------------------------------------------------------------

add.to.cleaning.log.other.recode.LOOP <- function(data, x){
  if (x$ref.type[1]=="select_one") res <- add.to.cleaning.log.other.recode.one.LOOP(x)
  if (x$ref.type[1]=="select_multiple") res <- add.to.cleaning.log.other.recode.multiple.LOOP(data, x)
  if (res == "err") cat("Errors encountered while recoding other. Check the warnings!")
}


add.to.cleaning.log.other.remove.LOOP <- function(data, x){
  issue <- "Invalid other response"
  # remove text of the response
  df <- data.frame(uuid=x$uuid, loop_index=x$loop_index, variable=x$name, issue=issue, 
                   old.value=x$response.uk, new.value=NA)
  cleaning.log.other <<- rbind(cleaning.log.other, df)
  # remove relative entries
  if (x$ref.type[1]=="select_one"){
    old.value <- "other"
    df <- data.frame(uuid=x$uuid,loop_index=x$loop_index, variable=x$ref.name, issue=issue, old.value=old.value, new.value=NA)
    cleaning.log.other <<- rbind(cleaning.log.other, df)
  }
  if (x$ref.type[1]=="select_multiple"){
    if (!is.na(x$loop_index) & str_starts(x$loop_index[1],"loop1")){
      old.value <- as.character(data[data$loop1_index==x$loop_index[1], x$ref.name])
    } else if (!is.na(x$loop_index) & str_starts(x$loop_index[1],"loop2")) {
      old.value <- as.character(data[data$loop2_index==x$loop_index[1], x$ref.name])
    } else {
      old.value <- as.character(data[data$uuid==x$uuid[1], x$ref.name])
    }
    l <- str_split(old.value, " ")[[1]]
    new.value <- paste(l[l!="other"], collapse=" ")
    new.value <- ifelse(new.value=="", NA, new.value)
    df <- data.frame(uuid=x$uuid, loop_index=x$loop_index, variable=x$ref.name, issue=issue, old.value=old.value, new.value=new.value)
    cleaning.log.other <<- rbind(cleaning.log.other, df)
    if (is.na(new.value)){
      # set all choices columns to NA
      cols <- colnames(data)[str_starts(colnames(data), paste0(x$ref.name, "/"))]
      df <- data.frame(uuid=x$uuid, loop_index=x$loop_index, variable=cols, issue=issue, old.value="0 or 1", new.value=NA)
      cleaning.log.other <<- rbind(cleaning.log.other, df)
    } else{
      df <- data.frame(uuid=x$uuid, loop_index=x$loop_index, variable=paste0(x$ref.name, "/other"), issue=issue,
                       old.value="1", new.value="0")
      cleaning.log.other <<- rbind(cleaning.log.other, df)
    }
  }
}

add.to.cleaning.log.other.recode.multiple.LOOP <- function(data, x){
  issue <- "Recoding other response"
  # remove text of the response
  df <- data.frame(uuid=x$uuid, loop_index=x$loop_index, variable=x$name, issue=issue,
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
  df <- data.frame(uuid=x$uuid, loop_index=x$loop_index,  variable=paste0(x$ref.name, "/other"), issue=issue,
                   old.value="1", new.value="0")
  cleaning.log.other <<- rbind(cleaning.log.other, df)
  # get list of choices already selected
  
  if (!is.na(x$loop_index) & str_starts(x$loop_index[1],"loop1")){
    old.value <- as.character(data[data$loop1_index==x$loop_index[1], x$ref.name])
  } else if (!is.na(x$loop_index) & str_starts(x$loop_index[1],"loop2")) {
    old.value <- as.character(data[data$loop2_index==x$loop_index[1], x$ref.name])
  } else {
    old.value <- as.character(data[data$uuid==x$uuid[1], x$ref.name])
  }
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
      if (!is.na(x$loop_index) & str_starts(x$loop_index[1],"loop1")){
        old.boolean <- data[[variable.name]][data$loop1_index==x$loop_index[1]]
      } else if (!is.na(x$loop_index) & str_starts(x$loop_index[1],"loop2")){
        old.boolean <- data[[variable.name]][data$loop2_index==x$loop_index[1]]
      } else {
        old.boolean <- data[[variable.name]][data$uuid==x$uuid[1]]
      }
    } else stop("Column not found")
    if (!is.na(old.boolean) & old.boolean=="0"){
      df <- data.frame(uuid=x$uuid, loop_index=x$loop_index, variable=variable.name, issue=issue,
                       old.value=old.boolean, new.value="1")
      cleaning.log.other <<- rbind(cleaning.log.other, df)
    }
    l.cumulative <- unique(c(l.cumulative, new.code$name))
  }
  # update cumulative variable
  new.value <- paste(sort(l.cumulative), collapse=" ")
  df <- data.frame(uuid=x$uuid, loop_index=x$loop_index, variable=x$ref.name, issue=issue,
                   old.value=old.value, new.value=new.value)
  cleaning.log.other <<- rbind(cleaning.log.other, df)
  return("succ")
}

add.to.cleaning.log.other.recode.one.LOOP <- function(x){
  issue <- "Recoding other response"
  # remove text of the response
  df <- data.frame(uuid=x$uuid,loop_index=x$loop_index, variable=x$name, issue=issue,
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
    df <- data.frame(uuid=x$uuid,loop_index=x$loop_index, variable=x$ref.name, issue=issue,
                     old.value="other", new.value=new.code$name)
    cleaning.log.other <<- rbind(cleaning.log.other, df)
    return("succ")
  }
}

add.to.cleaning.log.LOOP <- function(checks, check.id, question.names=c(), issue=""){
  for(q.n in question.names){
    new.entries <- checks %>% filter(flag) %>% 
      mutate(uuid=uuid,
             variable = q.n,
             issue=issue,
             old.value =!!sym(q.n),
             new.value=NA,
             explanation =NA)
    new.entries[["check.id"]] <- check.id 
    new.entries <- new.entries %>% select(today, uuid, enumerator_num, check.id, 
                                          variable,issue, old.value, new.value, explanation) %>%
      dplyr::rename(enumerator.code="enumerator_num", survey.date=today)
    cleaning.log.checks <<- arrange(rbind(cleaning.log.checks, new.entries),country, uuid)
  }
}

# ------------------------------------------------------------------------------------------

add.to.cleaning.log.trans.remove.LOOP <- function(data, x){
  issue <- "Invalid other response"
  # remove text of the response
  df <- data.frame(uuid=x$uuid,loop_index=x$loop_index, variable=x$name, issue=issue, 
                   old.value=x$response.uk, new.value=NA)
  cleaning.log.trans <<- rbind(cleaning.log.trans, df)
}

# ------------------------------------------------------------------------------------------
save.follow.up.requests.LOOP <- function(cleaning.log, data){
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
    select(uuid, submission_time, country, enumerator.code, check.id, 
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
    
    addStyle(wb, "Follow-up", style = style.col.color, rows = 1:(nrow(cl1)+1), cols=9)
    addStyle(wb, "Follow-up", style = style.col.color, rows = 1:(nrow(cl1)+1), cols=10)
    
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
    addStyle(wb, "Follow-up", style = style.col.color.first, rows = 1, cols=9)
    addStyle(wb, "Follow-up", style = style.col.color.first, rows = 1, cols=10)
    filename <- paste0("output/checking/requests/",i,"_follow_up_requests.xlsx")
    saveWorkbook(wb, filename, overwrite = TRUE)
    rm(cl1)
  }
}

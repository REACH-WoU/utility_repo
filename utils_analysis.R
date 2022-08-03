################################################################################################
# OTHER RESPONSES

get.ref.question <- function(x){
  x.1 <- str_split(x, "\\{")[[1]][2]
  return(str_split(x.1, "\\}")[[1]][1])
}
get.choice.list.name <- function(x){
  x.1 <- str_split(x, " ")[[1]]
  if (length(x.1)==1) return(NA)
  else return(x.1[2])
}
get.q.type <- function(x) return(str_split(x, " ")[[1]][1])

get.select.db <- function(){
  # list of choices for each list_name (from TOOL_CHOICES)
  list.choices <- tool.choices %>% filter(!is.na(list_name)) %>% group_by(list_name) %>% 
    mutate(choices=paste(name, collapse=";\r\n"),
           choices.label=paste(`label_colname`, collapse=";\r\n")) %>% 
    summarise(choices=choices[1], choices.label=choices.label[1])
  # list of choices for each question
  select.questions <- tool.survey %>% 
    rename(q.label=`label_colname`) %>% 
    select(type, name, q.label) %>% 
    mutate(q.type=as.character(lapply(type, get.q.type)),
           list_name=as.character(lapply(type, get.choice.list.name))) %>% 
    filter(list_name!="NA" & list_name!="group" & list_name!="repeat") %>% 
    left_join(list.choices, by="list_name") %>% 
    filter(!is.na(choices))
  return(select.questions)
}

get.other.db <- function(){
  select.questions <- get.select.db()
  
  # for each "other" question, get ref.question and list of choices
  df1 <- tool.survey %>% filter(str_ends(name, "_other")) %>% 
    rename(label="label_colname") %>% 
    select("name", "label", "relevant") %>% 
    mutate(ref.name=as.character(lapply(relevant, get.ref.question))) %>% 
    left_join(select(select.questions, "name", "q.type", "q.label", "list_name", "choices", "choices.label"),
              by=c("ref.name"="name")) %>% 
    rename(ref.label=q.label, ref.type=q.type) %>% 
    select(name, ref.name, ref.type, choices, choices.label) %>% 
    left_join(select(var_labels, "name", "full.label"), by="name") %>% 
    select(name, ref.name, full.label, ref.type, choices, choices.label)
  
  q6_4_8_other_choices <- c("Married and left the house",
                            "Left the house to seek employment",
                            "Left the house to study",
                            "Left the house to live in a safer location (e.g. with relative)",
                            "Left for disability related reasons",
                            "Missing (left and no news)")
  df1[df1$name=="q6_4_8_other", "choices"] <- NA
  df1[df1$name=="q6_4_8_other", "choices.label"] <- paste(q6_4_8_other_choices, collapse=";\r\n")
  
  return(df1)
}

save.other.responses <- function(df, or.submission=""){
  style.col.color <- createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000", 
                                 valign="top", wrapText=T)
  style.col.color2 <- createStyle(fgFill="#CCE5FF", border="TopBottomLeftRight", borderColour="#000000", 
                                  valign="top", wrapText=T)
  style.col.color.first <- createStyle(textDecoration="bold", fgFill="#E5FFCC", valign="top",
                                       border="TopBottomLeftRight", borderColour="#000000", wrapText=T)
  style.col.color.first2 <- createStyle(textDecoration="bold", fgFill="#CCE5FF", valign="top",
                                        border="TopBottomLeftRight", borderColour="#000000", wrapText=T)
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet1")
  writeData(wb = wb, x = df, sheet = "Sheet1", startRow = 1)
  addStyle(wb, "Sheet1", style = style.col.color, rows = 1:(nrow(df)+1), cols=11)
  addStyle(wb, "Sheet1", style = style.col.color, rows = 1:(nrow(df)+1), cols=12)
  addStyle(wb, "Sheet1", style = style.col.color, rows = 1:(nrow(df)+1), cols=13)
  addStyle(wb, "Sheet1", style = style.col.color2, rows = 1:(nrow(df)+1), cols=14)
  if ("response" %in% colnames(df)) addStyle(wb, "Sheet1", style = style.col.color2, rows = 1:(nrow(df)+1), cols=15)
  setColWidths(wb, "Sheet1", cols=c(6, 8), widths=50)
  setColWidths(wb, "Sheet1", cols=c(9:10), widths=25)
  setColWidths(wb, "Sheet1", cols=c(3:5, 7), widths=20)
  setColWidths(wb, "Sheet1", cols=c(11:14), widths=40)
  if ("response" %in% colnames(df)) setColWidths(wb, "Sheet1", cols=c(15), widths=40)
  addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=1)
  addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=2)
  addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=3)
  addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=4)
  addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=5)
  addStyle(wb, "Sheet1", style = createStyle(wrapText=T, valign="top"), rows = 1:(nrow(df)+1), cols=6)
  addStyle(wb, "Sheet1", style = createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=7)
  addStyle(wb, "Sheet1", style = createStyle(wrapText=T, valign="top"), rows = 1:(nrow(df)+1), cols=8)
  addStyle(wb, "Sheet1", style = createStyle(wrapText=T, valign="top"), rows = 1:(nrow(df)+1), cols=9)
  addStyle(wb, "Sheet1", style = createStyle(wrapText=T, valign="top"), rows = 1:(nrow(df)+1), cols=10)
  addStyle(wb, "Sheet1", style = createStyle(textDecoration="bold"), 
           rows = 1, cols=1:ncol(df))
  addStyle(wb, "Sheet1", style = style.col.color.first, rows = 1, cols=11:13)
  addStyle(wb, "Sheet1", style = style.col.color.first2, rows = 1, cols=14)
  if ("response" %in% colnames(df)) addStyle(wb, "Sheet1", style = style.col.color.first2, rows = 1, cols=15)
  modifyBaseFont(wb, fontSize = 10, fontColour = "black", fontName = "Calibri")
  if ("response" %in% colnames(df)){
    filename <- paste0("output/checking/responses/intermediate/", 
                       or.submission, " - other_responses_followed_up.xlsx")
  } else{
    filename <- paste0("output/checking/requests/", latest.submission, " - other_responses.xlsx")
  }
  saveWorkbook(wb, filename, overwrite=T)
}

add.to.cleaning.log.other.remove <- function(x){
  issue <- "Removing other response"
  # remove text of the response
  df <- data.frame(id=x$id, rel.index=x$rel.index, variable=x$name, issue=issue, 
                   old.value=x$response.ar, new.value=NA)
  cleaning.log.other <<- rbind(cleaning.log.other, df)
  # remove relative entries
  if (x$ref.type[1]=="select_one"){
    # choice.name for "other" for q2_1 is wrongly set as "na". All other choice.names are "other".
    old.value <- ifelse(x$ref.name[1]=="q2_1", "na", "other")
    df <- data.frame(id=x$id, rel.index=x$rel.index, variable=x$ref.name, issue=issue, 
                     old.value=old.value, new.value=NA)
    cleaning.log.other <<- rbind(cleaning.log.other, df)
  }
  if (x$ref.type[1]=="select_multiple"){
    df <- data.frame(id=x$id, rel.index=x$rel.index, variable=paste0(x$ref.name, "/other"), issue=issue,
                     old.value="1", new.value="0")
    cleaning.log.other <<- rbind(cleaning.log.other, df)
    old.value <- ifelse(x$name[1]=="q4_2_3_other", 
                        raw.loop$q4_2_3[raw.loop$id==x$id[1] & raw.loop$calc_position==x$rel.index[1]],
                        as.character(raw.main[raw.main$id==x$id[1], x$ref.name]))
    l <- str_split(old.value, " ")[[1]]
    new.value <- paste(l[l!="other"], collapse=" ")
    new.value <- ifelse(new.value=="", NA, new.value)
    df <- data.frame(id=x$id, rel.index=x$rel.index, variable=x$ref.name, issue=issue,
                     old.value=old.value, new.value=new.value)
    cleaning.log.other <<- rbind(cleaning.log.other, df)
  }
}

add.to.cleaning.log.other.recode <- function(x){
  if (x$ref.type[1]=="select_one") add.to.cleaning.log.other.recode.one(x)
  if (x$ref.type[1]=="select_multiple") add.to.cleaning.log.other.recode.multiple(x)
}

add.to.cleaning.log.other.recode.one <- function(x){
  issue <- "Recoding other response"
  # remove text of the response
  df <- data.frame(id=x$id, rel.index=x$rel.index, variable=x$name, issue=issue, 
                   old.value=x$response.ar, new.value=NA)
  cleaning.log.other <<- rbind(cleaning.log.other, df)
  if (x$name=="q6_4_8_other"){
    q6_4_7.value <- filter(raw.main, id==x$id)$q6_4_7
    q6_4_8_other.value <- trimws(str_replace(x$existing.other, ";", ""))
    q_new <- case_when(
      q6_4_8_other.value=="Married and left the house" ~ "q6_4_1",
      q6_4_8_other.value=="Left the house to seek employment" ~ "q6_4_2",
      q6_4_8_other.value=="Left the house to study" ~ "q6_4_3",
      q6_4_8_other.value=="Left the house to live in a safer location (e.g. with relative)" ~ "q6_4_4",
      q6_4_8_other.value=="Left for disability related reasons" ~ "q_6_4_5b",
      q6_4_8_other.value=="Missing (left and no news)" ~ "q6_4_6",
      TRUE ~ NA_character_)
    if (is.na(q_new)) stop("Error in recoding q6_4_8_other")
    df <- data.frame(id=x$id, rel.index=x$rel.index, variable="q6_4_7", issue=issue,
                     old.value=q6_4_7.value, new.value=NA)
    cleaning.log.other <<- rbind(cleaning.log.other, df)
    df <- data.frame(id=x$id, rel.index=x$rel.index, variable=q_new, issue=issue,
                     old.value=NA, new.value=q6_4_7.value)
    cleaning.log.other <<- rbind(cleaning.log.other, df)
  } else{
    # get list of choices from other response
    if (str_detect(x$existing.other, ";")) {
      choices <- str_trim(str_split(x$existing.other, ";")[[1]])
    } else {
      choices <- str_trim(str_split(x$existing.other, "\r\n")[[1]])
    }
    choices <- choices[choices!=""]
    if (length(choices)>1) {
      print(select(x, id, name))
      stop("More than one existing.option for a select_one question")
    }
    # recode choice
    choice <- choices[1]
    list.name <- filter(tool.survey, name==x$ref.name[1])$list_name
    new.code <- filter(tool.choices, list_name==list.name & !!sym(label_colname)==choice)
    if (nrow(new.code)!=1) stop(paste0("Choice is not in the list. ID: ", x$id,"; recode.into: ", choice))
    else{
      df <- data.frame(id=x$id, rel.index=x$rel.index, variable=x$ref.name, issue=issue,
                       old.value="other", new.value=new.code$name)
      cleaning.log.other <<- rbind(cleaning.log.other, df)
    }
  }
}

add.to.cleaning.log.other.recode.multiple <- function(x){
  issue <- "Recoding other response"
  # remove text of the response
  df <- data.frame(id=x$id, rel.index=x$rel.index, variable=x$name, issue=issue, 
                   old.value=x$response.ar, new.value=NA)
  cleaning.log.other <<- rbind(cleaning.log.other, df)
  # get list of choices from other response
  if (str_detect(x$existing.other, ";")) {
    choices <- str_trim(str_split(x$existing.other, ";")[[1]])
  } else {
    choices <- str_trim(str_split(x$existing.other, "\r\n")[[1]])
  }
  choices <- choices[choices!=""]
  # set variable/other to "0"
  df <- data.frame(id=x$id, rel.index=x$rel.index, variable=paste(x$ref.name, "/other", sep=""), issue=issue, 
                   old.value="1", new.value="0")
  cleaning.log.other <<- rbind(cleaning.log.other, df)
  # get list of choices already selected
  old.value <- ifelse(x$name[1]=="q4_2_3_other", 
                      raw.loop$q4_2_3[raw.loop$id==x$id[1] & raw.loop$calc_position==x$rel.index[1]],
                      as.character(raw.main[raw.main$id==x$id[1], x$ref.name[1]]))
  l <- str_split(old.value, " ")[[1]]
  l.cumulative <- l[l!="other"]
  # load constraint on max 3 answers (TRUE/FALSE)
  is_max_3 <- F
  if (x$ref.name %in% constraints$ref.name)
    if (!is.na(constraints$max_3[constraints$ref.name==x$ref.name])) is_max_3 <- T
  # add to the cleaning log each choice in the other response
  for (choice in choices){
    # set corresponding variable to "1" if not already "1"
    list.name <- filter(tool.survey, name==x$ref.name[1])$list_name
    new.code <- filter(tool.choices, list_name==list.name & `label_colname`==choice)
    if (nrow(new.code)!=1) stop(paste0("Choice is not in the list. ID: ", x$id,"; recode.into: ", choice))
    variable.name <- paste0(x$ref.name, "/", new.code$name)
    if (variable.name %in% c(colnames(raw.main), colnames(raw.loop))){
      old.boolean <- ifelse(x$name[1]=="q4_2_3_other", 
                            raw.loop[[variable.name]][raw.loop$id==x$id[1] & raw.loop$calc_position==x$rel.index[1]],
                            raw.main[[variable.name]][raw.main$id==x$id[1]])
    } else stop("Column not found")
    if (old.boolean=="0"){
      df <- data.frame(id=x$id, rel.index=x$rel.index, variable=variable.name, issue=issue, 
                       old.value=old.boolean, new.value="1")
      cleaning.log.other <<- rbind(cleaning.log.other, df)
    }
    # add choice to cumulative variable and remove duplicates
    if (is_max_3 & length(unique(c(l.cumulative, new.code$name))) > 3) print("max_3 constraint applied")
    if (!is_max_3 | (is_max_3 & length(unique(c(l.cumulative, new.code$name))) <= 3)){
      l.cumulative <- unique(c(l.cumulative, new.code$name))
    }
  }
  # update cumulative variable
  new.value <- paste(sort(l.cumulative), collapse=" ")
  df <- data.frame(id=x$id, rel.index=x$rel.index, variable=x$ref.name, issue=issue,
                   old.value=old.value, new.value=new.value)
  cleaning.log.other <<- rbind(cleaning.log.other, df)
}

################################################################################################
# CHECKS

add.to.fu.requests <- function(checks, check.id, question.names=c()){
  for(q.n in question.names){
    new.entries <- checks %>% filter(flag) %>% 
      mutate(id=id,
             variable=q.n,
             old.value=!!sym(q.n),
             new.value=NA)
    new.entries[["check.id"]] <- check.id
    new.entries <- new.entries %>% select(id, check.id, variable, old.value, new.value)
    fu.requests <<- arrange(rbind(fu.requests, new.entries), id)
  }
}

check.last.digit <- function(df){
  res <- data.frame()
  for (col in colnames(df)[2:length(colnames(df))]){
    df.temp <- data.frame(id=df$id, value=df[[col]]) %>% 
      filter(!is.na(value) & value!="0") %>%
      mutate(len=str_length(value),
             last1=ifelse(len==1, value, str_sub(value, len, len)),
             last2=ifelse(len<=2, value, str_sub(value, len-1, len)),
             variable=col) %>% 
      filter(!(last1 %in% c("0"))) %>% 
      mutate(check.id="Typing", old.value=value, new.value=NA) %>%
      select(id, check.id, variable, old.value, new.value)
    res <- rbind(res, df.temp)
  }
  return(res)
}

detect.outliers <- function(df, n.sd, method="o1"){
  res <- data.frame()
  for (col in colnames(df)[2:length(colnames(df))]){
    df.temp <- data.frame(id=df$id, value=as.numeric(df[[col]])) %>% filter(!is.na(value) & value>0) %>% 
      mutate(col.log=log10(value),
             is.outlier=case_when(
               method=="o1" ~ ifelse(col.log > mean(col.log, na.rm=T) + n.sd*sd(col.log, na.rm=T) | 
                                       col.log < mean(col.log, na.rm=T) - n.sd*sd(col.log, na.rm=T), T, F),
               method=="o2" ~ ifelse(col.log > 1.5*quantile(col.log, 0.95) |
                                       col.log < quantile(col.log, 0.05)/1.5, T, F),
               method=="o3" ~ ifelse(col.log > quantile(col.log, 0.75) + 
                                       4*(quantile(col.log, 0.75) - quantile(col.log, 0.25)) |
                                       col.log < quantile(col.log, 0.25) - 
                                       4*(quantile(col.log, 0.75)-quantile(col.log, 0.25)), T, F),
               method=="o4" ~ ifelse(col.log > median(col.log) + 3*mad(col.log) |
                                       col.log < median(col.log) - 3*mad(col.log), T, F)),
             variable=col) %>% 
      filter(is.outlier) %>% 
      mutate(check.id="Outlier", old.value=value, new.value=NA) %>%
      select(id, check.id, variable, old.value, new.value)
    res <- rbind(res, df.temp)
  }
  return(res)
}

generate.boxplot <- function(outliers){
  outliers <- outliers %>% 
    mutate(detected=ifelse(submission==latest.submission, 2, 1)) %>% 
    select(mid, detected)
  data.boxplot <- raw.main.all %>% 
    select(-submission) %>% 
    pivot_longer(cols = all_of(outlier.variables), names_to="variable", values_to="value") %>% 
    mutate_at("value", as.numeric) %>% 
    filter(!is.na(value) & value>0) %>% 
    mutate(mid=paste0(id, variable)) %>% 
    left_join(outliers, by="mid") %>% 
    mutate(detected=ifelse(is.na(detected), 0, detected),
           log.value=log10(value))
  f.alpha <- function(x) return(ifelse(x==2 | x==1, 1, 0))
  f.colour <- function(x) return(ifelse(x==2, "#FF0000", ifelse(x==1, "#FFCCCC", "#00FF00")))
  g <- ggplot(data.boxplot) +
    geom_boxplot(aes(x=q_k7, y=log.value)) + ylab("Values (log10)") +
    geom_point(aes(x=q_k7, y=log.value), 
               alpha=f.alpha(data.boxplot$detected),
               colour=f.colour(data.boxplot$detected)) +
    facet_wrap(~variable, scales="free_y", nrow = 9, ncol = 2)
  ggsave(paste0("output/checking/outliers/", latest.submission, " - outlier_analysis.pdf"), g, 
         width = 40, height = 80, units = "cm", device="pdf")
}

add_to_groups <- function(id1, id2){
  gid1 <- which(str_detect(groups, id1))
  gid2 <- which(str_detect(groups, id2))
  if (length(gid1)==0 & length(gid2)==0)
    groups <<- c(groups, paste0(id1, ";", id2))
  else{
    if (length(gid1)>=1){
      l <- str_split(groups[gid1], ";")[[1]]
      groups[gid1] <<- paste0(unique(c(l, id2)), collapse=";")
    }
    if (length(gid2)>=1){
      l <- str_split(groups[gid2], ";")[[1]]
      groups[gid2] <<- paste0(unique(c(l, id1)), collapse=";")
    }
  }
}

get.label <- function(variable){
  return(var.labels[var.labels$name==variable, "label.full"])
}

choice.name2label <- function(list.name, name){
  return(as.character(tool.choices[tool.choices$list_name==list.name & 
                                     tool.choices$name==name, label_colname]))
}

choice.label2name <- function(list.name, label){
  return(as.character(tool.choices[tool.choices$list_name==list.name & 
                                     tool.choices[label_colname]==label, "name"]))
}

get.old.value.label <- function(cl){
  for (r in 1:nrow(cl)){
    list.name <- as.character(cl[r, "list_name"])
    old.value <- as.character(cl[r, "old.value"])
    if (is.na(old.value)){
      cl[r, "old.value"] <- "No data (no value was reported)"
    } else if (!is.na(list.name)){
      cl[r, "old.value"] <- choice.name2label(list.name, old.value)
    }
  }
  return(cl)
}

save.follow.up.requests <- function(){
  cols <- c("id", "rel.index", "dataset.submission", "submission.time", 
            "admin1Pcode", "admin2Pcode", "admin3Pcode", "admin4Pcode", "locationPcode",
            "enumerator.code", "check.id", "variable", "variable.label", 
            "issue", "issue.arabic", "old.value", "new.value", "explanation")
  # prepare cleaning log
  cl <- left_join(fu.requests, 
                  select(raw.main.all, id, q_k2, q_k7, q_k8, q_k9, q_k10, q_k11, "_submission_time", submission),
                  by="id") %>% 
    dplyr::rename(enumerator.code=q_k2,
                  admin1Pcode=q_k7, admin2Pcode=q_k8, admin3Pcode=q_k9,
                  admin4Pcode=q_k10, locationPcode=q_k11,
                  submission.time="_submission_time",
                  dataset.submission=submission) %>% 
    mutate(explanation=NA)
  cl <- left_join(cl, select(get.select.db(), name, list_name), by=c("variable"="name"))
  cl <- get.old.value.label(cl)
  cl$name <- as.character(unlist(lapply(cl$variable, function(c) str_split(c, "\\/")[[1]][1])))
  cl <- left_join(cl, var_labels, by=c("name")) %>% 
    mutate(variable.label=ifelse(is.na(full.label), "", full.label)) %>% 
    select(all_of(cols))
  cl <- rbind(filter(cl, check.id!="Soft_duplicate") %>% arrange(id, check.id),
              filter(cl, check.id=="Soft_duplicate"))
  # save follow-up requests
  input.style <- createStyle(fgFill="#DDFFDD", border="TopBottomLeftRight", borderColour="#000000")
  col.style <- createStyle(textDecoration="bold", fgFill="#CECECE", halign="center",
                           border="TopBottomLeftRight", borderColour="#000000")
  temp <- read_excel(filename.follow.up.readme)
  wb <- loadWorkbook(filename.follow.up.readme)
  renameWorksheet(wb, 1, "ReadMe")
  setColWidths(wb, "ReadMe", cols=2, widths=25)
  setColWidths(wb, "ReadMe", cols=3:4, widths=50)
  addStyle(wb, "ReadMe", style = createStyle(valign="top"), rows = 1:(nrow(temp)+1), cols=1)
  addStyle(wb, "ReadMe", style = createStyle(wrapText=T, valign="top"), rows = 1:(nrow(temp)+1), cols=2)
  addStyle(wb, "ReadMe", style = createStyle(wrapText=T, valign="top"), rows = 1:(nrow(temp)+1), cols=3)
  addStyle(wb, "ReadMe", style = createStyle(wrapText=T, valign="top"), rows = 1:(nrow(temp)+1), cols=4)
  addStyle(wb, "ReadMe", style = col.style, rows = 1, cols=1:ncol(temp))
  addWorksheet(wb, "Follow-up")
  writeData(wb = wb, x = cl, sheet = "Follow-up", startRow = 1)
  addStyle(wb, "Follow-up", style = input.style, rows = 2:(nrow(cl)+1), cols=(ncol(cl)-1))
  addStyle(wb, "Follow-up", style = input.style, rows = 2:(nrow(cl)+1), cols=ncol(cl))
  setColWidths(wb, "Follow-up", cols=3, widths=32)
  setColWidths(wb, "Follow-up", cols=4, widths=18)
  setColWidths(wb, "Follow-up", cols=5:9, widths=12)
  setColWidths(wb, "Follow-up", cols=10, widths=15)
  setColWidths(wb, "Follow-up", cols=11, widths=13)
  setColWidths(wb, "Follow-up", cols=12, widths=14)
  setColWidths(wb, "Follow-up", cols=13:15, widths=50)
  setColWidths(wb, "Follow-up", cols=16, widths=25)
  setColWidths(wb, "Follow-up", cols=17, widths=20)
  setColWidths(wb, "Follow-up", cols=18, widths=30)
  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(nrow(cl)+1), cols=13)
  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(nrow(cl)+1), cols=14)
  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(nrow(cl)+1), cols=15)
  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(nrow(cl)+1), cols=16)
  addStyle(wb, "Follow-up", style = col.style, rows = 1, cols=1:ncol(cl))
  col.id <- which(colnames(cl)=="old.value")
  random.color <- randomColor(1, luminosity = "light")
  for (r in 1:(nrow(filter(cl, check.id!="Soft_duplicate"))-1)){
    if (r>1) 
      if (cl$id[r]!=cl$id[r-1] | cl$check.id[r]!=cl$check.id[r-1])
        random.color <- randomColor(1, luminosity = "light")
    if (cl$id[r]==cl$id[r+1] & cl$check.id[r]==cl$check.id[r+1] & 
        !(cl$check.id[r] %in% c("Outlier", "Typing", "Other_response"))){
      addStyle(wb, "Follow-up", style = createStyle(fgFill=random.color, wrapText=T), rows = (r+1):(r+2), cols=col.id)
    }
  }
  # add style for Soft_duplicate
  if (nrow(filter(cl, check.id=="Soft_duplicate"))>0){
    for (r in (nrow(filter(cl, check.id!="Soft_duplicate"))+1):nrow(cl)){
      if (r==1) 
        addStyle(wb, "Follow-up", style = createStyle(fgFill=random.color, wrapText=T), rows=(r+1), cols=c(1, col.id))
      else if (cl$variable[r-1]==cl$variable[r])
        addStyle(wb, "Follow-up", style = createStyle(fgFill=random.color, wrapText=T), rows=(r+1), cols=c(1, col.id))
      else{
        random.color <- randomColor(1, luminosity = "light")
        addStyle(wb, "Follow-up", style = createStyle(fgFill=random.color, wrapText=T), rows=(r+1), cols=c(1, col.id))
      }
    }
  }
  # worksheetOrder(wb) <- c(2, 1)
  # activeSheet(wb) <- 2
  filename <- paste0("output/checking/requests/", latest.submission, " - follow_up_requests.xlsx")
  saveWorkbook(wb, filename, overwrite=T)
}

get_type <- function(variable){
  if (str_detect(variable, "/")) return("select_multiple")
  else return(tool.survey$q.type[tool.survey$name==variable])
}

get_list_name <- function(variable){
  if (str_detect(variable, "/")) variable <- str_split(variable, "/")[[1]][1]
  return(tool.survey$list_name[tool.survey$name==variable])
}

add_choice <- function(concat_value, choice){
  l <- str_split(concat_value, " ")[[1]]
  l <- sort(unique(c(l, choice)))
  return(paste(l, collapse=" "))
}

remove_choice <- function(concat_value, choice){
  l <- str_split(concat_value, " ")[[1]]
  l <- l[l!=choice]
  return(paste(l, collapse=" "))
}

################################################################################################
# ANALYSIS

# "a2_border_crossing",

### CHECK This one here
convertColumnType <- function(df, col){
  cols_exclude <- c("a5_settlement_of_origin","a4_2_resp_age")
  if ((col %in% tool.survey$name) & !(col %in% cols_exclude)){
    q <- tool.survey[tool.survey$name==col,]
    if (str_starts(q$type, "select_one")){
      choices <- tool.choices %>% filter(list_name==q$list_name) %>%
        select(name, `label_colname`) %>% rename(label=`label_colname`)
      d <- data.frame(col=as.character(df[[col]])) %>% 
        left_join(choices, by=c("col"="name"))
      return(d$label)
    }
    else if (q$type=="integer" | q$type=="decimal") return(as.numeric(df[[col]]))
    else if (q$type=="date") return(as.character(as.Date(convertToDateTime(as.numeric(df[[col]])))))
    else return(df[[col]])
  } else if (str_detect(col, "/"))
    return(factor(as.numeric(df[[col]]), levels=c(0, 1)))
  else return(df[[col]])
}

################################################################################################
# CONVERT NAMES TO LABELS

name2label.question <- function(col){
  if (str_detect(col, "/")) {
    q.name <- str_split(col, "/")[[1]][1]
    c.name <- str_split(col, "/")[[1]][2]
  } else {
    q.name <- col
    c.name <- NA
  }
  if ((q.name %in% repair_colnames$name) & is.na(c.name)){
    label <- repair_colnames$new.label[repair_colnames$name==q.name]
  } else if (q.name %in% tool.survey$name){
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

name2label.select_multiple.values <- function(df, col.name){
  col <- df[[col.name]]
  if (col.name %in% tool.survey$name){
    q <- tool.survey[tool.survey$name==col.name,]
    if (str_starts(q$type, "select_multiple")){
      print(as.character(q$type))
      q.list_name <- str_split(q$type, " ")[[1]][2]
      col.labels <- as.character(lapply(col, function(x){
        if (!is.na(x)){
          l <- str_split(x, " ")[[1]]
          l.labels <- lapply(l, function(l_elem) as.character(
            tool.choices[tool.choices$list_name==q.list_name & tool.choices$name==l_elem, label_colname]))
          y <- paste0(as.character(l.labels), collapse = "; ")
        } else y <- NA
      }))
      col.labels[col.labels=="NA"] <- NA
      return(col.labels)
    } else return(col)
  } else return(col)
}

name2label <- function(df){
  df.label <- df
  col.names <- colnames(df)
  for (i in 1:length(col.names)){
    # code column name into label
    colnames(df.label)[i] <- name2label.question(col.names[i])
    # code values of select_multiple columns
    df.label[[colnames(df.label)[i]]] <- name2label.select_multiple.values(df, col.names[i])
  }
  return(df.label)
}
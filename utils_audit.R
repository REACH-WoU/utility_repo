load.audit.files <- function(dir.audits, is.pilot=F){
  audit.filenames <- list.files(dir.audits, pattern="audit.csv", recursive=TRUE, full.names=TRUE)
  print(paste("Loading",length(audit.filenames),"audit logs"))
  for (filename in audit.filenames){
    # get uuid from filename
    sp <- strsplit(filename, "\\/")[[1]]
    uuid <- sp[length(sp)-1]
    # load file
    audit <- read_csv(filename,show_col_types = FALSE, locale = locale(encoding = "UTF-8")) %>% 
    # %>% 
    #   rename("old.value" = `old-value`,
    #          "new.value" = `new-value`) %>% 
      mutate(uuid=uuid, .before=1)
    if (filename==audit.filenames[1]) res <- audit
    else res <- rbind(res, audit)
  }
  res <- res  %>% 
    mutate(duration=(end-start)/1000,
           group=sapply(str_split(node, '\\/'), function(x){
             id.group <- ifelse("G_survey" %in% x, 4, 3)
             return(x[id.group])}),
           question=sapply(str_split(node, '\\/'), function(x){return(x[length(x)])})) %>% 
    mutate(event=str_replace_all(event, " ", "."))
  return(res)
}

# remove_node <- function(node, is.pilot=T){
#   if (is.pilot){
#     str_detect(node, "G_wg_ss_respondent/") |
#       str_detect(node, "G_wg_ss/") |
#       str_detect(node, "G_expenditure/") | 
#       str_detect(node, "G_food_consumption/") | 
#       str_detect(node, "G_food_security/") | 
#       str_detect(node, "G_food_nutrition/") | 
#       str_detect(node, "G_food_coping_strategies/") | 
#       str_detect(node, "G_priority_needs/") | 
#       str_detect(node, "G_primary_income_source/")
#   } else{
#     str_detect(node, "G_demographics/") |
#       str_detect(node, "G_food/G_food_consumption/") |
#       str_detect(node, "G_food/G_food_coping_strategies/") | 
#       str_detect(node, "G_livelihoods/G_expenditure/") | 
#       str_detect(node, "G_priority_needs/")
#   }
# }

process_uuid <- function(df){
  max.num.iterations <- 15
  t <- list() # Time of each iteration
  rt <- list() # response time of each iteration 
  j <- list() # number of jumps
  q <- list() # number of questions
  w <- list() # waiting time
  # e <- list() # number of edits
  t1 <- df$start[1]
  if (df$event[1]!="form.start") stop()
  status <- "filling"
  for (r in 2:nrow(df)){
    if (status=="filling" & df$event[r]=="form.exit"){
      t2 <- df$start[r]
      t <- append(t, (t2-t1)/1000/60)
      sub.df <- filter(df, start>=t1 & start<=t2)
      questions <- filter(sub.df, event %in% c("question", "group.questions"))
      rt <- append(rt, sum(questions$duration)/60)
      q <- append(q, length(unique(questions$node)))
      j <- append(j, sum(sub.df$event=="jump"))
      # e <- append(e, nrow(filter(questions, !is.na(`old.value`) & `old.value`!="")))
      status <- "waiting"
    } else if (status=="waiting" & df$event[r]=="form.resume"){
      t1 <- df$start[r]
      w <- append(w, (t1-t2)/1000/60)
      status <- "filling"
    } else if (status=="waiting" & df$event[r]=="form.exit") stop()
  }
  res <- data.frame()
  res[1, "n.iteration"] <- length(t)
  res[1, "tot.t"] <- round((df[nrow(df), "start"] - df[1, "start"])/60000, 1)
  res[1, "tot.rt"] <- round(sum(filter(df, event %in% c("question", "group.questions"))$duration)/60, 1)
  for (i in 1:max.num.iterations){
    res[1, c(paste0("t", i), paste0("rt", i), 
             paste0("q", i), paste0("j", i),
             # paste0("e", i),
             paste0("w", i))] <- NA
  }
  if (length(t)==0) stop()
  else{
    for (i in 1:min(length(t), max.num.iterations)){
      res[1, paste0("t", i)] <- round(t[[i]], 1)
      res[1, paste0("rt", i)] <- round(rt[[i]], 1)
      res[1, paste0("q", i)] <- q[[i]]
      res[1, paste0("j", i)] <- j[[i]]
      # res[1, paste0("e", i)] <- e[[i]]
    }
  }
  if (length(w)>0){
    for (i in 1:min(length(w), max.num.iterations)) res[1, paste0("w", i)] <- round(w[[i]], 1)
  }
  return(res)
}
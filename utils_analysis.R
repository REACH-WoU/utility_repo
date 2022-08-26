
###-----------------------------------------------------------------------------
### CHECKS - why are they here...?
###-----------------------------------------------------------------------------

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

###-----------------------------------------------------------------------------
### DAP FUNCTIONS
###-----------------------------------------------------------------------------


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
  #' For 'integer'-type questions, the result is a numeric vector.
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
      if(omit_na) return(factor(d$label, levels = choices$label, exclude = NA))
      else        return(factor(d$label, levels = append(choices$label, NA), exclude = NULL))
    }
    else if (get.type(col)=="integer" | get.type(col)=="decimal") return(as.numeric(df[[col]]))
    else if (get.type(col)=="date") return(as.character(as.Date(convertToDateTime(as.numeric(df[[col]])))))
    else return(df[[col]])
  } else if (str_detect(col, "/")){
    # branch: column name present in data but not in tool.survey
    # meaning it's one of select_multiple options and should contain a "/"
    return(factor(as.numeric(df[[col]]), levels=c(0, 1), exclude = NA))
  }
  return(df[[col]])
}


###-----------------------------------------------------------------------------
### CONVERT NAMES TO LABELS
###-----------------------------------------------------------------------------

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
    if (str_starts(get.type(col), "select_multiple")){
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
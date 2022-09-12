###############################################################################
# UTILITY FUNCTIONS RELATED TO KOBO TOOLS
###############################################################################

get.type <- function(variable){
  #' find the type of variable
  #' @param variable This is the name of the header from raw data.
  if (str_detect(variable, "/")) return("select_multiple")
  else return(tool.survey$q.type[tool.survey$name==variable])
}

get.label <- function(variable){
  #' find the label of a variable
  #' @param variable This is the name of the header from raw data.
  if (str_detect(variable, "/")) variable <- str_split(variable, "/")[[1]][1]
  return(tool.survey[tool.survey$name == variable, ][[label_colname]])
}

get.choice.label <- function(choice, list){
  #' finds the label of a choice in a list
  #' @param choice the name of the choice
  #' @param list the name of the list containing choice
  res <- tool.choices %>% filter(list_name == list & name == choice)
  if(nrow(res) == 0) stop("choice not in the list!")
  return(pull(res, label_colname))
}

get.choice.list.from.name <- function(variable){
  #' find the choices list name
  #' @param variable This is the name of the header from raw data.
  if (str_detect(variable, "/")) variable <- str_split(variable, "/")[[1]][1]
  return(tool.survey %>% filter(name == variable) %>% pull(list_name))
}

get.choice.list.from.type <- function(q_type){
  #' finds the choice list for a question basing on its type
  q_type.1 <- str_split(q_type, " ")[[1]]
  if (length(q_type.1)==1) return(NA)
  else return(q_type.1[2])
}

get.ref.question <- function(q_relevancy){
  #' smart function that finds ref.question basing on relevancy text
  q_relevancy.1 <- str_split(q_relevancy, "\\{")[[1]][2]
  return(str_split(q_relevancy.1, "\\}")[[1]][1])
}

# ------------------------------------------------------------------------------------------
split.q.type <- function(x) return(str_split(x, " ")[[1]][1])

# ------------------------------------------------------------------------------------------
get.other.variables <- function(other_cnames = c()){
  #' finds all 'other' question in tool.survey 
  #' 
  #' This function is superceded by get.other.db and should be considered deprecated.
  #' 
  #' question needs to have 'other' in its relevancy, or be in `other_cnames`
  #' @returns Dataframe containing ref.question, label, name etc.
  
  ov <- tool.survey %>% 
    filter(type=="text" & 
             (str_detect(tolower(relevant), "other'") |
                name %in% other_cnames)) %>%
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
get.select.db <- function(){
  #' finds all 'select' type questions, and their choices
  
  # list of choices for each list_name (from TOOL_CHOICES)
  list.choices <- tool.choices %>% filter(!is.na(list_name)) %>% group_by(list_name) %>% 
    mutate(choices=paste(name, collapse=";\r\n"),
           choices.label=paste(!!sym(label_colname), collapse=";\r\n")) %>% 
    summarise(choices=choices[1], choices.label=choices.label[1])
  # list of choices for each question
  select.questions <- tool.survey %>% 
    rename(q.label=label_colname) %>% 
    select(type, name, q.label) %>% 
    mutate(q.type=as.character(lapply(type, split.q.type)),
           list_name=as.character(lapply(type, get.choice.list.from.type))) %>% 
    filter(list_name!="NA" & list_name!="group" & list_name!="repeat") %>% 
    left_join(list.choices, by="list_name") %>% 
    filter(!is.na(choices))
  return(select.questions)
}

# ------------------------------------------------------------------------------------------
get.other.db <- function(){
  #' finds all 'other' questions and their ref question and choices
  #' #' @returns Dataframe containing name, ref.name, full.label, choices etc.
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
  #' finds all questions which should be translated (meaning all 'text'-type question that are not 'other's)
  #' somewhat obsolete because it searches for ref questions too which are unnecesary
  select.questions <- get.select.db()
  
  df1 <- tool.survey %>% filter(type == "text" & !(str_ends(name, "_other"))) %>%
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

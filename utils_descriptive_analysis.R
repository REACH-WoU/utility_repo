###--------------------------------------------------------------------------------------------------------------
### SELECT_ONE
###--------------------------------------------------------------------------------------------------------------
# function to run the analysis
select_one.analysis <- function(srv.design, entry){
  # get list of disaggregations to be done (disaggregate.variable + admin.level) and keep only those not NA
  disaggregations <- c(entry$disaggregate.variable, entry$admin)
  disaggregations <- disaggregations[!is.na(disaggregations)]
  if (length(disaggregations)==0) {
    warning(paste("length(disaggregations)=0 for variable", entry$variable))
    # get proportions and confidence intervals (using svymean)
    res.prop <- svymean(make.formula(entry$variable), srv.design, na.rm=T)
    res.ci <- confint(res.prop, level=0.90)
    # arrange output formatting
    res <- cbind(res.prop, res.ci) %>% data.frame() %>% mutate(name=rownames(.)) %>%
      mutate(pct=round(res.prop*100, 1),
             ci=paste0(format(round(X5..*100, 1), scientific=F), "%-", format(round(X95..*100, 1), scientific=F), "%"))
    res[[entry$variable]] <- str_remove(res$name, entry$variable)
    res <- res %>% select(pct, ci, entry$variable)
  } else {
    # get proportions and confidence intervals (using svyby + svymean)
    res.prop <- svyby(make.formula(entry$variable), make.formula(disaggregations), srv.design, svymean, 
                      drop.empty.groups=F, multicore=F, na.rm=T, keep.names=F)
    res.ci <- data.frame(confint(res.prop, level=0.90))
    res.ci[,1] <- pmax(res.ci[, 1], 0)
    res.ci[,2] <- pmin(res.ci[, 2], 1)
    # arrange output formatting
    res.ci$name <- as.character(lapply(rownames(res.ci), function(x) str_split(x, entry$variable)[[1]][2]))
    res.ci <- res.ci %>% 
      mutate(ci=paste0(format(round(X5..*100, 1), scientific=F), "%-", 
                       format(round(X95..*100, 1), scientific=F), "%")) %>% 
      select(-c(X5.., X95..))
    res <- select(res.prop, -colnames(res.prop)[str_starts(colnames(res.prop), "se")])
    res <- mutate_if(res, is.numeric, ~round(.*100, 1))
    for (lev in unique(res.ci$name)){
      res.ci.sub <- res.ci[res.ci$name==lev,] %>% select(-name)
      colnames(res.ci.sub) <- paste0("ci.", lev)
      res <- cbind(res, res.ci.sub)
    }
    colnames(res) <- str_remove(colnames(res), entry$variable)
    cols1 <- colnames(res)[!(colnames(res) %in% disaggregations) & !str_starts(colnames(res), "ci.")]
    cols2 <- colnames(res)[!(colnames(res) %in% disaggregations) & str_starts(colnames(res), "ci.")]
    res1 <- pivot_longer(res %>% select(rev(disaggregations), all_of(cols1)), cols=all_of(cols1),
                         names_to = entry$variable, values_to = "pct")
    res2 <- pivot_longer(res %>% select(rev(disaggregations), all_of(cols2)), cols=all_of(cols2),
                         names_to = entry$variable, values_to = "ci")
    res <- cbind(res1, res2 %>% select(ci))
    if (!is.na(entry$disaggregate.variable)){
      res <- res %>% filter(!is.na(pct))
    }
  }
  return(res)
}
# function to produce HTML table
select_one.to_html <- function(res, entry, include.CI=T){
  var.full <- entry$variable
  var_list_name <- tool.survey$list_name[tool.survey$name==var.full]
  choices <- tool.choices[[label_colname]][tool.choices$list_name==var_list_name]
  res <- res %>% mutate(pct=ifelse(is.na(pct), NA, paste0(pct, "%")))
  res <- res %>% arrange(match(!!sym(entry$variable), choices)) %>% 
    pivot_wider(names_from=entry$variable[1], values_from=c("pct", "ci"), names_sep=".", values_fill=list(pct="0%"))
  colnames(res) <- as.character(lapply(colnames(res), function(x)
    if (str_starts(x, "pct.")) return(str_sub(x, 5, str_length(x)))
    else if (str_starts(x, "ci.")) return(paste0(str_sub(x, 4, str_length(x)), " [CI]"))
    else return(x)))
  if (!include.CI) res <- select(res, -all_of(colnames(res)[str_detect(colnames(res), "\\[CI\\]")]))
  if (is.na(entry$disaggregate.variable)){
    t <- data %>%
      filter(!is.na(!!sym(entry$variable))) %>%
      group_by(!!sym(entry$admin)) %>%
      summarise(num_samples=n())
    n_rows <- nrow(res)
    res <- res %>% 
      left_join(t, by=set_names(entry$admin)) %>% 
      relocate("num_samples", .after=1)
    if (nrow(res)!=n_rows) stop()
  }
  if (!is.na(entry$disaggregate.variable)){
    t <- data %>%
      filter(!is.na(!!sym(entry$variable))) %>%
      group_by(!!sym(entry$admin), !!sym(entry$disaggregate.variable)) %>%
      summarise(num_samples=n()) %>% 
      ungroup()
    n_rows <- nrow(res)
    res <- res %>% 
      left_join(t, by=c(set_names(entry$admin), set_names(entry$disaggregate.variable))) %>% 
      relocate("num_samples", .after=2)
    if (nrow(res)!=n_rows) stop()
  }
  res <- res %>% filter(!is.na(num_samples))
  write_xlsx(res,paste0("combine/",entry$xlsx_name,".xlsx"))
  return(subch(datatable(res)))
}

###--------------------------------------------------------------------------------------------------------------
### SELECT_MULTIPLE
###--------------------------------------------------------------------------------------------------------------
# function to run the analysis
select_multiple.analysis <- function(srv.design, entry){
  # get list of columns for the selected question --> variables 
  q.list_name <- str_split(tool.survey[tool.survey$name==entry$variable, "type"], " ")[[1]][2]
  choices <- tool.choices %>% filter(list_name==q.list_name) %>% 
    select(name, `label_colname`) %>% rename(label=`label_colname`) %>% 
    mutate(label=ifelse(name %in% c("other", "Other"), "Other", label))
  variables <- colnames(srv.design$variables)[str_starts(colnames(srv.design$variables), 
                                                         paste0(entry$variable, "___"))]
  # get list of disaggregations to be done (disaggregate.variable + admin.level)
  disaggregations <- c(entry$disaggregate.variable, entry$admin)
  disaggregations <- disaggregations[!is.na(disaggregations)]
  if (length(disaggregations)==0) {
    warning(paste("length(disaggregations)=0 for variable", entry$variable))
    # get proportions and confidence intervals (using svymean)
    res.prop <- svymean(make.formula(variables), srv.design, na.rm=T)
    res.ci <- confint(res.prop, level=0.90)
    # arrange output formatting
    res <- cbind(res.prop, res.ci) %>% data.frame() %>% mutate(name=rownames(.)) %>% 
      filter(str_ends(name, "1")) %>% 
      mutate(pct=round(res.prop*100, 1),
             ci=paste0(format(round(X5..*100, 1), scientific=F), "%-", format(round(X95..*100, 1), scientific=F), "%"),
             choice=str_sub(name, 1, str_length(name)-1)) %>% select(pct, ci, choice)
  } else{
    # get proportions and confidence intervals (using svyby + svymean)
    res.prop <- svyby(make.formula(variables), make.formula(disaggregations), srv.design, svymean, 
                      drop.empty.groups=F, multicore=F, na.rm=T)
    res.ci <- data.frame(confint(res.prop, level=0.9))
    res.ci[,1] <- pmax(res.ci[, 1], 0)
    res.ci[,2] <- pmin(res.ci[, 2], 1)
    # arrange output formatting
    res.ci$name <- as.character(lapply(rownames(res.ci), function(x) str_split(x, ":")[[1]][2]))
    res.ci <- res.ci %>% 
      mutate(ci=paste0(format(round(X5..*100, 1), scientific=F), "%-", 
                       format(round(X95..*100, 1), scientific=F), "%")) %>% 
      select(-c(X5.., X95..))
    res <- select(res.prop, -colnames(res.prop)[str_starts(colnames(res.prop), "se")])
    res <- mutate_if(res, is.numeric, ~round(.*100, 1))
    for (lev in unique(res.ci$name)){
      res.ci.sub <- res.ci[res.ci$name==lev,] %>% select(-name)
      colnames(res.ci.sub) <- paste0("ci.", lev)
      res <- cbind(res, res.ci.sub)
    }
    cols <- colnames(res)[str_ends(colnames(res), "1") & !(colnames(res) %in% disaggregations)]
    res <- res[c(rev(disaggregations), cols)]
    colnames(res) <- c(rev(disaggregations), str_sub(cols, 1, str_length(cols)-1))
    cols2 <- colnames(res)[str_starts(colnames(res), "ci.")]
    res1 <- pivot_longer(res %>% select(rev(disaggregations), all_of(variables)), cols=all_of(variables),
                         names_to = "choice", values_to = "pct")
    res2 <- pivot_longer(res %>% select(rev(disaggregations), all_of(cols2)), cols=all_of(cols2),
                         names_to = "choice", values_to = "ci")
    res <- cbind(res1, res2 %>% select(ci))
    if (!is.na(entry$disaggregate.variable)){
      res <- res %>%
        filter(!is.na(pct)) %>%
        group_by(!!sym(entry$admin), !!sym(entry$disaggregate.variable)) %>%
        mutate(sum.pct=sum(pct)) %>%
        ungroup() %>%
        filter(pct>0) %>%
        select(-sum.pct)
    }
  }
  # recode choice name -> label
  res <- mutate(res, choice=str_sub(choice, str_length(entry$variable) + 4, str_length(choice))) %>% 
    left_join(choices, by=c("choice"="name")) %>% select(-choice)
  res <- res %>% arrange(label)
  return(res)
}

# function to produce HTML table
select_multiple.to_html <- function(res, entry, include.CI=T){
  var.full <- entry$variable
  var_list_name <- tool.survey$list_name[tool.survey$name==var.full]
  choices <- tool.choices[tool.choices$list_name==var_list_name, label_colname]
  res <- arrange(res, match(label, choices))
  res <- res %>% mutate(pct=ifelse(is.na(pct), NA, paste0(pct, "%")))
  res1 <- res %>% select(-ci) %>% pivot_wider(names_from="label", values_from=pct, values_fill="0%")
  res2 <- res %>% select(-pct) %>% pivot_wider(names_from="label", values_from=ci, values_fill="NA")
  res2 <- res2[, (ncol(res)-3+1):ncol(res2)]
  colnames(res2) <- as.character(lapply(colnames(res2), function(x) paste0(x, " [CI]")))
  if (include.CI){
    res <- cbind(res1, res2)
  } else{
    res <- res1
  }
  if (is.na(entry$disaggregate.variable)){
    t <- data %>%
      filter(!is.na(!!sym(entry$variable))) %>%
      group_by(!!sym(entry$admin)) %>%
      summarise(num_samples=n())
    n_rows <- nrow(res)
    res <- res %>% 
      left_join(t, by=set_names(entry$admin)) %>% 
      relocate("num_samples", .after=1)
    if (nrow(res)!=n_rows) stop()
  }
  if (!is.na(entry$disaggregate.variable)){
    res <- res %>% arrange(!!sym(entry$admin), !!sym(entry$disaggregate.variable))
    t <- data %>%
      filter(!is.na(!!sym(entry$variable))) %>%
      group_by(!!sym(entry$admin), !!sym(entry$disaggregate.variable)) %>%
      summarise(num_samples=n()) %>% 
      ungroup()
    n_rows <- nrow(res)
    res <- res %>% 
      left_join(t, by=c(set_names(entry$admin), set_names(entry$disaggregate.variable))) %>% 
      relocate("num_samples", .after=2)
    if (nrow(res)!=n_rows) stop()
  }
  res <- res %>% filter(!is.na(num_samples))
  if(!entry$omit_na){
    res$num_samples <- nrow(data) # spit and fix
  }
  write_xlsx(res,paste0("combine/",entry$xlsx_name,".xlsx"))
  return(subch(datatable(res)))
}

###--------------------------------------------------------------------------------------------------------------
### MEAN
###--------------------------------------------------------------------------------------------------------------
# function to run the analysis
mean.analysis <- function(srv.design, entry){
  srv.design.grouped <- srv.design
  if (!is.na(entry$admin))
    srv.design.grouped <- srv.design.grouped %>% group_by(!!sym(entry$admin), .add=T, .drop=T)
  if (!is.na(entry$disaggregate.variable))
    srv.design.grouped <- srv.design.grouped %>% group_by(!!sym(entry$disaggregate.variable), .add=T, .drop=T)
  if (all(is.na(srv.design.grouped$variables[[entry$variable]]))) return(data.frame())
  res <- srv.design.grouped %>% 
    filter(!is.na(!!sym(entry$variable))) %>% 
    summarise(mean = survey_mean(!!sym(entry$variable), vartype="ci", level=0.90))
  if (str_starts(entry$variable, "pct.")){
    res <- res %>% mutate(mean=round(mean, 1),
                          ci=paste0(format(round(mean_low, 1), scientific=F), "%-", 
                                    format(round(mean_upp, 1), scientific=F), "%"))
  } else{
    res <- res %>% mutate(mean=round(mean, 2),
                          ci=paste0(format(round(mean_low, 2), scientific=F), "-", 
                                    format(round(mean_upp, 2), scientific=F)))
  }
  res <- res %>% select(-c(mean_low, mean_upp))
  if (!is.na(entry$disaggregate.variable))
    res <- filter(res, !is.na(!!sym(entry$disaggregate.variable)))
  return(res)
}
# function to produce HTML table
mean.to_html <- function(res, entry, include.CI=T){
  if (str_starts(entry$variable, "pct.")){
    res <- res %>% mutate(mean=ifelse(is.na(mean), NA, paste0(mean, "%")))
  }
  
  if (!include.CI) res <- res %>% select(-CI)
  
  if (is.na(entry$disaggregate.variable)){
    t <- data %>%
      filter(!is.na(!!sym(entry$variable))) %>%
      group_by(!!sym(entry$admin)) %>%
      summarise(num_samples=n())
    n_rows <- nrow(res)
    res <- res %>% 
      left_join(t, by=set_names(entry$admin)) %>% 
      relocate("num_samples", .after=1)
    if (nrow(res)!=n_rows) stop()
  }
  if (!is.na(entry$disaggregate.variable)){
    t <- data %>%
      filter(!is.na(!!sym(entry$variable))) %>%
      group_by(!!sym(entry$admin), !!sym(entry$disaggregate.variable)) %>%
      summarise(num_samples=n()) %>% 
      ungroup()
    n_rows <- nrow(res)
    res <- res %>% 
      left_join(t, by=c(set_names(entry$admin), set_names(entry$disaggregate.variable))) %>% 
      relocate("num_samples", .after=2)
    if (nrow(res)!=n_rows) stop()
  }
  res <- res %>% filter(!is.na(num_samples))
  write_xlsx(res,paste0("combine/",entry$xlsx_name,".xlsx"))
  return(subch(datatable(res)))
}

###--------------------------------------------------------------------------------------------------------------
### OTHER FUNCTIONS
###--------------------------------------------------------------------------------------------------------------
# add table to HTML
subch <- function(g) {
  g_deparsed <- paste0(deparse(function() {g}), collapse = '')
  sub_chunk <- paste0("\n\n", "```{r sub_chunk_", floor(runif(1) * 10000), ", echo=FALSE}\n(", 
                      g_deparsed, ")()\n```", "\n")
  cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
}
# add section to HTML
add_to_html.section <- function(name) cat(paste0(paste0(rep("\n",2), collapse=""), "# ", name))
# add title to HTML
add_to_html.title <- function(entry){
  # title <- ifelse(is.na(entry$variable), entry$name, entry$variable)
  # cat(paste0(paste0(rep("\n",2), collapse=""), paste0(rep("#",3), collapse=""), " ", title))
  cat(paste0(paste0(rep("\n",2), collapse=""), paste0(rep("#",3), collapse=""), " ", entry$label))
}
# add subtitle to HTML
add_to_html.sub_title <- function(disaggregate.variable){
  if (is.na(disaggregate.variable)) {
    cat(paste0(paste0(rep("\n",2), collapse=""), paste0(rep("#",5), collapse=""), " No disaggregation"))
  } else{
    cat(paste0(paste0(rep("\n",2), collapse=""), paste0(rep("#",5), collapse=""), " Disaggregated by ",
               disaggregate.variable))
  }
}
# load entry from analysis plan
load.entry <- function(analysis.plan.row){
  section <- as.character(analysis.plan.row$section)
  label <- as.character(analysis.plan.row$label)
  variable <- as.character(analysis.plan.row$variable)
  func <- as.character(analysis.plan.row$func)
  admin <- as.character(analysis.plan.row$admin)
  disaggregate.variable <- as.character(analysis.plan.row$disaggregate.variable)
  data <- as.character(analysis.plan.row$data)
  xlsx_name <- as.character(analysis.plan.row$xlsx_name)
  calculation <- as.character(analysis.plan.row$calculation)
  comments <- as.character(analysis.plan.row$comments)
  if (is.na(disaggregate.variable)) {
    disaggregate.variables <- c(NA)
  } else{
    disaggregate.variables <- c(str_split(disaggregate.variable, ";")[[1]])
  }
  return(list(section=section, label=label, variable=variable, func=func, 
              admin=admin, disaggregate.variables=disaggregate.variables, data=data,
              xlsx_name=xlsx_name, comments=comments, calculation=calculation))
}

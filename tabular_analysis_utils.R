###--------------------------------------------------------------------------------------------------------------
### Styling of the tabular analysis
###--------------------------------------------------------------------------------------------------------------

# add table to HTML
subch <- function(g, fig_height=7, fig_width=5) {
  g_deparsed <- paste0(deparse(function() {g}), collapse = '')
  sub_chunk <- paste0("\n\n<center>\n", "```{r sub_chunk_", as.numeric(Sys.time())*1000, 
                      ", fig.height=", fig_height, ", fig.width=", fig_width, ", echo=FALSE}\n(", 
                      g_deparsed, ")()\n```", "\n</center>")
  cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
}

# add section to HTML
add_to_html.section <- function(name) cat(paste0("\n\n## ", name," {.tabset}"))

# add title to HTML
add_to_html.title <- function(entry){
  cat(paste0(paste0(rep("\n",2), collapse=""), paste0(rep("#",4), collapse=""), " ", "<strong>",entry$label,"</strong>"))
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
#--------------------------------------------------------------------------------------------------------------
# list to add to datatable to style the table
tableFormat <-list(
  dom = 'T<"clear">lfrtip',
  scrollX = TRUE)

# jsFunc <- "(function(value){
#   // find a number preceeded by an open parenthesis with an optional minus sign
#   var matches = value.replace('%','');
#   // ignore values which do not match our pattern, returning white as the background color
#   if(!matches || matches.length < 2) { 
#     return 'white'; 
#   }
#   // attempt to convert the match we found into a number
#   var int = parseInt(matches[1]); 
#   // if we can't ignore it and return a white color
#   if(isNaN(int)) { 
#     return 'white';
#   } 
#   // if the value is negative, return red
#   if(int < 50) { 
#     return 'red' 
#   }
#   // otherwise, by default, return green
#   return 'green';
# })(value)"
#TO DEBUGGG
# function to produce HTML table

sm_ccols_to_choices <- function(ccols){
  #' small utility to split names of select_multiple choice columns into choice names
  str_split(ccols %>% str_remove_all("_prop$"), "___", simplify = T)[,2]
}

make_table.select_one <- function(srvyr.design.grouped, entry, add_total = FALSE) {
  res <- srvyr.design.grouped %>% 
    group_by(!!sym(entry$variable), .add = T) %>% 
    summarise(n = survey_total(na.rm = T, vartype = "var"),
              prop = survey_prop(na.rm = T, vartype = "var")) %>% 
    pivot_wider(names_from = !!sym(entry$variable), values_from = prop) %>% 
    group_by(!!sym(entry$admin), .add = T) %>%
    summarise(across( .fns = ~sum(., na.rm = T)))
  
  if(add_total){
    total <- make_table.select_one(srvyr.design.grouped %>% ungroup %>% 
                               select(!!sym(entry$admin), !!sym(entry$variable)) %>% 
                               group_by(!!sym(entry$admin)), entry)
    res <- res %>% bind_rows(total)
  }
  
  return(res)
}

make_table.select_multiple <- function(srvyr.design.grouped, entry, add_total = FALSE){

  disagg_vars <- c(entry$admin, entry$disaggregate.variables)
  if(any(isna(disagg_vars))) disagg_vars <- NULL
  disagg_vars <- disagg_vars[disagg_vars %in% (srvyr.design.grouped %>% variable.names)]
  
  # calculate the totals and percentages, then join them together
  s_props <- srvyr.design.grouped %>% 
    summarise(across(.fns = list(prop = ~ survey_mean(., na.rm = T, vartype = "var"))))
    
  s_samples <- srvyr.design.grouped %>% 
    summarise(n = survey_total(na.rm = T, vartype = "var"))
  
  res <- s_samples %>% left_join(s_props, by = disagg_vars) #%>% relocate(n, .after = any_of(disagg_vars))
  if(add_total){
    # calculate total:
    total <- srvyr.design.grouped %>% ungroup %>% group_by(!!sym(entry$admin)) %>% select(contains("___")) %>%
               summarise(across(.fns = list(prop = ~ survey_mean(., na.rm = T, vartype = "var"))), .groups = "drop") 
    
    total_samples <- s_samples %>% group_by(!!sym(entry$admin)) %>% summarise(n = sum(n))
     
    res <- res %>% bind_rows(total_samples %>% left_join(total, by = entry$admin))
  }
  res <- res %>% 
    rename_with(~get.choice.label(sm_ccols_to_choices(.), entry$list_name, simplify = T), ends_with("_prop"))
  
  return(res)
}

make_table <- function(srvyr.design, entry, disagg.var){
  if(isna(disagg.var)) disagg.var <- entry$admin   
  srvyr.design.grouped <- srvyr.design %>% group_by(!!sym(disagg.var), .add = T)
  
  # select the relevant columns / variables
  srvyr.design.grouped <- switch (entry$func,
                                  select_multiple = { srvyr.design.grouped %>% select(starts_with(entry$variable) & contains("___")) },
                                  # default - just select the relevant variable:
                                  { srvyr.design.grouped %>% select(!!sym(entry$variable)) }
  )
  
  # calculate metrics
  res <- switch (entry$func,
                 numeric =    { srvyr.design.grouped %>% 
                     summarise(
                       n = n(),
                       mean  =  survey_mean(  !!sym(entry$variable), na.rm = T, vartype = "var"),
                       median = survey_median(!!sym(entry$variable), na.rm = T, vartype = "var"),
                       min = min(!!sym(entry$variable), na.rm = T),
                       max = max(!!sym(entry$variable), na.rm = T)) },
                 select_one = { srvyr.design.grouped  %>%  make_table.select_one(entry, add_total = entry$add_total & disagg.var != entry$admin) },
                 select_multiple = { srvyr.design.grouped %>% make_table.select_multiple(entry, add_total = entry$add_total & disagg.var != entry$admin)  }
  )
  
  ##### cleaning up the res #####
  
  # remove the variance columns, filter out choices with n = 0
  res <- res %>% select(-ends_with("_var")) 
  if("n" %in% names(res)) res <- res %>% filter(n > 0)
  
  # rename props columns, convert to percentages etc:
  res <- switch (entry$func,
                 numeric = res %>% mutate(across(where(is.numeric), ~round(., 2))), 
                 # default (select_one or multiple):
                 { res %>% mutate(across(where(is.numeric) & !matches("^n$"), as_perc)) %>% 
                     # add label for the TOTAL row
                     mutate(!!disagg.var := replace_na(!!sym(disagg.var) %>% as.character, "<TOTAL>"))  }
  )
  return(res)
}

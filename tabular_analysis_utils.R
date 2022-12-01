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
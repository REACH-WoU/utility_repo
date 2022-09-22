# RECODING:
#-------------------------------------------------------------------------------

recode.new.site <- function(raw.data, x, f){
  #' Recode a new site.
  #'
  #' Creates cleaning log entries and assigns new pCodes based on newsite request entry.
  #' Function `f` is used to generate a new pCode and is assumed to not require any arguments.
  #'
  #' @param raw.data Raw dataset containing columns: uuid, site_pCode, site_Name
  #' @param x Entry from a newsite requests file (containing columns uuid and true.v)
  #' @param f Function that returns a character string containing new pCode
  #' @returns Cleaning log entries - dataframe with columns uuid, variable, issue, old and new values.

  cl <- data.frame(
    uuid=x$uuid, variable="site_pCode", issue="Recoding an identified new site.",
    old.value=pull(raw.data %>% filter(uuid==x$uuid), site_pCode), new.value=f())
  cl <- rbind(cl, data.frame(
    uuid=x$uuid, variable="site_Name", issue="Recoding an identified new site.",
    old.value=pull(raw.data %>% filter(uuid==x$uuid), site_Name), new.value=x$true.v))
  return(cl)
}

# REQUESTS:
#-------------------------------------------------------------------------------
create.gis.checks <- function(data){
  cols_to_keep_gis_check <- append(c("enumerator_id","uuid","site_Name",
                                     "admin1", "admin2", "admin2_Label",
                                     "site_pCode", "pulled_Address", "comments_text"),
                             colnames(data)[str_starts(colnames(data), "site_(Add?ress)") |  str_starts(colnames(data), "site_Loc_GPS") ])
  gis.check.df <- data %>% filter(site_pCode != "new") %>%
    select(all_of(cols_to_keep_gis_check)) %>%
    arrange(site_pCode) %>%
    relocate(all_of(colnames(data)[str_starts(colnames(data), "(site_Add?ress)|(admin)")]), .after = last_col()) %>%
    relocate(comments_text, .after = last_col()) %>%
    relocate(site_pCode, uuid) %>%
    mutate("TRUE NEW site (provide a name)"=NA,
           "EXISTING (paste the correct pCode for the site)"=NA,
           "INVALID (provide an explanation)"=NA)
  return(gis.check.df)
}

save.gis.checks <- function(df, wb_name, blue_cols = NULL){
  style.col.blue <- createStyle(fgFill="#CCE5FF", valign="top",
                                border="TopBottomLeftRight", borderColour="#000000", wrapText=T)
  style.col.green <- createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000",
                                 valign="top", fontSize = 10, fontName = "Arial Narrow", wrapText=T)
  style.col.green.bold <- createStyle(textDecoration="bold", fgFill="#E5FFCC", valign="top",
                                      border="TopBottomLeftRight", borderColour="#000000",
                                      fontSize = 10, fontName = "Arial Narrow", wrapText=T)

  wb <- loadWorkbook("resources/gis_checks_template.xlsx")
  addWorksheet(wb, "Sheet2")
  writeData(wb = wb, x = df, sheet = "Sheet2", startRow = 1)
  setColWidths(wb, "Sheet2", cols = 1:(ncol(df)-4), widths = "auto")
  setColWidths(wb, "Sheet2", cols = (ncol(df)-4):(ncol(df)), widths = 35)
  i <- grep("comments", colnames(df))
  setColWidths(wb, "Sheet2", cols = i, widths = 60)
  addStyle(wb, "Sheet2", style = createStyle(wrapText=T, valign="top", fontSize = 10, fontName = "Arial Narrow"),
           rows = 1:(nrow(df)+1), cols=i, stack = T)
  for (col in blue_cols) {
    i <- grep(paste0('^',col,'$'), colnames(df))
    if(length(i) == 0) stop(paste(col,"not found in df!"))
    addStyle(wb, "Sheet2", style = style.col.blue, rows = 1:(nrow(df)+1), cols = i, stack = T)
  }
  addStyle(wb, "Sheet2", style = createStyle(textDecoration="bold"), rows = 1, cols=1:ncol(df), stack = T)
  addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df)-2, stack = T)
  addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df)-1, stack = T)
  addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df), stack = T)
  addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df)-2, stack = T)
  addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df)-1, stack = T)
  addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df), stack = T)

  filename <- paste0(dir.requests, wb_name, ".xlsx")
  saveWorkbook(wb, filename, overwrite=TRUE)

}

#-------------------------------------------------------------------------------
create.newsite.requests <- function(data){

    tryCatch({
        cols_to_keep_new_sites <- append(c("enumerator_id","uuid",
                                            "admin1", "admin2", "admin2_Label",
                                            "site_New_name", "comments_text"),
                          colnames(data)[str_starts(colnames(data), "site_(Add?ress)|(Manager)") | str_detect("site_Loc_GPS", colnames(data))])
        new.sites.df <- data %>% filter(site_pCode == "new") %>%
           arrange(`_index`) %>%
           select(all_of(cols_to_keep_new_sites)) %>%
           relocate(all_of(colnames(data)[str_starts(colnames(data), "(site_Add?ress)|(admin)")]), .after = last_col()) %>%
           relocate(comments_text, .after = last_col()) %>%
           mutate("TRUE NEW site (provide a better name if necessary)"=NA,
                    "EXISTING (paste the correct pCode for the site)"=NA,
                    "INVALID (provide an explanation)"=NA) %>%
          relocate(uuid)

    }, error = function(err){
        warning("Error while saving newsite requests\n::",err)
    })

    return(new.sites.df)
}

save.newsite.requests <- function(df, wb_name, blue_cols = NULL){

  style.col.green <- createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000",
                                 valign="top", wrapText=T)
  style.col.green.bold <- createStyle(textDecoration="bold", fgFill="#E5FFCC", valign="top",
                                       border="TopBottomLeftRight", borderColour="#000000", wrapText=T)
  style.col.blue <- createStyle(fgFill="#CCE5FF", valign="top",
                                        border="TopBottomLeftRight", borderColour="#000000", wrapText=T)

  wb <- loadWorkbook("resources/newsite_requests_template.xlsx")
  addWorksheet(wb, "Sheet2")
  writeData(wb = wb, x = df, sheet = "Sheet2", startRow = 1)
  setColWidths(wb, "Sheet2", cols = 1:(ncol(df)-4), widths = "auto")
  setColWidths(wb, "Sheet2", cols = (ncol(df)-4):(ncol(df)), widths = 35)
  i <- grep("comments", colnames(df))
  setColWidths(wb, "Sheet2", cols = i, widths = 60)
  addStyle(wb, "Sheet2", style = createStyle(wrapText=T, valign="top", fontSize = 10, fontName = "Arial Narrow"),
            rows = 1:(nrow(df)+1), cols=i, stack = T)
  for (col in blue_cols) {
     i <- grep(paste0('^',col,'$'), colnames(df))
     if(length(i) == 0) stop(paste(col,"not found in df!"))
     addStyle(wb, "Sheet2", style = style.col.blue, rows = 1:(nrow(df)+1), cols = i, stack = T)
  }
  addStyle(wb, "Sheet2", style = createStyle(textDecoration="bold"), rows = 1, cols=1:ncol(df), stack = T)
  addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df)-2, stack = T)
  addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df)-1, stack = T)
  addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df), stack = T)
  addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df)-2, stack = T)
  addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df)-1, stack = T)
  addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df), stack = T)

  filename <- paste0("output/checking/requests/", wb_name, ".xlsx")
  saveWorkbook(wb, filename, overwrite=TRUE)
}

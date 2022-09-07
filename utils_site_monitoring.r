save.other.requests <- function(df, wb_name){
  
  style.col.green <- createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000", 
                                 valign="top", fontSize = 10, fontName = "Arial Narrow", wrapText=T)
  style.col.green.bold <- createStyle(textDecoration="bold", fgFill="#E5FFCC", valign="top",
                                      border="TopBottomLeftRight", borderColour="#000000",
                                      fontSize = 10, fontName = "Arial Narrow", wrapText=T)
  
  wb <- loadWorkbook("resources/other_requests_template.xlsx")
  addWorksheet(wb, "Sheet2")
  writeData(wb = wb, x = df, sheet = "Sheet2", startRow = 1)
  setColWidths(wb, "Sheet2", cols = 2:3, widths = "auto")
  setColWidths(wb, "Sheet2", cols = (ncol(df)-4):(ncol(df)), widths = 35)
  addStyle(wb, "Sheet2", style = createStyle(fontSize = 10, fontName = "Arial", wrapText = T), rows = 2:nrow(df), cols=ncol(df) - 3)
  addStyle(wb, "Sheet2", style = createStyle(fontSize = 10, fontName = "Arial Narrow", wrapText = T), rows = 2:nrow(df), cols=ncol(df) - 4)
  addStyle(wb, "Sheet2", style = createStyle(fontSize = 10, fontName = "Arial Narrow", wrapText = T), rows = 2:nrow(df), cols=ncol(df) - 6)
  addStyle(wb, "Sheet2", style = createStyle(textDecoration="bold", border = "Bottom", fontName = "Arial"), rows = 1, cols=1:ncol(df), stack = T)
  addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df)-2, stack = T)
  addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df)-1, stack = T)
  addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df), stack = T)
  addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df)-2, stack = T)
  addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df)-1, stack = T)
  addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df), stack = T)
  
  filename <- paste0("output/checking/requests/", wb_name, ".xlsx")
  saveWorkbook(wb, filename, overwrite=TRUE)
  
}

create.gis.checks <- function(data){
  cols_to_keep_gis_check <- append(c("enumerator_nr","uuid",
                                     "admin1", "admin2",
                                     "admin2_Label",
                                     "site_pCode",
                                     "comments_text"),
                             colnames(data)[str_starts(colnames(data), "site_(Add?ress)") | str_detect("site_Loc_GPS", colnames(data)) ])
  gis.check.df <- data %>% select(all_of(cols_to_keep_gis_check)) %>% 
    relocate(all_of(colnames(data)[str_starts(colnames(data), "(site_Add?ress)|(admin)")]), .after = last_col()) %>%
    relocate(comments_text, .after = last_col()) %>% 
    relocate(uuid)
  return(gis.check.df)
}

save.gis.checks <- function(df, wb_name, blue_cols = NULL){
  style.col.blue <- createStyle(fgFill="#CCE5FF", valign="top",
                                border="TopBottomLeftRight", borderColour="#000000", wrapText=T)
  
  wb <- createWorkbook()
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
  
  filename <- paste0("output/checking/requests/", wb_name, ".xlsx")
  saveWorkbook(wb, filename, overwrite=TRUE)
  
}

create.newsite.requests <- function(data){

    tryCatch({
        cols_to_keep_new_sites <- append(c("enumerator_nr","uuid",
                                            "admin1", "admin2",
                                            "admin2_Label",
                                            "site_New_name",
                                            "comments_text"),
                          colnames(data)[str_starts(colnames(data), "site_(Add?ress)|(Manager)") | str_detect("site_Loc_GPS", colnames(data))])
        new.sites.df <- data %>% filter(site_pCode == "new") %>%
                               arrange(`_index`) %>%
                               select(all_of(cols_to_keep_new_sites)) %>%
                               relocate(all_of(colnames(data)[str_starts(colnames(data), "(site_Add?ress)|(admin)")]), .after = last_col()) %>%
                               relocate(comments_text, .after = last_col())
        new.sites.df <- new.sites.df %>% 
            mutate("TRUE NEW site (provide a better name if necessary)"=NA,
                    "EXISTING (paste the correct pCode for the site)"=NA,
                    "INVALID (insert yes)"=NA) %>% 
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

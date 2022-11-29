#------------------------------------------------------------------------------------------------------------
# utility operators & other miscellaneous functions
#------------------------------------------------------------------------------------------------------------

"%==%" <- function(a, b) ifelse(!is.na(a), a==b, F)
"%!=%" <- function(a, b) ifelse(!is.na(a), a!=b, F)
"%_<_%" <- function(a, b) ifelse(!is.na(a), as.numeric(a)<b, F)
"%_<=_%" <- function(a, b) ifelse(!is.na(a), as.numeric(a)<=b, F)
"%_>_%" <- function(a, b) ifelse(!is.na(a), as.numeric(a)>b, F)
"%_>=_%" <- function(a, b) ifelse(!is.na(a), as.numeric(a)>=b, F)
"%_+_%" <- function(a, b) as.numeric(a) + as.numeric(b)
"%==na%" <- function(e1, e2) ifelse(is.na(e1 == e2), is.na(e1) == is.na(e2), e1 == e2)
"%!=na%" <- function(e1, e2) (e1 != e2 | (is.na(e1) & !is.na(e2)) | (is.na(e2) & !is.na(e1))) & !(is.na(e1) & is.na(e2))


qview <- function(datum, title = NULL, ..., n = NULL){ 
  view(datum %>% select(-contains("/")), title = ifelse(is.null(title), "qview", title), ..., n = NULL) 
}

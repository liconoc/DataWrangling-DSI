source("detectMetaParams_base.R")


## Punctuation

has_2slash <- function(input){
  if(!is.na(input) && str_count(input, "/")==2) {
    return(TRUE)
  }else {
      return(FALSE)
    }
}

has_2dash <- function(input){
  if(!is.na(input) && str_count(input, "-")==2) {
    return(TRUE)
  }else {
    return(FALSE)
  }
}

has_2dot <- function(input){
  if(!is.na(input) && str_count(input, "[.]")==2) {
    return(TRUE)
  }else {
    return(FALSE)
  }
}

has_2blank <- function(input){
 if(!is.na(input) && length(strsplit(input, " ")[[1]])==2) {
   return(TRUE)
 }else {
  return(FALSE)
 }
}

has_1comma <- function(input){
  if(!is.na(input) && str_count(input, ",")==1) {
    return(TRUE)
  }else {
    return(FALSE)
  }
}

### Structure

has_onlyNumbersAndSymbols <- function(input){
  return(has_punctuation(input) & has_numbers(input) & !grepl("[[:alpha:]]", input))
}

has_6digits <- function(input){
  return(grepl("^\\D*(?:\\d\\D*){6}$", input))
}

has_8digits <- function(input){
  return(grepl("^\\D*(?:\\d\\D*){8}$", input))
}

is_shortDateStructure <- function(input){
  return(grepl("\\b\\d{2}[^\\d]\\d{2}[^\\d]\\d{2}\\b", input))
}

is_LongDateStructure <- function(input){
  return(grepl("\\b(\\d{2}[^\\d]\\d{2}[^\\d]\\d{4})|(\\d{2}[^\\d]\\d{4}[^\\d]\\d{2})|(\\d{4}[^\\d]\\d{2}[^\\d]\\d{2})\\b", input))
}

has_ordinalNumbers <- function(input){
  return(grepl("\\b(\\d+)(?:st|nd|rd|th)\\b", input))
}

## Content

has_dayName <- function(input){
  return(grepl("\\b(((m|M)on|(t|T)ues|(w|W)ed(nes)?|(t|T)hur(s)?|(f|F)ri|(s|S)at(ur)?|(s|S)un)(day)?)|((l|L)un(es)?|(m|M)ar(tes)?|(m|M)i(e|é)(rcoles)?|(j|J)ue(ves)?|(v|V)ie(rnes)?|(s|S)(a|á)b(ado)?|(d|D)om(ingo)?)\\b", input))
}

has_monthName <- function(input){
  return(grepl("\\b((j|J)an(uary)?|(f|F)eb((ruary)|(rero))??|(m|M)ar((ch)|(zo))??|(a|A)pr(il)?|(m|M)ay(o)?|(j|J)un((e)|(io))?|(j|J)ul((y)|(io))?|(a|A)ug(ust)?|(s|S)ep((tember)|(tiembre))?|(o|O)ct((ober)|(ubre))?|(n|N)ov((ember)|(iembre))?|(d|D)ec(ember)?|(e|E)ne(ro)?|(a|A)br(il)?|(a|A)go(sto)?|(d|D)ic(iembre)?)\\b", input))
}
source("detectMetaParams_base.R")

## Empiezan en...

start_upper <- function(input){
  return(grepl("^[[:upper:]]", input))
}

start_lower <- function(input){
  return(grepl("^[[:lower:]]", input))
}

start_digit <- function(input){
  return(grepl("^[[:digit:]]", input))
}

## Acaban en...

end_upper <- function(input){
  return(grepl("[[:upper:]]$", input))
}

end_lower <- function(input){
  return(grepl("[[:lower:]]$", input))
}

end_digit <- function(input){
  return(grepl("[[:digit:]]$", input))
}

## Contienen...

has_uppers <- function(input){
  return(grepl("[[:upper:]]", input))
}

has_lowers <- function(input){
  return(grepl("[[:lower:]]", input))
}

has_blanks <- function(input){
  return(grepl("[[:blank:]]", input))
}

has_punctuation <- function(input){
  return(grepl("[[:punct:]]", input))
}

has_numbers <- function(input){
  return(grepl("\\d", input))
}

has_slash <- function(input){
  return(base_has_X(input, "/"))
}

has_dash <- function(input){
  return(base_has_X(input, "-"))
}

has_dot <- function(input){
  return(base_has_X(input, "[.]"))
}


## Es ...

is_NA <- function(input){
  return(is.na(input))
}

is_empty <- function(input){
  empty<-input==""
  if(is.na(empty)) return(FALSE)
  else return(empty)
}

is_onlyNumeric <- function(input){
  return(is.numeric(input))
}

is_onlyAlphabetic <- function(input){
  return(grepl("[:alpha:]$", input))
}

is_onlyPunctuation <- function(input){
  return(grepl("[\\.,_'#@!?\\-]$", input))
}

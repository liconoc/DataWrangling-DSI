source("detectMetaParams_base.R")

## Punctiation

has_colon <- function(input){
  return(base_has_X(input, ":"))
}

has_2colon <- function(input){
  if(!is.na(input) && str_count(input, ":")==2) {
    return(TRUE)
  }else {
    return(FALSE)
  }
}

## Structure

has_4digits <- function(input){
  return(grepl("^\\D*(?:\\d\\D*){4}$", input))
}

has_2digits <- function(input){
  return(grepl("^\\D*(?:\\d\\D*){2}$", input))
}

has_hoursWords <- function(input){
  return(grepl("\\b.*\\d+.*(AM|am|PM|pm|UTC|WET|CST|EST|HST|h)\\b", input))
}

has_hourStructure <- function(input){
  return(grepl("\\b\\d{1,2}(:)\\d{1,2}((:)\\d{1,2})?\\b", input))
}

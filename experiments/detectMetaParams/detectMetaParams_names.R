source("detectMetaParams_base.R")

## Estructura

has_capitalAndDot <- function(input){
  return(grepl("([A-Z]+\\.)+", input))
}

has_lettersAndDot <- function(input){
  return(grepl("([A-z]+\\.)+", input))
}

has_wordAndComma <- function(input){
  return(grepl("\\w+(,)+", input))
}

has_only1Word <- function(input){
  return(!is.na(input) & sapply(gregexpr("[[:alpha:]]+", input), function(x) sum(x > 0))==1)
}

has_only2Words <- function(input){
  return(!is.na(input) & sapply(gregexpr("[[:alpha:]]+", input), function(x) sum(x > 0))==2)
}

has_only3Words <- function(input){
  return(!is.na(input) & sapply(gregexpr("[[:alpha:]]+", input), function(x) sum(x > 0))==3)
}

has_wordsJointByDash <- function(input){
  return(grepl("\\w+(-)\\w+", input))
}

## Content

has_courtesyTitles <- function(input){
  return(grepl("\\b(?:Dr|(P|p)(h|H)(d|D)|Mr|Mr|Mrs|Ms|Dr|Miss|Sir|Madam|Sr|Sra|Srta|Dra|Jr)\\.*\\b", input))
}







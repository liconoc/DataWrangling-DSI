source("detectMetaParams_base.R")

## Punctuation
has_at <- function(input){
  return(base_has_X(input, "@"))
}

has_1at <- function(input){
  if(!is.na(input) && str_count(input, "@")==1) {
    return(TRUE)
  }else {
    return(FALSE)
  }
}

## Structure

is_emailStructure <- function(input){
  return(grepl("^[[:alnum:].-_]+@[[:alnum:].-]+$", input))
}

end_dotAndWord <- function(input){
  return(grepl("\\b.*[.]\\w+\\b", input))
}


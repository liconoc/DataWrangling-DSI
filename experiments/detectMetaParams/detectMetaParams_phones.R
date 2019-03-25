source("detectMetaParams_base.R")

## Content

has_numbersInsideParenthesis <- function(input){
  return(grepl(".*\\(((\\+)*[\\d]+)\\).*", input))
}

has_plus <- function(input){
  return(grepl("\\+", input))
}


## Structure

has_9OrMoredigits <- function(input){
  return(grepl("(\\d.*){9}", input))
}

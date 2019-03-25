
source("detectMetaParams_base.R")

## Estructura


has_unitsSystem <- function(input){
  return(grepl("\\b(?:Length|Mass|Volume|Electricity|Temperature|Time)\\.*\\b", input))
}
if (!require("dplyr")) {install.packages("dplyr",dependencies=TRUE); library("dplyr")}
if (!require("stringr")) {install.packages("stringr",dependencies=TRUE); library("stringr")}

setwd("F:/Dropbox/Dropbox/MagicWrangler_2017-2018/IJCAI2018v3/code/detectMetaParams")

## Automatizar... poner en carpetas separadas
## Funciones generales
source("detectMetaParams_general.R")
## Funciones especificas de dominios
source("detectMetaParams_dates.R")
source("detectMetaParams_names.R")
source("detectMetaParams_phones.R")
source("detectMetaParams_emails.R")
source("detectMetaParams_words.R")
source("detectMetaParams_hours.R")
source("detectMetaParams_units.R")

## Lista de las funciones
functions <- lsf.str()

## Función para detectar las metacaracterísticas (Hay que añadirlas todas)
## Mejorar para que se añadan las funciones solas?
metaParams <- function(input){
  
  results<-c()

  for(f in 1:length(functions)){
    #Aplico todas las funciones menos las de base
    if(!startsWith(as.character(functions[f]), "base")) {
      ##print(as.character(functions[f]))
      ##print(lapply(input,functions[f])[[1]])
      results<-c(results,lapply(input,functions[f])[[1]])
    }
  }
  
  return(results)
}

##Cargar un dataset/ejemplo
setwd("F:/Dropbox/Dropbox/MagicWrangler_2017-2018/IJCAI2018v3/datasets")
datasets<-list.files("examples")

for(file in datasets)
{
  ds<-read.csv(paste("examples/",file,sep = ""))
  ds$functions<-NULL
  
  ## Generar dataset de metacaracteristicas
  ds_metaParams <- data.frame()
  
  ## Automatizar para un dataset/ejemplo, input1/input2/.../inputn/output
  inputs<-ncol(ds)-2
  for(i in 1:nrow(ds)){
    data_row<-c(inputs)
    for(j in 1:(inputs+1))
    {
      data_row<-c(data_row, metaParams(as.character(ds[i,j])))
    }
    
    data_row<-c(data_row, as.character(ds[i,"domain"]))
    
    if(i==1){ 
      ds_metaParams <-rbind(ds_metaParams, data_row)
      ds_metaParams <- mutate_all(ds_metaParams,as.character)
    }else{
      ds_metaParams <-rbind(ds_metaParams, data_row)
    }
  }
  
  ## Incluir el dominio real en el dataset
  names<-c()
  for(f in 1:length(functions)){
    #Aplico todas las funciones menos las de base
    if(!startsWith(as.character(functions[f]), "base")) {
      names<-c(names, as.character(functions[f]))
    }
  }
  
  cols<-c("inputs")
  for(i in 1:(inputs+1))
  {
    cols<-c(cols,paste(names,colnames(ds)[i],sep = "_"))
  }
  cols<-c(cols,"domain")
  
  colnames(ds_metaParams) <- cols
  
  write.csv(ds_metaParams, paste("examples-metaparams/metaparams_",file,sep = ""), row.names = FALSE)
  
}

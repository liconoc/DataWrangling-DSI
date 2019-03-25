setwd("F:/Dropbox/Dropbox/MagicWrangler_2017-2018/IJCAI2018v3")

##Las funciones ||, && y ++ de haskell dan problemas en R. Las he quitado.

library(plyr)
datasets<-list.files("datasets/examples")

##Vectores con todas las funciones
functions<-read.csv("datasets/functions/functions.csv")
functions_original<-read.csv("datasets/functions/original.csv")
functions_constants<-read.csv("datasets/functions/constants.csv")
functions_freetext<-read.csv("datasets/functions/freetext.csv")
functions_freetext<-rbind(functions_freetext,functions_original,functions_constants)

functions_dates<-read.csv("datasets/functions/dates.csv")
functions_dates<-rbind(functions_dates,functions_original,functions_constants, functions_freetext)
functions_names<-read.csv("datasets/functions/names.csv")
functions_names<-rbind(functions_names,functions_original,functions_constants, functions_freetext)
functions_emails<-read.csv("datasets/functions/emails.csv")
functions_emails<-rbind(functions_emails,functions_original,functions_constants, functions_freetext)
functions_phones<-read.csv("datasets/functions/phones.csv")
functions_phones<-rbind(functions_phones,functions_original,functions_constants, functions_freetext)
functions_times<-read.csv("datasets/functions/times.csv")
functions_times<-rbind(functions_times,functions_original,functions_constants, functions_freetext)
functions_units<-read.csv("datasets/functions/units.csv")
functions_units<-rbind(functions_units,functions_original,functions_constants)



##dataset con *una* solución que resuelve el problema en Haskell. Si existe.
for(file in datasets){
  ds<-read.csv(paste("datasets/examples/", file,sep = ""))
  
  ### 1. Sacar para cada  problema, las funciones que se aplican
  ds_functions_final <- data.frame()
  ds_functions_domain <- data.frame()
  
  ##Metaparametros de los problemas del dataset
  ds_metaparams<-read.csv(paste("datasets/examples-metaparams/metaparams_", file,sep = ""))
  
  #Para cada ejemplo
  for(p in 1:dim(ds)[1]){
    #Saco su dominio
    domain<-as.character(ds[p,"domain"])
    if(substr(domain,1,1)==" "){domain<-substring(domain, 2)}
    row<-c()
    row_domain<-c()
    
    #Si tiene solución, para cada función existente
    for(f in 1:dim(functions)[1]){
      primitive<-as.character(functions[f,1])
      #Si la función está en la solucion, añado uno, si no, cero
      if(primitive %in% strsplit(gsub("\\(|\\)","",as.character(ds[p,"functions"]))," ")[[1]]){
        row<-c(row,1)
        value<-1
      }else{
        row<-c(row,0)
        value<-0
      }
    }
    
    for(f in 1:dim(get(paste("functions",domain,sep = "_")))[1]){
      primitive<-as.character(get(paste("functions",domain,sep = "_"))[f,1])
      if(primitive %in% strsplit(gsub("\\(|\\)","",as.character(ds[p,"functions"]))," ")[[1]]){
        row_domain<-c(row_domain,1)
      }else{
        row_domain<-c(row_domain,0)
      }
    }
    
    
    row<-t(as.data.frame(row))
    colnames(row) <- as.character(functions$function.)
    row_domain<-t(as.data.frame(row_domain))
    colnames(row_domain) <- as.character(get(paste("functions",domain,sep = "_"))[,"function."])
    
    ds_functions_final<-rbind(ds_functions_final, row)
    ds_functions_domain<-rbind(ds_functions_domain, row_domain)
    
  }

  write.csv(ds_functions_final, file = paste("datasets/examples-functions/functions_",file,sep = ""), row.names = FALSE)
  write.csv(ds_functions_domain, file = paste("datasets/examples-functions/domain_functions_",file,sep = ""), row.names = FALSE)
  
}


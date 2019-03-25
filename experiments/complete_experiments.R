
setwd("~/Dropbox/MagicWrangler_2018-2019/ICCS 2019")

#Libraries
library("caret")
library("dplyr")

#Datasets de ejemplos del paper de TDE (descargados de Github)
folders<-list.dirs("TDE_SIGMOD_DWRepo/data_TDE/", recursive = FALSE)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

##Modificar esto: hay datasets con uno, dos o tres inputs. No se puede hacer as? bien, por ahra lo dejo.
#two_inputs<-c("emails-1.csv", "emails-2.csv","emails-3.csv", "names-6.csv", "names-7.csv", "names-8.csv", "names-9.csv", "phones-5.csv", "phones-6.csv","phones-9.csv","phones-10.csv","phones-11.csv","phones-12.csv", "phones-13.csv","phones-14.csv","phones-15.csv","phones-16.csv", "phones-17.csv","phones-18.csv", "times-13.csv", "times-18.csv","times-14.csv", "times-9.csv","times-10.csv","times-17.csv", "units-7.csv", "units-8.csv")
#three_inputs<-c("units-10.csv","units-9.csv", "times-21.csv", "times-22.csv", "times-23.csv", "times-24.csv")

#solo ranking porque no tengo todos los dominios
#strategies<-c("default","freetext","global","real_domain","pred_domain","ranking","dom_ranking")
strategy<-"ranking"

## Creamos la tabla de resultados
## dataset = identifica al archivo de datos
## total_rows = numero de ejemplos totales en el archivo (numero de filas)
## total_inputs = numero de inputs en el archivo, total de columnas menos el output esperado
## primitive_set = set de primitivas utilizado para la ejecución
## num_examples = numero de ejemplos usados para buscar la solución. Menor que el número de filas total.
## function = función que se ha utilizado para obtener la solución (la primera que devuelva MH). NA si no hay solución.
## correct_outputs = numero de ejemplos que se han resuelto correctamente
## incorrect_outputs = numero de ejemplos que no se han resuelto correctamente
## accuracy = porcentaje de acierto
## time = tiempo de ejecucion de MH
options(stringsAsFactors = FALSE)
results <- matrix(ncol = 17, nrow=0)
results<-as.data.frame(results, stringsAsFactors=FALSE)

functions_type<-read.csv("experimentos/functions/functions_types.csv", sep = ",")

for(folder in folders){
  
  datasets<-list.files(folder, pattern = "*.txt")
  
  for(dataset in datasets){
    print(paste("Procesando: ", dataset, sep = ""))
    #dataset original, usado para test
    ds<-read.csv(paste(folder,"/",dataset, sep = ""), sep = ";", skip = 1)
    #metaparams del dataset
    ds_meta<-read.csv(paste(folder,"/metaparams/metaparams_",dataset, sep = ""))
    
    example<-ds_meta[1,]
    example$inputs<-NULL
    example$X<-NULL
    example$X.1<-NULL
    
    ### Detectar las funciones con global
    models_functions<-list.files("experimentos/models/function_prediction")
    
    predicted_function_all_probs<-data.frame()
    modelos<-c()
    
    for(modelo in models_functions){
      model_function<-readRDS(paste("experimentos/models/function_prediction/",modelo, sep = ""))
      pred_function<-predict(model_function, example, type = "prob")
      predicted_function_all_probs<-rbind(predicted_function_all_probs,pred_function)
      modelos<-c(modelos,strsplit(strsplit(modelo,"_")[[1]][2],"[.]")[[1]][1])
    }
    
    predicted_function_all_probs<-cbind(modelos,predicted_function_all_probs)
    predicted_function_all_probs<-predicted_function_all_probs[order(predicted_function_all_probs$X1, decreasing = TRUE),]
    predicted_function_probs_X1<-filter(predicted_function_all_probs, X1>0)
    
    ###############################
    #### MagicHaskeller ###########
    ###############################
    
      num_inputs <- 1
      ds$input2<-NULL
      ds$input3<-NULL
      num_cols <- num_inputs+1
      num_rows <- nrow(ds)

      #Lo hago con un solo ejemplo. La pr?xima vez eber?a coger m?s de uno.
      #Escribo el predicado de entrada
      #si hab?aa un ejemplo previo lo cargo y aádo &&
      printer = file("/home/liconoc/Dropbox/MagicWrangler_2017-2018/NIPS2018/code/MH/in_MH.txt","r")
      predicate_in <- readLines(con = printer)
      close(printer)
      if(num_inputs==1){
        predicate <- paste('f ','"',ds[1,1], '"', " == ", '"', substring(as.character(ds[1, 2]),1), '"', sep="")
      }else{
        predicate<-"f "
        for(col in 1:num_inputs){
          if(col>1){
            predicate<-paste(predicate,'"',substring(as.character(ds[1,col]),2), '"',sep="")
          }else{
            predicate<-paste(predicate,'"',ds[1,col], '"',sep="")
          }
          
        }
        predicate<-paste(predicate, " == ", '"', substring(as.character(ds[1, c(num_cols)]),2), '"', sep="")
      }
      
      printer = file("/home/liconoc/Dropbox/MagicWrangler_2017-2018/NIPS2018/code/MH/in_MH.txt","w")
      writeLines(predicate, con=printer, sep="")
      close(printer)
      
      #Ejecuto MH con los ejemplos de in
      ## Cuando haya memoria, ir aumentando la d
      d=8
      
      
      primitive="ddsbk"
      #crear ddsbk (b=12 max, por ahora)
      printer = file("/home/liconoc/Dropbox/MagicWrangler_2017-2018/NIPS2018/code/MH/primitives/ddsbk","w")
      ## ranking
      for(f in predicted_function_probs_X1[,1]){
          writeLines(paste("0",paste(functions_type[which(functions_type$functions==f),], collapse = "::"), "\n", sep = " "), con=printer, sep="")
       }
      b=nrow(predicted_function_probs_X1)
      close(printer)
      
      print(paste(1, " ejemplo(s) con el set de primitivas ", primitive, " y d=", d, sep = ""))  
      
      cmd <- paste("MagicHaskeller --individual=/home/liconoc/Dropbox/MagicWrangler_2017-2018/NIPS2018/code/MH/primitives/", primitive," -i -d ", d," < /home/liconoc/Dropbox/MagicWrangler_2017-2018/NIPS2018/code/MH/in_MH.txt 1> /home/liconoc/Dropbox/MagicWrangler_2017-2018/NIPS2018/code/MH/out_MH.txt 2> /home/liconoc/Dropbox/MagicWrangler_2017-2018/NIPS2018/code/MH/morralla.txt", sep = "")
      
      system(cmd)
      
      #Leo del archivo de resultado
      printer <- file("/home/liconoc/Dropbox/MagicWrangler_2017-2018/NIPS2018/code/MH/out_MH.txt","r")
      out <- readLines(con = printer)
      close(printer)
      
      
      #Si hay solucion, pero puede estar el archivo vaco ??con una l??nea en blanco
      if(length(out)>1){
        #Busco la primera linea que no sea ""
        function_out = ""
        for(f in 2:length(out)){
          if(function_out == "" && out[f]!="" && !is.na(out[f])){
            function_out <- out[f]
          }
        }
        
        if(function_out==""){ #Hay linea en blanco
          results<-rbind(results, c(dataset, strategy, "fixed_b", b, d, function_out, -1, num_rows-1, 0, 0))
          colnames(results)<-c("dataset","strategy","type_DDSBK","b","d","inferred_solution", "n","n_correct","accuracy")
          
          write.csv(results, file = paste("/home/liconoc/Dropbox/MagicWrangler_2018-2019/ICCS 2019/results/results-confunciones_d8.csv", sep = ""))
          
          
        }else{ #todo ha ido bien
          ##Comprobar si es sin \a o no, o si hay más de un parámetro
          start_function<-7
          if(substr(function_out,1,1)=="\\"){ ##Hay solución y empieza por \a
            if(num_inputs==1){ #solo hay un input
              function_out<-substr(function_out,start_function,nchar(function_out))
            }else{ ## hay más de un input a b c...
              function_out<-substr(function_out,start_function+(2*(num_inputs-1)),nchar(function_out))
            }
          }else{ ##la solución es única y aplicable al ejemplo, hay que añadir la letras/s porque en las soluciones únicas no hay
            input_letters<-c()
            for(letter in 1:num_inputs){
              input_letters<-c(input_letters,letters[letter])
            }
            function_out<-paste(function_out,input_letters,sep = " ")
          }
          
          correctos=0
          
          ###Por cada nuevo ejemplo, hay que aplicar la función
          ptm <- proc.time()
          for(row in 2:num_rows){
            
            result<-function_out
            ##Ponemos el ejemplo en el resultado
            for(letter in 1:num_inputs){
              if(letter==1){
                result<-gsub(paste(" ",letters[letter],sep = ""),paste(" \"", ds[row,letter],"\"", sep = ""),result)
              }else{
                result<-gsub(paste(" ",letters[letter],sep = ""),paste(" \"", substring(ds[row,letter],2),"\"",sep = ""),result)
              }
            }
            ## in_H debe incluir las librerias necesarias
            printer = file("/home/liconoc/Dropbox/MagicWrangler_2017-2018/NIPS2018/code/MH/in_H.txt","w")
            writeLines(paste(":load /home/liconoc/Dropbox/MagicWrangler_2017-2018/NIPS2018/datasets/functions/functions.hs", "\n", sep = ""), con=printer, sep="")
            writeLines(result, con=printer, sep="")
            close(printer)
            
            #Ejecutamos haskell
            
            cmd <- paste("ghci < /home/liconoc/Dropbox/MagicWrangler_2017-2018/NIPS2018/code/MH/in_H.txt 1> /home/liconoc/Dropbox/MagicWrangler_2017-2018/NIPS2018/code/MH/out_H.txt 2> /home/liconoc/Dropbox/MagicWrangler_2017-2018/NIPS2018/code/MH/morralla.txt", paste="")
            
            system(cmd)
            
            
            printer <- file("/home/liconoc/Dropbox/MagicWrangler_2017-2018/NIPS2018/code/MH/out_H.txt","r")
            out <- readLines(con = printer)
            close(printer)
            
            #Haskell ha ido bien
            if(substr(out[3],1,2)=="Ok"){
              result_row<-gsub("\"","",substring(out[4], 9))
              if(substring(ds[row,num_inputs+1],1,1)==" "){
                if(result_row==substring(ds[row,num_inputs+1],2)){ correctos<-correctos+1 }
              }else {
                if(result_row==substring(ds[row,num_inputs+1],1)){ correctos<-correctos+1 }
              }
            }
            
            
          }# for row (transformaciones)
          t_transformacion <- proc.time() - ptm
          
          results<-rbind(results, c(dataset, strategy, "fixed_b", b, d, ds$functions[1], function_out, num_rows-1, correctos, correctos/(num_rows-1)))
          colnames(results)<-c("dataset","strategy","type_DDSBK","b","d","inferred_solution", "n","n_correct","accuracy")
          
          write.csv(results, file = paste("/home/liconoc/Dropbox/MagicWrangler_2018-2019/ICCS 2019/results/results-confunciones_d8.csv", sep = ""))
        }
        
        
        
      }else{ #No hay solución
        results<-rbind(results, c(dataset, strategy, "fixed_b", b, d, "-", -1, 0, 0, 0))
        colnames(results)<-c("dataset","strategy","type_DDSBK","b","d","inferred_solution", "n","n_correct","accuracy")
        
        write.csv(results, file = paste("/home/liconoc/Dropbox/MagicWrangler_2018-2019/ICCS 2019/results/results-confunciones_d8.csv", sep = ""))
      }
      
      
      
  }
}


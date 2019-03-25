library(mldr)
library(RWeka)
library("caret")
library(randomForest)
### Sigo el tutorial del paper: Working with multi-lablel datasets in R: The mldr Package

setwd("F:/Dropbox/Dropbox/MagicWrangler_2017-2018/IJCAI2018v3")

datasets <- list.files("datasets/examples")

###################################
######### CLASSIFICATION ##########
###################################

#Genero un modelo por dataset, incluyendo a todos menos a él, con todos los datasets o solo los deominio
for(file in datasets){
  print(file)
  #Generamos conjunto de train para ese
  train<-datasets[!(datasets %in% file)]
  ds_functions<-data.frame()
  ds_metaparams<-data.frame()
  for(dataset in train){
    dataset_functions<-read.csv(paste("datasets/examples-functions/functions_",dataset,sep = ""))
    ds_functions<-rbind(ds_functions, dataset_functions)
    dataset_metaparams<-read.csv(paste("datasets/examples-metaparams/metaparams_",dataset,sep = ""))
    ds_metaparams<-rbind(ds_metaparams, dataset_metaparams)
  }
  ds_metaparams$inputs<-NULL
  ds_metaparams$domain<-NULL
  
  ds<-cbind(ds_metaparams,ds_functions)
  
  #Genero un modelo por funcion
  ds_ml <- mldr_from_dataframe(ds, labelIndices = c((dim(ds_metaparams)[2]+1):dim(ds)[2]), name = "ds")
  ########## 1. Binary Relevance (BR): Transforms the original multilabel dataset into several binary datasets.
  ## transforma a un problema binario. Ahora se puede usar cualquier clasificador
  ds_ml_br<-mldr_transform(ds_ml, type = "BR")
  ####Caret
  train_control<- trainControl(method="cv", number=10, savePredictions = TRUE,  classProbs = TRUE)
  
  
  for(i in 1:length(ds_ml_br)){
    
    label<-colnames(ds_ml_br[[i]][dim(ds_ml_br[[1]])[2]])
    print(label)
    colnames(ds_ml_br[[i]])[ncol(ds_ml_br[[i]])] <- "Class"
    
    if(sum(ds_ml_br[[i]]$Class)>0){
      #ds_ml_br[[i]]$Class<-as.factor(ds_ml_br[[i]]$Class)
      
      ds_ml_br[[i]]$Class<-make.names(ds_ml_br[[i]]$Class)
      ds_ml_br[[i]]$Class<-as.factor(ds_ml_br[[i]]$Class)
      model<-train(Class~., data=ds_ml_br[[i]], trControl=train_control, method="rf")
      saveRDS(model, paste("code/models/modelo_",label,"_",strsplit(file,"[.]")[[1]][1],".rds", sep = ""))
     
      colnames(ds_ml_br[[i]])[ncol(ds_ml_br[[i]])] <- label
     
    }else{
      colnames(ds_ml_br[[i]])[ncol(ds_ml_br[[i]])] <- label
    }
    
    
  }
  
}


install.packages("nametagger")


library(udpipe)
library(stringr)
library(nametagger)
library(xml2)
library(lattice)
library(igraph)
library(ggplot2)
library(ggraph)


model_fr<-function(df_fr,wd){
  if(length(df_fr>0)){
    udmodel <- udpipe_download_model(language = "french")
    udmodel <- udpipe_load_model(file = "french-gsd-ud-2.5-191206.udpipe")
    #Annotate text in dataframe
    x <- udpipe_annotate(udmodel, x = df_fr$texte, doc_id = df_fr$doc_id)
    x <- as.data.frame(x, detailed = TRUE)
    
    #Add NER
    #model_NER <- nametagger_download_model("METTRE LE MODEL FRACAIS ICI", model_dir = wd)
    #ner <- predict(model_NER, x_fr$token)
    #x_en <-cbind(x_fr,ner)
    names(x)
    #Place morphological features as variables in the dataframe
    x <-cbind_morphological(x, term = "feats", which = c("lexical", "inflectional_noun", "inflectional_verb"))
    x_fr <-merge(x, df_fr, by = "doc_id")
    
    return(x_fr)
  }
}

model_en<-function(df_en,wd){
  if(length(df_en)>0){
    udmodel <- udpipe_download_model(language = "english")
    udmodel <- udpipe_load_model(file = "english-ewt-ud-2.5-191206.udpipe")
    #Annotate text in dataframe
    x <- udpipe_annotate(udmodel, x = df_en$texte, doc_id = df_en$doc_id)
    x <- as.data.frame(x, detailed = TRUE)
    
    #Add NER
    #model_NER <- nametagger_download_model("english-conll-140408", model_dir = wd)
    #ner <- predict(model_NER, x$token)
    #x <-cbind(x,ner)
    names(x)
    #Place morphological features as variables in the dataframe
    x <-cbind_morphological(x, term = "feats", which = c("lexical", "inflectional_noun", "inflectional_verb"))
    x_en <-merge(x, df_en, by = "doc_id")
   
    return(x_en)
  }
}

main<-function(){
  #A modifier selon  son repertoire
  wd=setwd("D:/script")
  #getwd()
  df=read.csv("my_data.csv")
 # print(df$texte)
  #suppression de la colonne id_learner en trop
  df=df[,-3]
  #renommage de la colonne id_learner
  colnames(df)[colnames(df) == "id_learner"]<- "doc_id"
  print(colnames(df))
  #print(df$l2[1])
  #nettoyage du texte
  df$texte=gsub("\\[x2","",df$texte)
  df$texte=gsub("\\[x","",df$texte)
  df$texte=gsub("\\[x 2","",df$texte)
  df$texte=gsub("3\\]","",df$texte)
  df$texte=gsub("\\]","",df$texte)
  df$texte=gsub("\\[x4","",df$texte)
  
  df$texte=gsub("\\+"," ",df$texte)
  df$texte=gsub("<","",df$texte)
  df$texte=gsub(">","",df$texte)
  df$texte=gsub("XXX","",df$texte)
  
  df$texte
  df_fr=df[df$l2=="fra 	French",]
  x_fr=model_fr(df_fr,wd)
  print(x_fr)
  df_en=df[df$l2=="eng	English",]
  x_en=model_en(df_en,wd)
 
  #creation de jeu de donnée pour le français
  write.csv(x_fr,"m_data_fr.csv")
  #creation de jeu de donnée pour l'anglais
  write.csv(x_en,"m_data_en.csv")
  
  print("fichiers crÃ©Ã©s")
  }
main()







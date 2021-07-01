

library(httr)
library(tibble)
library(readr)
library(xml2)
library(dplyr)
library(httr)
library(stringi)
library(stringr)
library(data.table)
getwd()
setwd("D:/script")
header<-function(key){
  
  headers = c(
    `accept` = 'application/json'
    #`X-API-KEY` = key
  )
  
  
  return(headers)
}
param<-function(page,limit){
  params = list(
    `page` = page,
    `limit` = limit
  )
}
data<-function(id_collection,nbPage,header,extension){
  #modification de l'identifiant 
  a=gsub("/","%2F",id_collection)
  u=paste('https://api.nakala.fr/collections',a, 'datas', sep='/')
  #initialisation des listes
  lt=list()
  listCsv=list()
  dataFram=list()
  createcsv=list()
  #parcourir les pages
  for (i in 1 : nbPage) {
    #recuperer la liste paginée des données contenues dans la collection
    contenu <- httr::GET(u, httr::add_headers(.headers=header), query = param(toString(i),'100'))
    #recuperation du fichier json retourné par la requete
    get2json<- content(contenu, as = "parsed")
   
    #recupérer la partie data du fichier JSON
    c=getMetasFichier(get2json[['data']],extension)
    d=getFicherCsv(get2json[['data']])
    createcsv=CreateCsv(c,d,extension,i)
    #convertir en les dataframe en liste lors de la separation
    #liste contenant le datadrame des eaf
    lt[i]=list(createcsv[[1]])
    #liste contenant le dataframe des csv
    listCsv[i]=list(createcsv[[2]])
    
    #convertir les listes en dataframe avant de les fusionner
    df <- data.frame(matrix(unlist(createcsv[[1]]), nrow=length(createcsv[[1]]), byrow=TRUE),stringsAsFactors=FALSE)
    df=df%>%
      setNames(c("id_learner","texte"))
    #print(df)
    dfCsv=data.frame(matrix(unlist(createcsv[[2]]), nrow=length(createcsv[[2]]), byrow=TRUE),stringsAsFactors=FALSE)
    dCsv=dfCsv%>%
      setNames(c("id_learner","n_years_L2","sex","birth_country","previous_country1_time","previous_country1",
                 "previous_country2_time","previous_country2","previous_country3_time","previous_country3",
                 "current_country","education","l1","l1_variety","l2","l2_autoevaluation_written","l3",
                 "l3_autoevaluation_oral","l3_autoevaluation_written","date_recording","place_recording",
                 "birth_year","occupation","duration_conv","duration_read","CECRecrit_Etendue",
                 "CECRecrit_Coherence","CECRecrit_Correction","CECRecrit_Description","CECRecrit_Argumentation",
                 "CECRecrit_Competence_generale_CECRL","CECRoral_Etendue","CECRoral_Correction","CECRoral_Aisance",
                 "CECRoral_Interaction","CECRoral_Coherence","CECRoral_Phonologie","CECRoral_Niveau_general_CECRL"))
    
    
    #fusion des deux dataframes obtenu plus haut mais l'on les mets dans une liste
    #il faudra par la suite trouver un moyenne de transformer la liste de dataframe en un dataframe
    dataFram[i]=list(cbind(df,dCsv))
  }
  
  #ajouter un dataFram avec le numero de la page s'il y a de nouvelle page
  #dataFrame[[5]]
  dt=rbind(dataFram[[1]],dataFram[[2]],dataFram[[3]],dataFram[[4]])
  write_delim(dt,"my_data.csv", delim = ",")
  print('Fichier créé dans le repertoire:')
  print(getwd())
}
getMetasFichier<-function(dt,ext){
  #initilisation liste et elements de parcours
  sh=list()
  uri=list()
  nom=list()
  langue=list()
  k=1
  c=1
  #print(dt[[1]]$lastPage)
  #rechercher tout les fichiers qui ont l'extension passé en parametre
  while (c <= length(dt)) {
    for(i in 1: length(dt)){
      for(j in 1: length(dt[[i]]$files)){
        #les metas données des fichiers supprimé apparaissent en status delete et ca pose problemme 
        #d'ou cette condition
        if (dt[[i]]$status=="published") {
          if(dt[[i]]$files[[j]]$extension==ext){
            nom[k]=dt[[i]]$files[[j]]$name
            sh[k]=dt[[i]]$files[[j]]$sha1
            langue[k]=dt[[i]]$metas[[1]]$lang
            uri[k]=gsub("https://doi.org/","",dt[[i]]$uri)
            k=k+1
          }
        }
        
      }
      c=c+1
    }
  }
  #creation d'une liste contant les metadonnées 
  lt=list(nom,sh,uri, langue)
  return(lt)
}
getFicherCsv<-function(dt){
 
  sh=list()
  uri=list()
  nom=list()
  k=1
  c=1
  #print(dt[[1]]$lastPage)
  #rechercher tout les fichiers qui ont l'extension passé en parametre
  while (c <= length(dt)) {
    for(i in 1: length(dt)){
      for(j in 1: length(dt[[i]]$files)){
        #les metas données des fichiers supprimé apparaissent en status delete et ca pose problemme 
        #d'ou cette condition
        if (dt[[i]]$status=="published") {
          if(dt[[i]]$files[[j]]$extension=="csv"){
            nom[k]=dt[[i]]$files[[j]]$name
            sh[k]=dt[[i]]$files[[j]]$sha1
            uri[k]=gsub("https://doi.org/","",dt[[i]]$uri)
            k=k+1
          }
        }
        
      }
      c=c+1
    }
  }
  #creation d'une liste contant les metadonnées 
  lt=list(nom,sh,uri)
  print(lt)
  return(lt)
}
CreateCsv<- function(c,d,ext,i){
  
  nom=c[[1]]
  #print(length(nom))
  sh=c[[2]]
  id_data=c[[3]]
  langue=c[[4]]
  k=1
  lt=list()
  nomCsv=d[[1]]
  shaCsv=d[[2]]
  idCsv=d[[3]]
  listCsv=list()
  #former un dataframe à deux colonnes (nom du fichier et le texte contenu dans le fichier )
  while (k<=length(nom)) {
    URL=paste('https://api.nakala.fr/data',id_data[[k]], sh[[k]], sep='/')
    #URL="https://apitest.nakala.fr/data/10.34847/nkl.801e4wy5/0d81dbaa8bc6cf2679f8506d589e7371649cc6e2"
    urlCsv=paste('https://api.nakala.fr/data',idCsv[[k]], shaCsv[[k]], sep='/')
    #lecture des eaf
    if(ext=="eaf"){
      #print(URL)  
      #print(langue[k])
      fichier=read_xml(URL)
      if (langue[k]=="fr") {
        fl=str_extract(fichier,"TIER_ID=.*FLE")
        fle=gsub('TIER_ID=','',fl)
        #recup tout ce qui vient apres \"
        fle=str_extract(fle,'[^\"].+')
        #print(fle)
        text=xml_find_all(fichier,paste(".//TIER[@TIER_ID=",fle,"]/ANNOTATION/ALIGNABLE_ANNOTATION/ANNOTATION_VALUE",sep = "'") ) %>%  xml_text() 
        
      }else{
        al=str_extract(fichier,"TIER_ID=.*ALE")
        ale=gsub('TIER_ID=','',al)
        #recup tout ce qui vient apres \"
        ale=str_extract(ale,'[^\"].+')
        print(ale)
        text=xml_find_all(fichier,paste(".//TIER[@TIER_ID=",ale,"]/ANNOTATION/ALIGNABLE_ANNOTATION/ANNOTATION_VALUE",sep = "'")) %>%  xml_text() 
        
      }
      #lecture des txt
    }else{
      text=read_file(URL)
      
    }
    
    #creation des df
    nm=gsub(paste(".",ext,sep=""),"",nom[[k]])
    df <- tibble(
      id_learner =nm,
      texte = text,
    )
    
    df_sent <- summarize(group_by(df,id_learner), texte = paste(texte, collapse = ""))
    lt[k] <- split(df_sent, seq(nrow(df_sent)))
    
    #recup du csv contenu contenu dans la donnée 
    r= fread(urlCsv)
    
    #print(length(r))
    #certains csv n'ont pas les colonnes CECR, il faut donc les ajouter 
    if(length(r)>26){
      #creation du data frame avec le csv
      rdCsv<- tibble(
        id_learner=r$id_learner,
        n_years_L2=r$n_years_L2,
        sex=r$sex,birth_country=r$birth_country,previous_country1_time=r$previous_country1_time,previous_country1=r$previous_country1,
        previous_country2_time=r$previous_country2_time,previous_country2=r$previous_country2,previous_country3_time=r$previous_country3_time,
        previous_country3=r$previous_country3,
        current_country=r$current_country,education=r$education,l1=r$l1,l1_variety=r$l1_variety,l2=r$l2,
        l2_autoevaluation_written=r$l2_autoevaluation_written,l3=r$l3,
        l3_autoevaluation_oral=r$l3_autoevaluation_oral,l3_autoevaluation_written=r$l3_autoevaluation_written,
        date_recording=r$date_recording,place_recording=r$place_recording,
        birth_year=r$birth_year,occupation=r$occupation,duration_conv=r$duration_conv,
        duration_read=r$duration_read,CECRecrit_Etendue=r$CECRecrit_Etendue, CECRecrit_Coherence=r$CECRecrit_Coherence,
        CECRecrit_Correction=r$CECRecrit_Correction,CECRecrit_Description=r$CECRecrit_Description,CECRecrit_Argumentation=r$CECRecrit_Argumentation,
        CECRecrit_Competence_generale_CECRL=r$CECRecrit_Competence_generale_CECRL,CECRoral_Etendue=r$CECRoral_Etendue,
        CECRoral_Correction=r$CECRoral_Correction,CECRoral_Aisance=r$CECRoral_Aisance,
        CECRoral_Interaction=r$CECRoral_Interaction,CECRoral_Coherence=r$CECRoral_Coherence,
        CECRoral_Phonologie=r$CECRoral_Phonologie,CECRoral_Niveau_general_CECRL=r$CECRoral_Niveau_general_CECRL
      )
      
    }else{
      
      rdCsv<- tibble(
        id_learner=r$id_learner,
        n_years_L2=r$n_years_L2,
        sex=r$sex,birth_country=r$birth_country,previous_country1_time=r$previous_country1_time,previous_country1=r$previous_country1,
        previous_country2_time=r$previous_country2_time,previous_country2=r$previous_country2,previous_country3_time=r$previous_country3_time,
        previous_country3=r$previous_country3,
        current_country=r$current_country,education=r$education,l1=r$l1,l1_variety=r$l1_variety,l2=r$l2,
        l2_autoevaluation_written=r$l2_autoevaluation_written,l3=r$l3,
        l3_autoevaluation_oral=r$l3_autoevaluation_oral,l3_autoevaluation_written=r$l3_autoevaluation_written,
        date_recording=r$date_recording,place_recording=r$place_recording,
        birth_year=r$birth_year,occupation=r$occupation,duration_conv=r$duration_conv,
        duration_read=r$duration_read)
      #pour mettre les tableaux aux memes nombre de colonne
      rdCsv['CECRecrit_Etendue'] <- NA
      rdCsv['CECRecrit_Coherence'] <- NA
      rdCsv['CECRecrit_Correction'] <- NA
      rdCsv['CECRecrit_Description'] <- NA
      rdCsv['CECRecrit_Argumentation'] <- NA
      rdCsv['CECRecrit_Competence_generale_CECRL'] <- NA
      rdCsv['CECRoral_Etendue'] <- NA
      rdCsv['CECRoral_Correction'] <- NA
      rdCsv['CECRoral_Aisance'] <- NA
      rdCsv['CECRoral_Interaction'] <- NA
      rdCsv['CECRoral_Coherence'] <- NA
      rdCsv['CECRoral_Phonologie'] <- NA
      rdCsv['CECRoral_Niveau_general_CECRL'] <- NA
      
      
    }
   
    
    #print(rdCsv)
    rd<- summarize(group_by(rdCsv,id_learner,n_years_L2,sex,birth_country,previous_country1_time,previous_country1,
                            previous_country2_time,previous_country2,previous_country3_time,previous_country3,
                            current_country,education,l1,l1_variety,l2,l2_autoevaluation_written,l3,
                            l3_autoevaluation_oral,l3_autoevaluation_written,date_recording,place_recording,
                            birth_year,occupation,duration_conv,duration_read,CECRecrit_Etendue,
                            CECRecrit_Coherence,CECRecrit_Correction,CECRecrit_Description,CECRecrit_Argumentation,
                            CECRecrit_Competence_generale_CECRL,CECRoral_Etendue,CECRoral_Correction,CECRoral_Aisance,
                            CECRoral_Interaction,CECRoral_Coherence,CECRoral_Phonologie,CECRoral_Niveau_general_CECRL))
    listCsv[k]=split(rd, seq(nrow(rd)))
    
    
    
    k=k+1
  }
  
  #retourner un liste contenant deux dataframe
  return(list(lt,listCsv))

  
}
recupPage<-function(id_collection,param,header){
  #modification de l'identifiant 
  a=gsub("/","%2F",id_collection)
  u=paste('https://api.nakala.fr/collections',a, 'datas', sep='/')
  #recuperer la liste paginée des données contenues dans la collection
  contenu <- httr::GET(u, httr::add_headers(.headers=header), query = param)
  #recuperation du fichier json retourné par la requete
  get2json<- content(contenu, as = "parsed")
 
  #la partie data du tableau json
  return(get2json[['lastPage']])
  
}
main<-function(){
  #key=as.character(readline("Entrer la clé de connexion  "))
  id_collection=as.character(readline("Entrer l'identifiant de la collection  "))
  extension=as.character(readline("Entrer l'extension des fichiers que vous voulez avoir  "))
  #On cherche le nombre total de page avec 100 resultats par parge 
  param=param('1','100')
  nbPage=recupPage(id_collection,param,header(key))
  #passer le nmbrer de page et apres on pourra parcourir les pages
  data(id_collection,nbPage,header(key),extension)
  
}

main()

11280/4caeaf9c
eaf


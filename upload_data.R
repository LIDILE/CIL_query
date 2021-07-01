install.packages("httr")
install.packages("rjson")
install.packages("jsonlite")
install.packages("fs")
install.packages("stringr")
#setwd("D:/test")

library(httr)
library(rjson)
library(jsonlite)
library(stringr)
library(fs)


#verifier l'environnement de travail
getwd()
#se mettre dans l'environnement dans lequel se trouvent les fichiers à uploader           
setwd("D:/test")
head <- function(key){
  headers = c(
    `accept` = 'application/json',
    `X-API-KEY` = key,
    `Content-Type` = 'multipart/form-data'
  )
  
  return(headers)
}
hder <- function(key){
  headers = c(
    `accept` = 'application/json',
    `X-API-KEY` = key,
    `Content-Type` = 'application/json'
  )
  
  return(headers)
}
extract<- function(file){
  j=1
  choix=list('file'=list())
  #file donne le chemin d'acces au fichier 
  #retrait du nom du fichier ce pattern permet d'avoir quelque de la forme fre_ale_te_88_f_21_write.pdf 
  #parttern à modifier si besoin
  a=gsub("(([A-Z]|[a-z])+_)+(([0-9][0-9])|XX|xx)_([a-z]+_)[0-9][0-9]_([A-Z]*[a-z]+)[[:punct:]][[:alnum:]]+","",file)
  setwd(a[1])
  getwd()
  #chargement de fichiers à uploader
  for(k in 1:length(file)){
    
    #chargement du fichier en recuperant le nom du fichier contenu dans le chemin d'acces. pattern à modifier si besoin
    name= upload_file(str_extract(file[k], "(([A-Z]|[a-z])+_)+(([0-9][0-9])|XX|xx)_([a-z]+_)[0-9][0-9]_([A-Z]*[a-z]+)[[:punct:]][[:alnum:]]+"))
    choix[k]= list('file'= name )
  }
  
  
  names(choix)[names(choix) == ""] <- "file"
  # names(choix)=file
  return(choix)
}
postDataUpload <- function(a,b){
  table= list()
  
  #charger le fichier en ligne en recuperant les sha1
  for (i in 1:(length(a))) {
    
    res= (httr::POST(url = 'https://api.nakala.fr/datas/uploads', httr::add_headers(.headers=b), body = a[i]))
    table[i]= content(res)$sha1
    #print(content(res)$sha1)
    # head('application/json','01234567-89ab-cdef-0123-456789abcdef','multipart/form-data')
  }
  return(table)
}
postData <- function(head,sha,files,langue,collectionID){
  #recuperer le nom du premier fichier pour le mettre en tant que nom de la donnée
  #la donnée contient un ou plusieurs fichiers
  fil=as.character(files[1])
  #pattern de nom de fichier à modifier si besoin
  title=str_extract(fil, "(([A-Z]|[a-z])+_)+(([0-9][0-9])|XX|xx)_([a-z]+_)[0-9][0-9]")
  
  #création de la liste file du table json. 
  lst=list()
  for(i in 1:length(sha)){
    #lorsque  les données ne sont pas sous embargoed executer que la premiere ligne
    lst[i] <- list( c(sha1=sha[i]))
    #lorsque les données sont sous embargoed preciser la date de fin d'embargoed 
    #et decommenter les deux lignes suivantes (commenter celle d'en haut)
    #l <- c( c(sha1=sha[i]),c(embargoed="23/05/2323"))
    #lst[i]<-list(l)
  }
  print(lst)
  #formlisation des champs obligatoire pour associer les metas données aux fichiers
  #uploadés (respectant la syntaxe json de nakala)
  #création de la donnée et association de celle-ci aux fichiers chargés ainsi qu'aux collections
  #auxquelles elle appartient
  tree <-
    list(status="published",
         metas=list(
           list(value= title, lang=langue, typeUri="http://www.w3.org/2001/XMLSchema#string", propertyUri="http://nakala.fr/terms#title"),
           list(value=NULL, lang=langue,propertyUri="http://nakala.fr/terms#creator"),
           list(value="CC-BY-4.0", lang=langue, typeUri="http://www.w3.org/2001/XMLSchema#string", propertyUri="http://nakala.fr/terms#license"),
           list(value="http://purl.org/coar/resource_type/c_1843", lang=langue,typeUri="http://www.w3.org/2001/XMLSchema#anyURI",propertyUri="http://nakala.fr/terms#type"),
           list(value="2021", lang=langue, typeUri="http://www.w3.org/2001/XMLSchema#string", propertyUri="http://nakala.fr/terms#created")
           
         ),
         files=lst,
         collectionsIds=collectionID
         #pour les droits inserer le code ici
         #rights= list(list(id="16e23d70-c9ff-11eb-8c1f-52540084ccd3",role="ROLE_ADMIN"))
    )
  #convertion en json
  data= toJSON(tree,null = "null", auto_unbox = TRUE )
  print(data)
  
  #insertion de la donnée dans Nakala
  rest <- httr::POST(url = 'https://api.nakala.fr/datas', httr::add_headers(.headers=head), body = data)
  #rest
  # content(rest)
  return(rest)
  
}
parcours <- function(filname,key,langue,collectionID){
  
  #k=1
  for (variable in filname) {
    a=dir()
    b=paste(variable,a,sep = "/")
    
    if(is_file(b)){
      print(b)
      ls=extract(b)
      print(ls)
      sha= postDataUpload(ls,head(key))
      post=postData(hder(key),sha,ls,langue,collectionID)
      print(content(post))
      # k=k+1
      #print(choix[k])
    }else{
      #print(b)
      for (i in b ) {
        #print(i)
        setwd(i)
        parcours(i,key,langue,collectionID)
      }  
      
    }
    #print(choix)
    #names(choix)[names(choix) =="" ] <- "file"
  }
  
  
  # names(choix)=file
  #return(choix)
  #return(post)
}
main<- function(){
  if (interactive() && .Platform$OS.type == "windows"){
    flname=choose.dir(getwd(), "Choisir le repertoire source")
  }else{
    #mettre le working directory 
    flname=choose.dir.linux(default = NA, caption = NA)
  }
  
  tst=setwd(flname)
  key=as.character(readline("Entrer la clé de connexion  "))
  #header=head(key)
  #head=hder(key)
  langue=as.character(readline("Entrer la langue (fr pour Français et en pour anglais)  "))
  collectionID=list()
  j=1
  i=1
  while(j>0) {
    col=as.character(readline("Entrer l'identifiant de la collection ('non' pour arreter) "))
    if(col != "non"){
      collectionID[i]=col
      i=i+1
    }else{
      j=-1
    }
    
  }
  
  parcour=parcours(tst,key,langue,collectionID)
}

setwd("D:/Cil")
main()

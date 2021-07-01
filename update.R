library(httr)
library(rjson)
library(jsonlite)
library(stringr)
library(fs)
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

header<-function(key){
  
  headers = c(
    `accept` = 'application/json',
    `X-API-KEY` = key
  )
  
  
  return(headers)
}

param<-function(page,limit){
  params = list(
    `page` = page,
    `limit` = limit
  )
}

extract<- function(file){
  j=1
  choix=list('file'=list())
  #file donne le chemin d'acces au fichier 
  #retrait du nom du fichier ce pattern permet d'avoir quelque de la forme fre_ale_te_88_f_21_write.pdf parttern à modifier si besoin
  a=gsub("(([A-Z]|[a-z])+_)+(([0-9][0-9])|XX|xx)_([a-z]+_)[0-9][0-9]_([A-Z]*[a-z]+)[[:punct:]][[:alnum:]]+","",file)
  setwd(a[1])
  print(a[1])
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

data<-function(id_collection,param,header){
  a=gsub("/","%2F",id_collection)
  u=paste('https://api.nakala.fr/collections',a, 'datas', sep='/')
  contenu <- httr::GET(u, httr::add_headers(.headers=header), query = param)
  #recuperation du fichier json 10.34847%2Fnkl.ecb8kv70
  get2json<- content(contenu, as = "parsed")
  #get2json
  #recherche pour recupperer les sha1 des fichiers qui se terminent par eaf
  return(get2json[['data']])
}

postDataUpdate<- function(sha,id_data,hder){
  #print(sh)
  #print(sha)
  #k=1
  #i=1
 
  lst<-list()
  for (i in 1:length(sha)) {
    a=toString(sha[i])
    #for (j in 1: length(sh)) {
   # b=toString(sh[i])
    #if(b!=a){
      #lorsque  les données ne sont pas sous embargoed executer que la premiere ligne
      lst <- list( c(sha1=sha[i]))
      #lorsque les données sont sous embargoed preciser la date de fin d'embargoed 
      #et decommenter les deux lignes suivantes (commenter celle d'en haut)
      #l <- c( c(sha1=sha[i]),c(embargoed="23/05/2323"))
      #lst[k]<-list(l)
      #k=k+1
      
      
      
      #print(lst)
      #data = '{ "sha1": "3de92a2c19f905d4ed8ed9fc078c06f971d43372", "embargoed": "2021/05/2021"}'
      data= toJSON(lst,pretty = TRUE, auto_unbox = TRUE)
      data=gsub("^\\[","",data)
      data=gsub("\\]$","",data)
      
      #data
      print(data)
      a=gsub("/","%2F",id_data)
      print(a)
      u=paste('https://api.nakala.fr/datas',a, 'files', sep='/')
      print(u)
      #'https://apitest.nakala.fr/datas/10.34847%2Fnkl.6d55796x/files'
      res <- httr::POST(url = u, httr::add_headers(.headers=hder), body = data)
      print(content(res))
      
      #}
    #}
    
    
    
  }
  
  
  
}

postData <- function(hder,sha,files,langue,collectionID){
  #recuperer le nom du premier fichier pour le mettre en tant que nom de la donnée
  #la donnée contient un ou plusieurs fichiers
  fil=as.character(files[1])
  #pattern de nom de fichier à modifier si besoin
  title=str_extract(fil, "([a-z]+_)+((\\d\\d)|XX|xx)(_[a-z]_)+\\d\\d")
  
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
         collectionsIds=list(collectionID)
         #pour les droits inserer le code ici
         #rights= list(list(id="16e23d70-c9ff-11eb-8c1f-52540084ccd3",role="ROLE_ADMIN"))
    )
  #convertion en json
  data= toJSON(tree,null = "null", pretty = TRUE, auto_unbox = TRUE )
  print(data)
  
  #insertion de la donnée dans Nakala
  rest <- httr::POST(url = 'https://apitest.nakala.fr/datas', httr::add_headers(.headers=hder), body = data)
  #rest
  print(content(rest)) 
  return(rest)
  
}
getMetasFichier<-function(dt,files,name,sha,key,collectionID,langue){
 
  a=list()
  sh=list()
  uri=list()
  nm=list()
  l=1
  c=1
  nam=str_extract(name[1], "([a-z]+_)+((\\d\\d)|XX|xx)(_[a-z]_)+\\d\\d")
  print(nam)
  while ( c <= length(dt)) {
    for(i in 1: length(dt)){
      #on recup le nom du premier fichier et on extrait le nom de la donnée 
      a=str_extract(dt[[i]]$files[[1]]$name,"([a-z]+_)+((\\d\\d)|XX|xx)(_[a-z]_)+\\d\\d")
      if(a==nam){
        for(j in 1: length(dt[[i]]$files)){
          
          #print(a)
          #recuperer tous les sha1, les noms des fichiers et idData de donnees qui ont comme nom de donnée 
          #le nom du dossier qu'on a chargé dans nakala
          #Les noms qui ne seront pas trouvés, on les considera comme des données à créer
          
          sh[l]=dt[[i]]$files[[j]]$sha1
          nm[l]=dt[[i]]$files[[j]]$name
          uri[l]=gsub("https://doi.org/","",dt[[i]]$uri)
          l=l+1
          
        }
      }
      c=c+1
    }
  }
  print(sh)
  print("aaaaaaaaaaaaaaaaaaaaaaaaaa")
  print(nm)
  
  if(length(sh)==0){
    #poster tous les fichiers
    postData(hder(key),sha,files,langue,collectionID)
  }else{
    
    
    for (i in 1:length(sha)) {
      cpt=0
      for (j in 1:length(sh)) {
        
        #comparer le sha1 qu'on vient de charger à tous ceux qui sont dans la collection
        #s'il y a une egalité ou pas
        if ((toString(sh[j]) != toString(sha[i])) & (toString(name[i])==toString(nm[j]))) {
          print("nom f de ordi")
          print(toString(name[i]))
          print(toString(sha[i]))
          print("nom f en ligne")
          print(toString(nm[j]))
          print(toString(sh[j]))
          #ajouter l'élement dans la donnée
          postDataUpdate(sha[i],uri[1],hder(key))
          deleFile(uri[1],sh[j],header(key))
          print("remplacement de fichier réussit")
        }
        
        #rechercher l'élement manquant et l'ajouter
        if (toString(name[i])==toString(nm[j])) {
          cpt=1
        }
        
      }
      if(cpt==0){
        postDataUpdate(sha[i],uri[1],hder(key))
        print("Fichier ajouté!")
      }
    }
    
    
  }
 
  
  #lt=list(sh,uri)
  # return(lt)
}


parcours <- function(filname,langue,key,collectionID){
  
  #k=1
  for (variable in filname) {
    a=dir()
    b=paste(variable,a,sep = "/")
    
    if(is_file(b)){
      print(b)
      name=list()
      for (i in 1:length(b)) {
        name[i]=str_extract(b[i], "([a-z]+_)+((\\d\\d)|XX|xx)(_[a-z]_)+\\d\\d_([A-Z]*[a-z]+)[[:punct:]][[:alnum:]]+")
        
      }
      print(name)
      ls=extract(b)
      print(ls)
      dt=data(collectionID,param('1','100'),header(key))
      #print(dt)
      sha= postDataUpload(ls,head(key))
      #print(sha)
      meta=getMetasFichier(dt,ls,name,sha,key,collectionID,langue)
    }else{
      #print(b)
      for (i in b ) {
        #print(i)
        setwd(i)
        parcours(i,langue,key,collectionID)
      }  
      
    }
    #print(choix)
    #names(choix)[names(choix) =="" ] <- "file"
  }
  
  
  # names(choix)=file
  #return(choix)
  #return(post)
}
deleFile<- function(id_data,sh,header){
  id_dat=gsub("/","%2F",id_data)
  print(id_dat)
  u=paste('https://apit.nakala.fr/datas',id_dat, 'files',sh, sep='/')
  print(u)
  res <- httr::DELETE(url = u, httr::add_headers(.headers=header))
  
  print(content(res))
  #return(content(res))
}
main<- function(){
  if (interactive() && .Platform$OS.type == "windows"){
    flname=choose.dir(getwd(), "Choisir le repertoire source")
    tst=setwd(flname)
    key=as.character(readline("Entrer la clé de connexion  "))
    #header=head(key)
    #head=hder(key)
    langue=as.character(readline("Entrer la langue (fr pour Français et en pour anglais)  "))
    collectionID=as.character(readline("Entrer l'identifiant de la collection  "))
    
    
    parcour=parcours(tst,langue,key,collectionID)
  }
}

setwd("D:/Cil")

main()
01234567-89ab-cdef-0123-456789abcdef
fr
10.34847/nkl.fe65o9du


##






b3290ad0-92a1-360a-b9b7-fdde17a5fcca
fr
11280/4caeaf9c

#10.34847/nkl.fcdf4n31
setwd("D:/Test Juvenal")
main()

non

#key='01234567-89ab-cdef-0123-456789abcdef'
#id_collection='10.34847/nkl.fcdf4n31'


#10.34847/nkl.b9f2it56


#10.34847/nkl.d791xos4








































































































#############################################################################################################################################
extract<- function(file){
  j=1
  choix=list('file'=list())
  #file donne le chemin d'acces au fichier 
  #retrait du nom du fichier ce pattern permet d'avoir quelque de la forme fre_ale_te_88_f_21_write.pdf parttern à modifier si besoin
  a=gsub("([a-z]+_)+((\\d\\d)|XX)(_[a-z]_)+\\d\\d(_[a-z]+)[[:punct:]][[:alnum:]]+","",file)
  setwd(a[1])
  getwd()
  #chargement de fichiers à uploader
  #for(k in 1:length(file)){
  
  #chargement du fichier en recuperant le nom du fichier contenu dans le chemin d'acces. pattern à modifier si besoin
  name= upload_file(str_extract(file, "([a-z]+_)+((\\d\\d)|XX)(_[a-z]_)+\\d\\d(_[a-z]+)[[:punct:]][[:alnum:]]+"))
  choix= list('file'= name )
  #}
  
  
  names(choix)[names(choix) == ""] <- "file"
  # names(choix)=file
  return(choix)
}

postDataUpload <- function(a,b){
  #table= list()
  
  #charger le fichier en ligne en recuperant les sha1
  #for (i in 1:(length(a))) {
  
  res= (httr::POST(url = 'https://apitest.nakala.fr/datas/uploads', httr::add_headers(.headers=b), body = a))
  #print(content(res))
  table= content(res)$sha1   
  #print(content(res)$sha1)
  # head('application/json','01234567-89ab-cdef-0123-456789abcdef','multipart/form-data')
  #}
  return(table)
}









updateData<-function(metaFile,header,sha,hder){
  sh=metaFile[[1]]
  id_data=metaFile[[2]]
  postDataUpdate(sha,id_data,hder)
  deleFile(id_data,sh,header)
  
}

main<- function(){
  file<-choose.files(default = "", caption = "Select files",
                     multi = TRUE, filters = Filters,
                     index = nrow(Filters))
  print(file)
  name=str_extract(file, "([a-z]+_)+((\\d\\d)|XX)(_[a-z]_)+\\d\\d(_[a-z]+)[[:punct:]][[:alnum:]]+")
  print(name)
  key=as.character(readline("Entrer la clé de connexion  "))
  id_collection=as.character(readline("Entrer l'identifiant de la collection   "))
  #id_data=as.character(readline("Entrer l'identifiant de la donée   "))
  f<-extract(file)
  print(f)
  sha<-postDataUpload(f,head(key))
  print(sha)
  dt=data(id_collection,param('1','100'),header(key))
  metaFile=getMetasFichier(dt,name)
  print(metaFile)
  updateData(metaFile,header(key),sha,hder(key))
  
}

#key=as.character(readline("Entrer la clé de connexion  "))
setwd("D:/test")
getwd()
main()
#01234567-89ab-cdef-0123-456789abcdef
#10.34847/nkl.fcdf4n31
#10.34847/nkl.6d55796x





key
headers = c(
  `accept` = 'application/json',
  `X-API-KEY` = '01234567-89ab-cdef-0123-456789abcdef'
)

a=list()
sh=list()
uri=list()
nm=list()
cmpt=0
l=1
c=1
nam=str_extract(name[1], "([a-z]+_)+((\\d\\d)|XX|xx)(_[a-z]_)+\\d\\d")
print(nam)
while ( c <= length(dt)) {
  for(i in 1: length(dt)){
    #on recup le nom du premier fichier et on extrait le nom de la donnée 
    a=str_extract(dt[[i]]$files[[1]]$name,"([a-z]+_)+((\\d\\d)|XX|xx)(_[a-z]_)+\\d\\d")
    for(j in 1: length(dt[[i]]$files)){
      
      #print(a)
      #recuperer tous les sha1, les noms des fichiers et idData de donnees qui ont comme nom de donnée 
      #le nom du dossier qu'on a chargé dans nakala
      #Les noms qui ne seront pas trouvés, on les considera comme des données à créer
      if(a==nam){
        sh[l]=dt[[i]]$files[[j]]$sha1
        nm[l]=dt[[i]]$files[[j]]$name
        uri[l]=gsub("https://doi.org/","",dt[[i]]$uri)
        l=l+1
        cmpt=cmpt+1
      }
    }
    c=c+1
  }
}
#si aucune donnée n'a le meme nom que la donnée le dossier qu'on veut uploader 
#alors on crée une nouvelle donnée
if(cmpt==0){
  
  postData(hder(key),sha,files,langue,collectionID)
}else{
  s=list()
  n=list()
  #print(uri)
  print(sh)
  cpt=0
  k=1
  for (i in 1:length(sha)) {
    for (j in 1:length(sh)) {
      
      #comparer le sha1 qu'on vient de charger à tous ceux qui sont dans la collection
      #s'il y a une egalité ou pas
      if (toString(sh[j]) == toString(sha[i])) {
        cpt=cpt+1
      }
    }
    #quand les sha1 de la base sont dif de ceux qu'on a chargé alors on garde les noms et sha1 ds
    #fichiers qu'on vient de charger 
    if(cpt==0) {
      s[k]=sha[i]
      n[k]=name[i]
      k=k+1
      #cpt=0
    }
    
    
  }
  
  print(s)
  #doublonstest<-which(duplicated(s))
  #print(doublonstest)
  #sh2<-s[-doublonstest]
  if (length(s)>0) {
    sameName=list()
    notSameName=list()
    k=1
    l=1
    cpt=0
    #on cherche maintenant les fichiers qui ont le meme nom mais pas le meme sha1
    for (i in 1:length(n)) {
      for (j in 1: length(nm)) {
        if (n[i]==nm[j]) {
          sameName[k]=s[i]
          cpt=cpt+1
          k=k+1
        }
      }
      if(cpt==0){
        notSameName[l]=s[i]
        l=l+1
      }
      
    }
    if(length(notSameName)>0){
      postDataUpdate(notSameName,uri[1],hder(key))
      print("ajout de fichier dans la collection ")
    }
    if(length(sameName)>0){
      postDataUpdate(sameName,uri[1],hder(key))
      deleFile(uri[1],sameName,header)
      print("remplacement de fichier réussit")
    }
  }
  
}
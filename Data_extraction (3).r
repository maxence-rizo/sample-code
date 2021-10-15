Sys.setenv(CURL_SSL_BACKEND="openssl")

library(curl)
library(xml2)

  # Paramétres utilisateur
  username <- Sys.info()[["user"]]
  #penser à mettre en place son mot de passe internet si besoin :
  #file.edit('~/.Renviron') puis taper son mdp et l'enregistrer en variable Renviron
  
  source("read.properties.r")

  # Paramétres Docapost
  myProps <- read.properties("dfast_R.properties")
  
  relPath <- paste0("/webdav/004/",myProps$fast_username,"/arecuperer/")
  if (!myProps$fast_emetteur =="") {
    relPath <- paste(relPath,myProps$fast_emetteur,"/")
  }
  dav <- myProps$fast_url
  fast_uri <- URLencode(paste0(dav, relPath))
 
  # Paramétrage de la connexion au serveur Docapost
  h <- new_handle()
  handle_setopt(h, username = myProps$fast_username)
  handle_setopt(h, password = myProps$fast_password)
  # Paramétrage du proxy
  handle_setopt(h, proxy = ie_get_proxy_for_url(fast_uri), proxyuserpwd =paste(username,Sys.getenv("https_proxy_password"),sep=":"), proxyauth=3,verbose = TRUE)
  # Paramétrage/neutralisation SSL
  handle_setopt(h,sslversion=4,ssl_verifypeer=0L, ssl_verifyhost = 0L)
  # Ici on force le useragent car le useragent associé é R n'est pas authorisé au niveau du proxy
  # handle_setopt(h, useragent="curl/7.24.0 (x86_64-apple-darwin12.0) libcurl/7.24.0 OpenSSL/0.9.8y zlib/1.2.5")
  handle_setopt(h, useragent="R (3.5.0 x86_64-w64-mingw32 x86_64 mingw32)")
  
  # Paramétre PROPFIND pour lister le contenu du répertoire pointé par uri
  handle_setopt(h, customrequest = "PROPFIND")
  # Paramétre depth pour itérer sur les sous-répertoires
  handle_setheaders(h,"depth" = "infinity")
  #Exécution de la requéte
  response <- curl_fetch_memory(fast_uri, h)
  
  #Lecture de la réponse, au format XML
  text <- rawToChar(response$content)
  #Parsing du XML
  x <- read_xml(text, encoding = "",  as_html = FALSE,  options = "NOBLANKS")
  #On récupére le contenu de toutes les balises </D:href>
  fics <-as_list(xml_contents(xml_find_all(x,'//D:href') ))
  
  # Les fichiers sont enregistrés dans le répertoire myProps$Local_repertoire\dfast\fichiers\recuperes
  desPath <- paste0(myProps$local_repertoire,"\\","dfast")
  dir.create(desPath, showWarnings = FALSE)
  desPath <- paste0(desPath,"\\fichiers")
  dir.create(desPath, showWarnings = FALSE)
  desPath <- paste0(desPath,"\\recuperes")
  dir.create(desPath, showWarnings = FALSE)
  
  #desPath <- paste(paste("C:\\Users\\",username,sep=""),"\\concours\\fichiers\\recuperes\\",sep="")
  
  #Fonction de téléchargement 
  downloadFile <- function(relPath) {
    #Todo: gérer les traces
    fast_uri <- URLencode(paste0(dav, relPath))
    fast_emetteur <- strsplit(fast_uri, "/")[[1]][8]
    fast_fichier <-strsplit(fast_uri, "/")[[1]][9]
    #View(uri)
    if (!is.na(fast_fichier)) {
      handle_setopt(h, customrequest = "GET")
      dir.create(file.path(desPath, fast_emetteur), showWarnings = FALSE)
      destFile <-paste0(desPath,"\\",fast_emetteur,"\\", fast_fichier)
      #View(destfile)
      curl_download(fast_uri, destFile, quiet = FALSE, mode = "wb",  handle = h)  
    }
  }
  
  # Appel itératif de la fonction download sur chaque élément de la liste fic
  lapply(fics,  downloadFile)
  
  
  
  # Libération de la connexion Docapost
  handle_reset(h)
 
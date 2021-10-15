#library(errorlocate)
#library(validatetools)
library(data.table)
library(validate)

#récupération des grades à jours sur le RIE

require("httr")
require("jsonlite")
base <- "https://api.cisirh.rie.gouv.fr/ingres/nomenclatures/"
endpoint <- "GRADE"
username <- Sys.info()[["user"]]
httr::set_config(config(proxy = curl::ie_get_proxy_for_url(base), proxyuserpwd =paste(username,Sys.getenv("https_proxy_password"),sep=":"), proxyauth=3))
call <- paste0(base,endpoint)
get_api <- GET(call)
get_api_text <- content(get_api, "text")
get_api_rcc <- fromJSON(get_api_text, flatten = TRUE)

#paramétrage des noms de variables

concols <- c("AOR_NOM","TYPE_ENVOI","RECRUT_ID","RECRUT_LIB","ANNEE_SESSION","NOMENC_GRADE","GRADE_COD","GRADE_LIB","VOIE_ACCES","RECRUT_MUTUALISE","ORG_TERRITORIALE","AFFECTATION_LIB","NUMERO_SESSION","CARACTERE_RESERVE","SPECIALITE_LIB","NUM_NOR","DATE_CONV","DATE_EPR","DATE_RES","NBRE_POSTES","NIVETU_REQUIS","MBRE_JURY_H","MBRE_JURY_F","SEXE_PRES_JURY","CANDIDAT_ID","NOM_FAMILLE","NOM_USAGE","PRENOM","SEXE","DATE_NAIS","PAYS_NAIS","DEP_NAIS","COMM_NAIS","ADRESSE1","ADRESSE2","ADRESSE3","CODE_POSTAL","COMM_RES","PAYS_RES","COURRIEL","TEL","VALIDATION_INSCR","NATIO_LIB","NIVETU_CAND","DEROG_NIVETU","ACT_3CONCOURS","STATUT_CAND","CAT_AG_PUB","VERSANT_ORG_AG_PUB","PREPA_CONC","NOM_PREPA_CONC","VOEU_CC_1","VOEU_CC_2","VOEU_CC_3","VOEU_CC_4","VOEU_CC_5","VOEU_CC_6","VOEU_CC_7","VOEU_CC_8","VOEU_CC_9","VOEU_CC_10","PARTICIPANT","PRE_ADMISSIBILITE","ADMISSIBILITE","PRE_ADMISSION","ADMISSION","POINT_PRE_ADMISSIBILITE","POINT_ADMISSIBILITE","POINT_ADMISSION","POINT_ADMISSION_SANS_REPORT","RANG_ADMISSION","NB_EPR","ETAPE_EPR_1","NAT_EPR_1","DATE_EPR_1","NOM_EPR_1","COEF_EPR_1","NOTE_EPR_1","NOTE_BASE_1","NOTE_ELIM_1","NOTE_MAX_1","ETAPE_EPR_2","NAT_EPR_2","DATE_EPR_2","NOM_EPR_2","COEF_EPR_2","NOTE_EPR_2","NOTE_BASE_2","NOTE_ELIM_2","NOTE_MAX_2","ETAPE_EPR_3","NAT_EPR_3","DATE_EPR_3","NOM_EPR_3","COEF_EPR_3","NOTE_EPR_3","NOTE_BASE_3","NOTE_ELIM_3","NOTE_MAX_3","ETAPE_EPR_4","NAT_EPR_4","DATE_EPR_4","NOM_EPR_4","COEF_EPR_4","NOTE_EPR_4","NOTE_BASE_4","NOTE_ELIM_4","NOTE_MAX_4","ETAPE_EPR_5","NAT_EPR_5","DATE_EPR_5","NOM_EPR_5","COEF_EPR_5","NOTE_EPR_5","NOTE_BASE_5","NOTE_ELIM_5","NOTE_MAX_5","ETAPE_EPR_6","NAT_EPR_6","DATE_EPR_6","NOM_EPR_6","COEF_EPR_6","NOTE_EPR_6","NOTE_BASE_6","NOTE_ELIM_6","NOTE_MAX_6","ETAPE_EPR_7","NAT_EPR_7","DATE_EPR_7","NOM_EPR_7","COEF_EPR_7","NOTE_EPR_7","NOTE_BASE_7","NOTE_ELIM_7","NOTE_MAX_7","ETAPE_EPR_8","NAT_EPR_8","DATE_EPR_8","NOM_EPR_8","COEF_EPR_8","NOTE_EPR_8","NOTE_BASE_8","NOTE_ELIM_8","NOTE_MAX_8","ETAPE_EPR_9","NAT_EPR_9","DATE_EPR_9","NOM_EPR_9","COEF_EPR_9","NOTE_EPR_9","NOTE_BASE_9","NOTE_ELIM_9","NOTE_MAX_9","ETAPE_EPR_10","NAT_EPR_10","DATE_EPR_10","NOM_EPR_10","COEF_EPR_10","NOTE_EPR_10","NOTE_BASE_10","NOTE_ELIM_10","NOTE_MAX_10","ETAPE_EPR_11","NAT_EPR_11","DATE_EPR_11","NOM_EPR_11","COEF_EPR_11","NOTE_EPR_11","NOTE_BASE_11","NOTE_ELIM_11","NOTE_MAX_11","ETAPE_EPR_12","NAT_EPR_12","DATE_EPR_12","NOM_EPR_12","COEF_EPR_12","NOTE_EPR_12","NOTE_BASE_12","NOTE_ELIM_12","NOTE_MAX_12","ETAPE_EPR_13","NAT_EPR_13","DATE_EPR_13","NOM_EPR_13","COEF_EPR_13","NOTE_EPR_13","NOTE_BASE_13","NOTE_ELIM_13","NOTE_MAX_13","ETAPE_EPR_14","NAT_EPR_14","DATE_EPR_14","NOM_EPR_14","COEF_EPR_14","NOTE_EPR_14","NOTE_BASE_14","NOTE_ELIM_14","NOTE_MAX_14")
colClasses = list(character=1:4, numeric=5, character=6:19, numeric=20, character=21, numeric=22:23, character=24:66, numeric=67:72)

#je passe par une liste fichiers pour éviter les problèmes de slashs avec windows et ou d'accents
files<-list.files(pattern = "csv",full.names = T,recursive = T)
DataSet0 <- data.table::fread(input=files[12],encoding="UTF-8",dec=",",fill=T,colClasses=colClasses,na.strings=c("","."),sep = "|",header = F) 
DataSet <-  data.table::setnames(DataSet0, skip_absent=TRUE, old = c(paste0("V",seq(1:198))), new = c("AOR_NOM","TYPE_ENVOI","RECRUT_ID","RECRUT_LIB","ANNEE_SESSION","NOMENC_GRADE","GRADE_COD","GRADE_LIB","VOIE_ACCES","RECRUT_MUTUALISE","ORG_TERRITORIALE","AFFECTATION_LIB","NUMERO_SESSION","CARACTERE_RESERVE","SPECIALITE_LIB","NUM_NOR","DATE_CONV","DATE_EPR","DATE_RES","NBRE_POSTES","NIVETU_REQUIS","MBRE_JURY_H","MBRE_JURY_F","SEXE_PRES_JURY","CANDIDAT_ID","NOM_FAMILLE","NOM_USAGE","PRENOM","SEXE","DATE_NAIS","PAYS_NAIS","DEP_NAIS","COMM_NAIS","ADRESSE1","ADRESSE2","ADRESSE3","CODE_POSTAL","COMM_RES","PAYS_RES","COURRIEL","TEL","VALIDATION_INSCR","NATIO_LIB","NIVETU_CAND","DEROG_NIVETU","ACT_3CONCOURS","STATUT_CAND","CAT_AG_PUB","VERSANT_ORG_AG_PUB","PREPA_CONC","NOM_PREPA_CONC","VOEU_CC_1","VOEU_CC_2","VOEU_CC_3","VOEU_CC_4","VOEU_CC_5","VOEU_CC_6","VOEU_CC_7","VOEU_CC_8","VOEU_CC_9","VOEU_CC_10","PARTICIPANT","PRE_ADMISSIBILITE","ADMISSIBILITE","PRE_ADMISSION","ADMISSION","POINT_PRE_ADMISSIBILITE","POINT_ADMISSIBILITE","POINT_ADMISSION","POINT_ADMISSION_SANS_REPORT","RANG_ADMISSION","NB_EPR","ETAPE_EPR_1","NAT_EPR_1","DATE_EPR_1","NOM_EPR_1","COEF_EPR_1","NOTE_EPR_1","NOTE_BASE_1","NOTE_ELIM_1","NOTE_MAX_1","ETAPE_EPR_2","NAT_EPR_2","DATE_EPR_2","NOM_EPR_2","COEF_EPR_2","NOTE_EPR_2","NOTE_BASE_2","NOTE_ELIM_2","NOTE_MAX_2","ETAPE_EPR_3","NAT_EPR_3","DATE_EPR_3","NOM_EPR_3","COEF_EPR_3","NOTE_EPR_3","NOTE_BASE_3","NOTE_ELIM_3","NOTE_MAX_3","ETAPE_EPR_4","NAT_EPR_4","DATE_EPR_4","NOM_EPR_4","COEF_EPR_4","NOTE_EPR_4","NOTE_BASE_4","NOTE_ELIM_4","NOTE_MAX_4","ETAPE_EPR_5","NAT_EPR_5","DATE_EPR_5","NOM_EPR_5","COEF_EPR_5","NOTE_EPR_5","NOTE_BASE_5","NOTE_ELIM_5","NOTE_MAX_5","ETAPE_EPR_6","NAT_EPR_6","DATE_EPR_6","NOM_EPR_6","COEF_EPR_6","NOTE_EPR_6","NOTE_BASE_6","NOTE_ELIM_6","NOTE_MAX_6","ETAPE_EPR_7","NAT_EPR_7","DATE_EPR_7","NOM_EPR_7","COEF_EPR_7","NOTE_EPR_7","NOTE_BASE_7","NOTE_ELIM_7","NOTE_MAX_7","ETAPE_EPR_8","NAT_EPR_8","DATE_EPR_8","NOM_EPR_8","COEF_EPR_8","NOTE_EPR_8","NOTE_BASE_8","NOTE_ELIM_8","NOTE_MAX_8","ETAPE_EPR_9","NAT_EPR_9","DATE_EPR_9","NOM_EPR_9","COEF_EPR_9","NOTE_EPR_9","NOTE_BASE_9","NOTE_ELIM_9","NOTE_MAX_9","ETAPE_EPR_10","NAT_EPR_10","DATE_EPR_10","NOM_EPR_10","COEF_EPR_10","NOTE_EPR_10","NOTE_BASE_10","NOTE_ELIM_10","NOTE_MAX_10","ETAPE_EPR_11","NAT_EPR_11","DATE_EPR_11","NOM_EPR_11","COEF_EPR_11","NOTE_EPR_11","NOTE_BASE_11","NOTE_ELIM_11","NOTE_MAX_11","ETAPE_EPR_12","NAT_EPR_12","DATE_EPR_12","NOM_EPR_12","COEF_EPR_12","NOTE_EPR_12","NOTE_BASE_12","NOTE_ELIM_12","NOTE_MAX_12","ETAPE_EPR_13","NAT_EPR_13","DATE_EPR_13","NOM_EPR_13","COEF_EPR_13","NOTE_EPR_13","NOTE_BASE_13","NOTE_ELIM_13","NOTE_MAX_13","ETAPE_EPR_14","NAT_EPR_14","DATE_EPR_14","NOM_EPR_14","COEF_EPR_14","NOTE_EPR_14","NOTE_BASE_14","NOTE_ELIM_14","NOTE_MAX_14"))
setDF(DataSet)
DataSet <-dplyr::mutate_at(DataSet,dplyr::vars(dplyr::matches("NOTE_")), ~replace(., is.na(.), 0))
DataSet <-dplyr::mutate_all(DataSet, list(~dplyr::na_if(.,"")))
DataSet[concols[!(concols %in% colnames(DataSet))]] = 0


#emplacement des blocs de règles à tester, peuvent être remplacées au besoin
RuleSet <- validate::validator(.file="E:\\BaseConcours\\validaty-master\\Ctrl_1_Recutement_Identification.R")  
RuleSet <- validate::validator(.file="E:\\BaseConcours\\validaty-master\\Ctrl_2_Concours_Recrutement_Modalités.R")  
RuleSet <- validate::validator(.file="E:\\BaseConcours\\validaty-master\\Ctrl_3_Candidat_Identification.R")  
RuleSet <- validate::validator(.file="E:\\BaseConcours\\validaty-master\\Ctrl_4_Candidat_Données_Inscription.R")  
RuleSet <- validate::validator(.file="E:\\BaseConcours\\validaty-master\\Ctrl_5_Candidat_Selection.R")  
RuleSet <- validate::validator(.file="E:\\BaseConcours\\validaty-master\\Ctrl_Concours.R")  
RuleSet <- validate::validator(.file="E:\\BaseConcours\\validaty-master\\Ctrl_5.R")  
RuleSet <- validate::validator(.file="E:\\BaseConcours\\validaty-master\\Ctrl_5.yml")
voptions(RuleSet, lin.eq.eps=0, lin.ineq.eps=0)
ResultSet <- confront(DataSet, RuleSet,ref=get_api_rcc)
summary(ResultSet)
summary(ResultSet[1:2])
#locate_errors(DataSet, RuleSet)
as.data.frame(RuleSet)[c("name","label","rule")]
RuleSet[1:2]
RuleSet


#Number of rules evaluated.
length(ResultSet)
#Check if all validations result in TRUE
all(ResultSet)
#Gather results in a (list of) array(s)
out <-values(ResultSet)
as.data.frame(out)
rownames(out)
#find out what records violate at least one rule as follows
ifail <- apply(out[[1]], 1, all, na.rm=TRUE)
warnings()
#Check if any validation resulted in FALSE
any(ResultSet)
#List of warning signals
warnings(ResultSet)
#List of error signals
errors(ResultSet)
#Create barplot(s) of results les plots sont à changer, il faudrait passer du GGplot plutot que du plot base qui plante souvent
plot(ResultSet,main="Results by rule")
barplot(ResultSet,main="Results by rule")
# Download plot in a pdf file 
pdf("toto.pdf") # open the pdf device
barplot(ResultSet,main="Results by rule")
dev.off()

#rmarkdown nécessite des packages cachés
rmarkdown::render("E:\\BaseConcours\\validaty-master\\Ctrl_bilan.Rmd", params = list(
  data = files[11]))

rmarkdown::render(files[11], params = "ask")


ResultSetDf <-as.data.frame(ResultSet)

#Utiliser le code suivant en cas d'impossibilité d'utiliser Rmarkdown 
# 
# ifail <- apply(out[[1]], 9, all, na.rm=TRUE)
# 
# Sys.Date()
# 
#     
# )
# validate::export_yaml(RuleSet, "E:\\BaseConcours\\validaty-master\\Ctrl_1_Recutement_Identification.yml")
# validate::export_yaml(RuleSet, "E:\\BaseConcours\\validaty-master\\Ctrl_2_Concours_Recrutement_Modalit?s.yml")
# validate::export_yaml(RuleSet, "E:\\BaseConcours\\validaty-master\\Ctrl_3_Candidat_Identification.yml")
# validate::export_yaml(RuleSet, "E:\\BaseConcours\\validaty-master\\Ctrl_4_Candidat_Donn?es_Inscription.yml")
# validate::export_yaml(RuleSet, "E:\\BaseConcours\\validaty-master\\Ctrl_5_Candidat_Selection.yml")
# validate::export_yaml(RuleSet, "E:\\BaseConcours\\validaty-master\\Ctrl_5.yml")
# length(RuleSet)
# names(RuleSet)
# label(RuleSet)
# description(RuleSet)
# RuleSet[[1]]
# 
# 
# rules <- validator(G := var_group(AOR_NOM,TYPE_ENVOI,RECRUT_ID,RECRUT_LIB,ANNEE_SESSION,GRADE_COD,GRADE_LIB,VOIE_ACCES,RECRUT_MUTUALISE,SPECIALITE_LIB),H := var_group(ORG_TERRITORIALE,NUM_COR), !(is.na(G)),if (AOR_NOM=="FPE") !(is.na(H)))
# 
# 
# summary(confront(DataSet, rules))
# !grepl("\\D", x)
# !grepl("\\D", (gsub(",", ".",DataSet$POINT_PRE_ADMISSIBILITE)))
# !grepl("\\D", (gsub(",", ".",DataSet$POINT_ADMISSIBILITE)))
# !grepl("\\d|,", (DataSet$POINT_ADMISSION))
# is.numeric(gsub(",", ".",DataSet$POINT_ADMISSION_SANS_REPORT))
# is.numeric(gsub(",", ".","10,7"))
# 
# 
# grepl("NOTE_EPR", names(DataSet), fixed = TRUE)
# result <- data.frame(a=rowSums(data[, i]), bt=rowSums(data[, !i]))
# ifelse (DataSet$ETAPE_EPR_1=="2", ifelse(is.na(as.numeric(gsub(",", ".",DataSet$NOTE_EPR_1))),0,as.numeric(gsub(",", ".",DataSet$NOTE_EPR_1))) * as.numeric(gsub(",", ".",DataSet$COEF_EPR_1)), 0)
# ifelse (DataSet$TYPE_ENVOI=="2" & DataSet$ETAPE_EPR_1=="2",ifelse(is.na(as.numeric(gsub(",", ".",DataSet$NOTE_EPR_1))),0,as.numeric(gsub(",", ".",DataSet$NOTE_EPR_1))) * as.numeric(DataSet$COEF_EPR_1), 0) +
# ifelse (DataSet$TYPE_ENVOI=="2" & DataSet$ETAPE_EPR_2=="2",ifelse(is.na(as.numeric(gsub(",", ".",DataSet$NOTE_EPR_2))),0,as.numeric(gsub(",", ".",DataSet$NOTE_EPR_2))) * as.numeric(DataSet$COEF_EPR_2), 0) +
# ifelse (DataSet$TYPE_ENVOI=="2" & DataSet$ETAPE_EPR_3=="2",ifelse(is.na(as.numeric(gsub(",", ".",DataSet$NOTE_EPR_3))),0,as.numeric(gsub(",", ".",DataSet$NOTE_EPR_3))) * as.numeric(DataSet$COEF_EPR_3), 0) +
# ifelse (DataSet$TYPE_ENVOI=="2" & DataSet$ETAPE_EPR_4=="2",ifelse(is.na(as.numeric(gsub(",", ".",DataSet$NOTE_EPR_4))),0,as.numeric(gsub(",", ".",DataSet$NOTE_EPR_4))) * as.numeric(DataSet$COEF_EPR_4), 0) 
# 
# ifelse (DataSet$TYPE_ENVOI=="2" & DataSet$ETAPE_EPR_1=="2",ifelse(is.na(as.numeric(gsub(",", ".",DataSet$NOTE_EPR_1))),0,max(0,as.numeric(gsub(",", ".",DataSet$NOTE_EPR_1))-ifelse(is.na(DataSet$NOTE_BASE_1),0,as.numeric(DataSet$NOTE_BASE_1)))) * as.numeric(DataSet$COEF_EPR_1), 0)
#   
# pmax(0,as.numeric(gsub(",", ".",DataSet$NOTE_EPR_2))-ifelse(is.na(DataSet$NOTE_BASE_2),0,as.numeric(DataSet$NOTE_BASE_2)))* as.numeric(DataSet$COEF_EPR_2)+
# pmax(0,as.numeric(gsub(",", ".",DataSet$NOTE_EPR_3))-ifelse(is.na(DataSet$NOTE_BASE_3),0,as.numeric(DataSet$NOTE_BASE_3)))* as.numeric(DataSet$COEF_EPR_3)+
# pmax(0,as.numeric(gsub(",", ".",DataSet$NOTE_EPR_4))-ifelse(is.na(DataSet$NOTE_BASE_4),0,as.numeric(DataSet$NOTE_BASE_4)))* as.numeric(DataSet$COEF_EPR_4)
# 
# grepl("[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}",as.character(DataSet$COURRIEL), ignore.case=TRUE)
# grepl("[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}","felix@nicebread.de", ignore.case=TRUE)
# 
# if (DataSet$PAYS_NAIS == "FRANCE") DataSet$DEP_NAIS  %in% stringr::str_pad(c(paste0("",seq(1:95)), "2A", "2B", paste0("97",seq(1:4)),"976" ),2,"left","0")
# 
# stringr::str_pad(c(paste0("",seq(1:95)), "2A", "2B", paste0("97",seq(1:4)),"976" ),2,"left","0")
# 
# if (DataSet$NOMENC_GRADE=="RCC") DataSet$GRADE_COD %in% get_api_rcc_df$identifiant
# 
# if (DataSet$TYPE_ENVOI == "2" & is.na(as.numeric(gsub(",", ".",DataSet$NOTE_EPR_1)))) DataSet$NOTE_EPR_1  %in% c("REFUS","ADMIS","ABSENT","BLANCHE","NON_RENDU","ANONY","FRAUDE","AUTRE")
# 
# ifelse (DataSet$TYPE_ENVOI=="2" & DataSet$ETAPE_EPR_1=="1",ifelse(is.na(as.numeric(gsub(",", ".",DataSet$NOTE_EPR_1))),0,as.numeric(gsub(",", ".",DataSet$NOTE_EPR_1))) * as.numeric(DataSet$COEF_EPR_1), 0)
# ifelse (DataSet$TYPE_ENVOI=="2" & DataSet$ETAPE_EPR_1=="2",ifelse(is.na(as.numeric(gsub(",", ".",DataSet$NOTE_EPR_1))),0,pmax(0,as.numeric(gsub(",", ".",DataSet$NOTE_EPR_1))-ifelse(is.na(DataSet$NOTE_BASE_1),0,as.numeric(DataSet$NOTE_BASE_1)))) * as.numeric(DataSet$COEF_EPR_1), 0)
# ifelse (DataSet$TYPE_ENVOI=="2" & DataSet$ETAPE_EPR_1=="4",ifelse(is.na(as.numeric(gsub(",", ".",DataSet$NOTE_EPR_1))),0,pmax(0,as.numeric(gsub(",", ".",DataSet$NOTE_EPR_1))-ifelse(is.na(DataSet$NOTE_BASE_1),0,as.numeric(DataSet$NOTE_BASE_1)))) * as.numeric(DataSet$COEF_EPR_1), 0)
# DataSet$POINT_ADMISSION_SANS_REPORT  

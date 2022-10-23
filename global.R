# Chargement des packages : 9 sec, mapview est parmis les plus couteux
library(tidyverse)
library(lubridate)
library(sf)
library(sp)
library(rgdal)
library(readxl)

library(shinydashboard)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(bsplus)
library(htmltools)
library(htmlwidgets)
library(markdown)
library(patchwork)
library(png)

library(leaflet)
library(leaflet.extras)
library(mapview)
library(leafpop)
# library(leafem)
library(geojsonsf)
library(geojson)

library(ggiraph)
library(ggthemes)
library(cowplot)

library(kableExtra)
library(DT)
library(aws.s3)

if(!webshot::is_phantomjs_installed()) {webshot::install_phantomjs(force = FALSE)}

options(scipen=999)
# options("encoding"="UTF-8")

# chargement des données, moins d'un centieme de seconde
# setwd("app")

#récupération des données internes depuis le disque ou S3
if (file.exists("data_netoyee.RData")) {
  load("data_netoyee.RData")
} else {
  s3load("data_netoyee.RData", bucket="projet-connaissance-enr", region="")
}


#année
mil <- 2021
codetoword<-c("eol_ter"="Eolien","pvq"="Photovoltaïque","metha"="Cogeneration electrique")


# Pour repartir de données brutes au format csv : la clause en if assure de ne pas refaire l'import si l'image a été sauvegardée.

# récupération des csv ODRE du dossier extdata, à voir pour faire un appel direct à l'API...
if(is.null(files)){
  
  
files<-list.files("extdata/donnees_odre/",pattern = "^registre",full.names = T)



#gestion des noms de fichiers pour l'ID
  
  names(files)<-str_remove(files,"extdata/donnees_odre/registre-national-installation-production-stockage-electricite-agrege-3112")
  names(files)<-paste0("20",str_remove(names(files),".csv"))
  
  #lecture
  
  #attention type colonne entre les années
  imports<-map_df(files,read_delim,delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""),trim_ws = TRUE,.id="annee",
                  col_types = cols(codeEPCI=col_character(),
                                   puisMaxInstallee = col_number(),
                                   puisMaxRac  = col_number(),
                                   maxPuis=col_number()))
  
  odre_gaz_v4 <- read_delim("extdata/donnees_odre/220803 - Export detail gaz - V4.csv", 
                            delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                grouping_mark = ""), trim_ws = TRUE)
  
  
  }
  #gaz
imports<-imports%>%mutate(code_typo=case_when(
  codeFiliere=="SOLAI"~"pvq",
  codeFiliere=="EOLIE"~"eol_ter",
  codeFiliere=="BIOEN"&codeCombustible%in%c("B.EPU","B.MET","B.STO","BAGAS")~"metha",
  T~codeFiliere # assure de ne pas modifier les autres
))
  
  #aggregation : important d'avoir le code EPCI à la fin
  
  # attention nom des colonnes pourra peut être changer?
  imports_aggregats<-imports%>%group_by(annee,codeDepartement,codeRegion,filiere,code_typo,codeEPCI)%>%
    summarise(across(.cols=c(puisMaxInstallee,puisMaxRac,nbInstallations,energieAnnuelleGlissanteInjectee,energieAnnuelleGlissanteProduite,energieAnnuelleProduite,energieAnnuelleInjectee,maxPuis),
                     ~ sum(.x, na.rm = TRUE)))
  
  # retraitements métiers
  
  imports_clean<-imports_aggregats%>%mutate(DEP = paste0("D", codeDepartement), 
                                                                            REG = paste0("R",codeRegion),
                                                                            Puissance.totale.en.MW=case_when(
                                                                              puisMaxInstallee>puisMaxRac~puisMaxInstallee/1000,
                                                                                puisMaxInstallee<puisMaxRac~puisMaxRac/1000,
                                                                              annee==2021~maxPuis/1000
                                                                            ),
                                                                              annee=as.numeric(annee))
  
  #création des codeZone et typezone
  imports_clean<-imports_clean%>%
    mutate(
    CodeZone=case_when(
    !is.na(codeEPCI)~codeEPCI,
    is.na(codeEPCI)&!is.na(codeDepartement)~DEP,
    is.na(codeEPCI)&is.na(codeDepartement)~REG
    ), 
    TypeZone= case_when(
      !is.na(codeEPCI)~"EPCI",
      is.na(codeEPCI)&!is.na(codeDepartement)~"DEP",
      is.na(codeEPCI)&is.na(codeDepartement)~"REG"
      ),
    Energie.totale.en.MWh.an=case_when(
      annee %in%c(2017:2019)~energieAnnuelleInjectee/1000,
      annee>2019 ~energieAnnuelleGlissanteInjectee/1000
      ),
    source="ODRE")
  
  #harmonisation des filières
  
  # imports_clean<-imports_clean%>%mutate(code_typo=case_when(
  #   code_typo=="SOLAI"~"pvq",
  #   code_typo=="EOLIE"~"eol_ter",
  #   code_typo=="BIOEN"&codeCombustible%in%c("B.EPU","B.MET","B.STO","BAGAS")~"metha",
  #   T~code_typo # assure de ne pas modifier les autres
  # ))
  
  
  #selection
  ENR<-c("eol_ter","metha","pvq","biogaz")
  
  imports_elec_format<-imports_clean%>%ungroup%>%
    select(origine=source,TypeZone,annee,CodeZone,code_typo,`Nombre de sites`=nbInstallations,Energie.totale.en.MWh.an,Puissance.totale.en.MW,DEP,REG)%>%
    filter(code_typo%in%ENR)
  
  #obligé de passer apr ces retraitements car les données ODRE contiennent bien des données "région et départements" mais elles sont incomplètes et non des aggrégats
  
  imports_elec_format_dep<-imports_elec_format%>%group_by(annee,code_typo,DEP)%>%summarise(`Nombre de sites`=sum(`Nombre de sites`,na.rm=T),
                                                                                           Energie.totale.en.MWh.an=sum(Energie.totale.en.MWh.an,na.rm=T),
                                                                                           Puissance.totale.en.MW=sum(Puissance.totale.en.MW,na.rm=T),REG=max(REG))%>%mutate(CodeZone=DEP,TypeZone="Departement")
  
  imports_elec_format_reg<-imports_elec_format%>%group_by(annee,code_typo,REG)%>%summarise(`Nombre de sites`=sum(`Nombre de sites`,na.rm=T),
                                                                                           Energie.totale.en.MWh.an=sum(Energie.totale.en.MWh.an,na.rm=T),
                                                                                           Puissance.totale.en.MW=sum(Puissance.totale.en.MW,na.rm=T),DEP=NA_character_)%>%mutate(CodeZone=REG,TypeZone="Region")
  
  
  imports_elec_format_clean<-imports_elec_format%>%filter(TypeZone=="EPCI")%>%bind_rows(imports_elec_format_dep)%>%bind_rows(imports_elec_format_reg)
  gaz_format$TypeZone<-str_replace_all(gaz_format$TypeZone,"Régon","Region")
  
  
  odre_complet_V5<-bind_rows(imports_elec_format_clean,gaz_format)
  odre_complet_V5<-odre_complet_V5%>%left_join(liste_zone_complete_france,by=c("CodeZone"),suffix=c("",".y"))
  
  

#inst_reg utilisé pour les données individuelles

inst_reg_france <- inst_reg %>%
  filter(code_typo == "metha" | code_typo == "pvq" | code_typo == "eol_ter") %>%
  mutate(DEP = paste0("D", DEP), REG = paste0("R",REG))
inst_biogaz_france<-inst_biogaz%>%  mutate(DEP = paste0("D", DEP), REG = paste0("R",REG))


# définition carto elec et gaz, dans une clause en IF pour ne pas les recalculer si présents dans les data
# car les changements de projections sont longs


dta <- inst_reg_france
# coordonnées carto


dta<-dta%>%st_sf() %>%
  
  st_set_crs(2154) %>% 
  st_transform(crs = 4326) %>%
  st_set_crs(4326) %>% 
 # st_jitter(factor = 0.008) %>% 
  mutate(typo=as.character(typo))  %>%
  transmute(Commune = NOM_DEPCOM,
            Installation = nominstallation,
            Puissance_en_MW = puiss_MW,
            Type = typo,
            code_typo,
            Part_renouvelable = part_EnR,
            Mise_en_service = date_inst,
            DEP,REG,EPCI)

#définition carto gaz
inst_biogaz_carte<-inst_biogaz_france%>%st_sf() %>%st_set_crs(2154) %>% 
                                                   st_transform(crs = 4326) %>%
                                                   st_set_crs(4326) %>% 
                                                   #st_jitter(factor = 0.008) %>%
                                                    transmute(Commune = NOM_DEPCOM,
                                                    Installation = lib_installation,
                                                    Puissance_en_MW = capacite_de_production_gwh_an/8760*1000,
                                                    Production_en_Gwh=quantite_annuelle_injectee_en_mwh/1000,
                                                    Type = "Biogaz",
                                                    code_typo="biogaz",
                                                    Part_renouvelable = 1,
                                                    Mise_en_service = date_de_mes,
                                                    DEP,REG,EPCI)

# merge des deux :

dta<-dta%>%bind_rows(inst_biogaz_carte)



# inst_reg_france_biogaz<-inst_biogaz%>%select(any_of(names(inst_reg_france)),nom_du_projet,lib_installation,date_de_mes,
#                                              capacite_dinjection_au_31_12_en_nm3_h,quantite_annuelle_injectee_en_mwh,NOM_IRIS)

liste_regions <- inst_reg %>%
  select(REG, NOM_REG) %>%
  filter (! duplicated(REG)) %>%
  drop_na(REG) %>%
  mutate(TypeZone = "Région", REG = paste0("R",REG)) %>%
  rename(CodeZone = REG, Zone = NOM_REG) %>%
  arrange(Zone) 

liste_departements <- inst_reg %>%
  select(DEP, NOM_DEP) %>%
  filter(! duplicated(DEP)) %>%
  drop_na(DEP) %>%
  mutate(TypeZone = "Département", DEP = paste0("D", DEP)) %>%
  rename(CodeZone = DEP, Zone = NOM_DEP) %>%
  arrange(Zone)

liste_epci <- inst_reg %>%
  select(EPCI, NOM_EPCI) %>%
  filter(! duplicated(EPCI)) %>%
  drop_na(EPCI) %>%
  mutate(TypeZone = "Epci") %>%
  rename(CodeZone = EPCI, Zone = NOM_EPCI) %>%
  arrange(Zone)

liste_zone_complete_france <- rbind(liste_regions[,c("CodeZone", "TypeZone", "Zone")], liste_departements[,c("CodeZone", "TypeZone", "Zone")], liste_epci[,c("CodeZone", "TypeZone", "Zone")])
liste_zone_complete_france$CodeZone <- as.factor(liste_zone_complete_france$CodeZone)
liste_zone_complete_france$TypeZone <- as.factor(liste_zone_complete_france$TypeZone)
liste_zone_complete_france$Zone <- as.factor(liste_zone_complete_france$Zone)

#tables des zones uniques

unique_reg_dep <- inst_reg %>%
  select(REG, NOM_REG, DEP, NOM_DEP) %>%
  distinct(REG, NOM_REG, DEP, NOM_DEP) %>%
  drop_na(REG) %>%
  mutate(DEP = paste0("D", DEP), REG = paste0("R", REG)) %>%
  arrange(NOM_REG, NOM_DEP)

unique_dep_epci <- inst_reg %>%
  select(REG, NOM_REG, DEP, NOM_DEP, EPCI, NOM_EPCI) %>%
  distinct(REG, NOM_REG, DEP, NOM_DEP, EPCI, NOM_EPCI) %>%
  drop_na(DEP) %>%
  mutate(DEP = paste0("D", DEP), REG = paste0("R", REG)) %>%
  arrange(NOM_REG, NOM_DEP, NOM_EPCI)


#carto

ign = 'https://wxs.ign.fr/essentiels/geoportail/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=normal&TILEMATRIXSET=PM&FORMAT=image/png&LAYER=GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}' # noqa

carto_reg_france <- carto_reg %>%
  mutate(REG = paste0("R", REG))

carto_reg_france_hors_OM <- carto_reg_france %>%
  filter(REG != "R01" & REG != "R02" & REG != "R03" & REG != "R04" & REG != "R05" & REG != "R06")


carto_dep_france <- carto_dep %>%
  mutate(DEP = paste0("D", DEP))

carto_epci_france <- carto_epci %>%
  select(EPCI, Zone, geometry)

carto_reg_dep_epci_france <- rbind(
  carto_reg_france %>%
    rename(CodeZone = REG, Zone = NOM_REG),
  carto_dep_france %>%
    rename(CodeZone = DEP, Zone = NOM_DEP),
  carto_epci_france %>%
    rename(CodeZone = EPCI)
)

#indic_registre

### ---- Objectifs

objectifs$annee <- as.numeric(as.character(objectifs$annee))
objectifs<-objectifs%>%mutate(prod_MWh_an=valeur)
objectifs<-objectifs%>%mutate(code_typo=Filiere.de.production)

#ajout des installations de biogaz (ne pas confondre avec les aggrégats biogaz)




########### fonctions utilitaires #######################

is_not_empty <- function(x) all(!is.na(x)&x!="")
is_pas_zero  <- function(x) {
  if(is.numeric(x)) {all(x!=0)}
  else {TRUE}
}

format_fr_nb <- function(x, dec = 1, big_mark, sign=FALSE) {
  attempt::stop_if_not(x, is.numeric, msg = "x n'est pas un nombre, revoyez la saisie de l'argument de format_fr_pct(x, dec)")
  if(missing("big_mark")) {big_mark <- "\u202f"}
  res <- format(x, scientific = FALSE, big.mark = big_mark, 
                decimal.mark = ",", justify = "right", digits = dec)
  
  return(res)
}

# fonctions ggplot--------------------------------

theme_TEO <- theme_bw() %+replace% 
  theme(panel.grid = element_blank(), legend.position = "bottom", plot.margin = margin(0,0,0,0, unit="pt"),
        text = element_text(family = "sans", color = "black", face="plain", size = 15, hjust=0.5, vjust=0.5, angle=0, 
                            lineheight=1, margin=margin(0,0,0,0, unit="pt"), debug=F), 
        axis.text = element_text(size = 14),
        panel.background = element_blank(), legend.text = element_text(size = 14), legend.margin = margin(0,0,0,0, unit="pt"),
        legend.direction="horizontal") 

theme_TEO_carto <- theme_TEO %+replace% theme(axis.text = element_blank(), axis.ticks=element_blank(), axis.line=element_blank(),
                                              strip.background=element_blank(), panel.grid = element_blank(), panel.border = element_blank(),
                                              legend.position = "bottom")


ggteo <- function(data) {
  ggplot(data) + theme_TEO + 
    labs(title=element_blank(), x=element_blank(), y=element_blank(), colour = NULL#, fill=NULL
    )
}

theme_set(theme_TEO)

girafeTEO <- function(plot, fill_tooltip=TRUE) {
  girafe(print(plot), width_svg = 9, height_svg = 6, pointsize=14)  %>% 
    girafe_options(opts_tooltip(use_fill = fill_tooltip, opacity = .8),
                   opts_toolbar(position = "bottomright", saveaspng = TRUE),
                   sizingPolicy(browser.defaultWidth = "100%", viewer.defaultWidth = "100%",
                                browser.defaultHeight = 400, viewer.defaultHeight = 400, 
                                browser.padding = 1, viewer.padding = 0,
                                browser.fill = TRUE, viewer.fill = TRUE),
                   opts_sizing(rescale = T))
}
#### Couleurs -------------------

cols<-c("Eolien "="#479CB7", 
        "Cogénération électrique"="#003F5C", 
        "Photovoltaïque"="#B63D41",
        "Biogaz"="#006400")

col_registre <- c("#479CB7", #eol_ter 1
                  "#003F5C" , #metha 2
                  "#B63D41", #pvq 3
                  "#006400", #eol ter 4
                  rgb(0, 163, 224, maxColorValue =255), #hydro 5
                  rgb(72, 162, 63, maxColorValue =255), #metha 6
                  rgb(234, 170, 0, maxColorValue =255), #pv 7
                  rgb(0, 0, 0),
                  rgb(0, 0, 0),
                  rgb(0, 0, 0),
                  rgb(0, 0, 0),
                  rgb(0, 0, 0),
                  rgb(0, 0, 0),
                  rgb(0, 0, 0)
) %>%
  setNames(levels(inst_reg_france$typo))

col_enedis <- c("Bio Energie" = col_registre[[6]], "Eolien"=col_registre[[4]], 
                "Hydraulique" = col_registre[[5]], "Photovoltaïque"=col_registre[[7]])

col_biogaz <- c("Déchets ménagers" = col_registre[[2]],
                "Station d'épuration" = col_registre[[5]],
                "Agricole" = col_registre[[6]],
                "Territorial" = col_registre[[7]],
                "Industriel" = col_registre[[4]])

col_ter <- "#01a2d9"



# carte des installations-------------------


#carte de france IGN vide
base<-mapview(filter(carto_reg_france,!REG %in% c("R01", "R02", "R03", "R04", "R05", "R06")),legend=FALSE, alpha = 1,
              col.regions="green", alpha.regions=0.15, homebutton=F, label=NULL, layer.name="Contours")

base@map<-base@map%>%addTiles(
  urlTemplate = "https://wxs.ign.fr/essentiels/geoportail/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=normal&TILEMATRIXSET=PM&FORMAT=image/png&LAYER=GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}",
  options = WMSTileOptions(tileSize = 256),
  group = "IGN") %>%
  addProviderTiles("GeoportailFrance.ignMaps", group = "ESRI Topo")



couche_fil <- function(dta, col, legende = FALSE) {
    carte_fil <- mapview(dta, zcol="type", col.regions=col_registre[[col]], map.types = c("GeoportailFrance.ignMaps"), legend=legende, #label=tiquette,
                         popup = popupTable(dta, zcol=c("commune","installation","puissance_en_MW","type","part_renouvelable","mise_en_service")),
                         cex="puissance_en_MW", alpha = 0, layer.name=dta$type[1],
                         homebutton = FALSE)
  
  carte_fil
}

#fonction pour le gaz, je n'aime pas l'avoir dans une autre base MAIS le biogaz n'a pas les mêmes concepts ni unités donc 
#impossible d'utiliser la même fonction carto


couche_fil_gaz <- function(dta, col = 4, legende = FALSE) {
    carte_fil <- mapview(dta, col.regions=col_biogaz[[col]], map.types = c("GeoportailFrance.ignMaps"),
                         legend=legende,# label=tiquette_gaz, 
                         popup = popupTable(dta, zcol=c("commune", "installation", "type", "mise_en_service", "capacite_en_GWh_par_an", 
                                                         paste0("production_injectee_en_MWh_", mil), "iris")),
                         cex = "capacite_en_GWh_par_an", alpha = 0,
                         layer.name="Biogaz",
                         homebutton = FALSE)
    carte_fil
  } 


tag_style_titre <- tags$style(HTML("
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 0%;
    text-align: left;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 18px;
    }"))




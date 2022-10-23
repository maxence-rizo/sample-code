shinyServer(function(session, input, output) {
  
  
  
  #### intéractivité via requête URL-------------------
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query$fil)) {
      updateTabItems(session, inputId = "menu", selected = as.character(query$fil)) 
    }
    if(!is.null(query$dpt)) {
      updateSelectInput(session, inputId = "mon_dept", selected = query$dpt) 
    }
  })
  
  ########## interactivité pour puissance
  
  observe({
    query_puiss <- parseQueryString(session$clientData$url_search)
    if(!is.null(query_puiss$fil)) {
      updateTabItems(session, inputId = "menu_puiss", selected = as.character(query_puiss$fil)) 
    }
    if(!is.null(query_puiss$dpt)) {
      updateSelectInput(session, inputId = "mon_dept_puiss", selected = query_puiss$dpt) 
    }
  })
  
  ########## interactivité pour production
  
  observe({
    query_prod <- parseQueryString(session$clientData$url_search)
    if(!is.null(query_prod$fil)) {
      updateTabItems(session, inputId = "menu_prod", selected = as.character(query_prod$fil)) 
    }
    if(!is.null(query_prod$dpt)) {
      updateSelectInput(session, inputId = "mon_dept_prod", selected = query_prod$dpt) 
    }
  })
  
  ########## interactivité pour installations
  
  observe({
    query_inst <- parseQueryString(session$clientData$url_search)
    if(!is.null(query_inst$fil)) {
      updateTabItems(session, inputId = "menu_inst", selected = as.character(query_inst$fil)) 
    }
    if(!is.null(query_inst$dpt)) {
      updateSelectInput(session, inputId = "mon_dept_inst", selected = query_inst$dpt) 
    }
  })
  
  ######-----------------------------
  
  reactive({
    req(input$mon_dept)
    epci_dep <- filter(liste_zones_dep, dep==input$mon_dept | dep==input$mon_epci) %>% unnest()
  })
  
  
  #-------------- Menu déroulant page d'accueil
  
  output$dep_selec <-renderUI({
    req(input$ma_reg)
    affiche_dep <- append(
      input$ma_reg,
      filter(unique_reg_dep, REG == input$ma_reg) %>% pull(DEP) %>% as.character()
    ) %>%
      setNames(
        append(
          "Tous les départements",
          filter(unique_reg_dep, REG == input$ma_reg) %>% pull(NOM_DEP)
        )%>% as.character()
      )
    
    selectInput(inputId ="mon_dep", label="sélectionner un département",
                choices = affiche_dep,
                selected=NULL) 
  }) 
  
  ###################
  
  output$epci_selec <-renderUI({
    req(input$mon_dep)
    if (input$mon_dep == input$ma_reg) {
      affiche_epci <- append(
        input$mon_dep,
        filter(unique_dep_epci, REG == input$mon_dep) %>% pull(EPCI) %>% as.character() 
      ) %>%
        setNames(
          append(
            "Tous les EPCI",
            filter(unique_dep_epci, REG == input$mon_dep) %>% pull(NOM_EPCI)
          ) %>% as.character()
        )
    }
    else {
      affiche_epci <- append(
        input$mon_dep,
        filter(unique_dep_epci, DEP == input$mon_dep) %>% pull(EPCI) %>% as.character() 
      ) %>%
        setNames(
          append(
            "Tous les EPCI",
            filter(unique_dep_epci, DEP == input$mon_dep) %>% pull(NOM_EPCI)
          ) %>% as.character()
        )
    }
    
    selectInput(inputId ="mon_epci", label="sélectionner un EPCI",
                choices = affiche_epci,
                selected=NULL) 
  }) 
  
  
  
  #------------menu déroulant puissance 
  
  output$dep_selec_puiss <-renderUI({
    req(input$ma_reg)
    affiche_dep_puiss <- append(
      input$ma_reg,
      filter(unique_reg_dep, REG == input$ma_reg) %>% pull(DEP) %>% as.character()
    ) %>%
      setNames(
        append(
          "Tous les départements",
          filter(unique_reg_dep, REG == input$ma_reg) %>% pull(NOM_DEP)
        )%>% as.character()
      )
    
    selectInput(inputId ="mon_dep_puiss", label="sélectionner un département",
                choices = affiche_dep_puiss,
                selected=NULL) 
  }) 
  
  ###################
  
  
  output$epci_selec_puiss <-renderUI({
    req(input$mon_dep_puiss)
    if (input$mon_dep_puiss == input$ma_reg) {
      affiche_epci_puiss <- append(
        input$mon_dep_puiss,
        filter(unique_dep_epci, REG == input$mon_dep_puiss) %>% pull(EPCI) %>% as.character() 
      ) %>%
        setNames(
          append(
            "Tous les EPCI",
            filter(unique_dep_epci, REG == input$mon_dep_puiss) %>% pull(NOM_EPCI)
          ) %>% as.character()
        )
    }
    else {
      affiche_epci_puiss <- append(
        input$mon_dep_puiss,
        filter(unique_dep_epci, DEP == input$mon_dep_puiss) %>% pull(EPCI) %>% as.character() 
      ) %>%
        setNames(
          append(
            "Tous les EPCI",
            filter(unique_dep_epci, DEP == input$mon_dep_puiss) %>% pull(NOM_EPCI)
          ) %>% as.character()
        )
    }
    
    selectInput(inputId ="mon_epci_puiss", label="sélectionner un EPCI",
                choices = affiche_epci_puiss,
                selected=NULL) 
  }) 
  
  
  #-------------menu déroulant production 
  
  
  output$dep_selec_prod <-renderUI({
    req(input$ma_reg)
    affiche_dep_prod <- append(
      input$ma_reg,
      filter(unique_reg_dep, REG == input$ma_reg) %>% pull(DEP) %>% as.character()
    ) %>%
      setNames(
        append(
          "Tous les départements",
          filter(unique_reg_dep, REG == input$ma_reg) %>% pull(NOM_DEP)
        )%>% as.character()
      )
    
    selectInput(inputId ="mon_dep_prod", label="sélectionner un département",
                choices = affiche_dep_prod,
                selected=NULL) 
  }) 
  
  
  ###################
  
  output$epci_selec_prod <-renderUI({
    req(input$mon_dep_prod)
    if (input$mon_dep_prod == input$ma_reg) {
      affiche_epci_prod <- append(
        input$mon_dep_prod,
        filter(unique_dep_epci, REG == input$mon_dep_prod) %>% pull(EPCI) %>% as.character() 
      ) %>%
        setNames(
          append(
            "Tous les EPCI",
            filter(unique_dep_epci, REG == input$mon_dep_prod) %>% pull(NOM_EPCI)
          ) %>% as.character()
        )
    }
    else {
      affiche_epci_prod <- append(
        input$mon_dep_prod,
        filter(unique_dep_epci, DEP == input$mon_dep_prod) %>% pull(EPCI) %>% as.character() 
      ) %>%
        setNames(
          append(
            "Tous les EPCI",
            filter(unique_dep_epci, DEP == input$mon_dep_prod) %>% pull(NOM_EPCI)
          ) %>% as.character()
        )
    }
    
    selectInput(inputId ="mon_epci_prod", label="sélectionner un EPCI",
                choices = affiche_epci_prod,
                selected=NULL) 
  }) 
  
  
  ##############me déroulant installation #############
  
  
  output$dep_selec_inst <-renderUI({
    req(input$ma_reg)
    affiche_dep_inst <- append(
      input$ma_reg,
      filter(unique_reg_dep, REG == input$ma_reg) %>% pull(DEP) %>% as.character()
    ) %>%
      setNames(
        append(
          "Tous les départements",
          filter(unique_reg_dep, REG == input$ma_reg) %>% pull(NOM_DEP)
        )%>% as.character()
      )
    
    selectInput(inputId ="mon_dep_inst", label="sélectionner un département",
                choices = affiche_dep_inst,
                selected=NULL) 
  }) 
  
  
  ###########
  
  output$epci_selec_inst <-renderUI({
    req(input$mon_dep_inst)
    if (input$mon_dep_inst == input$ma_reg) {
      affiche_epci_inst <- append(
        input$mon_dep_inst,
        filter(unique_dep_epci, REG == input$mon_dep_inst) %>% pull(EPCI) %>% as.character() 
      ) %>%
        setNames(
          append(
            "Tous les EPCI",
            filter(unique_dep_epci, REG == input$mon_dep_inst) %>% pull(NOM_EPCI)
          ) %>% as.character()
        )
    }
    else {
      affiche_epci_inst <- append(
        input$mon_dep_inst,
        filter(unique_dep_epci, DEP == input$mon_dep_inst) %>% pull(EPCI) %>% as.character() 
      ) %>%
        setNames(
          append(
            "Tous les EPCI",
            filter(unique_dep_epci, DEP == input$mon_dep_inst) %>% pull(NOM_EPCI)
          ) %>% as.character()
        )
    }
    
    selectInput(inputId ="mon_epci_inst", label="sélectionner un EPCI",
                choices = affiche_epci_inst,
                selected=NULL) 
  }) 
  
  
  ######################################
  
  
  maille_terr <- reactive({
    req(input$mon_epci)
    filter(liste_zone_complete_france, CodeZone == input$mon_epci) %>%
      pull(TypeZone) %>% as.character()
  }) 
  
  lib_terr <- reactive({
    req(input$mon_epci)
    filter(liste_zone_complete_france, CodeZone == input$mon_epci) %>%
      pull(Zone) %>% as.character()
  })
  
  liste_ter <- reactive ({
    req(input$mon_epci)
    filter(liste_zone_complete_france, CodeZone %in% c(reg, input$mon_dept, input$mon_epci)) %>%
      arrange(desc(row_number()))
  })
  
  liste_filiere <- reactive ({
    req(input$choix_filiere)
    filter(indic_registre_france, variable %in% input$choix_filiere) %>%
      arrange(desc(row_number()))
  })
  
  ter_evol <- reactive ({
    req(input$mon_epci)
    if(maille_terr()=="Départements") {
      filter(liste_zone_complete_france, CodeZone %in% c(setdiff(liste_dep, input$mon_epci))) %>%
        bind_rows(filter(liste_zone_complete_france, CodeZone == input$mon_epci))
    } else { liste_ter() }
  })
  
  leg_evol <- reactive({
    req(input$mon_epci)
    if(maille_terr()!="Epci") "Source : ENEDIS jusque 2017 puis registre" else "indice 100 - Source : ENEDIS jusque 2017 puis registre"
  })
  
  leg_evol_MW <- reactive({
    req(input$mon_epci)
    if(maille_terr()!="Epci") "MW - Source : ENEDIS jusque 2017 puis registre " else "indice 100 - Source : ENEDIS jusque 2017 puis registre"
  })
  
  

  # affichage du nom du territoire---------------------------
  output$nom_terr <- renderText({
    req(input$mon_dept)
    lib_terr()})
  
  output$nom_terr2 <- renderText({
    req(input$mon_dept)
    lib_terr()})
  
  output$nom_terr3 <- renderText({
    req(input$mon_dept)
    lib_terr()})
  
  output$nom_terr4 <- renderText({
    req(input$mon_dept)
    lib_terr()})
  
  output$nom_terr5 <- renderText({
    req(input$mon_dept)
    lib_terr()})
  
  output$nom_terr6 <- renderText({
    req(input$mon_dept)
    lib_terr()})
  
  output$nom_terr7 <- renderText({
    req(input$mon_dept)
    lib_terr()})
  
  ##########------------CARTE-----------------------
  
  #creation de la map
  
  # input<-list()
  # input$mon_epci="D01"
  # input$choix_filiere_carte=c("eol_ter","metha")
  # input$ma_reg="F"

  contours <- reactive({
    if (input$ma_reg == "France") {
      cont <- carto_reg_france_hors_OM %>% st_geometry %>% st_set_crs(2154) %>% st_transform(crs = 4326) %>%
        st_set_crs(4326) %>% 
        st_jitter(factor = 0.008)
    }else {
      cont <- filter(carto_reg_dep_epci_france, CodeZone == input$mon_dep) %>% st_geometry %>% st_set_crs(2154)%>% st_transform(crs = 4326) %>%
        st_set_crs(4326) %>% 
        st_jitter(factor = 0.008)
    }
    
    mapview(cont, legend=FALSE, map.types = mapviewGetOption("Esri.WorldImagery"), col="gray", alpha = 1,
            col.regions="papayawhip", alpha.regions=0.15, homebutton=TRUE, label=NULL, layer.name="Contours")
    
  })
# 
#     nom_couche <- dta$Type[1]
#     tiquette <- paste0(dta$Puissance_en_MW, " MW", " (", dta$Type, ")")
#   
  
  foundational.map<-reactive({
    if(is.null(input$choix_filiere_carte)){base}
    else{
    req(input$mon_epci,input$choix_filiere_carte,input$ma_reg)
    #définition des données pour la carte 
        if(#test
          input$ma_reg == "France"){
          #yes : enlever les OM       
          carto<-filter(dta,code_typo%in%input$choix_filiere_carte,
                 !REG %in% c("R01", "R02", "R03", "R04", "R05", "R06"))} else{
                   #no filtrer sur l'EPCI coché     
                   carto<-filter(dta,code_typo%in%input$choix_filiere_carte,REG==input$mon_epci|DEP==input$mon_epci|EPCI==input$mon_epci)}
    
 colorcarte<-mapviewColors(carto,colors=c("#479CB7","#B63D41","#006400","#003F5C"),at=c("biogaz","eol_ter","metha","pvq"))
      
 carte<-mapview(carto,zcol="Type",col.regions=sf.colors,maxpoint=1000,legend=T)
      
      # il faudra vraiment qu'on explique ce que cet opérateur C++ fait ici!
      
    `%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))
     contours <- (contours())
   contours %+=% carte
##### c'est ici que l'on choisit l'origine des tiles (IGN) le défaut étant openstreetmap
    carte<- carte@map %>%
      addFullscreenControl()  %>%
      addTiles(
        urlTemplate = "https://wxs.ign.fr/essentiels/geoportail/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=normal&TILEMATRIXSET=PM&FORMAT=image/png&LAYER=GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}",
        options = WMSTileOptions(tileSize = 256),
        group = "IGN") %>%
      addProviderTiles("GeoportailFrance.ignMaps", group = "ESRI Topo")}
    
    
  }) #fin de la définition de la carte
  
  
 # Rendu dela carte en fonction
  
  output$map <- 
    renderLeaflet({
      if(is.null(input$choix_filiere_carte)){
#Ici la map "Base" qui est utilisée pour une carte vide n'est pas du même type que 
#foundational map il est donc nécéssaire d'employer l'argument @map pour faire référence à
#la partie leaflet tandis que foundational map doit être utilisée tel quel.
        foundational.map()@map}
      else{foundational.map()}
      })
 
  
  
  # store the current user-created version
  # of the Leaflet map for download in 
  # a reactive expression
  user.created.map <- reactive({
    
    # call the foundational Leaflet map
    foundational.map() %>%
      
      # store the view based on UI
      setView( lng = input$map_center$lng
               ,  lat = input$map_center$lat
               , zoom = input$map_zoom
      )
    
  })
  
  
  ####download map###
  
  # create the output file name
  # and specify how the download button will take
  # and save as a PDF
  output$dl<- downloadHandler(
    filename = paste("Carte_ENR", '.png', sep=''),
     content = function(file) {
      mapshot( x = user.created.map()
               , file = file
               , cliprect = "viewport" # the clipping rectangle matches the height & width from the viewing port
               , selfcontained = FALSE # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
      )
    } # end of content() function
  ) # end of downloadHandler() function
    
  
  ####Table installations avec téléchargements, l'usage de DT permet de se passer d'UI
  
  #definition du reactive
  tab_glob<-reactive({
    req(input$choix_filiere_carte)
    dta%>%as.data.frame%>%filter(code_typo==input$choix_filiere_carte)%>%dplyr::select(Commune, Installation, 'Puissance (MW)'=Puissance_en_MW,Type,
                                                                                       'Production annuelle (GWh)'= Production_en_Gwh, 'Part renouvelable'=Part_renouvelable,
                                                                                       'Mise en service'=Mise_en_service,-geometry) %>%
      arrange(Commune,desc(`Puissance (MW)`), Type, Installation)
    
  })
  #elements de personalisation
  output$tab_glob <- DT::renderDataTable(
    tab_glob(),
    extensions = 'Buttons', rownames = FALSE,
    options = list(dom = 'Bfrtip', pageLength = 10, language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'),
                   #bouton de téléchargement intégré
                   buttons = list(c(list(extend = 'csv', file=paste0(Sys.Date(), '-registre_elec_3112', mil, '-', input$mon_epci, '.csv')))))
     
  )
  
  
  
  
  
  
   #--------------------------TUILES---------------------------------------------------

  output$boxEol2_toute_france <- renderValueBox({
    req(input$mon_epci)
    
    if (input$mon_epci == "France"){
      valueBox(value = filter(odre_complet_V5,code_typo %in% input$choix_filiere_carte,TypeZone=="Region",annee==mil) %>%ungroup%>%
                 summarise(valeur = sum(`Nombre de sites`,na.rm=T)) %>%
                 pull(valeur)%>%
                 prettyNum(big.mark=" ", decimal.mark=","),
               subtitle=paste0("installations actives en ", mil),
               color="blue"
      )
    }
    else {
      valueBox(value = filter(odre_complet_V5,CodeZone== input$mon_epci,code_typo %in% input$choix_filiere_carte) %>%ungroup%>%
                 summarise(valeur= sum(`Nombre de sites`,na.rm=T)) %>%
                 pull(valeur)%>%
                 prettyNum(big.mark=" ", decimal.mark=","),
               subtitle=paste0("installations actives en ", mil),
               color="blue" #icon = icon("chart-area")
               
      )
    }
  })

  
  output$boxEol3_toute_france <- renderValueBox({
    req(input$mon_epci)
    if (input$mon_epci == "France"){
      valueBox(
        value=filter(odre_complet_V5, annee==mil,code_typo %in% input$choix_filiere_carte,TypeZone=="Region") %>%
          summarise(valeur= sum(Energie.totale.en.MWh.an,na.rm=T)/1000) %>% 
          pull(valeur)%>%
          round(digits=1) %>% prettyNum(big.mark=" ", decimal.mark=",") %>%
          paste0(" GWh"),
        subtitle=paste0("GWh produit en ", mil),  width=100,
        color="blue"#, icon = icon("chart-area")
      )
    }
    else {
      valueBox(
        value=filter(odre_complet_V5,annee==mil, CodeZone==input$mon_epci, annee==mil,code_typo %in% input$choix_filiere_carte) %>%
          summarise(valeur= sum(Energie.totale.en.MWh.an,na.rm=T)/1000) %>% 
          pull(valeur)%>%
          round(digits=1) %>% prettyNum(big.mark=" ", decimal.mark=",") %>%
          paste0(" GWh"),
        subtitle=paste0("GWh produit en ", mil),  width=100,
        color="blue"#, icon = icon("chart-area")
      )
    }
  })
  

  output$boxEol4_toute_france <- renderValueBox({
    req(input$mon_epci)
    if (input$mon_epci == "France"){
      valueBox(
        value=filter(odre_complet_V5,annee==mil,code_typo %in% input$choix_filiere_carte,TypeZone=="Region") %>%ungroup%>%
          summarise(valeur= sum(Puissance.totale.en.MW,na.rm=T))%>% round(digits=0) %>% prettyNum(big.mark=" ", decimal.mark=",") %>%
          paste0(" MW"),
        subtitle=paste0("puissance max en ",mil) ,
        color="blue"#, icon = icon("chart-area")
      )
    }
    else {
      valueBox(
        value=filter(odre_complet_V5,annee==mil, CodeZone==input$mon_epci,code_typo %in% input$choix_filiere_carte) %>%ungroup()%>%
          summarise(valeur= sum(Puissance.totale.en.MW,na.rm=T))%>%pull(valeur)%>% round(digits=1) %>% prettyNum(big.mark=" ", decimal.mark=",") %>%
          paste0(" MW"),
        subtitle=paste0("puissance max en ",mil),
        color="blue"#, icon = icon("chart-area")
      )
    }
  }) 
  
 
  ############ Graphe Puissance ###################
  
  puiss<-reactive({    
    req(input$mon_epci, input$choix_filiere_bar_puiss)
    #definition data pour adapter france entier ou régions
    puiss<- if(input$mon_epci!="France")
    {filter(odre_complet_V5, CodeZone==input$mon_epci,
            code_typo %in% input$choix_filiere_bar_puiss)
    } else {filter(odre_complet_V5,
                   code_typo %in% input$choix_filiere_bar_puiss,TypeZone=="EPCI")%>%group_by(annee,code_typo)%>%summarise(Puissance.totale.en.MW=sum(Puissance.totale.en.MW,na.rm=T),Zone="France")}
  })
  
  output$bar_puiss_eol <- renderGirafe ({
    validate(need(nrow(puiss())>0,"Aucune installation dans cette zone"))
    girafeTEO(
      puiss()%>%
      ggteo() + expand_limits(y = 1) +
      geom_bar_interactive(aes(x = annee, y = Puissance.totale.en.MW, fill = code_typo,
                               tooltip=paste0(round(Puissance.totale.en.MW, 1), " MW")
      ),
      position="stack",
      stat = "identity"
      )+ labs(x="Année", y="Puissance en MW") + 
      theme(panel.grid.major = element_line(
        size = (0.2), colour =
          "grey"
      ))        +
      scale_fill_manual_interactive("légende", values = c(biogaz="#479CB7", 
                                             eol_ter="#003F5C", 
                                             metha="#006400",
                                             pvq="#B63D41"
                                                                                        )
      )+
        ggtitle("Puissance installée chaque année"))

    
  })
  
  
  #graphique évolution puissance
  
  output$bar_evol_puiss<-renderGirafe ({
    #error handler
    validate(need(nrow(puiss())>0,"Aucune installation dans cette zone"))
# appel au réactive puiss (cf supra)
        puiss<-puiss()%>%group_by(code_typo,Zone)%>%mutate(Evolution=100*(Puissance.totale.en.MW-lag(Puissance.totale.en.MW,order_by = annee))/Puissance.totale.en.MW)
    girafeTEO(
      puiss%>%
        ggteo() + expand_limits(y = 1) +
        geom_bar_interactive(aes(x = annee, y = Evolution, fill = code_typo,
                                 tooltip=paste0(round(Evolution, 1), "%")
        ),
        position="dodge",
        stat = "identity"
        )+ labs(x="Année", y="Evolution de la puissance installée") + 
        theme(panel.grid.major = element_line(
          size = (0.2), colour =
            "grey"
        ))        +
        scale_fill_manual_interactive("légende", values = c(biogaz="#479CB7", 
                                                            eol_ter="#003F5C", 
                                                            metha="#006400",
                                                            pvq="#B63D41"
        )
        )+
        ggtitle("Croissance de la puissance installée chaque année")
    )
    
  })
  
  ####### Table puissance  ##################
  
  output$tab_filieres_puiss <- function() ({
    req(input$mon_epci, input$choix_filiere_bar_puiss)
    puiss_t<- if(input$mon_epci!="France")
    {filter(odre_complet_V5, CodeZone==input$mon_epci,
            code_typo %in% input$choix_filiere_bar_puiss)
    } else {filter(odre_complet_V5,
                   code_typo %in% input$choix_filiere_bar_puiss,TypeZone=="Region")%>%group_by(annee,code_typo)%>%summarise(Puissance.totale.en.MW=sum(Puissance.totale.en.MW,na.rm=T),Zone="France")}
      
    #affichage nom complets
    puiss_t$code_typo<-puiss_t$code_typo%>%str_replace_all(codetoword)
    #tableau
      puiss_t %>%mutate(Puissance_MW=round(Puissance.totale.en.MW,2))%>%
      select(Année=annee,Energie=code_typo,Puissance_MW,Zone)%>%
      #spread(key = CodeZone, value=valeur) %>%
      kable(align="c", row.names=F) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive"), full_width = T) %>%
      row_spec(0,  color = "black", bold = "T"
      )
    
  })
 #téléchargements de la table complète à 3 endroits
  
  data_puissance<-reactive(odre_complet_V5)
  
  output$fic_prod<-downloadHandler(
    filename = "ENR_ODRE.csv", 
    content = function(fname){
      write.csv(data_puissance(), fname)
    }
  )
  
  output$fic_puiss<-downloadHandler(
    filename = "ENR_ODRE.csv", 
    content = function(fname){
      write.csv(data_puissance(), fname)
    }
  )
  
  output$fic_inst<-downloadHandler(
    filename = "ENR_ODRE.csv", 
    content = function(fname){
      write.csv(data_puissance(), fname)
    }
  )
  
  ####### Table production  ##################
  
  output$tab_filieres_prod <- function() ({
    req(input$mon_epci, input$choix_filiere_bar_prod)
    
    prod_t<- if(input$mon_epci!="France")
    {filter(odre_complet_V5, CodeZone==input$mon_epci,
            code_typo %in% input$choix_filiere_bar_prod)%>%mutate(Production_Gwh=Energie.totale.en.MWh.an/1000)
    } else {filter(odre_complet_V5,code_typo %in% input$choix_filiere_bar_prod,TypeZone=="EPCI")%>%group_by(annee,code_typo)%>%summarise(Production_Gwh=sum(Energie.totale.en.MWh.an,na.rm=T)/1000,Zone="France")}
    
    # 
    # odre_complet_V5 %>%
    #   filter(CodeZone==input$mon_epci, code_typo %in% input$choix_filiere_bar_prod)%>%
    #   transmute(Année=annee,Zone=CodeZone,type=code_typo,Production_Gwh=Energie.totale.en.MWh.an/1000,DEP,REG)%>%
    prod_t$code_typo<-prod_t$code_typo%>%str_replace_all(codetoword)
    
    prod_t%>%arrange(annee)%>%select(Annee=annee,Energie=code_typo,Production_Gwh,Zone)%>% kable(align="c", row.names=F) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive"), full_width = T) %>%
      row_spec(0,  color = "black", bold = "T")  })
  
  
  ###### Graphe Production ############
  
  #data
  # #définitions des données, nécéssaire de filtrer sur france et typezone pour garder le EPCI car sinonles valeurs sont triplées : EPCI, région et départements apparaissent
  
  prod<- reactive( {if(input$mon_epci!="France")
  {odre_complet_V5%>%filter( CodeZone==input$mon_epci,code_typo %in% input$choix_filiere_bar_prod)%>%mutate(Energie.totale.en.GWh.an=Energie.totale.en.MWh.an/1000)
  } else {odre_complet_V5%>%mutate(Energie.totale.en.GWh.an=Energie.totale.en.MWh.an/1000)%>%
      filter(code_typo %in% input$choix_filiere_bar_prod,TypeZone=="EPCI")%>%group_by(annee,code_typo)%>%
      summarise(Energie.totale.en.GWh.an=sum(Energie.totale.en.GWh.an,na.rm=T),Zone="France")}})
  
  
  #les OBJECTIFS ne sont valide que pour le grand est sinon pas de données pour l'instant
  #Attention, la base OBJECTIFS est la seule ou la colonne valeur est conservée
  output$bar_prod_eol <- renderGirafe ({
    validate(need(nrow(prod())>0,"Aucune installation dans cette zone"))
    
    girafeTEO( 
            prod()    
    %>%
      
      ggteo() + expand_limits(y = 1) +
      geom_point_interactive(aes(x = annee, y = Energie.totale.en.GWh.an,col=code_typo, size = 8,
                                 tooltip=paste0(round(Energie.totale.en.GWh.an, 1), " GWh")
      ))
    
    + geom_line_interactive(aes(x = annee, y = Energie.totale.en.GWh.an,  group = code_typo, color = code_typo,
                                tooltip=paste0(round(Energie.totale.en.GWh.an, 1), " GWh")))
    #themes et esthétiques 
    
    
    + plot_layout(guides = "collect")
     +guides(color=guide_legend(nrow=1,
                               byrow=TRUE),
             title=NULL,
            size = "none",
            shape = guide_legend(title = NULL))+
      scale_color_manual_interactive("légende", values = c(biogaz="#479CB7", 
                                                          eol_ter="#003F5C", 
                                                          metha="#006400",
                                                          pvq="#B63D41"
      ))
    
    + theme(text = element_text(size = 15),
            axis.text = element_text(size = 15),
            panel.grid.major = element_line(
              size = (0.2),
              colour = "grey"
            ))   #ajout de lignes sur les objectifs SRADDET
    
    +{if ((input$ma_reg == "R44")
        & (length(input$choix_annees_objectifs) != 0)
        & (length(input$choix_objectifs) != 0)){
       geom_line_interactive(data = filter(objectifs,
                                            Filiere.de.production %in% input$choix_filiere_bar_prod,
                                            annee %in% input$choix_annees_objectifs),
                              linetype="longdash",
                              aes(x = annee, y = valeur,  group = code_typo, color = code_typo,
                                  tooltip=paste0(round(valeur, 1), " GWh")))
                                  
                                  
      }}
    
    )
  })

 #graphique évolution en % de la production
  output$bar_evol_prod<-renderGirafe ({
    validate(need(nrow(prod())>0,"Aucune installation dans cette zone"))
    
    #calcul evolution
    prod<-prod()%>%group_by(code_typo,Zone)%>%mutate(Evolution=100*(Energie.totale.en.GWh.an-lag(Energie.totale.en.GWh.an,order_by = annee))/lag(Energie.totale.en.GWh.an,order_by = annee))
    #rendu graphique
    girafeTEO(
      prod%>%
        ggteo() + expand_limits(y = 1) +
        geom_bar_interactive(aes(x = annee, y = Evolution, fill = code_typo,
                                 tooltip=paste0(round(Evolution, 1), "%")
        ),
        position="dodge",
        stat = "identity"
        )+ labs(x="Année", y="Evolution de la production totale") + 
        theme(panel.grid.major = element_line(
          size = (0.2), colour =
            "grey"
        ))        +
        scale_fill_manual_interactive("légende", values = c(biogaz="#479CB7", 
                                                            eol_ter="#003F5C", 
                                                            metha="#006400",
                                                            pvq="#B63D41"
        )
        )+
        ggtitle("Croissance de la production chaque année")
    )
    
  })
  
   
  ############ Graphe nombre installation ###################
  
  #reactive data
  
  inst<- reactive({    
  if(input$mon_epci!="France")
    {odre_complet_V5%>%filter( CodeZone==input$mon_epci,code_typo %in% input$choix_filiere_bar_inst)
    } else {odre_complet_V5%>%
        filter(code_typo %in% input$choix_filiere_bar_inst,TypeZone=="EPCI")%>%group_by(annee,code_typo)%>%
        summarise(`Nombre de sites`=sum(`Nombre de sites`,na.rm=T),Zone="France")}
  })
  
  
  output$bar_nb_inst <- renderGirafe ({
    req(input$mon_epci, input$choix_filiere_bar_inst)
    validate(need(nrow(inst())>0,"Aucune installation dans cette zone"))
    
    girafeTEO(inst()%>%
                ggteo() + expand_limits(y = 1) 
              + geom_bar_interactive(aes(x = annee, y = `Nombre de sites`, fill = code_typo ,
                                         tooltip=paste(`Nombre de sites`, "Installations")
              ),
              position="stack",
              stat = "identity"
              )+ labs(x="Années", y="Nombre d'installations")+ 
                scale_fill_manual_interactive("légende", values = c(biogaz="#479CB7", 
                                                                    eol_ter="#003F5C", 
                                                                    metha="#006400",
                                                                    pvq="#B63D41"
                ))
    )
    
  })
  
  #graphe évolution en %
  
  output$bar_evol_inst<-renderGirafe ({
    req(input$mon_epci, input$choix_filiere_bar_inst)
    validate(need(nrow(inst())>0,"Aucune installation dans cette zone"))
    
    #calcul evol
    
    inst<-inst()%>%group_by(code_typo,Zone)%>%mutate(Evolution=100*(`Nombre de sites`-lag(`Nombre de sites`,order_by = annee))/lag(`Nombre de sites`,order_by = annee))
    
    #rendu graphique
    girafeTEO(
      inst%>%
        ggteo() + expand_limits(y = 1) +
        geom_bar_interactive(aes(x = annee, y = Evolution, fill = code_typo,
                                 tooltip=paste0(round(Evolution, 1), "%")
        ),
        position="dodge",
        stat = "identity"
        )+ labs(x="Année", y="Croissance du nombre de sites") + 
        theme(panel.grid.major = element_line(
          size = (0.2), colour =
            "grey"
        ))        +
        scale_fill_manual_interactive("légende", values = c(biogaz="#479CB7", 
                                                            eol_ter="#003F5C", 
                                                            metha="#006400",
                                                            pvq="#B63D41"
        )
        )+
        ggtitle("Croissance du nombre de sites")
    )
    
  })
  
  ####### Table nombre d'installations  ##################
  
  output$tab_filieres_inst <- function() ({
    req(input$mon_epci, input$choix_filiere_bar_inst)
    inst_t<- if(input$mon_epci!="France")
    {odre_complet_V5%>%filter( CodeZone==input$mon_epci,code_typo %in% input$choix_filiere_bar_inst)
    } else {odre_complet_V5%>%
        filter(code_typo %in% input$choix_filiere_bar_inst,TypeZone=="EPCI")%>%group_by(annee,code_typo)%>%
        summarise(`Nombre de sites`=sum(`Nombre de sites`,na.rm=T),Zone="France")}
   
    inst_t$code_typo<- inst_t$code_typo%>%str_replace_all(codetoword)
    inst_t<-inst_t%>%select(Annee=annee,Energie=code_typo,`Nombre de sites`,Zone)
    inst_t%>%arrange(Energie)%>%
      #spread(key = CodeZone, value=valeur) %>%
      kable(align="c", row.names=F) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive"), full_width = T) %>%
      row_spec(0,  color = "black", bold = "T")  })
  
  



  
#############" téléchargements###########
  
  thedata <- reactive(odre_complet_V5)
  
  output$bouton_carte_inst_elec <- downloadHandler(
    filename = "Liste_des_installations_ENR.csv", 
    content = function(fname){
      write.csv(thedata(), fname)
    })
  
  
  
})


shinyUI(
  fluidPage(
    navbarPage(title = "Outil EnR", # personnalisation du thème,
               id = "navbar",
               selected = "Accueil",
               theme = "styles.css", 
               fluid = T,
               footer = tags$div(title="Your footer here", 
                                 align = "right", 
                                 position = 'absolute',
                                 bottom=0,
                                 width=100,
                                 height=50,
                                 color= 'blue',
                                 padding= 10),            
               #En-tête-----------------------------------------------------------------------------              
               fluidRow(
                 column(status="primary",
                        solidHeader = TRUE, width=3,
                        selectInput(inputId = "ma_reg",
                                    label = "Sélectionner une région",
                                    choices = append("France", filter(liste_zone_complete_france, TypeZone == "Région") %>%
                                                       pull(CodeZone) %>% as.character()) %>%
                                      setNames(append("Toutes les régions", filter(liste_zone_complete_france, TypeZone == "Région") %>% 
                                                        pull(Zone) %>% as.character())),selected = "FRANCE")
                 ),
                 column(status="primary",
                        solidHeader = TRUE, width=3,
                        uiOutput("dep_selec")
                 ),
                 
                 column(status="primary",
                        solidHeader = TRUE, width=3,
                        uiOutput("epci_selec")
                 )
               ),
               #Barre de menu-----------------------------------------------------------------------------              
               
               tabPanel(title = 'Accueil',
                        
                        fluidRow(
                          tags$h1("Bienvenue sur Outil EnR"),
                          tags$h5("Les énergies renouvelables en un coup d’oeil, de l’échelle nationale à l’EPCI :")
                        ),
                   
                        
                        fluidRow(   #2e ligne
                          # tags$br(),
                          valueBoxOutput("boxEol4_toute_france"),
                          valueBoxOutput("boxEol2_toute_france"),
                          valueBoxOutput("boxEol3_toute_france")
                        ),
                        #cases à cocher
                        fluidRow(
                          checkboxGroupInput("choix_filiere_carte", "Choisissez la/les filière(s):",
                                             inline = TRUE,
                                             choiceNames =
                                               list("Biogaz", "Eolien","Cogénération électrique", "Photovoltaïque"),
                                             choiceValues =
                                               list("biogaz","eol_ter", "metha", "pvq"),
                                             selected = "biogaz"
                          )
                        ) ,
                        fluidRow(
                          
                          column(status="primary", solidHeader = TRUE,
                                 width=9,
                                 title = span("Installations de production électrique EnR&R", stiyle="color:white"),
                                 leafletOutput(outputId = "map", height = 370),
                                 downloadButton("dl","carte en pdf"),
                                 style="color:black",
                                 span(paste0("Source : registre ODRE au 31/12/", mil), style="font-size: 12px")
                          )
                        ),
                        
        
                        fluidRow(   #5e ligne
                          column(status="primary", solidHeader = TRUE,
                                 width=12,
                                 title = span("Installations de production électrique EnR&R", style="color:white"),
                                 dataTableOutput("tab_glob") %>% withSpinner(type=1),
                                 style="color:black",
                                 span(paste0("Source : registre ODRE au 31/12/", mil), style="font-size: 12px")
                          )
                        )
                        
               ),
               
               
               #--------------------------Puissance -----------------------------------------------------------------------------              
               
               
               tabPanel(title = 'Puissance',
                        
                        #menu deroulant
                        
                        fluidRow(
                          tags$h1("Puissance maximum raccordée")),
                        
                       
                        
                        #cases à cocher
                        
                        fluidRow(
                          checkboxGroupInput("choix_filiere_bar_puiss", "Choisissez la/les filière(s):",
                                             inline = TRUE,
                                             choiceNames =
                                               list("Biogaz","Eolien ", "Cogénération électrique", "Photovoltaïque"),
                                             choiceValues =
                                               list("biogaz","eol_ter", "metha", "pvq"),
                                             selected = list("biogaz","eol_ter", "metha", "pvq")
                          )
                        ),
                        
                        #barplot
                        
                        fluidRow(
                          column(status="primary",
                                 solidHeader = TRUE, width=6,
                                 title = span("Productions annuelles", style="color:white"),
                                 girafeOutput("bar_puiss_eol",  width="100%", height=320),
                                 style="color:black",
                                 span(paste0("MW - Source : Registre ODRE au 31/12/", mil), style="font-size: 12px")
                          ),
                          column(status="primary",
                                 solidHeader = TRUE, width=6,
                                 title = span("Capacité de production annuelle", style="color:white"),
                                 girafeOutput("bar_evol_puiss",  width="100%", height=320),
                                 style="color:black",
                                 span(paste0("MW - Source : Registre ODRE au 31/12/", mil), style="font-size: 12px")
                          )
                        ),
                        
                        #Tableau
                        
                        fluidRow(   #4e ligne
                          column(status="primary",
                                 solidHeader = TRUE, width=5,
                                 title = span("Evolution des puissances installées (MW)", style="color:white"),
                                 tableOutput("tab_filieres_puiss"),
                                 style="color:black",
                                 span(paste0("MW - Source : Registre ODRE au 31/12/", mil), style="font-size: 12px"),
                                 downloadButton("fic_puiss", "Télécharger la table des données complètes en CSV")
                                 
                          )
                        )
               ),
               
               
               
               # -------------------------PRODUCTION---------------------------------------------------
               tabPanel(title = "Production",
                        
                        #menu déroulant
                        
                        fluidRow(
                          
                          tags$h1("Énergie produite : évolution")),
                        
                
                        
                        #cases à cocher 
                        
                        fluidRow(
                          checkboxGroupInput("choix_filiere_bar_prod", "Choisissez la/les filière(s):",
                                             inline = TRUE,
                                             choiceNames =
                                               list("Biogaz","Eolien ", "Cogénération électrique", "Photovoltaïque"),
                                             choiceValues =
                                               list("biogaz","eol_ter", "metha", "pvq"),
                                             selected = list("biogaz","eol_ter", "metha", "pvq")
                          )
                        ),
                        
                        fluidRow(
                          column(width=6, 
                                 checkboxGroupInput("choix_objectifs", "Objectifs du SRADDET:",
                                                    inline = TRUE,
                                                    choiceNames =
                                                      list("Afficher"),
                                                    choiceValues =
                                                      list("Afficher"),
                                                    selected = NULL
                                                      #list("Afficher")
                                                    )
                          ),
                          
                          column(width=6, 
                                 checkboxGroupInput("choix_annees_objectifs", "Année des objectifs:",
                                                    inline = TRUE,
                                                    choiceNames =
                                                      list("2012", "2021", "2030", "2050"),
                                                    choiceValues =
                                                      list("2012", "2021", "2030", "2050"),
                                                    selected = list("2012", "2021")
                                 )
                          )
                        ),
                        
                        #barplot production
                        
                        fluidRow(  
                          column(status="primary",
                                 solidHeader = TRUE, width=6,
                                 title = span("Productions maximales annuelles", style="color:white"),
                                 girafeOutput("bar_prod_eol",  width="100%", height=320),
                                 style="color:black",
                                 span(paste0("GWh - Source : Registre ODRE au 31/12/", mil), style="font-size: 12px")
                          ),
                          column(status="primary",
                                 solidHeader = TRUE, width=6,
                                 title = span("Productions annuelles", style="color:white"),
                                 girafeOutput("bar_evol_prod",  width="100%", height=320),
                                 style="color:black",
                                 span(paste0("GWh - Source : Registre ODRE au 31/12/", mil), style="font-size: 12px")
                          )
                        )
                        ,
                        
                        #tableau production
                        
                        fluidRow(   #4e ligne
                          column(status="primary",
                                 solidHeader = TRUE, width=5,
                                 title = span("Evolution des puissances installées (MW)", style="color:white"),
                                 tableOutput("tab_filieres_prod"),
                                 style="color:black",
                                 span(paste0("GWh - Source : Registre ODRE au 31/12/", mil), style="font-size: 12px"),
                                 downloadButton("fic_prod", "Télécharger la table des données complètes en CSV")
                                 
                          )
                        )
               ),
               
               ######################## installations #####################
               
               tabPanel(title = 'Installations en service',
                        
                        # menu déroulant 
                        
                        fluidRow(
                          tags$h1("Installations")),
                        
                        #cases à cocher
                        
                        fluidRow(
                          checkboxGroupInput("choix_filiere_bar_inst", "Choisissez la/les filière(s):",
                                             inline = TRUE,
                                             choiceNames =
                                               list("Biogaz","Eolien ", "Cogénération électrique", "Photovoltaïque"),
                                             choiceValues =
                                               list("biogaz","eol_ter", "metha", "pvq"),
                                             selected = list("biogaz","eol_ter", "metha", "pvq")
                          )
                        ),
                        
                        #graphe nombre d'installation 
                        
                        fluidRow(  
                          column(status="primary",
                                 solidHeader = TRUE, width=6,
                                 title = span("Nombres d'installation annuelles", style="color:white"),
                                 girafeOutput("bar_nb_inst",  width="100%", height=320),
                                 style="color:black",
                                 span(paste0("Source : registre ODRE au 31/12/", mil), style="font-size: 12px")
                          ),
                          column(status="primary",
                                 solidHeader = TRUE, width=6,
                                 title = span("Nombre d'installations", style="color:white"),
                                 girafeOutput("bar_evol_inst",  width="100%", height=320),
                                 style="color:black",
                                 span(paste0("Source : registre ODRE au 31/12/", mil), style="font-size: 12px")
                          )
                        ),
                        
                        #tableau 
                        
                        fluidRow(   #4e ligne
                          column(status="primary",
                                 solidHeader = TRUE, width=5,
                                 title = span("Evolution du nombre d'installations", style="color:white"),
                                 tableOutput("tab_filieres_inst"),
                                 style="color:black",
                                 span(paste0("Source : registre ODRE au 31/12/", mil), style="font-size: 12px"),
                                 downloadButton("fic_inst", "Télécharger la table des données complètes en CSV")
                                 
                          )
                       ) # ),
                        # 
                        # #Tableau par installation
                        # 
                        # fluidRow(   #5e ligne
                        #   column(status="primary", solidHeader = TRUE,
                        #          width=12,
                        #          title = span("Installations de production électrique EnR&R", style="color:white"),
                        #          dataTableOutput("tab_inst") %>% withSpinner(type=1),
                        #          style="color:black",
                        #          span(paste0("Source : registre ODRE au 31/12/", mil), style="font-size: 12px")
                        #   )
                        #)
               ),
               
################### nous connaitre ###################

               tabPanel(title = 'Nous connaître'     
               )
    )
  )
)

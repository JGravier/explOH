################################
# Shiny app pour afficher les objets selon le temps
# mai 2018
# UI pour explOH_11
################################
library(shiny)
library(leaflet)


#define UI
shinyUI(
  
  dashboardPage(
    skin="black",
    
    #titre
    dashboardHeader(
      title=NULL,
      titleWidth = 0),
    
    dashboardSidebar(
      width=150,
      sidebarMenu(
        menuItem("Exploration", tabName="explo_carte", icon = icon("search")),
        menuItem("AFC", tabName="afc", icon=icon("chart-bar")),
        menuItem("Informations", tabName ="info", icon=icon("info"))
      )
    ),
    
    dashboardBody(
      
      tags$head(tags$link(rel="icon", type="image/png", href="favicon.png"),
                tags$link(rel="stylesheet", type="text/css", href="https://fonts.googleapis.com/css?family=Roboto"),
                tags$link(rel="stylesheet", type="text/css", href="https://fonts.googleapis.com/css?family=Josefin+Sans:400,600,700"),
                tags$script(src ="https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js"),
                tags$title("explOH"),
                includeCSS("www/style.css")
                #attention pas d'accent dans le css > erreur utf
      ),
      
      useShinyjs(),
      
      tabItems(
        tabItem(tabName="explo_carte",
                #------------------------------- 1. exploration ------------------
                fluidRow(
                  #curseur années
                  box (
                    id="temps_play",
                    width=9,
                    height = 128,
                    # slide temps
                    sliderInput("limites", label="",
                                min=-25, max=2015, value=c(-25, -25), round = 1, step=10, sep=" ",
                                animate = animationOptions(interval=1500))
                  ),#fin curseur années
                  
                  
                  #bornes temporelles
                  box (
                    id="menu_temps",
                    width=3,
                    splitLayout(
                      textInput("borne_temps_1", label ="année min", value = NULL),
                      textInput("borne_temps_2", label="année max", value = NULL)
                    ),
                    actionButton("selec_bornes_temps", label="Appliquer les bornes")
                  )#fin menu bornes temps
                ),#fin fluidRow temps
                
                
                #carte et affichage
                fluidRow(
                  
                  #carte
                  column(
                    id="map_col",
                    width=9,
                    leafletOutput("map", height = "600px")
                  ),#fin carte
                  
                  
                  #affichage
                  box(
                    id="affichage_OH",
                    width=3,
                    title="affichage des OH",
                    #selection valeurs d'usage
                    tags$span("Affichage des différents types d'activités :"),
                    br(),br(),
                    # fonctions à afficher
                    lapply(1:6, function(i) {
                      pickerInput(
                        inputId = paste("picker_vurb", i, sep="_"),
                        # label = liste_vurb[i],
                        choices = liste_vusage[substring(liste_vusage,1,2) < i*10+10 & substring(liste_vusage,1,2) >= i*10],
                        selected = liste_vusage[substring(liste_vusage,1,2) < i*10+10 & substring(liste_vusage,1,2) >= i*10],
                        multiple = TRUE,
                        options = list(
                          `selected-text-format` = "count>-1",
                          `count-selected-text` = paste(liste_vurb[i],"[{0}/{1}]", sep=" "),
                          `actions-box` = TRUE,
                          `deselect-all-text` = "Aucune",
                          `select-all-text` = "Toutes",
                          # `none-Selected-Text` =paste(liste_vurb[i],"(0/{1} activités)", sep=" "),
                          `live-Search` = TRUE,
                          style = paste("btn-",i, sep="")
                        )
                      )
                    } ),#fin sélection valeurs d'usage
                    br(),
                    # couleurs selon fonction, portée ou durée d'existance
                    tags$span("différenciation des OH selon leur :"),
                    radioButtons("couleur_OH", label =NULL,
                                 choices = list("activité" = "v_urb", 
                                                "portée" = "portee"), 
                                 selected = "v_urb",
                                 inline = TRUE)
                  ), #fin affichage
                  
                  
                  #identification
                  box(
                    id="id_OH",
                    width=3,
                    title="identification des OH",
                    #trouver OF
                    searchInput("selec_OH", 
                                label=NULL,
                                placeholder= "Entrer un n° d'OH à trouver", 
                                btnSearch = icon("search"),
                                btnReset = icon("remove"))
                  )#fin identification
                  
                  
                  
                  
                ), #fin fluidrow carte, affichage, identification
                
                
                fluidRow(
                  #téléchargement
                  box( 
                    id="menu_dl",
                    width=2,
                    title ="téléchargement des OH",
                    downloadButton("downloadData", label="Télécharger les OH"),
                    selectInput("type_dl", "Type de fichier:", choices = c("geojson", "sqlite","csv"))
                  ),#fin téléchargement
                  
                  #tableau OH
                  box(
                    id="tableau_OH",
                    width=10,
                    title="tableau des OH sélectionnés",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    dataTableOutput("tab_OH")
                  )
                )
                
        ),#fin exploration
        
        
        # tags$div(source.signature)
        
        
        #------------------------------ AFC ------------------------
        tabItem(tabName="afc",
                fluidRow(
                  box(
                    id="info_AFC",
                    width=12,
                    solidHeader = TRUE,
                    tags$span("Cet onglet permet d'explorer la structure fonctionnelle et temporelle de l'espace urbain tourangeau à travers des AFC. Celles-ci sont calculées sur des tableaux de contingence dénombrant le nombre d'OH selon des périodes temporelles et des caractéristiques fonctionnelles variées. "),
                    br(),br(),
                    tags$span("1. onglets bleus : Choisir le tableau de contingence et lancer le calcul de l'AFC "),br(),
                    tags$span("2. onglets oranges : Affichage de l'inertie expliquée par chaque axe et choix des axes à analyser"),br(),
                    tags$span("3. onglets rouges : Visualisation du biplot des axes selectionnés et tableaux des contributions des variables en ligne et en colonne ")
                    
                  )
                ),#fin info AFC ligne 1
                
                fluidRow( #ligne 2
                  
                  box(
                    id="choix_periodes",
                    width=4,
                    title= "choix du tableau de contingence",
                    collapsible = TRUE,
                    status="primary",
                    column(
                      width=6,
                      radioGroupButtons(
                        label="Découpage temporel",
                        inputId="select_periodes",
                        choices=c("tous les 25 ans"="25", 
                                  "tous les 50 ans"="50", 
                                  "tous les 100 ans"="100", 
                                  "par périodes historiques"="exp"),
                        direction="vertical",
                        status="primary",
                        size="sm"
                      )),#fin colonne temp
                    column(
                      width=6,
                      radioGroupButtons(
                        label="Caractéristiques fonctionnelles",
                        inputId="select_var",
                        choices=c("valeurs urbaines"="urb", 
                                  "valeurs d'usage"="usage", 
                                  "portées"="portee"),
                        direction="vertical",
                        status="primary",
                        size="sm"
                      )) #fin colonne var
                  ), #fin choix tableau de contingence
                  
                  column( 
                    width = 8, # colonne pour bouton action AFC et affichage tableau
                    box(
                      width =12,
                      status="primary",
                      actionButton("action_AFC", 
                                   label = "Calculer l'AFC") #calcul de l'AFC
                    ),
                    box(
                      id="affichage_tab_contingence",
                      width=12,
                      status="primary",
                      title="tableau de contingence pour l'AFC",
                      dataTableOutput("tab_contingence"),
                      collapsible = TRUE
                    )# fin tableau de contingence
                    
                  ) #fin colonne bouton action AFC et affichage tableau
                ), # fin ligne 2
                
        
        
        fluidRow( #ligne 3 : choix des axes
          box(
            id="inertie_axes",
            class="plot_AFC",
            width=8,
            status="warning",
            title="inertie des axes de l'AFC",
            plotOutput("plot_inertie_axes",
                       height = "200px"),
            collapsible = TRUE
          ),
          box(
            id="choix_axes_biplot",
            width=4,
            collapsible = TRUE,
            status="warning",
            title="choix des axes à analyser",
            pickerInput(
              inputId = "axe1",
              label="Choix du premier axe",
              choices=NULL
            ),
            pickerInput(
              inputId = "axe2",
              label="Choix du deuxième axe",
              choices=NULL
            )
            
          )

          
        ), #fin ligne 3 : choix des axes
        
        fluidRow(#ligne 4 : biplot
          box(
            id="biplot",
            width=12,
            class="plot_AFC",
            collapsible=TRUE,
            status="danger",
            title="biplot des axes sélectionnés",
            
            column(
            width=8,
              plotOutput("plot_biplot",
                       height = "600px"))
          )
          
        ),#fin ligne 4 : biplot
        
        fluidRow( #ligne 5 contrib axes
          
          box(
            id="contributions",
            width=12,
            collapsible=TRUE,
            status="danger",
            title="contribution des variables aux axes de l'AFC",
            column(
              width=6,
              class="tab_contrib_AFC",
              tags$span("Contribution des périodes temporelles"),
              br(),br(),
              dataTableOutput("contrib_periodes")
            ),
            column(
              width=6,
              class="tab_contrib_AFC",
              tags$span("Contribution des variables fonctionnelles"),
              br(),br(),
              dataTableOutput("contrib_variables")
            )            
            
          )
        )
      

        ), #fin tabAFC
        
        #------------------------------- INFO ------------------
        
        tabItem(tabName="info",
                fluidRow( 
                  column(6, source.info),
                  column (6, source.usage)
                )
        )
      ) #fin tabItems
      
    )
    
  )
  
)



################################
# Shiny app pour afficher les objets selon le temps
# L. Nahassia, 2019
# UI 
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
    
    #---- ordre des onglets----
    dashboardSidebar(
      width=160,
      sidebarMenu(
        menuItem("exploration globale", tabName="explo_carte", icon = icon("search")),
        menuItem("analyse rythmes", tabName="rythmes", icon=icon("angle-double-right")),
        menuItem("analyse factorielle", tabName="afc", icon=icon("sort-amount-desc")),
        menuItem("analyse environ.", tabName="zones", icon=icon("th-large")),
        menuItem("analyse voisinage", tabName="voisins", icon=icon("stop-circle")),
        menuItem("informations", tabName ="info", icon=icon("info"))
      )
    ),
    
    dashboardBody(
      tags$script(HTML('
                                            $(document).ready(function() {
                       $("header").find("nav").append(\'<div class="div_titre"> <p class="titre">explOH</p> <p class="stitre">plateforme d\\\'exploration spatio-temporelle des Objets Historiques de la ville de Tours (base ToToPI)</p></div>\');
                       }) 
                       ')),
      
      tags$head(tags$link(rel="icon", type="image/png", href="favicon.png"),
                tags$script(src ="https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js"),
                tags$title("explOH"),
                includeCSS("www/style.css"),#attention pas d'accent dans le css > erreur utf
                tags$meta(name="description",content="Cette application web permet d'explorer de manière interactive les Objets Historiques de la BDD ToToPI, et de générer automatiquement des résultats de traitements pour des analyses factorielles et des analyses spatiales.")
      ),
      
      useShinyjs(),
      
      tabItems(
        tabItem(tabName="explo_carte",
                #------------------------------- 1. EXPLORATION ------------------
                fluidRow(#---- ligne 1 : années ----
                         #curseur années
                         box ( 
                           id="temps_play",
                           width=9,
                           height = 128,
                           # slide temps
                           sliderInput("limites", label="",
                                       min=-25, max=2015, value=c(-25, -25), round = 1, step=5, sep=" ",
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
                
                #---- ligne 2 : carte, affichage ----
                fluidRow(
                  
                  #carte
                  column(
                    id="map_col",
                    width=9,
                    leafletOutput("map", height = "700px")%>% withSpinner(type=8, size=0.5)
                  ),#fin carte
                  
                  #affichage
                  box(
                    id="affichage_OH",
                    width=3,
                    title="affichage des OH",
                    #selection valeurs d'usage
                    tags$span("affichage des différents types d'activités :"),
                    br(),br(),
                    # fonctions pour afficher les types d'OH
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
                          `deselect-all-text` = "aucune",
                          `select-all-text` = "toutes",
                          # `none-Selected-Text` =paste(liste_vurb[i],"(0/{1} activités)", sep=" "),
                          `live-Search` = TRUE,
                          style = paste("btn-",i, sep="")
                        )
                      )
                    } ),#fin sélection valeurs d'usage
                    br(),
                    #affichage selon date début et date fin
                    tags$span("surligner les OH :"),
                    prettyRadioButtons("date_OH", label =NULL,
                                       choices = list("aucune"="sur_off",
                                                      "qui apparaissent pendant la période" = "date_deb", 
                                                      "qui disparaissent pendant la période" = "date_fin"
                                       ), 
                                       selected = "sur_off"),
                    br(),
                    # couleurs selon fonction, portée ou durée d'existance
                    tags$span("différenciation des OH selon leur :"),
                    prettyRadioButtons("couleur_OH", label =NULL,
                                       choices = list("valeur urbaine" = "v_urb", 
                                                      "portée" = "portee",
                                                      "fiabilité d'apparition"="fiab_a",
                                                      "fiabilité de disparition"="fiab_d"
                                       ), 
                                       selected = "v_urb")
                  )#fin affichage
                  
                ), #fin fluidrow carte, affichage
                
                #---- ligne 3 : téléchargement,identification OH et tableau OH ----
                fluidRow(
                  #téléchargement
                  box( 
                    id="menu_dl",
                    width=2,
                    title ="téléchargement",
                    HTML('<b>enregistrer la carte (raster) : </b> <img src="icone_dl.png", alt="deuxième bouton en haut à gauche"> sur la carte'),
                    br(),br(),
                    HTML('<b>télécharger les OH affichés (jeu de données) : </b>'),
                    selectInput("type_dl", label="", choices = c("geojson", "sqlite","csv")),
                    tags$span("identification"),
                    splitLayout(cellWidths = c("70%", "20%"),
                                passwordInput("password_dl", label=NULL, value = NULL),
                                actionButton("password_sub", "ok")
                    ),
                    uiOutput("place_dl")
                  ),#fin téléchargement
                  
                  #identification
                  box(
                    id="id_OH",
                    width=2,
                    title="identification des OH",
                    #trouver OF
                    searchInput("selec_OH", 
                                label=NULL,
                                placeholder= "Entrer un n° d'OH à trouver", 
                                btnSearch = icon("search"),
                                btnReset = icon("remove"))
                  ),#fin identification
                  
                  #tableau OH
                  box(
                    id="tableau_OH",
                    width=8,
                    title="tableau des OH sélectionnés",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    dataTableOutput("tab_OH")
                  )
                )
                
        ),#fin exploration
        
        #------------------------------ 2. RYTHMES ------------------------
        tabItem(tabName="rythmes",
                fluidRow(
                  box(
                    id="info_AFC",
                    width=6,
                    solidHeader = TRUE,
                    tags$span("Cet onglet permet d'explorer les rythmes du changement urbain à travers l'existence, l'apparition et la disparition des OH. Un commentaire de ces traitements peut être consulté dans le Chapitre 3 de la thèse de L. Nahassia (2019, en cours)."),
                    br(),br(),
                    tags$span("1. onglet bleu : répartition des OH dans le temps"),br(),
                    tags$span("2. onglet vert : rythme d'apparition et de disparition des OH dans le temps")
                  )),#fin info rythme ligne 1
                #---- ligne 2 : choix ----
                fluidRow(
                  box(id="choix_rythme",
                      width=12,                    
                      title="paramètres",
                      column(
                        width=7,
                        tags$span("1 : Choix des OH a utiliser"),br(),br(),
                        # fonctions pour afficher les types d'OH
                        lapply(1:6, function(i) {
                          pickerInput(
                            inputId = paste("picker_rythmes", i, sep="_"),
                            choices = liste_vusage[substring(liste_vusage,1,2) < i*10+10 & substring(liste_vusage,1,2) >= i*10],
                            selected = liste_vusage[substring(liste_vusage,1,2) < i*10+10 & substring(liste_vusage,1,2) >= i*10],
                            multiple = TRUE,
                            options = list(
                              `selected-text-format` = "count>-1",
                              `count-selected-text` = paste(liste_vurb[i],"[{0}/{1}]", sep=" "),
                              `actions-box` = TRUE,
                              `deselect-all-text` = "Aucune",
                              `select-all-text` = "Toutes",
                              `live-Search` = TRUE,
                              style = paste("btn-",i, sep="")
                            )
                          )
                        })
                      ),
                      column(
                        width=5,
                        radioGroupButtons(
                          label="2 : Trier les OH par",
                          inputId="select_fonction_rythmes",
                          choices=c("valeur urbaine"="vurb",
                                    "valeur d'usage"="vusage",
                                    "portée"="portee"),
                          direction="vertical",
                          size="sm"
                        ),
                        tags$span("3 : Génerer les tableaux selon la sélection. Attention : le temps de calcul des tableaux peut être long ! "),
                        br(),
                        actionBttn("generer_tableaux_exi",
                                   label="génerer",
                                   icon = icon("rocket", lib="font-awesome"),
                                   style="simple",
                                   color="danger",
                                   size="xs"
                        ),
                        br(),br(),
                        tags$span("4 : Créer les graphiques. Attention : le temps de création des graphiques est long !"),br(),br(),
                        actionBttn("generer_graphiques_exi", 
                                   label="Créer les graphiques de répartition simple",
                                   size="xs",
                                   style="simple",
                                   color="primary"),br(),br(),
                        actionBttn("generer_graphiques_appdisp", 
                                   label="Créer les graphiques d'apparition et de disparition",
                                   size="xs",
                                   style="simple",
                                   color="success")
                        
                      ))),#fin ligne 2
                #---- ligne 3 : plots répartition élémentaire ----
                fluidRow(
                  box(
                    id="existence",
                    width=12,
                    title= "répartition des OH au cours du temps",
                    status="primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    column(
                      width=6,
                      plotOutput("plot_exi_OH1")
                    ),
                    column(
                      width=6,
                      plotOutput("plot_exi_OH2")
                    )
                  )),#fin ligne 3
                #---- ligne 4 : plots apparition disparition ----
                fluidRow(
                  box(
                    id="appdisp",
                    width=12,
                    title= "répartition des OH au cours du temps",
                    status="success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput("plot_app_disp",
                               height = "600")
                    
                  ))#fin ligne 4
                
                
        ),
        
        #------------------------------ 3. AFC ------------------------
        tabItem(tabName="afc",
                fluidRow(
                  box(
                    id="info_AFC",
                    width=12,
                    solidHeader = TRUE,
                    tags$span("Cet onglet permet d'explorer la structure fonctionnelle et temporelle de l'espace urbain tourangeau à travers des Analyses Factorielles des Correspondances suivie de Classifications Ascendantes Hiérarchiques sur les périodes temporelles. Les AFC sont calculée à partir de tableaux de contingence dénombrant le nombre d'OH selon des périodes temporelles et des caractéristiques fonctionnelles variées. Un commentaire de ces analyses peut être consulté dans le Chapitre 4 de la thèse de L. Nahassia (2019, en cours)."),
                    br(),br(),
                    tags$span("1. onglets bleus : Choix du tableau de contingence"),br(),
                    tags$span("2. onglets oranges : Résultats de l'AFC - inertie expliquée par chaque axe, graphiques des plans factoriels et tableau des contributions des variables aux axes "),br(),
                    tags$span("3. onglets rouges : CAH sur les périodes (lignes AFC) - dendrogramme, inertie et caractérisation des classes ")
                    
                  )
                ),#fin info AFC ligne 1
                #---- ligne 2 : tableau de contingence ----
                fluidRow( 
                  box(
                    id="choix_periodes",
                    width=4,
                    title= "choix du tableau de contingence",
                    status="primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
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
                  ), #fin choix tableau
                  
                  box(
                    id="affichage_tab_contingence",
                    width=8,
                    title="tableau de contingence",
                    status="primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    dataTableOutput("tab_contingence")
                  )# fin tableau de contingence
                ), # fin ligne 2
                #---- ligne 3 : AFC ----
                fluidRow( #ligne 3 AFC
                  box(
                    id="afc",
                    width=12,
                    title="analyse en composante principale",
                    status="warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    fluidRow( #ligne 3.1 inertie
                      class="box_interne",
                      box(
                        id="inertie_axes",
                        width=8,
                        title="inertie des axes de l'AFC",
                        collapsible = TRUE,
                        tabBox(
                          id="tabbox_inertie",
                          side="left",
                          width=12,
                          tabPanel(
                            id="tab1",
                            title="Histogramme",
                            plotOutput("plot_inertie_axes",
                                       height = "200px") %>% withSpinner(type=8, size=0.5)),
                          tabPanel(
                            id="tab2",
                            title="Tableau",
                            tableOutput("tab_inertie_axes"))
                        )),
                      box(
                        id="choix_axes_biplot",
                        width=4,
                        title="choix des axes à analyser",
                        collapsible = TRUE,
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
                    ), #fin ligne 3.1
                    
                    fluidRow(#ligne 3.2 : biplot
                      class="box_interne",
                      box(
                        id="biplot",
                        width=12,
                        title="biplot des axes sélectionnés",
                        collapsible=TRUE,
                        column(
                          width=10,
                          class="plot_AFC",
                          scatterD3Output("plot_biplot",
                                          height = "600px") %>% withSpinner(type=8, size=0.5)
                        ),
                        column(
                          width=2,
                          tags$span("Le bouton des paramètres à droite du graphique permet d'accéder aux options (zoom, enregistrement)."),
                          br(), br(),
                          pickerInput(
                            inputId = "masquer_biplot",
                            label="masquer les lignes ou les colonnes :",
                            choices = c("aucune","périodes","caractéristiques fonctionnelles"),
                            selected = "aucune",
                            multiple=FALSE
                          )
                        )
                        
                      )
                      
                    ),#fin ligne 3.2
                    
                    fluidRow( #ligne 3.3. : contributions aux axes
                      class="box_interne",
                      box(
                        id="contributions",
                        width=12,
                        title="contribution des variables aux axes de l'AFC",
                        collapsible=TRUE,
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
                      ) #fin box contributions     
                    ) #fin ligne 3.3.
                  )),#fin ligne 3
                #---- ligne 4 : CAH ----
                fluidRow( 
                  box(
                    id="cah",
                    width=12,
                    title="classification ascendante hiérarchique",
                    status="danger",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    tags$span("Les CAH sont calculées sur les coordonnées des périodes (lignes) sur toutes les axes de l'AFC en distance euclidienne. Le critère d'aggrégation est fait selon la méthode de Ward qui maximise l'inertie interclasse pour chaque regroupement."),
                    br(),br(),
                    box(# ligne 4.2 visualisation dendrogramme et inertie des découpages
                      id="choix_nb_classes",
                      width=12,
                      title="Résultats de la CAH",
                      collapsible = TRUE,
                      plotOutput("plot_dendro") %>% withSpinner(type=8, size=0.5),
                      br(),
                      plotOutput("plot_in_cah",
                                 height = "200px") %>% withSpinner(type=8, size=0.5)
                    ),#fin ligne 4.2
                    box(#ligne 4.3 caractérisation des classes
                      id="car_classes",
                      width=12,
                      title="caractérisation des classes chronologiques",
                      footer="Chaque classe chronologique est caractérisée par son profil fonctionnel (par valeurs urbaines), en écarts à l'indépendance (/développer/).",
                      collapsible = TRUE,
                      fluidRow(
                        column(#ligne 4.3.1 choix et frise
                          id="tab_periodes_classes",
                          width=2,
                          numericInput(
                            inputId = "nombre_classes",
                            label="nombre de classes à conserver :",
                            value=4,
                            min=1,
                            max=10,
                            step=1,
                            width="auto")
                        ),
                        column(
                          id="graph_frise_classes",
                          width=10,
                          plotOutput("frise_classes",
                                     height = "110px")%>% withSpinner(type=8, size=0.5)
                        )),#fin ligne 4.3.1
                      #ligne 4.3.2 plot des classes
                      plotOutput("plot_classes")%>% withSpinner(type=8, size=0.5)  
                    )#fin ligne 4.3
                    
                  ))#fin ligne 4 
                
        ), #fin tabAFC
        
        #------------------------------- 4. ZONES ------------------
        
        tabItem(tabName="zones",
                fluidRow( # ---- ligne 1 : description + options + choix OH ----
                          column(
                            id="info_opt_zone",
                            width=8,
                            box( 
                              #description
                              id="info_zones",
                              width=12,
                              solidHeader = TRUE,
                              source.zones
                            ),
                            box(
                              #options
                              id="opt_zones",
                              width=12,
                              title="options des graphiques",
                              tags$span("personaliser le sous-titre des graphiques :"),
                              textInput("stitre_plot_zones",label=NULL, value = NA),
                              awesomeCheckbox("portee_plot_zones", label = "afficher les résultats par portées des OH", value = TRUE)
                            )), #fin première colonne
                          box( # choix OH
                            id="choix_zones",
                            width=4,
                            title="choix des OH à tester",
                            # fonctions pour afficher les types d'OH
                            lapply(1:6, function(i) {
                              pickerInput(
                                inputId = paste("picker_zones", i, sep="_"),
                                choices = liste_vusage[substring(liste_vusage,1,2) < i*10+10 & substring(liste_vusage,1,2) >= i*10],
                                selected = liste_vusage[1],
                                multiple = TRUE,
                                options = list(
                                  `selected-text-format` = "count>-1",
                                  `count-selected-text` = paste(liste_vurb[i],"[{0}/{1}]", sep=" "),
                                  `actions-box` = TRUE,
                                  `deselect-all-text` = "Aucune",
                                  `select-all-text` = "Toutes",
                                  `live-Search` = TRUE,
                                  style = paste("btn-",i, sep="")
                                )
                              )
                            } )#fin fonction
                          )#fin box
                ),
                #---- ligne 2 : graphiques apparition  ----
                fluidRow(
                  box(
                    id="apparition",
                    width=12,
                    title="caractérisation de la localisation des OH à leur apparition",
                    collapsible = TRUE,
                    box(#ligne 2.1 apparition occupation
                      id="graph_app_occ",
                      width=12,
                      title="par rapport aux types d'occupation du sol",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      column(width=7,plotOutput("plot_occupation") %>% withSpinner(type=8, size=0.5)),
                      column(width=5,plotOutput("plot_part_occupation") %>% withSpinner(type=8, size=0.5))
                    ),
                    box(#ligne 2.2 apparition densite
                      id="graph_app_dens",
                      width=12,
                      title="par rapport aux densité du bâti",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      column(width=7,plotOutput("plot_densite") %>% withSpinner(type=8, size=0.5)),
                      column(width=5,plotOutput("plot_part_densite") %>% withSpinner(type=8, size=0.5))
                    )
                  )),
                #---- ligne 3 : graphique existence ----
                fluidRow( 
                  box(
                    id="existence",
                    width=12,
                    title="caractérisation de la localisation des OH au cours de leur existence",
                    collapsible = TRUE,
                    box(#ligne 3.1 existence occupation
                      id="graph_exi_occ",
                      width=12,
                      title="par rapport aux types d'occupation du sol",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      column(width=7,plotOutput("plot_exi_occ") %>% withSpinner(type=8, size=0.5)),
                      column(width=5,plotOutput("plot_trans_occ") %>% withSpinner(type=8, size=0.5))
                    ),
                    box(#ligne 3.2 existence densite
                      id="graph_exi_dens",
                      width=12,
                      title="par rapport aux densité du bâti",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      column(width=7,plotOutput("plot_exi_dens") %>% withSpinner(type=8, size=0.5)),
                      column(width=5,plotOutput("plot_trans_dens") %>% withSpinner(type=8, size=0.5))
                    )
                  )
                )
                
        ), #fin tabzone
        #------------------------------- 5. VOISINAGE ------------------
        tabItem(tabName="voisins",
                fluidRow(
                  box(#---- ligne 1 : infos ---
                    id="info_voisins",
                    width=12,
                    solidHeader = TRUE,
                    source.voisinage                            
                  )
                ), #----fin ligne 1
                fluidRow(#---- ligne 2 : parametres ----
                         box ( 
                           id="choix_voisins",
                           title="paramètre du calcul de voisinage",
                           width=12,
                           column( #taille tampon et intervalle
                             id="parametres_dist_temps",
                             width=4,
                             tags$span("choix de la taille du voisinage :"),
                             
                             prettyRadioButtons("choix_tampon", label =NULL,
                                                choices = list("voisinage en trois périodes (voir ci-dessus)"="trois_tailles",
                                                               "voisinage unique avec une taille fixée (en m):" = "une_taille"
                                                ), 
                                                selected = "trois_tailles"),
                             numericInput(
                               "taille_tampon",
                               label="attention, le calcul du tableau de voisinage prend plusieurs minutes",
                               value=50
                             ),
                             tags$span("intervalle de temps à prendre en compte pour calculer le voisinage moyen (en années) :"),
                             numericInput(
                               inputId = "taille_intervalle",
                               label="si nombre impair, arrondi au nombre pair inférieur",
                               value=20
                             )
                           ),
                           column(#choix nature
                             width=5,
                             tags$span("Choix des valeurs urbaines ou des valeurs d'usage :"),br(),br(),
                             lapply(1:6, function(i) {
                               pickerInput(
                                 inputId = paste("picker_voisins", i, sep="_"),
                                 choices = liste_vusage[substring(liste_vusage,1,2) < i*10+10 & substring(liste_vusage,1,2) >= i*10],
                                 selected = liste_vusage[c(1,2,3,4,5,6,7,8)],
                                 multiple = TRUE,
                                 options = list(
                                   `selected-text-format` = "count>-1",
                                   `count-selected-text` = paste(liste_vurb[i],"[{0}/{1}]", sep=" "),
                                   `actions-box` = TRUE,
                                   `deselect-all-text` = "Aucune",
                                   `select-all-text` = "Toutes",
                                   `live-Search` = TRUE,
                                   style = paste("btn-",i, sep="")
                                 )
                               )
                             })#fin fonction
                           ),
                           column(#choix portée
                             width=3,
                             tags$span("choix des portées :"),br(),
                             awesomeCheckboxGroup(
                               inputId = "portees_voisins",
                               label=NULL,
                               status = "warning",
                               choices = list("1. petite portée"="1",
                                              "2. portée moyenne" = "2", 
                                              "3. grande portée " = "3", 
                                              "4. portée exceptionnelle " = "4"),
                               selected = c("1","2","3","4"))
                           )
                         )),#---- fin line 1
                fluidRow(#---- ligne 2 : plots resultats temps ----
                         box(
                           id="graph_voisins_temps",
                           width=12,
                           title="profil de voisinage d'apparition dans le temps",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           status = "primary",
                           column(
                             width=6,
                             plotOutput("plot_voisins_temps_absolu", height = "800px") %>% withSpinner(type=8, size=0.5)
                           ),
                           column(
                             width=6,
                             plotOutput("plot_voisins_temps_relatif", height = "800px") %>% withSpinner(type=8, size=0.5)
                           )
                         )
                ),#fin fluidRow année
                fluidRow(#---- ligne 3 : plots résultats état ----
                         box ( 
                           id="graph_voisins_etat",
                           title="voisinage d'apparition autour d'une année donnée",
                           width=12,
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           status = "success",
                           tags$span("année à afficher :"),
                            sliderTextInput("annee_voisin", label="seules les années où des OH sélectionnées apparaissent sont consultables",
                                         choices=seq(-25,2015, 100),
                                         grid=TRUE),
                           fluidRow(#graphes
                             column(
                               id="voisinage_absolu",
                               width=6,
                               plotOutput("plot_voisins_etat_absolu") %>% withSpinner(type=8, size=0.5)
                             ),
                             column ( 
                               id="voisinage_relatif",
                               width=6,
                               plotOutput("plot_voisins_etat_relatif") %>% withSpinner(type=8, size=0.5)
                             )
                           )
                         ))#fin fluidRow état
                         
                ), #fin tabitem
                #------------------------------- 6. INFO ------------------
                tabItem(tabName="info",
                        fluidRow( 
                          column(8, source.info)
                        )
                )#fin tabinfo
        )#fin tabitems
      )#fin dashboard body
    )#fin dashboard page
  )#fin UI
  
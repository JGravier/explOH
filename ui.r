################################
# Shiny app pour afficher les objets selon le temps
# novembre 2017
# UI pour explOH_10
################################
library(shiny)
library(leaflet)


#define UI
shinyUI(fluidPage(
  tags$head(tags$link(rel="icon", type="image/png", href="favicon.png"), 
            tags$link(rel="stylesheet", type="text/css", href="https://fonts.googleapis.com/css?family=Roboto"), 
            tags$link(rel="stylesheet", type="text/css", href="https://fonts.googleapis.com/css?family=Josefin+Sans:400,600,700"), 
            tags$title("explOH"),  
            #includeScript("https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"),
            includeScript("www/anim.js"),
            includeCSS("www/style.css")
            #attention pas d'accent dans le css > erreur utf
  ),
  
  
  useShinyjs(),
  
  #titre
  h1("explOH"),
  h4("Plateforme d'exploration des Objets Historiques de la ville de Tours (base ToToPI)"),
  img(id="bouton_info", src='info.png'),
  
  tags$div(id="information_panel",
           fluidRow( 
             column(6, source.info),
             column (6, source.usage)),
           fluidRow(img(id="bouton_fermer", src='fermer.png'))),
  
  #panneau haut
  fluidRow(
    column (id="temps_play", 11, wellPanel( 
      # slide temps
      tags$span(id="titre_annees", h3("Années")),
      sliderInput("limites", label="",
                  min=-25, max=2015, value=c(-25, -25), round = 1, step=10, sep=" ",
                  animate = animationOptions(interval=1500))
      )),
    #bouton explorer > disparaît
    column(1, wellPanel(id="explorer", tags$div("explorer"))),

    #bornes temporelles > apparaît
    column (class="menu_gauche", 2, 
            wellPanel( id="menu_temps",
                       splitLayout(
                         textInput("borne_temps_1", label ="Année min", value = NULL),
                         textInput("borne_temps_2", label="Année max", value = NULL)
                       ),
                       actionButton("selec_bornes_temps", label="Appliquer les bornes")
                       )
                      
      )),
            
    #panneau gauche
    fluidRow(
      
      column(class="menu_gauche", 3,
             
             wellPanel(id="menu_OH",
                       h3("Identification des OH"),
                       #trouver OF
                       searchInput("selec_OH", 
                                   label=NULL,
                                   placeholder= "Entrer un n° d'OH à trouver", 
                                   btnSearch = icon("search"),
                                   btnReset = icon("remove")),
                       
                       
                       # couleurs selon fonction, portée ou durée d'existance
                       radioButtons("couleur_OH", label = "Différenciation des OH selon leur :", 
                                    choices = list("activité" = "v_urb", 
                                                   "portée" = "portee"), 
                                    selected = "v_urb",
                                    inline = TRUE),
                       br(),
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
                       } )
                       
                       
             )#fin wellPanel2
             
             
      ), #fin menu gauche
      
      column(id="map_col", 12,
             #carte
             leafletOutput("map", height = "600px")
             # br(),
      ),
      
      tags$div(source.signature)
      
    )#fin fluidrow
    
    
    ))
  
  
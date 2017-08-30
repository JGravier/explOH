################################
# Shiny app pour afficher les objets selon le temps
# juin 2017
# UI pour explOH_7
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
           fluidRow(img(id="bouton_fermer", src='fermer.png'))
  ),
  
  fluidRow(
    
    column (id="temps_play", 12,
            wellPanel( # slide temps
              sliderInput("limites", label="",
                          min=-25, max=2015, value=c(-25, -25), round = 1, step=10, sep=" ",
                          animate = animationOptions(interval=150)),
              tags$div(id="explorer", "explorer"))),
  
  column (class="menu_gauche", 2,
          wellPanel( id="menu_temps",
                     # h3("Sélection temporelle"), 
                     # br(),
                     # text temps
                     splitLayout(
                       textInput("borne_temps_1", label ="Année min", value = NULL),
                       textInput("borne_temps_2", label="Année max", value = NULL)
                     ),
                     actionButton("selec_bornes_temps", label="Appliquer les bornes")
          )
  )
  
  # , 
  # playButton = HTML("<img src=\"play.svg\" >"),
  # pauseButton = HTML("<img src=\"pause.svg\" >")
),

#panneau gauche
fluidRow(
  
  column(class="menu_gauche", 3,
         
         wellPanel(id="menu_OH",
                   h3("Identification des OH"),
                   textInput("test", label="test", value = NULL),
                   #trouver OF
                   textInput("num_OH", label ="Trouver l'OH n°", value = NULL),
                   
                   actionButton("selec_OH", label="Afficher l'OH"),
                   actionButton("deselec_OH", label="Déselectionner"), #inline !
                   
                   # couleurs selon fonction, portée ou durée d'existance
                   radioButtons("couleur_OH", label = "Différencier les OH selon leur :", 
                                choices = list("valeur urbaine" = "v_urb", 
                                               "portée" = "portee", 
                                               "durée d'existance" = "duree"), 
                                selected = "v_urb",
                                inline = TRUE),
                   
                   
                   # fonctions à afficher
                   checkboxGroupInput("choix_fonctions", label ="Activités à afficher selon leurs valeurs urbaines :",
                                      choices = c("1.Voirie, aménagements"="1",
                                                  "2.Structures défensives et militaires"="2", 
                                                  "3.Constructions civiles"="3", 
                                                  "4.Edifices religieux"="4",
                                                  "5.Lieux d'inhumation"="5",
                                                  "6.Lieux de commerce, artisanat, production"="6"),
                                      selected = c("1","2","3","4","5","6"))
                   
                   
         )#fin wellPanel2
         
         
  ), #fin menu gauche
  
  column(id="map_col", 12,
         #carte
         leafletOutput("map", height = "600px")
         # br(),
         # dataTableOutput("tableau")
         
         # wellPanel(
         #   
         #  h3("Exploration graphique"),
         #   
         # #plots
         # plotOutput("ohfreq", brush = brushOpts(id = "ohfreq_brush", direction = "x")), 
         # 
         # # ajuster l'échelle y des plots ?
         # br(),
         # radioButtons("plot_echelle_y", label = "Uniformiser les échelles des graphes en ordonnées ?",
         #              choices = list("Oui"="fixed","Non"="free_y"),
         #              selected = "fixed",
         #              inline= TRUE))
         
  ),
  
  tags$div(source.signature)
  
)#fin fluidrow


))


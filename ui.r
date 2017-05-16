################################
# Shiny app pour afficher les objets selon le temps
# avril 2017
# UI pour explOH_5
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
  
  
  #panneau gauche
  fluidRow(
    
   column(class="menu_gauche", 3,
           
           wellPanel( id="menu_temps",
                      h3("Sélection temporelle"), 
                      br(),
                      # slide temps
                      sliderInput("limites", label ="Choix de la période :", 
                                  min=-25, max=2015, value=c(-25,2015), round = 1, step=50, sep=" "),
                      # text temps
                      splitLayout(
                        textInput("borne_temps_1", label ="Borne min", value = NULL),
                        textInput("borne_temps_2", label="Borne max", value = NULL)
                      ),
                      actionButton("selec_bornes_temps", label="Appliquer les bornes")
           ),
           
           wellPanel(id="menu_OH",
                     h3("Identification des OH"),
                     
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
                     checkboxGroupInput("choix_fonctions", label ="Activités à afficher :",
                                        choices = c("1.Voirie, aménagements"="1",
                                                    "2.Structures défensives et militaires"="2", 
                                                    "3.Constructions civiles"="3", 
                                                    "4.Edifices religieux"="4",
                                                    "5.Lieux d'inhumation"="5",
                                                    "6.Lieux de commerce, artisanat, production"="6"),
                                        selected = c("1","2","3","4","5","6"))
                     
                    
           )#fin wellPanel2
           
           
    ), #fin menu gauche
    
    column(9,
           #carte
           leafletOutput("map", height = "600px")
           #br(),
           
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


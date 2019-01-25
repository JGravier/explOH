################################
# Shiny app pour afficher les objets selon le temps 
# L. Nahassia, aout 2018
# elements globaux
################################

options(encoding = "UTF-8")

#---- LIBRAIRIES ---- 
#librairies generales
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(htmlwidgets)
library(leaflet)
library(RSQLite)
library(dplyr)
library(tidyr)
library(DT)
library(reshape2)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(ggdendro)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(cowplot)
library(scatterD3)
library(stringi)
library(stringr)
library(ade4)

#Spatialobjects
library(rgdal)
library(sf)
library(rgdal)

#sources fichiers
source("global_AFC.R")
source("global_zones.R")
source("charge_data.R", local=FALSE)


#----------------------------------- VARIABLES GLOBALES ----

#ligne NA pour addCircles de LeafletProxy
null_row <- OH_geom[st_geometry_type(OH_geom)=="POINT",][1,]
null_row$OH_NUM <- 0


#----------------------------------- PALETTES ----
##VURB
# palette_f <- c("#e72535", "#2fc6a0", "#ff9819", "#0a60b2", "#7d3ab9","#2ea949")
couleurs_vurb <- c("#e72535", "#0a8eb1", "#f6b01a", "#2fc6a0", "#703ab9","#ff7919")
# palette <- c("#E03535","#2C39E8","#29FF81","#E8D145","#FF9D4D","#3CE3E8")
# palette_f <- brewer.pal(6,"Set1")
#display.brewer.all()
palette_fonctions <- colorFactor(couleurs_vurb, levels(OH_geom$V_URB_NOM))
##PORTEE
couleurs_portees <-c("#febd2b", "#fe892f", "#ff5733", "#ff2b37")
palette_portees <- colorFactor(couleurs_portees, unique(OH_geom$PORTEE_NOM))
##APPARITION
couleurs_fiab <-c("#018571", "#80cdc1", "#dfc27d", "#a6611a", "grey")
palette_fiab_a <- colorFactor(couleurs_fiab, order(levels(OH_geom$FIAB_APP)))
palette_fiab_d <- colorFactor(couleurs_fiab, rev(order(levels(OH_geom$FIAB_DISP))))
#CAH
palette_CAH  <- colorRampPalette(c("#E8C95D","#eb8179","#85dbbd","#628aba","#a39e96"))
#ensembles urbains
couleurs_EU  <- c("#c8ab37","#a05050")
palette_EU <- colorFactor(couleurs_EU, unique(ens_urb$occupation))

  

#----------------------------------- FONCTIONS POPUP ----

texte_popup_OH <- function(df) {
  paste(sep="",
        "<h5> <b> Nom : ", df$NOM, "</b> <br/> OH n°", df$OH_NUM,
        "</h5>Valeur d'usage : <b>" ,  df$V_USAGE,"</b> / ", df$NOM_USAGE,
        "<br/> Portée de niveau : <b>", df$PORTEE, "</b>",
        "<br/> Apparition en <b>", df$DATE_DEB, "</b> (f", df$FIAB_APP,
        "), dispartion en <b>", df$DATE_FIN, "</b> (f", df$FIAB_DISP,")<br/><br/> Remarques : ",
        df$REMARQUES, "<br/> Ref : ", df$REFERENCE)
}


texte_popup_ens_urb <- function(df) {
  paste(sep="",
        "<h5> <b>ID: ", df$ID, ", ", df$ID_nom, " </h5></b>", 
        df$date_debut,"-",df$date_fin,
        "<br/> Type d'occupation : ", df$occupation,
        "<br/> Densité : ", df$densite,
        "<br/> Description : ", df$description)
}


texte_popup_traits_rive <- function(df) {
  paste(sep="",
        "<h5> <b> Trait de rive ", df$SELEC, "</b> </h5>(", df$DEBUT,"-",df$FIN,")")
}

texte_popup_poles <- function(df) {
  paste(sep="",
        "<h5> <b> Pole urbain ", df$type, " </h5>", df$date_deb,"-",df$date_fin)
}


#----------------------------------- ICONES POLES ----
polesIcons <- iconList(
  economique = makeIcon(iconUrl="icone_eco.png"),
  politique = makeIcon(iconUrl="icone_pol.png"),
  religieux = makeIcon(iconUrl="icone_rel.png")
  
)


#----------------------------------- GGPLOT THEMES ----

theme_ln <- function() 
{theme(legend.position = "none",
       legend.text = element_text(size=11),
       legend.title = element_text(size=11),
       panel.grid.major.x = element_line(linetype ="dotted", color ="grey75"),
       panel.grid.minor.y = element_blank(), #ajouter les ticks secondaires
       panel.background = element_rect(fill="white"),
       plot.background = element_rect(fill="grey87"),
       plot.title = element_text(size=13, face="bold"),
       plot.subtitle = element_text(size=11, face="plain"),
       plot.caption = element_text(size=10, color="grey60"),
       plot.margin = unit(c(0.5,1,0.3,1), "lines"),
       axis.ticks =  element_line(colour = "grey75"),
       axis.title=element_text(size=11),
       axis.text=element_text(size=10),
       strip.background = element_rect(fill = "grey80"),
       strip.text = element_text(size=11))}


#----------------------------------- éléments HTML ---------

source.info <- HTML('<p class="titre_info">Informations sur l\'application : </p>L\'application explOH permet d\'explorer temporellement et spatialement les données "Objets Historiques" (OH) accompagnées de données de contextes (les "ensembles urbains", les "traits de rives", les "pôles urbains") provenant du <a href="http://citeres.univ-tours.fr/spip.php?article504", target="_blank">SIG Topographie de Tours PréIndustrielle </a> (ToToPI), développé au <a href="http://citeres.univ-tours.fr/spip.php?rubrique57", target="_blank">Laboratoire d\'Archéologie Urbaine</a> à Tours (UMR 7324 CITERES).</br> </br>
                    L\'application est développée par <a href="http://www.parisgeo.cnrs.fr/spip.php?article6441" target="_blank">Lucie Nahassia</a> dans le cadre de sa thèse. Elle a pour objectif d\'accompagner la lecture des analyses statistiques et spatiales développées au cours de ce travail en permettant au lecteur/utilisateur de naviguer par lui-même dans les données utilisées au niveau le plus élémentaire de l\'individu topographique historique.</br></br>
                    <p class="titre_info">Sources : </p>
                    Données : ToToPI, UMR7324 CITERES-LAT Université de Tours/CNRS. </br>
                    Fond de carte : OpenStreetMap, CartoDB')

source.usage <-HTML('<p class="titre_info">Utilisation de l\'application :</p>Sur la <p class="stitre_info">carte</p>, <img src="icone_leaflet.png", alt="l\'icône en haut à droite"> permet de choisir différents fonds de carte et d\'afficher ou non les données OH et les données contextuelles. </br> </br>
                    Le panneau <p class="stitre_info">Années</p> permet choisir la période temporelle pour laquelles les OH sont afffichés, à partir du slider ou, pour un contrôle plus précis, en rentrant manuellement les dates minimum et maximum. Une seule année peut être sélectionnée en choisissant la même date pour les bornes  minimum et maximum. </br> </br>
                    Le panneau <p class="stitre_info">Identification des OH</p>, qui apparaît après avoir cliqué sur le bouton "explorer", permet de retrouver un OH par son identifiant, de modifier l\'affichage des OH en fonction de leurs différents attributs, et d\'afficher seulement certains OH en fonction de leur attribut fonctionnel.')

source.signature <- HTML(
  '<p class="signature">',
  "Application développée par Lucie Nahassia, 2017 | Données : ToToPI, UMR7324 CITERES-LAT Université de Tours/CNRS"
)

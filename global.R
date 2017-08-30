################################
# Shiny app pour afficher les objets selon le temps 
# juin 2017s
# elements globaux pour explOH_7
################################

options(encoding = "UTF-8")

#librairies generales
library(shiny)
library(shinyjs)
library(leaflet)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(stringi)


#Spatialobjects
library(rgdal)
library(sf)

#Import données
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
palette_portees <- colorFactor(couleurs_portees, unique(OH_geom$PORTEE))
##DUREE
ramp_duree <- colorNumeric(c("#ffeda0","#800026"), domain = OH_geom$OH_FIN - OH_geom$OH_DEB)
# previewColors(colorFactor(palette, domain = NULL),unique(OH_geom_4326@data$V_URB))
#il y avait plus de niveaux dans v_urb que dans palette donc interpolation et couleurs moches
#previewColors(colorRamp(c("#fee0d2","#a50f15")), OH_geom_4326@data$DATE_FIN)



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
        "<h5> <b>", df$nom, " </h5> <br/>", 
        df$date_debut,"-",df$date_fin,"<br/> </b> Urbain : ", df$urbain,
        "<br/> Densité : ", df$densite,
        "<br/> Description : ", df$descriptio)
}


texte_popup_traits_rive <- function(df) {
  paste(sep="",
        "<h5> <b> Trait de rive ", df$SELEC, "</b> </h5>(", df$DEBUT,"-",df$FIN,")")
}



#----------------------------------- GGPLOT THEMES ----

#theme ggplot facette clair
theme_facettes_clair <- function(){
  theme(legend.position="none",
        
        panel.grid.major.x = element_line(color = "#CDD2D4", linetype ="dotted", size = 0.5),
        panel.grid.minor.x = element_line(color = "#CDD2D4", linetype ="dotted", size = 0.5),
        panel.grid.major.y = element_line(color= "#CDD2D4", size= 0.5),
        
        axis.ticks =  element_line(color = "#CDD2D4"),
        axis.text = element_text(colour = "#CDD2D4"),
        axis.text.x = element_text(angle=90),
        
        text = element_text(colour = "#F5F5F3", face="bold"),
        plot.title = element_text(size = rel(1.8), face = "bold", margin = margin(10,10,20,10), hjust = 0.5),
        
        strip.background = element_rect(fill="#CDD2D4"),
        strip.text = element_text(colour = "#FFFFFF"),
        
        panel.background = element_rect(fill = "#F5F5F3",colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        
         # panel.spacing = margin(10), #ne marche pas avec les facettes
         plot.margin = margin(20,20,10,20)
  )
}


#----------------------------------- éléments HTML ---------

source.info <- HTML('<p class="titre_info">Informations sur l\'application : </p>L\'application explOH permet d\'explorer temporellement et spatialement les données "Objets Historiques" (OH) accompagnées de données de contextes, les "ensembles urbains" et les "traits de rives", provenant <a href="http://citeres.univ-tours.fr/spip.php?article504", target="_blank">SIG Topographie de Tours PréIndustrielle<a/>, développée au <a href="http://citeres.univ-tours.fr/spip.php?rubrique57", target="_blank">Laboratoire d\'Archéologie Urbaine</a> à Tours (UMR 7324 CITERES).</br> </br>
L\'application est développée par <a href="http://www.parisgeo.cnrs.fr/spip.php?article6441" target="_blank">Lucie Nahassia</a> dans le cadre de sa thèse. Elle a pour objectif d\'accompagner la lecture des analyses statistiques et spatiales développées au cours de ce travail en permettant au lecteur/utilisateur de naviguer par lui-même dans les données utilisées au niveau le plus élémentaire de l\'individu topographique historique.')

source.usage <-HTML('<p class="titre_info">Utilisation de l\'application :</p>Sur la <p class="stitre_info">carte</p>, <img src="icone_leaflet.png", alt="l\'icône en haut à droite"> permet de choisir différents fonds de carte et d\'afficher ou non les données OH et les données contextuelles. </br> </br>
Le panneau <p class="stitre_info">Sélection temporelle</p> permet choisir la période temporelle pour laquelles les OH sont afffichés en rentrant manuellement les dates minimum et maximum pour un contrôle plus précis que le slider. Une seule année peut être sélectionnée en choisissant la même date pour les bornes  minimum et maximum. </br> </br>Le panneau <p class="stitre_info">Identification des OH</p> permet de retrouver un OH par son identifiant, de modifier l\'affichage des OH en fonction de leurs différents attributs, et d\'afficher seulement certains OH en fonction de leur attribut fonctionnel.')

source.signature <- HTML(
'<p class="signature">',
"Application développée par",
"<a href=\"http://www.parisgeo.cnrs.fr/spip.php?article6441\" target=\"_blank\">Lucie Nahassia</a>",
", 2017"
)
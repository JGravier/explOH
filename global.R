################################
# Shiny app pour afficher les objets selon le temps 
# Octobre 2016
# elements globaux pour explOH_5
################################

options(encoding = "UTF-8")

#librairies generales
library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
library(shinythemes)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(shinyjs)


#SQl to R
library(RSQLite)
library(rgdal)
library(rgeos)

source("charge_data.R", local=FALSE)


#---- PALETTES ----
#VURB
# palette_f <- c("#e72535", "#2fc6a0", "#ff9819", "#0a60b2", "#7d3ab9","#2ea949")
palette_vurb <- c("#e72535", "#0a8eb1", "#f6b01a", "#2fc6a0", "#703ab9","#ff7919")
# palette <- c("#E03535","#2C39E8","#29FF81","#E8D145","#FF9D4D","#3CE3E8")
# palette_f <- brewer.pal(6,"Set1")
#display.brewer.all()
palette_fonctions <- colorFactor(palette_vurb, levels(OH_ponctuels_4326@data$V_URB_NOM))
#PORTEE
palette_p <-c("#febd2b", "#fe892f", "#ff5733", "#ff2b37")
palette_portees <- colorFactor(palette_p, unique(OH_ponctuels_4326@data$PORTEE))
#DUREE
ramp_duree <- colorNumeric(c("#ffeda0","#800026"), domain = OH_ponctuels_4326@data$OH_FIN - OH_ponctuels_4326@data$OH_DEB)
# previewColors(colorFactor(palette, domain = NULL),unique(OH_ponctuels_4326@data$V_URB))
#il y avait plus de niveaux dans v_urb que dans palette donc interpolation et couleurs moches
#previewColors(colorRamp(c("#fee0d2","#a50f15")), OH_ponctuels_4326@data$DATE_FIN)



#---- FONCTIONS POPUP ----

texte_popup_OH <- function(df) {
  paste(sep="",
        "<h5> <b> Nom : ", df@data$NOM, "</b> <br/> OH n°", df@data$OH_NUM,
        "</h5>Valeur d'usage : <b>" ,  df@data$V_USAGE,"</b> / ", df@data$NOM_USAGE,
        "<br/> Portée de niveau : <b>", df@data$PORTEE, "</b>",
        "<br/> Apparition en <b>", df@data$DATE_DEB, "</b> (f", df@data$FIAB_APP,
        "), dispartion en <b>", df@data$DATE_FIN, "</b> (f", df@data$FIAB_DISP,")<br/><br/> Remarques : ",
        df@data$REMARQUES, "<br/> Ref : ", df@data$REFERENCE)
}


texte_popup_ens_urb <- function(df) {
  paste(sep="",
        "<h5> <b>", df@data$nom, " </h5> <br/>", 
        df@data$date_debut,"-",df@data$date_fin,"<br/> </b> Urbain : ", df@data$urbain,
        "<br/> Densité : ", df@data$densite,
        "<br/> Description : ", df@data$descriptio)
}


texte_popup_traits_rive <- function(df) {
  paste(sep="",
        "<h5> <b> Trait de rive ", df@data$SELEC, "</b> </h5>(", df@data$DEBUT,"-",df@data$FIN,")")
}


#---- POPUP BASE ----
popup_pg_tout <-texte_popup_OH(OH_geom_pg_4326)
popup_pl_tout <-texte_popup_OH(OH_geom_pl_4326)
popup_pt_tout <-texte_popup_OH(OH_geom_pt_4326)
popup_ens_urb_tout <- texte_popup_ens_urb(ens_urb)
popup_traits_rive_tout <- texte_popup_traits_rive(traits_rive)

#---- GGPLOT THEMES ----

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


#---- éléments HTML ---------

source.info <- HTML('<p class="titre_info">Informations sur l\'application : </p>L\'application explOH permet d\'explorer temporellement et spatialement les données "Objets Historiques" (OH) accompagnées de données de contextes, les "ensembles urbains" et les "traits de rives", provenant <a href="http://citeres.univ-tours.fr/spip.php?article504", target="_blank">SIG Topographie de Tours PréIndustrielle<a/>, développée au <a href="http://citeres.univ-tours.fr/spip.php?rubrique57", target="_blank">Laboratoire d\'Archéologie Urbaine</a> à Tours (UMR 7324 CITERES).</br> </br>
L\'application est développée par <a href="http://www.parisgeo.cnrs.fr/spip.php?article6441" target="_blank">Lucie Nahassia</a> dans le cadre de sa thèse. Elle a pour objectif d\'accompagner la lecture des analyses statistiques et spatiales développées au cours de ce travail en permettant au lecteur/utilisateur de naviguer par lui-même dans les données utilisées au niveau le plus élémentaire de l\'individu topographique historique.')

source.usage <-HTML('<p class="titre_info">Utilisation de l\'application :</p>Sur la <p class="stitre_info">carte</p>, <img src="icone_leaflet.png", alt="l\'icône en haut à droite"> permet de choisir différents fonds de carte et d\'afficher ou non les données OH et les données contextuelles. </br> </br>
Le panneau <p class="stitre_info">Sélection temporelle</p> permet réduire la période temporelle pour laquelles les OH sont afffichés, à l\'aide d\'un slider ou en rentrant manuellement les dates minimum et maximum pour un contrôle plus précis. Une seule année peut être sélectionnée en choisissant la même date pour les bornes  minimum et maximum. </br> </br>Le panneau <p class="stitre_info">Identification des OH</p> permet de retrouver un OH par son identifiant, de modifier l\'affichage des OH en fonction de leurs différents attributs, et d\'afficher seulement certains OH en fonction de leur attribut fonctionnel.')

source.signature <- HTML(
'<p class="signature">',
"Application développée par",
"<a href=\"http://www.parisgeo.cnrs.fr/spip.php?article6441\" target=\"_blank\">Lucie Nahassia</a>",
", 2017"
)

# 
# tags$p(class="signature","Application en cours de développement par <a, href="http://www.parisgeo.cnrs.fr/spip.php?article6441", target="_blank">Lucie Nahassia</a>, 2017")
################################
# Shiny app pour afficher les objets selon le temps 
# L. Nahassia, 2019
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
library(shinyjqui)
library(htmlwidgets)
library(leaflet)
library(RSQLite)
library(tidyverse)
library(DT)
library(reshape2)
library(ggrepel)
library(ggthemes)
library(ggdendro)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(scatterD3)
library(stringi)
library(ade4)

#Spatialobjects
library(rgdal)
library(sf)
library(rgeos)

#sources fichiers
source("global_rythmes.R")
source("global_AFC.R")
source("global_zones.R")
source("charge_data.R", local=FALSE)


#----------------------------------- VARIABLES GLOBALES ----

#ligne NA pour addCircles de LeafletProxy
null_row <- OH_geom[st_geometry_type(OH_geom)=="POINT",][1,]
#labels pour voisinage
facet_labels_vois <- c("portee"="voisinage selon la \nportée des OH", "valeur urbaine"="voisinage selon la \nvaleur urbaine des OH")

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
palette_EU <- colorFactor(couleurs_EU, unique(zst$occupation))
couleurs_EU2 <- c("#aaddd4","#3ab29d","#016957")
palette_EU2 <- colorFactor(couleurs_EU2, unique(zst$densite))
#voisinages vurb + portee
palette_vurb_p <- c(couleurs_vurb, couleurs_portees)
names(palette_vurb_p) <- c("urb_1", "urb_2", "urb_3", "urb_4", "urb_5", "urb_6", "p_1", "p_2", "p_3", "p_4")




#----------------------------------- FONCTIONS POPUP ----

texte_popup_OH <- function(df) {
  paste(sep="",
        "<h5> <b> Nom : ", df$NOM, "</b> <br/> OH n°", df$OH_NUM,
        "</h5>Valeur d'usage : <b>" ,  df$V_USAGE,"</b> / ", df$NOM_USAGE,
        "<br/> Portée de niveau : <b>", df$PORTEE, "</b>",
        "<br/> Apparition en <b>", df$DATE_DEB, "</b> (f", df$FIAB_APP,
        "), disparition en <b>", df$DATE_FIN, "</b> (f", df$FIAB_DISP,")<br/><br/> Remarques : ",
        df$REMARQUES, "<br/> Ref : ", df$REFERENCE)
}


texte_popup_zst <- function(df) {
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

source.zones <- HTML('Cet onglet permet de caractériser la localisation des OH par rapport aux caractéristiques de l\'espace où ils apparaissent puis au cours de leur existence (type d\'occupation urbain/intermédiaire/non urbain et densités du bâti fort/moyen/faible). Le détail et un commentaire de ce traitement  peut être consulté dans le Chapitre 6 de la thèse de L. Nahassia (2019, en cours). 
                  </br></br>
                  Les zones d\'occupation du sol et de densité peuvent être affichées dans l\'onglet <a>exploration globale</a> (ensembles urbains sur la carte), et une schématisation de leur répartition au cours du temps est téléchargeable <a href="schematisations_zones_tours.pdf" target="_blank">ici</a>.
                  ')



source.voisinage <- HTML('Cet onglet permet de caractériser la localisation des OH par rapport à leur voisinage au moment de leur apparition. Le voisinage est défini comme l\'ensemble des activités situées dans un périmètre autour d\'une activité donnée, observé dans un intervalle de temps autour de l\'année d\'apparition de cette dernière. La taille du périmètre et la longueur de l\'intervalle de temps sont paramétrables.
                        Un voisinage évolutif en trois périodes, correspondant à celui utilisé dans la thèse (voir ci-dessous), est proposé : 100 m avant 600, 60 m entre 600 et 1550 et 30 m après 1550.
                        </br> Les traitements ci-desous représentent les profils des voisinages moyens pour une catégorie d\'activité choisie par l\'utilisateur (activités de valeur urbaine 1, activité de portée 2, etc.).Les voisinages sont représentés de manière absolue (nombre d\'activité de chaque type dans le voisinage) et de manière relative. Ils sont alors comparés (centrés-réduits) à chaque pas de temps au voisinage moyen de toutes les activités apparaissant à ce pas de temps.
                        </br></br>
                         Du point de vue temporel, le voisinage d\'apparition peut être étudié de deux manières :</br>
                         1. onglet bleu : étude des transformations du voisinage d\'un type d\'activité sur toute la durée d\'étude</br>
                         2. onglet vert : étude du voisinage d\'un type d\'activité à un moment donné (un intervalle de temps autour d\'une date choisie)</br> </br>
                         Le détail et un commentaire de ce traitement  peut être consulté dans le Chapitre 7 de la thèse de L. Nahassia (2019, en cours).')

source.info <- HTML('<p class="titre_info">Informations sur l\'application : </p>L\'application explOH permet d\'explorer temporellement et spatialement les données "Objets Historiques" (OH) accompagnées de données de contexte (les "ensembles urbains", les "traits de rives", ...) provenant du <a href="http://citeres.univ-tours.fr/spip.php?article504", target="_blank">SIG Topographie de Tours PréIndustrielle </a> (ToToPI), développé au <a href="http://citeres.univ-tours.fr/spip.php?rubrique57", target="_blank">Laboratoire Archéologie et Territoires</a> à Tours (UMR 7324 CITERES).</br> </br>
                    L\'application est développée par <a href="http://www.parisgeo.cnrs.fr/spip.php?article6441" target="_blank">Lucie Nahassia</a> dans le cadre de sa thèse (en cours). Elle a pour objectif d\'accompagner l\'analyse spatio-temporelle de la localisation des activités dans l\'espace urbain de Tours. Elle permet ainsi à l\'utilisateur de naviguer dans les données utilisées au niveau le plus élémentaire de l\'individu topographique historique, les Objets Historiques  (OH), et d\'afficher les résultats de divers traitements spatio-temporels de manière interactive et en fonction de paramètres ajustables.
                    </br></br>
                    
                    <p class="titre_info">Utilisation : </p>
                    
                    <a><span class=\"fa fa-search\"></span> exploration globale : </a></br> 
                    Dans cet onglet, les OH sont affichés sur la carte centrale en fonction d\'une <i>période</i> (outils en haut de la page) et de <i>caractéristiquesfonctionnelles ou temporelles</i> (outils à droite de la page) choisies par l\'utilisateur.</br>
                    <b>Sélection temporelle :</b> une seule année peut être sélectionnée en choisissant la même date pour les bornes  minimum et maximum. </br>
                    <b>Identification des OH  :</b> cet outil permet d\'identifier sur la carte un OH à partir de son identifiant unique.</br>
                    <b>Tableau des OH selectionnés :</b>  cet outil affiche les attributs des OH sélectionnés sous forme de table attributaire (une partie de ces attributs est directement lisible individuellement en cliquant sur un OH sur la carte).</br>
                    <b>Téléchargement :</b> la carte peut être enregistrée en format image (.png) à partir de  <img src=\"icone_dl.png\" alt=\"l\'icone de flèche\"> sur la carte. Les OH sélectionnés peuvent être téléchargé  à partir de l\'outil \"téléchargement\" sous différents formats.
                    
                    </br></br>
                    <a><span class=\"fa fa-angle-double-right\"></span> analyse rythme : </a></br> 
                    Cet ongle affiche les résultats d\'une analyse multi-factorielle de la structure temporelle et fonctionnelle des données. Les résultats sont générés en fonction des variables sélectionnées par l\'utilisateur.

                    </br></br>
                    <a><span class=\"fa fa-sort-amount-desc\"></span> analyse factorielle : </a></br> 
                    Cet ongle affiche les résultats d\'une analyse multi-factorielle de la structure temporelle et fonctionnelle des données. Les résultats sont générés en fonction des variables sélectionnées par l\'utilisateur.
                    
                    </br></br>
                    <a><span class=\"fa fa-square-o\"></span> analyse env. : </a></br> 
                    Cet ongle affiche les résultats d\'une analyse spatio-temporelle des caractéristiques de localisation des OH par rapport aux caractéristiques de l\'environnement où ils se situent. Les résultats sont générés en fonction du sous-ensemble d\'OH sélectionné par l\'utilisateur.
                    
                    </br></br>
                    <a><span class=\"fa fa-stop-circle\"></span> analyse voisinage : </a></br> 
                    Cet ongle affiche les résultats d\'une analyse du voisinage d\'apparition des OH à un moment donné et au cours du temps.
                    
                    </br></br>
                    <p class=\"titre_info\">Sources : </p>
                    Données : ToToPI, UMR7324 CITERES-LAT Université de Tours/CNRS. </br>
                    Fond de carte : OpenStreetMap, CartoDB.</br>
                    </br></br>
                    <p class=\"titre_info\">Licence : </p>
                    la plateforme explOH est sous licence libre AGPL3.0. </br>
                    <a target="_blank" href="https://doi.org/10.5281/zenodo.3256682"><img src="https://zenodo.org/badge/DOI/10.5281/zenodo.3256682.svg" alt="DOI"></a>  </br>
                    Le code source est disponibles sur le dépot Github : <a target="_blank" href="http://github.com/heylue/explOH">https://github.com/heylue/explOH</a>.
                    ')



source.signature <- HTML(
  '<p class="signature">',
  "Application développée par Lucie Nahassia, 2019 | Données : ToToPI, UMR7324 CITERES-LAT Université de Tours/CNRS"
)

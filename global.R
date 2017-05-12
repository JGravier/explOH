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

#Import données
source("charge_data.R", local=TRUE)

#---- PALETTES ----
#VURB
# palette_f <- c("#e72535", "#2fc6a0", "#ff9819", "#0a60b2", "#7d3ab9","#2ea949")
palette_f <- c("#e72535", "#0a8eb1", "#f6b01a", "#2fc6a0", "#703ab9","#ff7919")
# palette <- c("#E03535","#2C39E8","#29FF81","#E8D145","#FF9D4D","#3CE3E8")
# palette_f <- brewer.pal(6,"Set1")
#display.brewer.all()
palette_fonctions <- colorFactor(palette_f, unique(OH_ponctuels_4326@data$V_URB))
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


#---- preparation du tableau pour barplot -----------

## préparation des tableaux de contingence
#passage en valeurs urbaines
OH_ponctuels_4326@data$V_URB = as.factor(OH_ponctuels_4326@data$V_URB)
# structure du tableau
nb_fonction_an <- as.data.frame(matrix(nrow=max(OH_ponctuels_4326@data$DATE_FIN)+1,ncol=6))
colnames(nb_fonction_an) <-c(1:6)
# rownames(nb_fonction_an) <- as.numeric(rownames(nb_fonction_an))-1
nb_fonction_an$annee <- as.numeric(rownames(nb_fonction_an))-1
#remplissage du tableau
for (n in 1:6){ # pour toutes les colonnes valeurs urbaines existantes
  for (i in 0:max(nb_fonction_an$annee)) { #pour toutes les lignes(2016)
    nb_fonction_an[i+1,n] <- sum (OH_ponctuels_4326@data$V_URB==n & OH_ponctuels_4326@data$DATE_DEB<=i & OH_ponctuels_4326@data$DATE_FIN>=i)
  } # remplir cette colonne de la somme des individus ayant cette valeur urbaine et ayant existé dans les bornes données
}

m_fonction_an <- melt(nb_fonction_an, id.vars="annee")
m_fonction_an$v_urb <- as.numeric(m_fonction_an$variable)
m_fonction_an$v_urb <- cut (m_fonction_an$v_urb,
                            breaks=c(1,2,3,4,5,6,7),
                            labels=c("voirie, aménagement",
                                     "structures défensives et militaires",
                                     "constructions civiles",
                                     "édifices relifieux",
                                     "lieux d'inhumation",
                                     "lieux de commerce, artisanat, production"),
                            right=FALSE,
                            include.lowest = TRUE)


#HTMLS
source.info <- HTML("L'application \"explOH\" est développée par Lucie Nahassia dans le cadre de sa thèse de doctorat. </br>
                    <titre_info></titre_info>
                    Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam nec libero suscipit, interdum eros sed, dapibus tellus. Vestibulum consectetur, dui nec finibus rhoncus, velit nisl tristique tellus, non ultrices sapien libero eu velit. Donec tempus finibus auctor. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Maecenas eleifend elit nisi, a rutrum enim maximus rutrum. Donec porttitor blandit finibus. Ut nisi nibh, bibendum a metus eget, vestibulum mattis est. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Phasellus ac elit a lectus sodales porta. Etiam convallis tristique quam vitae mollis. Integer sit amet ipsum eu urna gravida vestibulum et a augue. Etiam dapibus mauris diam, in ornare nulla gravida nec. Pellentesque pharetra enim tortor, sit amet auctor felis tincidunt sed. Nullam vitae ante eu nisi accumsan aliquam eget sit amet diam.
                    Duis lobortis sagittis leo eget placerat. Proin lacinia gravida felis, eget venenatis felis euismod quis. Aenean rutrum tristique risus, lacinia scelerisque nunc placerat vel. Suspendisse eget elementum enim. Mauris vitae lacus velit. Aliquam porttitor imperdiet iaculis. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Morbi at commodo enim. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Etiam lobortis urna orci, ut accumsan nisi lobortis vitae. Quisque porttitor augue est, vel ultricies sem sollicitudin sit amet. Suspendisse vestibulum erat eu tempus consequat. Etiam a arcu sit amet lectus dapibus suscipit et quis leo. 
                    ")


source.signature <- HTML(
'<p class="signature">',
"Application développée par",
"<a href=\"http://www.parisgeo.cnrs.fr/spip.php?article6441\" target=\"_blank\">Lucie Nahassia</a>",
", 2017"
)

# 
# tags$p(class="signature","Application en cours de développement par <a, href="http://www.parisgeo.cnrs.fr/spip.php?article6441", target="_blank">Lucie Nahassia</a>, 2017")
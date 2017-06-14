################################
# Shiny app pour afficher les objets selon le temps 
# juin 2017
# chargement des données pour explOH_7
################################


EPSG = make_EPSG()
proj_2154 = EPSG[which(EPSG$code == 2154), "prj4"]
proj_4326 = EPSG[which(EPSG$code == 4326), "prj4"]



#####
## Import données 

## 1. Données contexte
#Import > shapefile
ens_urb_total <- st_read(dsn="./data/shapes", layer="ensembles_urbains", stringsAsFactors = FALSE, quiet = TRUE)
ens_urb <- st_transform(ens_urb_total, proj_4326)
ens_urb$date_fin <- ens_urb$date_fin-1 #dates à modifier dans shapefile d'origine
# View(ens_urb_total)
traits_rive_total <- read_sf(dsn="./data/shapes", layer="traits_rive_2154", stringsAsFactors = FALSE)
traits_rive <- st_transform(traits_rive_total, proj_4326)


## 2. Objets historiques
#geometries riches
#on peut rajouter du quiet
points <- st_read(dsn="./data/OH_3geom.sqlite", layer="oh_pt_uniques", quiet=TRUE)
lignes <- st_read(dsn="./data/OH_3geom.sqlite", layer="oh_pl_uniques", quiet=TRUE)
polygones <- st_read(dsn="./data/OH_3geom.sqlite", layer="oh_pg_uniques", quiet=TRUE)
OH_geom_base <- rbind(points,lignes,polygones)
OH_geom <- OH_geom_base[OH_geom_base$V_USAGE != 11
                        & OH_geom_base$V_USAGE < 70
                        & OH_geom_base$DATE_FIN > -25,]
#OH_ponctuels
# OH_ponctuels <- st_read(dsn="C:/Users/lue/Sync/1recherche/1these/SIG/BDD/ToToPi_GDB/ToToPI_V2/OH_3geom.sqlite", layer="oh_ponctuels", quiet=TRUE)

#reprojetction pour leaflet
OH_geom <- st_transform(OH_geom,proj_4326)
# OH_ponctuels <- st_transform(OH_ponctuels,proj_4326)


#ajout d'une colonne valeur urb = V_URB =>> MAPPLY ?
OH_geom$V_URB <- cut (OH_geom$V_USAGE,
                              breaks=c(0,20,30,40,50,60,70),
                              labels=c(1,2,3,4,5,6),
                              right=FALSE,
                              include.lowest = TRUE)

# OH_ponctuels$V_URB <- cut (OH_ponctuels$V_USAGE,
#                                 breaks=c(0,20,30,40,50,60,70),
#                                 labels=c(1,2,3,4,5,6),
#                                 right=FALSE,
#                                 include.lowest = TRUE)


#ajout d'une colonne nom valeur urb = V_URB_NOM
OH_geom$V_URB_NOM <- cut (OH_geom$V_USAGE,
                          breaks=c(0,20,30,40,50,60,70),
                          labels=c("1.voirie, aménagement",
                                   "2.structures défensives et militaires",
                                   "3.constructions civiles",
                                   "4.édifices religieux",
                                   "5.lieux d'inhumation",
                                   "6.lieux de commerce, artisanat, production"),
                          right=FALSE,
                          include.lowest = TRUE)


# OH_ponctuels$V_URB_NOM <- cut (OH_ponctuels$V_USAGE,
#                                breaks=c(0,20,30,40,50,60,70),
#                                labels=c("1.voirie, aménagement",
#                                         "2.structures défensives et militaires",
#                                         "3.constructions civiles",
#                                         "4.édifices religieux",
#                                         "5.lieux d'inhumation",
#                                         "6.lieux de commerce, artisanat, production"),
#                                right=FALSE,
#                                include.lowest = TRUE)

#recodage de la plus petite date = -25
OH_geom$DATE_DEB[OH_geom$DATE_DEB < -25] <- -25
# OH_ponctuels$DATE_DEB[OH_ponctuels$DATE_DEB < -25] <- -25

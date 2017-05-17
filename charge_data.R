################################
# Shiny app pour afficher les objets selon le temps 
# mars 2017
# chargement des données pour explOH_5
################################


EPSG = make_EPSG()
proj_2154 = EPSG[which(EPSG$code == 2154), "prj4"]
proj_4326 = EPSG[which(EPSG$code == 4326), "prj4"]



#####
## Import données 

## 1. Données contexte
#Import > shapefile
ens_urb_total <- readOGR(dsn="./data/shapes", layer="ensembles_urbains", stringsAsFactors = FALSE)
ens_urb <- spTransform(ens_urb_total, CRS(proj_4326))
ens_urb$date_debut <- as.numeric(ens_urb$date_debut)
ens_urb$date_fin <- as.numeric(ens_urb$date_fin)
ens_urb$date_fin <- ens_urb$date_fin-1 #dates à modifier dans shapefile d'origine
# View(ens_urb_total)
traits_rive_total <- readOGR(dsn="./data/shapes", layer="traits_rive_2154", stringsAsFactors = FALSE)
traits_rive <- spTransform(traits_rive_total, CRS(proj_4326))
traits_rive$SELEC <- iconv(traits_rive$SELEC, "UTF-8")
traits_rive$FIAB <- iconv(traits_rive$FIAB, "UTF-8")
traits_rive$DEBUT <- as.integer(traits_rive$DEBUT)
traits_rive$FIN <- as.integer(traits_rive$FIN)

## 2. Objets historiques
## PBM d'encoding (source et dest en UTF8 mais dans la connection DB est transformé :/) >> pour le moment réencodage ligne à ligne == dégueu
## import géométries complètes et tout en points

#--------- depuis shapes (exportés ci-dessous) pour aller + vite ----
## Attention pour le moment shape avec v_usage 11 et v_usage >70, date_deb < 0
# OH_geom_pg <- readOGR(dsn="./data/OH_geom", layer="OH_geom_pg", stringsAsFactors = FALSE)
# OH_geom_pl <- readOGR(dsn="./data/OH_geom", layer="OH_geom_pl", stringsAsFactors = FALSE)
# OH_geom_pt <- readOGR(dsn="./data/OH_geom", layer="OH_geom_pt", stringsAsFactors = FALSE)
#----

#-------- Depuis sqlite----
#---- connexions ----
db_geom <- dbConnect(SQLite(), "data/OH_3geom.sqlite")
OH <- dbGetQuery(db_geom, "select * from OH where V_USAGE != 11 and V_USAGE < 70 and APPARITION >= -25")
OH_geom_pt_source <- dbGetQuery(db_geom, "select * from OH_pt_uniques where V_USAGE != 11 and V_USAGE < 70 and DATE_FIN >= -25")
OH_geom_pl_source <- dbGetQuery(db_geom, "select * from OH_pl_uniques where V_USAGE != 11 and V_USAGE < 70 and DATE_FIN >= -25")
OH_geom_pg_source <- dbGetQuery(db_geom, "select * from OH_pg_uniques where V_USAGE != 11 and V_USAGE < 70 and DATE_FIN >= -25")
OH_ponctuels_source <- dbGetQuery(db_geom, "select * from OH_ponctuels where V_USAGE != 11 and V_USAGE < 70 and DATE_FIN >= -25")

#---- conversion des tables en spatial data frame ----

# AVEC GEOMETRIE
##POINTS
liste_pt <- mapply(function(geom, id){readWKT(text=geom, id=id, p4s=proj_2154)}, OH_geom_pt_source$geom_wkt, row.names(OH_geom_pt_source))
tab_pt <- do.call(rbind,liste_pt)
OH_geom_pt <- SpatialPointsDataFrame(tab_pt, OH_geom_pt_source[,c(1:11, 15)],proj4string=proj_2154)
rm(tab_pt,liste_pt)

##POLYGONES
liste_pg <- mapply(function(geom, id){readWKT(text=geom, id=id, p4s=proj_2154)}, OH_geom_pg_source$geom_wkt, row.names(OH_geom_pg_source))
#end.time <- Sys.time()
# pbm de noms d'objets trop long - l'ensemble de la description des polygones
names(liste_pg) <- c(1:length(liste_pg))
tab_pg <- do.call(rbind,liste_pg)
OH_geom_pg <- SpatialPolygonsDataFrame(tab_pg, OH_geom_pg_source[,c(1:11, 15)])
rm(tab_pg,liste_pg)

##LIGNES
liste_pl <- mapply(function(geom, id){readWKT(text=geom, id=id, p4s=proj_2154)}, OH_geom_pl_source$geom_wkt, row.names(OH_geom_pl_source))
tab_pl <- do.call(rbind,liste_pl)
OH_geom_pl <- SpatialLinesDataFrame(tab_pl, OH_geom_pl_source[,c(1:11, 15)])
rm(tab_pl,liste_pl)

#TOUT EN PONCTUEL
liste_ponctuels <- mapply(function(geom, id){readWKT(text=geom, id=id, p4s=proj_2154)}, OH_ponctuels_source$geom_wkt, row.names(OH_ponctuels_source))
tab_ponctuels <- do.call(rbind,liste_ponctuels)
OH_ponctuels <- SpatialPointsDataFrame(tab_ponctuels, OH_ponctuels_source[,1:11],proj4string=proj_2154)
rm(tab_ponctuels,liste_ponctuels)


#---- Préparation finale des tables : projection, encodage, ajout des v_urb (à faire dans sqlite ?) ----


#en reprojetant pour leaflet
OH_geom_pg_4326 <- spTransform(OH_geom_pg,CRS(proj_4326))
OH_geom_pl_4326 <- spTransform(OH_geom_pl,CRS(proj_4326))
OH_geom_pt_4326 <- spTransform(OH_geom_pt,CRS(proj_4326))
OH_ponctuels_4326 <- spTransform(OH_ponctuels,CRS(proj_4326))

#encodage (au moins faire un apply !)
OH_geom_pg_4326@data$NOM <- iconv(OH_geom_pg_4326@data$NOM, "UTF-8")
OH_geom_pg_4326@data$NOM_USAGE <- iconv(OH_geom_pg_4326@data$NOM_USAGE, "UTF-8")
OH_geom_pl_4326@data$NOM <- iconv(OH_geom_pl_4326@data$NOM, "UTF-8")
OH_geom_pl_4326@data$NOM_USAGE <- iconv(OH_geom_pl_4326@data$NOM_USAGE, "UTF-8")
OH_geom_pt_4326@data$NOM <- iconv(OH_geom_pt_4326@data$NOM, "UTF-8")
OH_geom_pt_4326@data$NOM_USAGE <- iconv(OH_geom_pt_4326@data$NOM_USAGE, "UTF-8")
OH_ponctuels_4326@data$NOM <- iconv(OH_ponctuels_4326@data$NOM, "UTF-8")
OH_ponctuels_4326@data$NOM_USAGE <- iconv(OH_ponctuels_4326@data$NOM_USAGE, "UTF-8")
ens_urb@data$descriptio <- iconv(ens_urb@data$descriptio, "UTF-8")


#ajout d'une colonne valeur urb = V_URB =>> MAPPLY ?

OH_geom_pg_4326$V_URB <- cut (OH_geom_pg_4326$V_USAGE,
                              breaks=c(0,20,30,40,50,60,70),
                              labels=c(1,2,3,4,5,6),
                              right=FALSE,
                              include.lowest = TRUE)

OH_geom_pt_4326$V_URB <- cut (OH_geom_pt_4326$V_USAGE,
                              breaks=c(0,20,30,40,50,60,70),
                              labels=c(1,2,3,4,5,6),
                              right=FALSE,
                              include.lowest = TRUE)

OH_geom_pl_4326$V_URB <- cut (OH_geom_pl_4326$V_USAGE,
                              breaks=c(0,20,30,40,50,60,70),
                              labels=c(1,2,3,4,5,6),
                              right=FALSE,
                              include.lowest = TRUE)

OH_ponctuels_4326$V_URB <- cut (OH_ponctuels_4326$V_USAGE,
                                breaks=c(0,20,30,40,50,60,70),
                                labels=c(1,2,3,4,5,6),
                                right=FALSE,
                                include.lowest = TRUE)



#ajout d'une colonne nom valeur urb = V_URB_NOM

OH_geom_pg_4326$V_URB_NOM <- cut (OH_geom_pg_4326$V_USAGE,
                                  breaks=c(0,20,30,40,50,60,70),
                                  labels=c("voirie, aménagement",
                                           "structures défensives et militaires",
                                           "constructions civiles",
                                           "édifices religieux",
                                           "lieux d'inhumation",
                                           "lieux de commerce, artisanat, production"),
                                  right=FALSE,
                                  include.lowest = TRUE)

OH_geom_pt_4326$V_URB_NOM <- cut (OH_geom_pt_4326$V_USAGE,
                                  breaks=c(0,20,30,40,50,60,70),
                                  labels=c("voirie, aménagement",
                                           "structures défensives et militaires",
                                           "constructions civiles",
                                           "édifices religieux",
                                           "lieux d'inhumation",
                                           "lieux de commerce, artisanat, production"),
                                  right=FALSE,
                                  include.lowest = TRUE)

OH_geom_pl_4326$V_URB_NOM <- cut (OH_geom_pl_4326$V_USAGE,
                                  breaks=c(0,20,30,40,50,60,70),
                                  labels=c("voirie, aménagement",
                                           "structures défensives et militaires",
                                           "constructions civiles",
                                           "édifices religieux",
                                           "lieux d'inhumation",
                                           "lieux de commerce, artisanat, production"),
                                  right=FALSE,
                                  include.lowest = TRUE)

OH_ponctuels_4326$V_URB_NOM <- cut (OH_ponctuels_4326$V_USAGE,
                                    breaks=c(0,20,30,40,50,60,70),
                                    labels=c("voirie, aménagement",
                                             "structures défensives et militaires",
                                             "constructions civiles",
                                             "édifices religieux",
                                             "lieux d'inhumation",
                                             "lieux de commerce, artisanat, production"),
                                    right=FALSE,
                                    include.lowest = TRUE)

#recodage de la plus petite date = -25
OH_geom_pt_4326@data$DATE_DEB[OH_geom_pt_4326@data$DATE_DEB < -25] <- -25
OH_geom_pg_4326@data$DATE_DEB[OH_geom_pg_4326@data$DATE_DEB < -25] <- -25
OH_geom_pl_4326@data$DATE_DEB[OH_geom_pl_4326@data$DATE_DEB < -25] <- -25
OH_ponctuels_4326@data$DATE_DEB[OH_ponctuels_4326@data$DATE_DEB < -25] <- -25

################################
# Shiny app pour afficher les objets selon le temps 
# L. Nahassia, 2019
# chargement des données
################################


EPSG = make_EPSG()
proj_2154 = EPSG[which(EPSG$code == 2154), "prj4"]
proj_4326 = EPSG[which(EPSG$code == 4326), "prj4"]



#-----------------------------------  2. Objets historiques ----

#geometries riches
points <- st_read(dsn="./data/ToToPI_VLN.gpkg", layer="OH_pt_uniques", quiet=TRUE)
lignes <- st_read(dsn="./data/ToToPI_VLN.gpkg", layer="OH_pl_uniques", quiet=TRUE)
polygones <- st_read(dsn="./data/ToToPI_VLN.gpkg", layer="OH_pg_uniques", quiet=TRUE)
OH_geom_base <- rbind(points,lignes,polygones)
OH_geom <- OH_geom_base[OH_geom_base$V_USAGE != 11
                        & OH_geom_base$V_USAGE < 70
                        & OH_geom_base$DATE_FIN > -25,]

#reprojection pour leaflet
OH_geom <- st_transform(OH_geom,proj_4326)


#ajout d'une colonne valeur urb = V_URB =>> MAPPLY ?
OH_geom$V_URB <- cut (OH_geom$V_USAGE,
                      breaks=c(0,20,30,40,50,60,70),
                      labels=c(1,2,3,4,5,6),
                      right=FALSE,
                      include.lowest = TRUE)

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

#ajout d'une colonne nom portee = V_URB_NOM
OH_geom$PORTEE_NOM <- cut (OH_geom$PORTEE,
                           breaks=c(1,2,3,4,5),
                           labels=c("1.petite - quartier",
                                    "2.moyenne - ville",
                                    "3.grande - région",
                                    "4.exceptionnelle - pays"),
                           right=FALSE,
                           include.lowest = TRUE)

#recodage de la plus petite date = -25 # voir si nécessaire fausse les données...
OH_geom$DATE_DEB[OH_geom$DATE_DEB < -25] <- -25
#recodage de la fiab disp des OH qui se finissent en 2015
OH_geom$FIAB_DISP[OH_geom$FIAB_DISP == ""] <- 0
OH_geom$FIAB_DISP <- droplevels(OH_geom$FIAB_DISP)
OH_geom$FIAB_APP <- as.factor(OH_geom$FIAB_APP)

#----------------------------------- 1. Données contextuelles & csv analyses ----

#shapefiles contextes
zst_total <-  st_read(dsn="./data/ToToPI_VLN.gpkg", layer="zones_spatio_temporelles",quiet=TRUE)
zst <- st_transform(zst_total, proj_4326)
traits_rive_total <- st_read(dsn="./data/shapes", layer="traits_rive_2154", stringsAsFactors = FALSE, quiet = TRUE)
traits_rive <- st_transform(traits_rive_total, proj_4326)

#OH pour analyse zone
OH_over_app <- read.csv("./data/OH_over_app.csv", encoding="UTF-8", header=TRUE)
OH_over_app$occupation <- factor(OH_over_app$occupation, levels=c("urbaine", "intermediaire","non urbaine"))
OH_over_app$densite <- factor(OH_over_app$densite, levels=c("3","2","1","0"))
OH_over_exi <-read.csv("./data/OH_over_exi.csv", encoding="UTF-8", header=TRUE)
OH_over_exi$densite <- factor(OH_over_exi$densite, levels=c("3","2","1","0"))
OH_over_exi$OH_NUM <- as.character(OH_over_exi$OH_NUM)
exi_ordre <- OH_over_exi %>% mutate(OH_NUM2 = forcats::fct_reorder(.f = OH_NUM,date_debut_OH)) #ordre pour tableau longitudinal
exi_transitions <- OH_over_exi %>% #tableau des transitions
  select(date_debut, date_fin, date_fin_OH, occupation, densite, OH_NUM, V_USAGE, V_URB, PORTEE) %>% 
  mutate(date_transition=date_fin, lead_occ =lead(occupation), lead_dens= lead(densite)) %>% 
  mutate(trans_occ = paste(occupation,"\nà\n", lead_occ, sep=" ") %>% as.factor,trans_dens = paste(densite,"à", lead_dens, sep=" ") %>% as.factor()) %>% 
  filter(date_fin!=date_fin_OH) %>% 
  select(date_transition, occupation, lead_occ, trans_occ, densite, lead_dens, trans_dens,OH_NUM, V_USAGE, V_URB, PORTEE)

#Tableau voisinage 3 périodes
OH_voisins_3P <- read.csv("./data/OH_voisins_3p.csv", encoding="UTF-8", header=TRUE)
# %>% as_tibble() %>% select(-geom) %>% mutate_at(.vars = seq(9,18), as.factor(.))

#----------------------------------- 3. Listes vurb et vusage ----

EF <- src_sqlite("./data/ToToPI_VLN.gpkg") %>% tbl("thesaurus") %>% as_tibble()
sub_vurb <- EF[EF$Code_EF %in% seq(10,60,10), c("V_urbaine", "Code_EF")]
liste_vurb <- paste( substring(sub_vurb$Code_EF,1,1), # vurb en 1,2,3
                     ".", stri_trans_totitle(sub_vurb$V_urbaine, opts_brkiter = stri_opts_brkiter(type = "sentence")), #nom vurb avec capitale
                     sep="")
liste_vurb <- liste_vurb[order(liste_vurb)]

sub_vusage <- EF[EF$Code_EF %in% unique(OH_geom$V_USAGE), c("Code_EF", "V_usage")]#pour le moment car nom_usages pas bon pour 41 et 43 (?)
liste_vusage <- paste(sub_vusage$Code_EF,
                      ".", 
                      stri_trans_totitle(sub_vusage$V_usage, opts_brkiter = stri_opts_brkiter(type = "sentence")), #nom vurb avec capitale
                      sep="")
liste_vusage <- liste_vusage[order(liste_vusage)] 


################################
# Shiny app pour afficher les objets selon le temps avec leaflet
# juin 2017
# Server pour explOH_8
################################

#####
library(shiny)

shinyServer(function(input, output, session) {
  
  #nombre random pour frise début
  date_random <- sample (-25:2015, 2)
  
  #----------------------------------- #1. carte de base ----
  #ne rien mettre dedans qui change
  output$map <- renderLeaflet({
    
    leaflet() %>%
      setView(lat=47.394211, lng=0.687247, zoom = 15) %>%
      
      ##tiles
      addProviderTiles("CartoDB.Positron", group="clair") %>%
      addProviderTiles("Esri.WorldImagery", group = "satellite") %>%
      
      ##layer control
      addLayersControl(
        baseGroups = c("clair", "satellite"),
        overlayGroups = c("géometries", "ensembles urbains", "traits de rive"),
        options=layersControlOptions(autoZIndex=TRUE)
      ) %>%
      hideGroup(c("traits de rive","ensembles urbains")) %>%    
      
      #légende
      addLegend(position="bottomlef", 
                title = "Valeurs urbaines des OH", 
                pal =  palette_fonctions, 
                values = OH_geom$V_URB_NOM, 
                opacity = 1)
    
  })
  
  
  #----------------------------------- #2. Declaration des reactive objects ----
  
  #OH à supprimer, ajouter, et tableau en memoire (au début toutes dates toutes valeurs urbaines)
  OH_subset <- reactiveValues(
    OH_A = NULL,
    OH_B = NULL,
    ID_del= NULL,
    ID_add = NULL,
    tab_add = NULL,
    message = NULL
  )
  #contexte
  ens_urb_subset <- reactiveValues(tab = ens_urb)
  traits_rive_subset <- reactiveValues(tab = traits_rive)
  #couleurs
  legende <- reactiveValues(
    alpha_polygones = NA,
    pal_couleurs= NA,
    pal_legend = NA,
    val_legend = NA,
    title_legend = NA)
  
  
  observe({
    # updateTextInput(session,"test", value=nrow(OH_subset$tab))
    
  })
  
  #----------------------------------- #3. INPUT > INPUT : Mise à jour slider temps selon autres éléments (graphes et élements textes) ----
  
  # observe({
  #   req(input$ohfreq_brush$xmin,input$ohfreq_brush$xmax)
  #   updateSliderInput(session = session,
  #                     inputId = "limites",
  #                     # value=c(subset_limites_temps$date_min,subset_limites_temps$date_max)
  #                     value=c(input$ohfreq_brush$xmin,input$ohfreq_brush$xmax)
  #   )
  # })
  
  #date de départ pour lancer l'appli
  req(date_random)
  updateSliderInput(session = session,
                    inputId = "limites",
                    value=c(min(date_random),max(date_random)))
  
  observeEvent(input$selec_bornes_temps, {
    req(input$borne_temps_1, input$borne_temps_2)
    updateSliderInput(session = session,
                      inputId = "limites",
                      # value=c(subset_limites_temps$date_min,subset_limites_temps$date_max)
                      value=c(input$borne_temps_1,input$borne_temps_2))
  })
  
  #----------------------------------- #4. INPUT > DONNEES : sélection des subset en fonction des INPUTS ----
  
  ##contextes (temps)
  observe({
    
    req(max(input$limites), min(input$limites))
    ens_urb_subset$tab <- ens_urb %>% filter (date_debut<=max(input$limites) & date_fin>=min(input$limites))
    traits_rive_subset$tab <- traits_rive %>% filter (DEBUT<=max(input$limites) & FIN>=min(input$limites))
    
  })
  
  ##Objets Historiques (temps, fonctions)
  observe({
    req(max(input$limites), min(input$limites), input$choix_fonctions)
    #subset demandé par utilisateur
    OH_subset$OH_B <- OH_geom %>%
      filter(DATE_DEB<=max(input$limites)) %>%
      filter(DATE_FIN>=min(input$limites)) %>%
      filter(V_URB %in% c(input$choix_fonctions[1:6]))
    #comparaison des ID du subset actuel et du subset précédent (OH_A)
    OH_subset$ID_del <- setdiff(OH_subset$OH_A$OH_NUM, OH_subset$OH_B$OH_NUM) #ID des Shapes à supprimer
    OH_subset$ID_add <- setdiff(OH_subset$OH_B$OH_NUM, OH_subset$OH_A$OH_NUM) #ID des Shapes à ajouter
    #subset des OH à ajouter
    OH_subset$tab_add <- OH_geom %>% filter(OH_NUM %in% OH_subset$ID_add)
    #subset actuel mis en mémoire
    OH_subset$OH_A <- OH_subset$OH_B
    
    #message HTML nombre d'OH affichés
    OH_subset$message <- paste(nrow(OH_subset$OH_B), " OH existants", sep="")
    
  })
  
  
  #----------------------------------- #5. INPUT > OUTPUT ----
  
  #CHANGEMENT COULEURS
  ## quand l'utilisateur choisi de modifier le type de legende
  ## > modification des couleurs et des valeurs pour construire la légende
  ## > réinitialisation des OH à afficher
  observeEvent (input$couleur_OH, {
    if (input$couleur_OH == "v_urb") # afficher selon valeurs urbaines
    { legende$alpha_polygones <- 0.7
    legende$pal_couleurs <- palette_fonctions
    legende$pal_legend <- palette_fonctions
    legende$val_legend <- OH_geom$V_URB_NOM
    legende$title_legend <- "Valeurs urbaines des OH"}
    
    else if (input$couleur_OH == "portee") # afficher selon portée
    { legende$alpha_polygones <- 0.9
    legende$pal_couleurs <- palette_portees
    legende$pal_legend <- palette_portees
    legende$val_legend <- OH_geom$PORTEE
    legende$title_legend <- "Niveau de portée des OH"}
    
    else if (input$couleur_OH == "duree") # afficher selon durée d'existence REVOIR CETTE LEGENDE
      #couleurs calculées sur le même intervalle (le plus global == OHgeom) sinon cela devrait donner une légende différente pour chaque type de geom
    { legende$alpha_polygones <- 0.8
    legende$pal_couleurs <- ramp_duree
    legende$pal_legend <- ramp_duree
    legende$val_legend <- (OH_geom$DATE_FIN - OH_geom$DATE_DEB)
    legende$title_legend <- "Durée d'existence des OH"}
    
    #Subset en cours d'affichage (OH_A) à redessiner
    OH_subset$tab_add <- OH_subset$OH_A
    
    #changement de légende
    leafletProxy("map") %>%
      clearControls() %>%
      addLegend(position="bottomlef", title = legende$title_legend, pal = legende$pal_legend, values = legende$val_legend, opacity = 1)
    
  })
  
  #MAP OUTPUT
  
  observe ({
    OH_add <- OH_subset$tab_add
    OH_pt <- OH_add[st_geometry_type(OH_add)=="POINT",] %>% st_cast("POINT")
    #ajout d'un OH dummy à OH_pt pour contourner le bug de leafletProxy avec df vide (OH généré dans global)
    OH_pt <- rbind(OH_pt, null_row)
    OH_pg <- OH_add[st_geometry_type(OH_add)=="MULTIPOLYGON",] %>% st_cast("MULTIPOLYGON")
    OH_pl <- OH_add[st_geometry_type(OH_add)=="MULTILINESTRING",] %>% st_cast("MULTILINESTRING")
    # output$tableau <- renderDataTable(OH_pt[,1:7])
    # updateTextInput(session,"test", value=nrow(OH_subset$tab_add))
    
    #couleurs des OH 
    if (input$couleur_OH == "v_urb")
    { couleurs_pg<- ~legende$pal_couleurs(OH_pg$V_URB_NOM)
    couleurs_pl<- ~legende$pal_couleurs(OH_pl$V_URB_NOM)
    couleurs_pt <- ~legende$pal_couleurs(OH_pt$V_URB_NOM)}
    else if (input$couleur_OH == "portee")
    {  couleurs_pg <- ~legende$pal_couleurs(OH_pg$PORTEE)
    couleurs_pl<- ~legende$pal_couleurs(OH_pl$PORTEE)
    couleurs_pt <- ~legende$pal_couleurs(OH_pt$PORTEE)}
    else if (input$couleur_OH == "duree")
    {  couleurs_pg <- ~legende$pal_couleurs(OH_pg$V_URB_NOM)
    couleurs_pl<- ~legende$pal_couleurs(OH_pl$V_URB_NOM)
    couleurs_pt <- ~legende$pal_couleurs(OH_pt$V_URB_NOM)}
    
    ## POPUP
    popup_ens_urb <- texte_popup_ens_urb(ens_urb_subset$tab)
    popup_traits_rive <- texte_popup_traits_rive(traits_rive_subset$tab)
    popup_pg <-texte_popup_OH(OH_pg)
    popup_pl <-texte_popup_OH(OH_pl)
    popup_pt <-texte_popup_OH(OH_pt)
    # browser()
    
    # CARTE
    leafletProxy("map", data=OH_pt) %>%
      clearMarkers() %>%
      clearGroup(group=c("ensembles urbains", "traits de rive")) %>%
      removeControl(layerId="nombre") %>% 
      #nombre d'OH
      addControl(position="bottomleft", html=OH_subset$message, layerId = "nombre") %>% 
      #contexte
      addPolygons(data=ens_urb_subset$tab,
                  group="ensembles urbains",
                  color="black",
                  weight=2,
                  dashArray="3,5",
                  fill=FALSE,
                  popup=popup_ens_urb) %>%
      addPolylines(data=traits_rive_subset$tab,
                   group="traits de rive",
                   color="blue",#pointillés ?
                   weight=3,
                   dashArray ="5,5",
                   fillOpacity = 0.9,
                   popup=popup_traits_rive) %>%
      #OH
      addPolygons(data=OH_pg,
                  stroke = TRUE,
                  weight=1,
                  opacity=legende$alpha_polygones,
                  color=couleurs_pg,
                  group="géometries",
                  layerId= as.character(OH_pg$OH_NUM),
                  popup=popup_pg) %>%
      addCircles(        radius=10,
                         color=couleurs_pt,
                         stroke = FALSE,
                         fillOpacity = 0.7,
                         group="géometries",
                         layerId= as.character(OH_pt$OH_NUM),
                         popup=popup_pt) %>%
      addPolylines(data=OH_pl,
                   weight=1,
                   color=couleurs_pl,
                   opacity= 0.7,
                   group="géometries",
                   layerId= as.character(OH_pl$OH_NUM),
                   popup = popup_pl) %>%
      removeShape(layerId=as.character(OH_subset$ID_del)) %>% 
      removeShape(layerId="0")
    # possible d'ajouter className pour css
    
    
    
  })
  
  
  #----------------------------------- #6. INTERACTIONS AUTRES ----
  
  # mise en valeur de l'OH sélectionnée, même si elle est hors de ce qui est affiché
  observeEvent(input$selec_OH, {
    
    #*******
    #A FAIRE
    # voir si zoom sur bouding box (pbm avec les points et les petits polygones)
    #*******
    
    ## selection selon le type de géométrie
    leafletProxy("map") %>% clearGroup("selection")
    choix_OH <- input$num_OH
    
    if (nrow(OH_geom[OH_geom$OH_NUM == choix_OH,])<1)
    { updateTextInput(session, "num_OH", value = "aucun OH correspondant") }
    
    else if (st_geometry_type(OH_geom[OH_geom$OH_NUM == choix_OH,])=="MULTIPOLYGON") {
      
      this.OH_geom <- OH_geom[OH_geom$OH_NUM == choix_OH,] %>% st_cast("MULTIPOLYGON")
      # this.OH_ponctuel <- subset(OH_ponctuels_subset$tab, OH_NUM ==  choix_OH)
      this.coords <- st_bbox(this.OH_geom)
      this.x <-  this.coords[[1]]
      this.y <- this.coords[[2]]
      this.popup <- texte_popup_OH(this.OH_geom)  ## POPUP
      
      leafletProxy("map") %>%
        setView(lat=this.y, lng=this.x, zoom = 18) %>%
        addPolygons(data=this.OH_geom,
                    stroke = TRUE,
                    color= "black",
                    opacity= 0.7,
                    weight= 5,
                    fill = FALSE,
                    group="selection",
                    popup=this.popup)
    }
    
    else if (st_geometry_type(OH_geom[OH_geom$OH_NUM == choix_OH,])=="MULTILINESTRING") {
      
      this.OH_geom <- OH_geom[OH_geom$OH_NUM == choix_OH,] %>% st_cast("MULTILINESTRING")
      # this.OH_ponctuel <- subset(OH_ponctuels_subset$tab, OH_NUM ==  choix_OH)
      this.coords <- st_bbox(this.OH_geom)
      this.x <-  this.coords[[1]]
      this.y <- this.coords[[2]]
      this.popup <- texte_popup_OH(this.OH_geom)  ## POPUP
      
      
      leafletProxy("map") %>%
        setView(lat=this.y, lng=this.x, zoom = 18) %>%
        addPolylines(data=this.OH_geom,
                     weight=4,
                     color="black",
                     group="selection",
                     popup=this.popup)
    }
    
    else if (st_geometry_type(OH_geom[OH_geom$OH_NUM == choix_OH,])=="POINT") {
      
      this.OH_geom <- OH_geom[OH_geom$OH_NUM == choix_OH,] %>% st_cast("POINT")
      
      this.coords <- st_bbox(this.OH_geom)
      this.x <-  this.coords[[1]]
      this.y <- this.coords[[2]]
      this.popup <- texte_popup_OH(this.OH_geom)  ## POPUP
      
      leafletProxy("map") %>%
        setView(lat=this.y, lng=this.x, zoom = 18) %>%
        addCircles(data=this.OH_geom,
                   radius=10,
                   stroke = TRUE,
                   color= "black",
                   opacity= 0.7,
                   weight= 5,
                   fill = FALSE,
                   group="selection",
                   popup=this.popup)
    }
    
    
    
  })
  
  #suppression selection
  observeEvent(input$deselec_OH, {leafletProxy("map") %>% clearGroup("selection")})
  
})



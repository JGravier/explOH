################################
# Shiny app pour afficher les objets selon le temps avec leaflet
# juin 2017
# Server pour explOH_7
################################

#####
library(shiny)




shinyServer(function(input, output, session) {
  
  #1. carte de base
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lat=47.394211, lng=0.687247, zoom = 15) %>%
      
      ##tiles
      addProviderTiles("CartoDB.Positron", group="clair") %>%
      # addProviderTiles("Esri.WorldImagery", group = "satellite") %>%
      
      ##layer control
      addLayersControl(
        baseGroups = c("clair", "satellite"),
        overlayGroups = c("géometries", "ensembles urbains", "traits de rive"),
        options=layersControlOptions(autoZIndex=TRUE)
      ) %>%
      hideGroup(c("traits de rive","ensembles urbains"))  
    
    
  })
  
  
  #2. Declaration des reactive objects
  
  #OH (au début toutes dates toutes valeurs urbaines)
  OH_subset <- reactiveValues(
    tab = OH_geom
  )
  #contexte
  ens_urb_subset <- reactiveValues(tab = ens_urb)
  traits_rive_subset <- reactiveValues(tab = traits_rive)
  #couleurs
  legende <- reactiveValues(
    couleurs_pt = NA,
    couleurs_pg = NA,
    couleurs_pl= NA,
    alpha_polygones = NA,
    pal_legend = NA,
    val_legend = NA,
    title_legend = NA)
  
  observe({
    updateTextInput(session,"test", value=nrow(OH_subset$tab))
    
  })
  #3. INPUT > INPUT : Mise à jour slider temps selon autres éléments (graphes et élements textes)
  
  # observe({
  #   req(input$ohfreq_brush$xmin,input$ohfreq_brush$xmax)
  #   updateSliderInput(session = session,
  #                     inputId = "limites",
  #                     # value=c(subset_limites_temps$date_min,subset_limites_temps$date_max)
  #                     value=c(input$ohfreq_brush$xmin,input$ohfreq_brush$xmax)
  #   )
  # })
  
  
  # observeEvent(input$selec_bornes_temps, {
  #   req(input$borne_temps_1, input$borne_temps_2)
  #   updateSliderInput(session = session,
  #                     inputId = "limites",
  #                     # value=c(subset_limites_temps$date_min,subset_limites_temps$date_max)
  #                     value=c(input$borne_temps_1,input$borne_temps_2))
  # })
  
  #4. INPUT > DONNEES : sélection des subset en fonction des INPUTS
  
  ##contextes (temps)
  observe({
    # req(max(input$limites), min(input$limites))
    ens_urb_subset$tab <- ens_urb %>% filter (date_debut<=max(input$limites) & date_fin>=min(input$limites))
    traits_rive_subset$tab <- traits_rive %>% filter (DEBUT<=max(input$limites) & FIN>=min(input$limites))
    
  })
  
  ##Objets Historiques (temps, fonctions)
  observe({
    # req(max(input$limites), min(input$limites), input$choix_fonctions)
    #quel que soit l'élément - temporel ou fonctionnel - qui change, tout le subset est recalculé = reste le + rapide
    OH_subset$tab <- OH_geom %>%
      filter(DATE_DEB<=max(input$limites)) %>%
      filter(DATE_FIN>=min(input$limites)) %>%
      filter(V_URB %in% c(input$choix_fonctions[1:6]))
    
  })
  
  
  #5. INPUT > OUTPUT
  
  #COULEURS LEGENDE SELON CHOIX
  observe ({
    
    OH_pt <- OH_subset$tab[st_geometry_type(OH_subset$tab)=="POINT",]
    OH_pg <- OH_subset$tab[st_geometry_type(OH_subset$tab)=="MULTIPOLYGON",]
    OH_pl <- OH_subset$tab[st_geometry_type(OH_subset$tab)=="MULTILINESTRING",]
    
    if (input$couleur_OH == "v_urb") # afficher selon valeurs urbaines
    {
      legende$couleurs_pt <- ~palette_fonctions(OH_pt$V_URB_NOM)
      legende$couleurs_pg <- ~palette_fonctions(OH_pg$V_URB_NOM)
      legende$couleurs_pl <- ~palette_fonctions(OH_pl$V_URB_NOM)
      legende$alpha_polygones <- 0.7
      legende$pal_legend <- palette_fonctions
      legende$val_legend <- OH_subset$tab$V_URB_NOM
      legende$title_legend <- "Valeurs urbaines des OH"}
    
    else if (input$couleur_OH == "portee") # afficher selon portée
    {
      legende$couleurs_pt <- ~palette_portees(OH_pt$PORTEE)
      legende$couleurs_pg <- ~palette_portees(OH_pg$PORTEE)
      legende$couleurs_pl <- ~palette_portees(OH_pl$PORTEE)
      legende$alpha_polygones <- 0.9
      legende$pal_legend <- palette_portees
      legende$val_legend <- OH_subset$tab$PORTEE
      legende$title_legend <- "Niveau de portée des OH (1=min, 4=max)"}
    
    else if (input$couleur_OH == "duree") # afficher selon durée d'existence REVOIR CETTE LEGENDE
      #couleurs calculées sur le même intervalle (le plus global == OHgeom) sinon cela devrait donner une légende différente pour chaque type de geom
    {
      legende$couleurs  <- ~ramp_duree(OH_subset$tab$DATE_FIN - OH_g_subset$tab$DATE_DEB)
      legende$alpha_polygones <- 0.8
      legende$pal_legend <- ramp_duree
      legende$val_legend <- (OH_subset$tab$DATE_FIN - OH_subset$tab$DATE_DEB)
      legende$title_legend <- "Durée d'existence des OH"}
    
  })
  
  
  #MAP OUTPUT
  observe ({
    
    OH_pt <- OH_subset$tab[st_geometry_type(OH_subset$tab)=="POINT",] %>% st_cast("POINT") #cast pour éviter les erreurs de class non comprises par leaflet
    OH_pg <- OH_subset$tab[st_geometry_type(OH_subset$tab)=="MULTIPOLYGON",] %>% st_cast("MULTIPOLYGON")
    OH_pl <- OH_subset$tab[st_geometry_type(OH_subset$tab)=="MULTILINESTRING",] %>% st_cast("MULTILINESTRING")
    
    ## POPUP
    popup_ens_urb <- texte_popup_ens_urb(ens_urb_subset$tab)
    popup_traits_rive <- texte_popup_traits_rive(traits_rive_subset$tab)
    popup_pg <-texte_popup_OH(OH_pg)
    popup_pl <-texte_popup_OH(OH_pl)
    popup_pt <-texte_popup_OH(OH_pt)
    
    ## CARTE
    leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls() %>%
      addPolygons(data=OH_pg,
                  stroke = TRUE,
                  weight=1,
                  opacity=legende$alpha_polygones,
                  color=legende$couleurs_pg,
                  group="géometries",
                  popup=popup_pg) %>%
      addCircles(data=OH_pt,
                 radius=10,
                 color=legende$couleurs_pt,
                 stroke = FALSE,
                 fillOpacity = 0.7,
                 group="géometries",
                 popup=popup_pt) %>%
      addPolylines(data=OH_pl,
                   weight=1,
                   color=legende$couleurs_pl,
                   opacity= 0.7,
                   group="géometries",
                   popup = popup_pl) %>%
      
      addLegend(position="bottomlef", title = legende$title_legend, pal = legende$pal_legend, values = legende$val_legend, opacity = 1) %>%
      
      addPolygons(data=ens_urb_subset$tab,
                  group="ensembles urbains",
                  color="black",
                  fill=FALSE,
                  weight=2,
                  popup=popup_ens_urb
      ) %>%
      addPolylines(data=traits_rive_subset$tab,
                   group="traits de rive",
                   color="blue",#pointillés ?
                   weight=3,
                   popup=popup_traits_rive
      )
    #possible d'ajouter className pour css
    #possible d'ajouter un ID pour suppprimer + spécifiquement cette légende
  })
  
  
  #6. INTERACTIONS AUTRES
  
  # mise en valeur de l'OH sélectionnée >> à gérer par rapport à la géométrie : zoom sur bounding box de la geom / mise en valeur des points et geom
  observeEvent(input$selec_OH, {
    
    #*******
    #A FAIRE
    #catch si pas numéro > ifelse sur typeof de la variable
    # voir si zoom sur bouding box (pbm avec les points et les petits polygones)
    #*******
    
    ## selection selon le type de géométrie
    leafletProxy("map") %>% clearGroup("selection")
    choix_OH <- as.numeric(input$num_OH)
    
    if (st_geometry_type(OH_subset$tab[OH_subset$tab$OH_NUM == choix_OH,])=="MULTIPOLYGON") {
      
      this.OH_geom <- OH_subset$tab[OH_subset$tab$OH_NUM == choix_OH,] %>% st_cast("MULTIPOLYGON")
      # this.OH_ponctuel <- subset(OH_ponctuels_subset$tab, OH_NUM ==  choix_OH)
      this.coords <- st_coordinates(this.OH_geom)
      this.x <-  this.coords[1]
      this.y <- this.coords[2]
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
    
    else if (st_geometry_type(OH_subset$tab[OH_subset$tab$OH_NUM == choix_OH,])=="MULTILINESTRING") {
      
      this.OH_geom <- OH_subset$tab[OH_subset$tab$OH_NUM == choix_OH,] %>% st_cast("MULTILINESTRING")
      # this.OH_ponctuel <- subset(OH_ponctuels_subset$tab, OH_NUM ==  choix_OH)
      this.coords <- st_coordinates(this.OH_geom)
      this.x <-  this.coords[1]
      this.y <- this.coords[2]
      this.popup <- texte_popup_OH(this.OH_geom)
      
      leafletProxy("map") %>%
        setView(lat=this.y, lng=this.x, zoom = 18) %>%
        addPolylines(data=this.OH_geom,
                     weight=4,
                     stroke="black",
                     group="selection",
                     popup=this.popup)
      
    }
    
    else if (st_geometry_type(OH_subset$tab[OH_subset$tab$OH_NUM == choix_OH,])=="POINT") {
      
      this.OH_geom <- OH_subset$tab[OH_subset$tab$OH_NUM == choix_OH,] %>% st_cast("POINT")
      # this.OH_ponctuel <- subset(OH_ponctuels_subset$tab, OH_NUM ==  choix_OH)
      this.coords <- st_coordinates(this.OH_geom)
      this.x <-  this.coords[1]
      this.y <- this.coords[2]
      this.popup <- texte_popup_OH(this.OH_geom)
      
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
    
    else {updateTextInput(session, "num_OH", value = "aucun OH correspondant")}
    
  })
  
  #suppression selection
  observeEvent(input$deselec_OH, {leafletProxy("map") %>% clearGroup("selection")})
  
})



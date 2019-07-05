################################
# Shiny app pour afficher les objets selon le temps avec leaflet
# L. Nahassia, 2019
# Server
################################

#####
library(shiny)

shinyServer(function(input, output, session) {
  
  ###########################################################################
  ##################### ONGLET 1 : exploration carte OH #####################
  ###########################################################################
  
  #nombre random pour frise début
  date_random <- sample (-25:2015, 2)
  
  
  #----------------------------------- #1.1. carte de base ----
  #ne rien mettre dedans qui change
  output$map <- renderLeaflet({
    
    leaflet() %>%
      setView(lat=47.394211, lng=0.687247, zoom = 15) %>%
      ##tiles
      addProviderTiles("CartoDB.Positron", group="clair") %>%
      addTiles(options = tileOptions(opacity =0), attribution=" données : ToToPI, UMR7324 CITERES-LAT Université de Tours/CNRS") %>% 
      ##échelle
      addScaleBar(position="bottomright", scaleBarOptions(imperial=FALSE)) %>% 
      
      ##layer control
      addLayersControl(
        overlayGroups = c("géometries des OH", "ensembles urbains occupation", "ensembles urbains densité","traits de rive"),
        position="bottomleft",
        options=layersControlOptions(autoZIndex=TRUE, 
                                     collapsed=FALSE)
      ) %>%
      hideGroup(c("traits de rive","ensembles urbains occupation", "ensembles urbains densité")) %>%    
      
      #bouton mesure
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "meters",
        secondaryLengthUnit = "kilometers",
        primaryAreaUnit = "sqmeters",
        activeColor = "#428BCA",
        completedColor="428BCA",
        localization="fr"
      ) %>% 
      
      #bouton pour export
      onRender(
        "function(el, x) {
            L.easyPrint({
              title:'Enregistrer la carte',
              sizeModes: ['Current', 'A4Landscape', 'A4Portrait'],
              filename: 'export_carte',
              exportOnly: true,
              hideClasses: ['.leaflet-control-zoom','leaflet-control-layers'],
              hideControlContainer:false,
              customSpinnerClass:'epLoader',
              tileWait:1000
            }).addTo(this);
            }"
      )
  })
  
  
  #----------------------------------- #1.2. Declaration des reactive objects ----
  #OH à supprimer, ajouter, et tableau en memoire (au début toutes dates toutes valeurs urbaines)
  OH_subset <- reactiveValues(
    OH_A = OH_geom,
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
  
  
  #----------------------------------- #1.3. INPUT > INPUT : Mise à jour slider temps selon autres éléments (graphes et élements textes) ----
  
  #date de départ pour lancer l'appli
  req(date_random)
  updateSliderInput(session = session,
                    inputId = "limites",
                    value=c(min(date_random),max(date_random)))
  
  observeEvent(input$selec_bornes_temps, {
    req(input$borne_temps_1, input$borne_temps_2)
    updateSliderInput(session = session,
                      inputId = "limites",
                      value=c(input$borne_temps_1,input$borne_temps_2))
  })
  
  #----------------------------------- #1.4. INPUT > DONNEES : sélection des subset en fonction des INPUTS ----
  
  ##contextes (temps)
  observe({
    
    req(max(input$limites), min(input$limites))
    ens_urb_subset$tab <- ens_urb %>% filter (date_debut<=max(input$limites) & date_fin>=min(input$limites))
    traits_rive_subset$tab <- traits_rive %>% filter (DEBUT<=max(input$limites) & FIN>=min(input$limites))
    
  })
  
  ##Objets Historiques (temps, fonctions)
  observe({
    req(max(input$limites), min(input$limites))
    
    val_usage <- lapply(
      1:6, 
      function(i){
        substring(
          eval(parse(text=paste("input$picker_vurb_",i,sep=""))), #récupère toutes les options cochées
          1,2 #ne garde que les deux premiers caractères (=les numéros)
        )
      }) %>% unlist %>% as.numeric
    
    tab <- OH_geom %>% filter(V_USAGE %in% val_usage)
    updateTextInput(session,"test", value=nrow(tab))
    
    #subset demandé par utilisateur
    OH_subset$OH_B <- OH_geom %>%
      filter(DATE_DEB<=max(input$limites)) %>%
      filter(DATE_FIN>=min(input$limites)) %>%
      filter(V_USAGE %in% val_usage)
    #comparaison des ID du subset actuel et du subset précédent (OH_A)
    OH_subset$ID_del <- setdiff(OH_subset$OH_A$OH_NUM, OH_subset$OH_B$OH_NUM) #ID des Shapes à supprimer
    OH_subset$ID_add <- setdiff(OH_subset$OH_B$OH_NUM, OH_subset$OH_A$OH_NUM) #ID des Shapes à ajouter
    #subset des OH à ajouter
    OH_subset$tab_add <- OH_geom %>% filter(OH_NUM %in% OH_subset$ID_add)
    #subset actuel mis en mémoire
    OH_subset$OH_A <- OH_subset$OH_B
    
    #message HTML nombre d'OH affichés
    OH_subset$message <- paste(nrow(OH_subset$OH_B), " OH correspondants à la sélection", sep="")
    
  })
  
  
  
  #----------------------------------- #1.5. INPUT > OUTPUT ----
  
  #CHANGEMENT COULEURS
  ## quand l'utilisateur choisi de modifier le type de legende
  ## > modification des couleurs et des valeurs pour construire la légende
  ## > réinitialisation des OH à afficher
  observe ({
    
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
    legende$val_legend <- as.factor(OH_geom$PORTEE_NOM)
    legende$title_legend <- "Niveau de portée des OH"}
    
    else if (input$couleur_OH == "fiab_a") # afficher selon fiabilité d'apparition
    { legende$alpha_polygones <- 0.9
    legende$pal_couleurs <- palette_fiab_a
    legende$pal_legend <- palette_fiab_a
    legende$val_legend <- as.factor(OH_geom$FIAB_APP)
    legende$title_legend <- "Fiabilité de la date d'apparition des OH"}
    
    else if (input$couleur_OH == "fiab_d") # afficher selon fiabilité de disparition
    { legende$alpha_polygones <- 0.9
    legende$pal_couleurs <- palette_fiab_d
    legende$pal_legend <- palette_fiab_d
    legende$val_legend <- as.factor(OH_geom$FIAB_DISP)
    legende$title_legend <- "Fiabilité de la date de disparition des OH"}
    
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
    # OH_pt <- rbind(OH_pt, null_row)
    OH_pg <- OH_add[st_geometry_type(OH_add)=="MULTIPOLYGON",] %>% st_cast("MULTIPOLYGON")
    OH_pl <- OH_add[st_geometry_type(OH_add)=="MULTILINESTRING",] %>% st_cast("MULTILINESTRING")
    
    #couleurs des OH 
    if (input$couleur_OH == "v_urb")
    { couleurs_pg<- ~legende$pal_couleurs(OH_pg$V_URB_NOM)
    couleurs_pl<- ~legende$pal_couleurs(OH_pl$V_URB_NOM)
    couleurs_pt <- ~legende$pal_couleurs(OH_pt$V_URB_NOM)}
    else if (input$couleur_OH == "portee")
    {  couleurs_pg <- ~legende$pal_couleurs(OH_pg$PORTEE_NOM)
    couleurs_pl<- ~legende$pal_couleurs(OH_pl$PORTEE_NOM)
    couleurs_pt <- ~legende$pal_couleurs(OH_pt$PORTEE_NOM)}
    else if (input$couleur_OH == "fiab_a")
    {  couleurs_pg <- ~legende$pal_couleurs(OH_pg$FIAB_APP)
    couleurs_pl<- ~legende$pal_couleurs(OH_pl$FIAB_APP)
    couleurs_pt <- ~legende$pal_couleurs(OH_pt$FIAB_APP)}
    else if (input$couleur_OH == "fiab_d")
    {  couleurs_pg <- ~legende$pal_couleurs(OH_pg$FIAB_DISP)
    couleurs_pl<- ~legende$pal_couleurs(OH_pl$FIAB_DISP)
    couleurs_pt <- ~legende$pal_couleurs(OH_pt$FIAB_DISP)}
    
    #POPUP
    popup_ens_urb <- texte_popup_ens_urb(ens_urb_subset$tab)
    popup_traits_rive <- texte_popup_traits_rive(traits_rive_subset$tab)
    popup_pg <-texte_popup_OH(OH_pg)
    popup_pl <-texte_popup_OH(OH_pl)
    popup_pt <-texte_popup_OH(OH_pt)
    
    #date
    subset_dates <-  paste("Période : ", min(input$limites)," - " ,max(input$limites), sep="")
    
    # CARTE
    leafletProxy("map", data=OH_pt) %>%
      clearMarkers() %>%
      clearGroup(group=c("ensembles urbains occupation", "ensembles urbains densité", "traits de rive")) %>%
      removeControl(layerId = c("nombre","periode")) %>% 
      #nombre d'OH
      addControl(position="topright", html=OH_subset$message, layerId = "nombre") %>% 
      addControl(position="topright", html=subset_dates, layerId = "periode") %>%
      #contexte
      addPolygons(data=ens_urb_subset$tab,
                  group="ensembles urbains occupation",
                  color=palette_EU(ens_urb_subset$tab$occupation),
                  fillOpacity=0.1*ens_urb_subset$tab$densite,
                  weight=1,
                  popup=popup_ens_urb) %>%
      addPolygons(data=ens_urb_subset$tab,
                  group="ensembles urbains densité",
                  color=palette_EU2(ens_urb_subset$tab$densite),
                  fillOpacity=0.2,
                  weight=1,
                  popup=popup_ens_urb) %>%
      addPolylines(data=traits_rive_subset$tab,
                   group="traits de rive",
                   color="#2260aa",#pointillés ?
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
                  group="géometries des OH",
                  layerId= as.character(OH_pg$OH_NUM),
                  popup=popup_pg ) %>%
      addCircles(        radius=10, #data passée en argument du leaflet général pour contourner erreur
                         color=couleurs_pt,
                         stroke = FALSE,
                         fillOpacity = 0.7,
                         group="géometries des OH",
                         layerId= as.character(OH_pt$OH_NUM),
                         popup=popup_pt) %>%
      addPolylines(data=OH_pl,
                   weight=1,
                   color=couleurs_pl,
                   opacity= 0.7,
                   group="géometries des OH",
                   layerId= as.character(OH_pl$OH_NUM),
                   popup = popup_pl) %>%
      removeShape(layerId=as.character(OH_subset$ID_del)) %>% 
      removeShape(layerId="0")
    # possible d'ajouter className pour css
    
    
    
  })
  
  
  #TAB OUTPUT
  observe({
    tabOH <- OH_subset$OH_A %>% as.data.frame %>% select(-geom, -PORTEE_NOM ) %>% mutate(V_USAGE=as.factor(V_USAGE))
    output$tab_OH <-renderDataTable(
      tabOH,
      filter="bottom",
      options=list(pageLength=10,
                   info=TRUE,
                   scrollX=TRUE
      )
    )
    
  })
  
  
  #TELECHARGEMENT SUBSET
  # affiche le bouton seulement si le mdp est bon
  observeEvent(input$password_sub, {
    if(input$password_dl=="archeo7324") {
      output$place_dl <-renderUI(
        tagList(
          downloadButton("downloadData", label="télécharger")
        )
      )
      output$downloadData <- downloadHandler(
        filename = function() {
          if (input$type_dl == "geojson"){return("selectionOH.geojson")}
          else if(input$type_dl == "sqlite"){return("selectionOH.sqlite")}
          else if(input$type_dl == "csv"){return("selectionOH.csv")}
          else if(input$type_dl == "shapefile"){return("selectionOH.shp")}
        },
        content = function(file) {
          dlOH <- st_transform(OH_subset$OH_A, proj_2154) %>% select(-geom_wkt)
          if(input$type_dl == "csv"){st_write(obj=dlOH, dsn=file, layer_options = "GEOMETRY=AS_WKT")}
          else if(input$type_dl == "geojson"){st_write(obj=dlOH, dsn=file, layer="OH")}
          else if(input$type_dl == "sqlite"){st_write(obj=dlOH, dsn=file, layer="OH", dataset_options="SPATIALITE=YES", layer_options="FORMAT=SPATIALITE")}
        }
      )
    }
    else{output$place_dl <- output$place_dl <-renderUI(tagList()) }
  })
  #----------------------------------- #1.6. INTERACTIONS AUTRES ----
  
  #mise en valeur des OH qui apparaissent/disparaissent pendant la période
  observe({
    
    surl <- input$date_OH
    date_min <- min(input$limites)
    date_max <- max(input$limites)
    
    if(surl =="date_deb"){ 
      OH_sur <- OH_subset$OH_A %>% filter(DATE_DEB>=date_min & DATE_DEB<=date_max)
      col <- "#224f77"
    }else if(surl =="date_fin"){
      OH_sur <- OH_subset$OH_A %>% filter(DATE_FIN>=date_min & DATE_FIN<=date_max)
      col <- "#770924"
    }else if (surl =="sur_off"){
      OH_sur <- null_row
    }
    
    OH_pt <- OH_sur[st_geometry_type(OH_sur)=="POINT",] %>% st_cast("POINT")
    #ajout d'un OH dummy à OH_pt pour contourner le bug de leafletProxy avec df vide (OH généré dans global)
    # OH_pt <- rbind(OH_pt, null_row)
    OH_pg <- OH_sur[st_geometry_type(OH_sur)=="MULTIPOLYGON",] %>% st_cast("MULTIPOLYGON")
    OH_pl <- OH_sur[st_geometry_type(OH_sur)=="MULTILINESTRING",] %>% st_cast("MULTILINESTRING")
    
    leafletProxy("map", data=OH_pt) %>%
      clearGroup("surlignés")%>% 
      addPolygons(data=OH_pg,
                  stroke = TRUE,
                  fill=FALSE,
                  weight=2,
                  color=col,
                  group="surlignés") %>%
      addCircles(        radius=10, #data passée en argument du leaflet général pour contourner erreur
                         color=col,
                         weight = 2,
                         stroke = TRUE,
                         fill=FALSE,
                         group="surlignés") %>%
      addPolylines(data=OH_pl,
                   weight=2,
                   color=col,
                   opacity= 0.7,
                   group="surlignés")
    
  })
  
  
  # mise en valeur de l'OH sélectionnée, même si elle est hors de ce qui est affiché
  observeEvent(input$selec_OH_search, {
    
    ## selection selon le type de géométrie
    # leafletProxy("map") %>% clearGroup("selection")
    choix_OH <- input$selec_OH
    
    if (nrow(OH_geom[OH_geom$OH_NUM == choix_OH,])>0)
    {if (st_geometry_type(OH_geom[OH_geom$OH_NUM == choix_OH,])=="MULTIPOLYGON") {
      
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
                    color= "yellow",
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
                       color="yellow",
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
                     color= "yellow",
                     opacity= 0.7,
                     weight= 5,
                     fill = FALSE,
                     group="selection",
                     popup=this.popup)
      }}
    
    else  {updateTextInput(session, "selec_OH", value = "Pas d'OH correspondant")}
    
    
  })
  
  #suppression selection
  observeEvent(
    input$selec_OH_reset, {
      leafletProxy("map") %>%
        clearGroup("selection")
    })
  
  
  ###########################################################################
  ##################### ONGLET 2 : RYTHMES ##############################
  ########################################################################### 
  
  #----------------------------------- #3.1.déclaration reactive objects----
  tabs_rythme <- reactiveValues(melt_nombre=NULL, #nombre d'OH par année et par valeur urbaine/d'usage/portee
                                melt_pourc=NULL, #pourcentage d'OH par année et par valeur urbaine/d'usage/portee
                                apparition=NULL, #nombre d'apparition par valeur urbaine/d'usage/portee
                                disparition=NULL #nombre d'apparition par valeur urbaine/d'usage/portee
  )
  
  #----3.1.création des tableaux ----
  observeEvent(input$generer_tableaux_exi,{
    
    #signal charge
    removeUI(selector="#fini_tab", immediate=TRUE)
    insertUI(selector="#generer_tableaux_exi", 
             where="afterEnd", 
             ui=HTML('<span id=charge_tab> &nbsp &nbsp <i class="fas fa-circle-notch fa-spin"></i> &nbsp en cours </span>' ),
             immediate=TRUE)
    
    val_usage <- lapply( #récupére la liste des OH sélectionnés
      1:6, 
      function(i){
        substring(
          eval(parse(text=paste("input$picker_zones_",i,sep=""))), #récupère toutes les options cochées
          1,2 #ne garde que les deux premiers caractères (=les numéros)
        )
      }) %>% unlist %>% as.numeric
    OH_subset_exi <- OH_geom %>% st_drop_geometry() %>% as_tibble() %>% filter(V_USAGE %in% val_usage) #tableau subset
    
    
    if (input$select_fonction_rythmes=="vurb"){
      withProgress(message="tableau en cours de calcul", 
                   calcul_tableaux <- tableau.melt.vurb(OH_subset_exi) #calcul des tableaux
      )
      tabs_rythme$melt_nombre <- calcul_tableaux$melt_nb #tableaux dans les reactive values
      tabs_rythme$melt_pourc <- calcul_tableaux$melt_pourc #tableaux dans les reactive values
      tabs_rythme$apparition <- OH_subset_exi %>% plyr::count(c("DATE_DEB","V_URB")) %>% filter(DATE_DEB >= -25)
      tabs_rythme$disparition <- OH_subset_exi %>% plyr::count(c("DATE_FIN","V_URB"))%>% filter(DATE_FIN >= -25)
      
    }
    else if (input$select_fonction_rythmes=="vusage"){
      withProgress(message="tableau en cours de calcul", 
                   calcul_tableaux <- tableau.melt.vusage(OH_subset_exi) #calcul des tableaux
      )
      tabs_rythme$melt_nombre <- calcul_tableaux$melt_nb #tableaux dans les reactive values
      tabs_rythme$melt_pourc <- calcul_tableaux$melt_pourc #tableaux dans les reactive values
      tabs_rythme$apparition <- OH_subset_exi %>% plyr::count(c("DATE_DEB","V_USAGE")) %>% filter(DATE_DEB >= -25) %>% mutate(v_urb=substr(V_USAGE,1,1))
      tabs_rythme$disparition <- OH_subset_exi %>% plyr::count(c("DATE_FIN","V_USAGE"))%>% filter(DATE_FIN >= -25) %>% mutate(v_urb=substr(V_USAGE,1,1))
      
    }
    else if (input$select_fonction_rythmes=="portee"){
      withProgress(message="tableau en cours de calcul", 
                   calcul_tableaux <- tableau.melt.portee(OH_subset_exi) #calcul des tableaux
      )
      tabs_rythme$melt_nombre <- calcul_tableaux$melt_nb #tableaux dans les reactive values
      tabs_rythme$melt_pourc <- calcul_tableaux$melt_pourc #tableaux dans les reactive values
      tabs_rythme$apparition <- OH_subset_exi %>% plyr::count(c("DATE_DEB","PORTEE")) %>% filter(DATE_DEB >= -25)
      tabs_rythme$disparition <- OH_subset_exi %>% plyr::count(c("DATE_FIN","PORTEE"))%>% filter(DATE_FIN >= -25)
      
    }
    
    removeUI(selector="#charge_tab", immediate=FALSE)
    insertUI(selector="#generer_tableaux_exi", 
             where="afterEnd", 
             ui=HTML('<span id=fini_tab> &nbsp &nbsp <i class="fas fa-check"></i> &nbsp tableau prêt </span>' ),
             immediate=FALSE)
    
    
  })
  
  #----3.1.plots répartition simple----
  observeEvent(input$generer_graphiques_exi,{
    
    
    if(is.null(tabs_rythme$melt_nombre)){ #erreur si tableau pas généré
      insertUI(selector="#generer_graphiques_exi", 
               where="afterEnd", 
               ui=HTML('<span id=erreur_plot1>  générer le tableau </span>' ),
               immediate=TRUE)
      
    }
    else{ #sinon génére les graphiques
      
      removeUI(selector="#erreur_plot1", immediate=TRUE) #suppression du message d'erreur
      #signal chargement
      removeUI(selector="#fini_plot1", immediate=TRUE)
      insertUI(selector="#generer_graphiques_exi", 
               where="afterEnd", 
               ui=HTML('<span id=charge_plot1> &nbsp &nbsp <i class="fas fa-circle-notch fa-spin"></i> &nbsp en cours </span>' ),
               immediate=TRUE)
      
      if (input$select_fonction_rythmes=="vurb"){
        
        tabOH_nb <- tabs_rythme$melt_nombre #chargement des tableaux
        tabOH_pourc <- tabs_rythme$melt_pourc
        
        repartition <-plot.repartition( #creation des plots
          dfnb=tabOH_nb,
          dfpourc=tabOH_pourc,
          titre="valeur urbaine",
          couleur=couleurs_vurb)
        
        output$plot_exi_OH1 <- renderPlot(repartition$nb)  #rendu des plots
        output$plot_exi_OH2 <- renderPlot(repartition$pourc)
        

      }
      else if (input$select_fonction_rythmes=="portee"){
        
        tabOH_nb <- tabs_rythme$melt_nombre #chargement des tableaux
        tabOH_pourc <- tabs_rythme$melt_pourc
        
        repartition <-plot.repartition( #creation des plots
          dfnb=tabOH_nb,
          dfpourc=tabOH_pourc,
          titre="portée",
          couleur=couleurs_portees)
        
        output$plot_exi_OH1 <- renderPlot(repartition$nb) #rendu des plots
        output$plot_exi_OH2 <- renderPlot(repartition$pourc)
      }
      else if (input$select_fonction_rythmes=="vusage"){
        
        tabOH_nb <- tabs_rythme$melt_nombre #chargement des tableaux
        tabOH_pourc <- tabs_rythme$melt_pourc
        
        repartition <-plot.repartition.usage( #creation des plots
          dfnb=tabOH_nb,
          dfpourc=tabOH_pourc)
        
        output$plot_exi_OH1 <- renderPlot(repartition$nb) #rendu des plots
        output$plot_exi_OH2 <- renderPlot(repartition$pourc)
      }
      
      #signal fin
      removeUI(selector="#charge_plot1", immediate=FALSE)
      insertUI(selector="#generer_graphiques_exi", 
               where="afterEnd", 
               ui=HTML('<span id=fini_plot1> &nbsp &nbsp <i class="fas fa-check"></i></span>' ),
               immediate=FALSE)
      }
    })
  
  
  #----3.2.plots apparition et disparition----
  
  observeEvent(input$generer_graphiques_appdisp,{
    
    if(is.null(tabs_rythme$melt_nombre)){ #erreur si tableau pas généré
      insertUI(selector="#generer_graphiques_appdisp", 
               where="afterEnd", 
               ui=HTML('<span id=erreur_plot2> générer le tableau </span>' ),
               immediate=TRUE)
      
    }
    else{ #sinon génére les graphiques

      removeUI(selector="#erreur_plot2", immediate=TRUE) #suppression du message d'erreur
      #signal chargement
      removeUI(selector="#fini_plot2", immediate=TRUE)
      insertUI(selector="#generer_graphiques_appdisp",
               where="afterEnd",
               ui=HTML('<span id=charge_plot2> &nbsp &nbsp <i class="fas fa-circle-notch fa-spin"></i> &nbsp en cours </span>' ),
               immediate=TRUE)
      
      if (input$select_fonction_rythmes=="vurb"){
        output$plot_app_disp <- renderPlot(
          ggplot()+
            geom_line(data = tabs_rythme$melt_nombre %>% mutate(V_URB=stri_sub(variable,1,1)),
                      aes(x=annee, y=value),
                      color="grey60")+
            geom_col(data=tabs_rythme$apparition,
                     aes(x=DATE_DEB, y=freq,fill=V_URB),
                     width=15)+
            geom_col(data=tabs_rythme$disparition,
                     aes(x=DATE_FIN, y=-freq, fill=V_URB),
                     alpha=0.5,
                     width=15)+
            scale_fill_manual(values=couleurs_vurb)+
            facet_wrap(~V_URB, ncol=1, scales="free_y",
                       labeller = as_labeller(labels_vurb))+
            labs(x="année",
                 y="nombre d'OH",
                 title="Rythme d'apparition et de disparition des OH par valeur urbaine",
                 caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT"
            )+
            theme_fivethirtyeight()+
            scale_x_continuous(breaks = c(1,seq(200,2000,200)))+
            theme_ln()+
            theme(strip.text=element_text(hjust = 0))
        )
      }
      else if (input$select_fonction_rythmes=="portee"){
        
        output$plot_app_disp <- renderPlot(
          ggplot()+
            geom_line(data = tabs_rythme$melt_nombre %>% mutate(PORTEE=stri_sub(variable,1,1)),
                      aes(x=annee, y=value),
                      color="grey60")+
            geom_col(data=tabs_rythme$apparition %>% mutate(PORTEE=as.factor(PORTEE)),
                     aes(x=DATE_DEB, y=freq,fill=PORTEE),
                     width=15)+
            geom_col(data=tabs_rythme$disparition%>% mutate(PORTEE=as.factor(PORTEE)),
                     aes(x=DATE_FIN, y=-freq, fill=PORTEE),
                     alpha=0.5,
                     width=15)+
            scale_fill_manual(values=couleurs_portees)+
            facet_wrap(~PORTEE, ncol=1, scales="free_y",
                       labeller = as_labeller(labels_portees))+
            labs(x="année",
                 y="nombre d'OH",
                 title="Rythme d'apparition et de disparition des OH par portée",
                 caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT"
            )+
            theme_fivethirtyeight()+
            scale_x_continuous(breaks = c(1,seq(200,2000,200)))+
            theme_ln()+
            theme(strip.text=element_text(hjust = 0))
        )
      }
      else if (input$select_fonction_rythmes=="vusage"){
        
        output$plot_app_disp <- renderPlot(
          ggplot()+
            geom_col(data=tabs_rythme$apparition,
                     aes(x=DATE_DEB, y=freq,fill=v_urb),
                     width=15)+
            geom_col(data=tabs_rythme$disparition,
                     aes(x=DATE_FIN, y=-freq, fill=v_urb),
                     alpha=0.5,
                     width=15)+
            scale_fill_manual(values=couleurs_vurb)+
            facet_wrap(~V_USAGE, ncol=4, scales="free_y")+
            labs(x="année",
                 y="nombre d'OH",
                 title="Rythme d'apparition et de disparition des OH par valeurs d'usage",
                 caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT"
            )+
            theme_fivethirtyeight()+
            scale_x_continuous(breaks = c(1,seq(200,2000,200)))+
            theme_ln()+
            theme(strip.text=element_text(hjust = 0),
                  axis.text.x=element_text(angle = 90))
        )
      }
      
      #signal fin
      removeUI(selector="#charge_plot2", immediate=FALSE)
      insertUI(selector="#generer_graphiques_appdisp", 
               where="afterEnd", 
               ui=HTML('<span id=fini_plot2> &nbsp &nbsp <i class="fas fa-check"></i></span>' ),
               immediate=FALSE)
    }
    
  })
  
  
  
  ###########################################################################
  ##################### ONGLET 3 : AFC & CAH ##############################
  ###########################################################################
  
  #----------------------------------- #3.1.déclaration reactive objects----
  tab_contingence <- reactiveValues(tab=NULL)
  reacAFC <- reactiveValues(data=NULL, #resultat du dudi.coa
                            periodes = NULL, #découpage par période
                            variables = NULL, #variables fonctionnelle utilisée
                            tab_biplot = NULL, #tableau pour le biplot
                            dist_mat = NULL #matrice de distance khi-2 entre les lignes
                            
  )
  reacCAH <- reactiveValues(data=NULL)
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  #----------------------------------- #3.2.INPUT > DONNEES : mise à jour du tableau de contingence & calcul AFC ----
  observe(priority = 11, {
    nom <- paste("tab",input$select_var,input$select_periodes, sep="_")
    tab_contingence$tab <- get(nom)
    
    # maj nombre de classes pour la CAH (= nombre de lignes)
    classe_max <- nrow(tab_contingence$tab)
    updatePickerInput(session,"nombre_classes",
                      choices = seq(1,classe_max,1),
                      selected="4")
    
    AFC <-dudi.coa(tab_contingence$tab, scannf=FALSE, nf=6)
    reacAFC$data <- AFC
    reacAFC$periodes <- input$select_periodes
    reacAFC$variables <- input$select_var
    reacAFC$dist_mat <- dist.dudi(AFC)
    
    
  })
  
  #----------------------------------- #3.3.INPUT > OUTPUT ----
  
  #---- tableau de contingence----
  observe(priority = 10, {
    output$tab_contingence <-renderDataTable(
      tab_contingence$tab,
      options=list(pageLength = 10, scrollX=TRUE, searching=FALSE)
    )
  })
  
  #---- graphique, tableau variance des axes & choix des axes à ploter ----
  observe(priority = 9, {
    
    AFC <- reacAFC$data
    inertie_AFC <- summary.variance.dudi(AFC)
    
    #plot variance axes
    output$plot_inertie_axes <- renderPlot(
      barplot.dudi.variance(data=AFC, 
                            sumdata=inertie_AFC,
                            titre=NULL))
    output$tab_inertie_axes <- renderTable(inertie_AFC %>%  select(-CUTSCOLOR),  
                                           striped=TRUE,
                                           bordered=TRUE)
    
    
    #update de l'UI pour choix des axes à ploter
    updatePickerInput(session,"axe1",
                      choices = seq(1,AFC$nf,1),
                      selected="1")
    updatePickerInput(session,"axe2",
                      choices = seq(1,AFC$nf,1),
                      selected="2")
  })
  
  #---- AFC : BIPLOT et TAB  contribution ----
  observe(priority=8, {
    
    req(input$axe1, input$axe2)
    AFC <- reacAFC$data
    
    #tableau pour biplot
    biplot_col <- setNames(AFC$co, names(AFC$li))
    biplot_col$type_var <- input$select_var
    biplot_col$taille <- 35
    biplot_li <- AFC$li
    biplot_li$type_var <- input$select_periodes
    biplot_li$taille <- 100
    biplot_li$color_var <- "périodes"
    biplot_li$nom_var <- "périodes"
    
    
    #actualisation palettes, titre et liste_variable selon variables
    if (reacAFC$variables=="urb"){
      biplot_col$color_var <- str_sub(row.names(biplot_col), start=-1)
      liste_var <- sort(unique(as.character(OH_geom$V_URB_NOM)))
      biplot_col$nom_var <- liste_var
      palette_biplot <- couleurs_vurb
      names(palette_biplot) <- liste_var
      biplot_titre<- paste("Analyse factorielle des correspondances : valeurs urbaines * périodes de ", reacAFC$periodes, " ans", sep="")
    }
    else if (reacAFC$variables=="portee"){
      biplot_col$color_var <- str_sub(row.names(biplot_col), start=-1)
      liste_var <- sort(unique(as.character(OH_geom$PORTEE_NOM)))
      biplot_col$nom_var <- liste_var
      palette_biplot <- couleurs_portees
      names(palette_biplot) <- liste_var
      biplot_titre <- paste("Analyse factorielle des correspondances : portées * périodes de ", reacAFC$periodes, " ans", sep="")
    }
    else if (reacAFC$variables=="usage"){
      biplot_col$color_var <- str_sub(row.names(biplot_col), start=-2)
      biplot_col$nom_var <- liste_vusage
      palette_biplot <- cut (biplot_col$color_var %>% as.numeric(),
                             breaks=c(10,20,30,40,50,60,70),
                             labels=c("#e72535", "#0a8eb1", "#f6b01a", "#2fc6a0", "#703ab9","#ff7919"),
                             right=FALSE,
                             include.lowest = TRUE) %>% as.character()
      names(palette_biplot) <- liste_vusage
      biplot_titre <- paste("Analyse factorielle des correspondances : valeurs d'usage * périodes de ", reacAFC$periodes," ans", sep="")
    }
    
    
    if(input$masquer_biplot=="aucune"){tab_biplot <- rbind(biplot_li, biplot_col)}
    else if (input$masquer_biplot=="périodes"){tab_biplot <-biplot_col}
    else if (input$masquer_biplot=="caractéristiques fonctionnelles"){tab_biplot <-biplot_li}
    
    #axes
    axe1 <- as.numeric(input$axe1)
    axe2 <- as.numeric(input$axe2)
    
    #variance expliquée 
    pctvar_A1 <- round(AFC$eig[axe1]*100/sum(AFC$eig))
    pctvar_A2 <- round(AFC$eig[axe2]*100/sum(AFC$eig))
    titre_axe1 <- paste("Axe n°", axe1, " (",pctvar_A1, "% de variance expliquée)", sep="")
    titre_axe2 <- paste("Axe n°", axe2, " (",pctvar_A2, "% de variance expliquée)", sep="")
    
    #BIPLOT
    biplot <- 
      scatterD3(x=tab_biplot[,axe1], y=tab_biplot[,axe2],
                lab=row.names(tab_biplot),
                col_var = tab_biplot$nom_var,
                colors=c("périodes"="grey", palette_biplot),
                point_opacity = 0.8,
                hover_opacity = 1,
                hover_size = 2,
                xlab=titre_axe1,
                ylab=titre_axe2,
                symbol_lab="variables en lignes et colonnes",
                col_lab="type de variable",
                transitions = FALSE,
                fixed=TRUE,
                caption =list(title=biplot_titre,
                              subtitle ="L. Nahassia, Géographie-cité, 2019 | Sources : ToToPI, LAT, CITERES")
                
      )
    output$plot_biplot <- renderScatterD3(biplot)
    
    # CONTRIBUTIONS
    contrib_AFC <- inertia.dudi(AFC, row.inertia = TRUE, col.inertia = TRUE)
    
    output$contrib_periodes <-renderDataTable(
      contrib_AFC$row.abs,
      options=list(pageLength = 10, scrollX=TRUE, searching=FALSE)
    )
    
    output$contrib_variables <-renderDataTable(
      contrib_AFC$col.abs,
      options=list(pageLength = 10, scrollX=TRUE, searching=FALSE)
    )
    
    
  }) # fin observe BIPLOT AFC
  
  #---- CAH : dendrogramme & inertie ----
  observe(priority=7, {
    
    req(reacAFC$data)
    
    #prépartion sous-titres titres plots
    if (reacAFC$variables=="urb"){stitre<- paste("valeurs urbaines * périodes de ", reacAFC$periodes, " ans", sep="")}
    else if (reacAFC$variables=="portee"){stitre <- paste("portées * périodes de ", reacAFC$periodes, " ans", sep="")}
    else if (reacAFC$variables=="usage"){stitre <- paste("valeurs d'usage * périodes de ", reacAFC$periodes," ans", sep="")}
    
    #calcul CAH
    CAH <- hclust(
      reacAFC$dist_mat,
      method= "ward.D2",
      members=apply(tab_contingence$tab, MARGIN=1, FUN=sum)
    )
    reacCAH$data <- CAH
    
    # palette_dendro <- palette_CAH(4)
    # browser()
    # par(bg = "#EFEFEF")
    # output$plot_dendro <- renderPlot(
    #   A2Rplot(
    #     x=CAH,
    #     k = 4,
    #     col.down = palette_dendro,
    #     show.labels=TRUE,
    #     main ="Dendrogramme de la CAH"
    #   )
    # )
    
    #dendrogramme
    output$plot_dendro <- renderPlot(
      
      ggdendrogram2(CAH)+
        labs(
          title="Dendrogramme de la CAH",
          subtitle=stitre,
          caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, LAT, CITERES",
          x="périodes",
          y="indice de similarité")+
        theme_fivethirtyeight()+
        theme_ln()+
        theme(
          axis.text=element_text(angle = 90, size=8.5, hjust=1, vjust=0.5),
          panel.grid.major.x=element_blank(),
          panel.border = element_blank())
      
    )
    
    #inertie découpage
    inertie_CAH <- sort(CAH$height, decreasing = TRUE)
    inertie_CAH <- inertie_CAH/sum(inertie_CAH)*100
    #gestion longueur graphique
    if (length(inertie_CAH)>40) {
      nb_coupes <- 40
      tab <- inertie_CAH[1:40]
      soustitre <- paste("pour les 40 premiers noeuds du dendrogramme",stitre, sep="\n")} 
    else {
      nb_coupes <-length(inertie_CAH)
      tab <- inertie_CAH
      soustitre <- "pour tous les noeuds du dendrogramme"} 
    
    #plot inertie
    output$plot_in_cah <- renderPlot(
      
      ggplot()+
        geom_bar(
          aes(x=c(1:nb_coupes),y=tab),
          stat="identity",
          fill="#DD4B39",
          alpha=0.7)+
        labs(
          title="Part de l'inertie des différentes coupes de la CAH",
          subtitle= soustitre,
          caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, LAT, CITERES",
          x="nombre de classes",
          y="Part de l'inertie totale (%)")+
        scale_x_continuous(breaks=c(1:length(tab)), expand=c(0.01, 0.01))+
        theme_fivethirtyeight()+
        theme_ln()
    )
    
  })#fin observe dendrogramme
  
  #---- CAH caractérisation des classes ----
  
  observe(priority=6, {
    req(input$nombre_classes, reacAFC, reacCAH)
    CAH <- reacCAH$data
    
    
    #préparation sous-titres plots
    if (reacAFC$variables=="urb"){stitre<- paste("valeurs urbaines * périodes de ", reacAFC$periodes, " ans", sep="")}
    else if (reacAFC$variables=="portee"){stitre <- paste("portées * périodes de ", reacAFC$periodes, " ans", sep="")}
    else if (reacAFC$variables=="usage"){stitre <- paste("valeurs d'usage * périodes de ", reacAFC$periodes," ans", sep="")}
    
    #tableaux classes et limites de classes
    nombre_classes <- input$nombre_classes
    flip <- classes.periodes.cah(CAH=CAH, nb=nombre_classes)
    classes_periodes <- flip$entier
    dates_axe <- flip$axe
    
    #frise chronologique avec classes
    output$frise_classes <- renderPlot(
      
      ggplot(classes_periodes) +
        geom_segment(aes(x=deb, xend=fin, y=0., yend=0., color=classes) , linetype=1, size=6) +
        scale_color_manual(values=palette_CAH(nombre_classes))+
        scale_x_continuous(breaks=c(dates_axe$deb,2016))+
        labs(colour=paste("classes de la CAH",stitre,sep="\n"))+
        theme_bw() + 
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major =  element_blank(),
              panel.border = element_blank(),
              text = element_text(colour="grey40"),
              axis.text.x = element_text(size=11, angle=90, vjust =0.6, hjust=1, colour="grey40"),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),  
              axis.ticks.y=element_blank(),
              aspect.ratio =0.02,
              legend.position="top",
              legend.direction = "horizontal",
              legend.title= element_text(size=12, hjust=1)
        )+
        guides(colour=guide_legend(nrow=1))
    )
    
    #choix tableau de contigence avec périodes -> caractérisation en valeurs urbaines et portées
    nom1 <- paste("tab_urb",input$select_periodes, sep="_")
    tab_cont_vurb <- get(nom1)
    nom2 <- paste("tab_portee",input$select_periodes, sep="_")
    tab_cont_portee <- get(nom2)
    #tab indépendance mis en forme pour plot
    #format long
    tab_ind <- indep.classes.cah(tab_cont_vurb = tab_cont_vurb,
                                 tab_cont_portee = tab_cont_portee,
                                 CAH = CAH,
                                 nb=nombre_classes) %>% gather(key=Fonction, value="data", v_urb.1 : portee.4)
    #transparence
    tab_ind$transparence <- 0
    tab_ind[grepl("p",tab_ind$Fonction),]$transparence <- 1
    
    # graphiques des caractéristiques des classes
    output$plot_classes <- renderPlot(
      
      ggplot(tab_ind) +
        geom_bar(aes(x=Fonction, y=data, fill=Cluster, color=Cluster, alpha=transparence), stat= "identity")+
        facet_wrap(~Cluster)+
        scale_fill_manual(values=palette_CAH(nombre_classes))+
        scale_color_manual(values=palette_CAH(nombre_classes))+
        scale_alpha(range=c(1,0.4))+
        coord_flip()+
        labs(
          x = "Moyenne des écarts standardisés par classe (résidus de Pearson)",
          y="Valeurs urbaines",
          title= "Caractérisation des périodes urbaines par type de fonctions",
          subtitle="Classe de périodes: CAH (distance khi2)",
          caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, LAT, CITERES")+
        theme_fivethirtyeight()+
        theme_ln()
      
    )
    
  })
  
  #####################################################################
  ##################### ONGLET 4 : ZONES ##############################
  #####################################################################
  
  observe({
    #tableau
    val_usage <- lapply(
      1:6, 
      function(i){
        substring(
          eval(parse(text=paste("input$picker_zones_",i,sep=""))), #récupère toutes les options cochées
          1,2 #ne garde que les deux premiers caractères (=les numéros)
        )
      }) %>% unlist %>% as.numeric
    tab <- OH_over_app %>% filter(V_USAGE %in% val_usage)
    tab2 <- exi_ordre %>% filter(V_USAGE %in% val_usage)
    tab3 <- exi_transitions %>% filter(V_USAGE %in% val_usage)
    #sous-titre
    if (!isTruthy(input$stitre_plot_zones)){
      stitre <- paste("OH de valeur d'usage :", paste(val_usage, sep = '', collapse = ', '))
    } else (stitre <- input$stitre_plot_zones)
    #facettes par portées ou non
    if(input$portee_plot_zones){
      wrap1 <- "occupation~PORTEE"
      wrap2<- "densite~PORTEE"
    } else {
      wrap1 <- "occupation~."
      wrap2 <- "densite~."
    }
    
    #plots apparrition
    output$plot_occupation <- renderPlot(plot_occupation(tab,stitre,wrap1))
    output$plot_part_occupation <- renderPlot(plot_part_occupation(tab,stitre))
    output$plot_densite <- renderPlot(plot_densite(tab,stitre,wrap2))
    output$plot_part_densite <- renderPlot(plot_part_densite(tab,stitre))
    
    #plots existence
    output$plot_exi_occ <- renderPlot(plot_exi_occupation(tab2,stitre))
    output$plot_trans_occ <- renderPlot(plot_transitions(tab3,"occupation",stitre))
    output$plot_exi_dens <- renderPlot(plot_exi_densite(tab2,stitre))
    output$plot_trans_dens <- renderPlot(plot_transitions(tab3,"densite",stitre))
    
    
    
  })
  
  
  }) # fin du serveur



################################
# Shiny app pour afficher les objets selon le temps avec leaflet
# mai 2018
# Server pour explOH_11
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
      
      ##tiless
      addProviderTiles("CartoDB.Positron", group="clair") %>%
      addProviderTiles("Esri.WorldImagery", group = "satellite") %>%
      addTiles(options = tileOptions(opacity =0), attribution=" données : ToToPI, UMR7324 CITERES-LAT Université de Tours/CNRS", group=c("clair", "satellite")) %>% 
      ##échelle
      addScaleBar(position="bottomright", scaleBarOptions(imperial=FALSE)) %>% 
      
      ##layer control
      addLayersControl(
        baseGroups = c("clair", "satellite"),
        overlayGroups = c("géometries des OH", "ensembles urbains", "traits de rive", "pôles urbains"),
        options=layersControlOptions(autoZIndex=TRUE)) %>%
      hideGroup(c("traits de rive","ensembles urbains", "pôles urbains")) %>%    
      
      #légende
      addLegend(position="bottomleft", 
                title = "Valeurs urbaines des OH", 
                pal =  palette_fonctions, 
                values = OH_geom$V_URB_NOM, 
                opacity = 1) %>% 
      
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
  poles_subset <- reactiveValues(tab=poles)
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
                      # value=c(subset_limites_temps$date_min,subset_limites_temps$date_max)
                      value=c(input$borne_temps_1,input$borne_temps_2))
  })
  
  #----------------------------------- #1.4. INPUT > DONNEES : sélection des subset en fonction des INPUTS ----
  
  ##contextes (temps)
  observe({
    
    req(max(input$limites), min(input$limites))
    ens_urb_subset$tab <- ens_urb %>% filter (date_debut<=max(input$limites) & date_fin>=min(input$limites))
    traits_rive_subset$tab <- traits_rive %>% filter (DEBUT<=max(input$limites) & FIN>=min(input$limites))
    poles_subset$tab <- poles %>% filter (date_deb<=max(input$limites) & date_fin>=min(input$limites))
    
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
    legende$val_legend <- as.factor(OH_geom$PORTEE_NOM)
    legende$title_legend <- "Niveau de portée des OH"}
    
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
    
    #couleurs des OH 
    if (input$couleur_OH == "v_urb")
    { couleurs_pg<- ~legende$pal_couleurs(OH_pg$V_URB_NOM)
    couleurs_pl<- ~legende$pal_couleurs(OH_pl$V_URB_NOM)
    couleurs_pt <- ~legende$pal_couleurs(OH_pt$V_URB_NOM)}
    else if (input$couleur_OH == "portee")
    {  couleurs_pg <- ~legende$pal_couleurs(OH_pg$PORTEE_NOM)
    couleurs_pl<- ~legende$pal_couleurs(OH_pl$PORTEE_NOM)
    couleurs_pt <- ~legende$pal_couleurs(OH_pt$PORTEE_NOM)}
    
    #POPUP
    popup_ens_urb <- texte_popup_ens_urb(ens_urb_subset$tab)
    popup_traits_rive <- texte_popup_traits_rive(traits_rive_subset$tab)
    popup_poles <- texte_popup_poles(poles_subset$tab)
    popup_pg <-texte_popup_OH(OH_pg)
    popup_pl <-texte_popup_OH(OH_pl)
    popup_pt <-texte_popup_OH(OH_pt)
    # browser()
    
    #date
    subset_dates <-  paste("Période : ", min(input$limites)," - " ,max(input$limites), sep="")
    
    # CARTE
    leafletProxy("map", data=OH_pt) %>%
      clearMarkers() %>%
      clearGroup(group=c("ensembles urbains", "traits de rive", "poles urbains")) %>%
      removeControl(layerId = c("nombre","periode")) %>% 
      #nombre d'OH
      addControl(position="bottomleft", html=OH_subset$message, layerId = "nombre") %>% 
      addControl(position="bottomleft", html=subset_dates, layerId = "periode") %>%
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
      addMarkers(data=poles_subset$tab,
                 group="pôles urbains",
                 icon=~polesIcons[type],
                 popup=popup_poles) %>%
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
    tabOH <- OH_subset$OH_A %>% as.data.frame %>% select(-geom_wkt, -geometry, -QGIS_ID, -V_URB, - V_URB_NOM, -PORTEE_NOM )
    output$tab_OH <-renderDataTable(
      tabOH,
      options=list(pageLength=10,
                   info=TRUE,
                   scrollX=TRUE
      )
    )
    
  })
  
  
  #tTELECHARGEMENT SUBSET
  
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
  
  # output$downloadMap <- downloadHandler(
  #   filename = "carte.png",
  #   content=function(file){
  #     mapshot(x=output$map, file=file, remove_controls =c("zoomControl", "layersControl"))
  #     
  #   })
  
  
  #----------------------------------- #1.6. INTERACTIONS AUTRES ----
  
  # mise en valeur de l'OH sélectionnée, même si elle est hors de ce qui est affiché
  observeEvent(input$selec_OH_search, {
    
    #*******
    #A FAIRE
    # voir si zoom sur bouding box (pbm avec les points et les petits polygones)
    #*******
    
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
  ##################### ONGLET 2 : AFC ##############################
  ###########################################################################
  
  #----------------------------------- #2.1.déclaration reactive objects----
  tab_contingence <- reactiveValues(tab=NULL)
  reacAFC <- reactiveValues(data=NULL, #resultat du dudi.coa
                            periodes = NULL, #découpage par période
                            variable = NULL #variable fonctionnelle utilisée
                            )
  #----------------------------------- #2.2.INPUT > DONNEES : mise à jour du tableau de contingence----
  observe({
    nom <- paste("tab",input$select_var,input$select_periodes, sep="_")
    tab_contingence$tab <- get(nom)
  })
  
  #----------------------------------- #2.3.INPUT > OUTPUT ----
  
  # AFFICHAGE DU TABLEAU
  observe({
    output$tab_contingence <-renderDataTable(
      tab_contingence$tab,
      options=list(pageLength = 10, scrollX=TRUE, searching=FALSE)
    )
  })
  
  # CALCUL DE L'AFC
  observeEvent(input$action_AFC, {
    
    AFC <-dudi.coa(tab_contingence$tab, scannf=FALSE, nf=6)
    
    reacAFC$data <- AFC
    reacAFC$periodes <- input$select_periodes
    reacAFC$variable <- input$select_var
    
    inertie_AFC <- summary.variance.dudi(AFC)
    
    #plot variance axes
    output$plot_inertie_axes <- renderPlot(
      barplot.dudi.variance(data=AFC, 
                            sumdata=inertie_AFC,
                            titre=NULL)
    )    
    
    #update de l'UI pour choix des axes à ploter
    updatePickerInput(session,"axe1",
                      choices = seq(1,AFC$nf,1),
                      selected="1")
    updatePickerInput(session,"axe2",
                      choices = seq(1,AFC$nf,1),
                      selected="2")
  })
  
  #PLOT AFC
  observe({
    
    req(input$axe1, input$axe2)
    AFC <- reacAFC$data
    
    #PREPARATION PLOT
    #Choix palettes et liste_variable selon variables
    if (reacAFC$variable=="urb"){
      palette <- adjustcolor(couleurs_vurb, alpha=0.7)
      liste_var <- sort(unique(OH_geom$V_URB))
      }
    else if (reacAFC$variable=="portee"){
      palette <- adjustcolor(couleurs_portees, alpha=0.7)
      liste_var <- sort(unique(OH_geom$PORTEE))
      }
    else if (reacAFC$variable=="usage"){
      usages <- as.numeric(str_sub(rownames(AFC$co), start=-2))
      palette <- cut (usages,
                              breaks=c(10,20,30,40,50,60,70),
                              labels=c("#e72535", "#0a8eb1", "#f6b01a", "#2fc6a0", "#703ab9","#ff7919"),
                              right=FALSE,
                              include.lowest = TRUE)
      names(palette) <- usages
      liste_var <- sort(unique(OH_geom$V_USAGE))
      }

      
    axe1 <- as.numeric(input$axe1)
    axe2 <- as.numeric(input$axe2)
    
    #BIPLOT 
    pctvar_A1 <- round(AFC$eig[axe1]*100/sum(AFC$eig))
    pctvar_A2 <- round(AFC$eig[axe2]*100/sum(AFC$eig))
    lab_periode <- AFC$li
    
    
    biplot <- 
      
      ggplot()+
      geom_hline(aes(yintercept=0), colour="gray25")+
      geom_vline(aes(xintercept=0), colour="gray25")+
      labs(title="titre",
           subtitle="soustitre",
           x=paste("Composante n°", axe1, " (",pctvar_A1, "% de variance expliquée)", sep=""),
           y=paste("Composante n°", axe2," (",pctvar_A2, "% de variance expliquée)", sep=""),
           caption="L. Nahassia, Géographie-cité, 2018 | Sources : ToToPI, LAT, CITERES"
      )+
      # élargissement des limites du plot
      expand_limits(x=c(min(AFC$li[,axe1])-0.1, max(AFC$li[,axe1])+0.1), y=c(min(AFC$li[,axe2])-0.1, max(AFC$li[,axe2])))+
      # points des variables
      geom_point(data=AFC$co,
                 mapping=aes(x=AFC$co[,axe1], y=AFC$co[,axe2]), #choix des composantes i et j
                 shape=16,
                 size=7,
                 colour=palette)+
      #labels variables
      geom_text(data=AFC$co,
                mapping=aes(x=AFC$co[,axe1], y=AFC$co[,axe2],label=liste_var),
                colour="white",
                fontface="bold")+
      # lignes reliant les individus
      geom_path(data=AFC$li, 
                mapping=aes(x=AFC$li[,axe1], y=AFC$li[,axe2]),
                linetype="dotted",
                colour= "gray18")+ #bcp trop compliqué de faire une line avec gradient
      #label des individus
      geom_text_repel(data=lab_periode,
                      mapping=aes(x=lab_periode[,axe1], y=lab_periode[,axe2],label=rownames(lab_periode)),
                      force=0.1,
                      # max.iter = 1000,
                      box.padding = unit(0.4, "lines"), 
                      size=3,
                      colour=color_ramp_ind(nrow(lab_periode)),
                      segment.color="grey60")+
      #points des individsu
      geom_point(data=AFC$li, 
                 mapping=aes(x=AFC$li[,axe1], y=AFC$li[,axe2]),
                 shape=18,
                 size=4,
                 colour= color_ramp_ind(nrow(AFC$li)))+
      theme_fivethirtyeight()+
      theme_ln()
    
  
    
    output$plot_biplot <- renderPlot(
      biplot
    )    
    
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


    # Entrée : data = resultat dudi.coa + sumdata = résultat summary.variance.dudi
    # plot_AFC <- biplot.AFC(data=AFC,
    #                        axe=list(input$axe1,input$axe2),
    #                        variable_color=palette,
    #                        liste_variable=liste_var,
    #                        titre=paste("Coordonnées des variables sur les axes",input$axe1,"et",input$axe2,sep=" "),
    #                        soustitre=paste(input$select_periodes,"x", input$select_var, sep=" ")
    # )
    # 
    # ouput$plot_biplot <- renderPlot(
    #   plot_AFC
    # )
    # 
    
    
  }) # fin observe AFC
  
  
  
  
}) # fin du serveur



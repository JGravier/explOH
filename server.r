################################
# Shiny app pour afficher les objets selon le temps avec leaflet
# L. Nahassia, aout 2018
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
    popup_poles <- texte_popup_poles(poles_subset$tab)
    popup_pg <-texte_popup_OH(OH_pg)
    popup_pl <-texte_popup_OH(OH_pl)
    popup_pt <-texte_popup_OH(OH_pt)

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
  ##################### ONGLET 2 : AFC & CAH ##############################
  ###########################################################################
  
  #----------------------------------- #2.1.déclaration reactive objects----
  tab_contingence <- reactiveValues(tab=NULL)
  reacAFC <- reactiveValues(data=NULL, #resultat du dudi.coa
                            periodes = NULL, #découpage par période
                            variables = NULL, #variables fonctionnelle utilisée
                            tab_biplot = NULL, #tableau pour le biplot
                            dist_mat = NULL #matrice de distance khi-2 entre les lignes
                            
  )
  reacCAH <- reactiveValues(data=NULL)
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  #----------------------------------- #2.2.INPUT > DONNEES : mise à jour du tableau de contingence & calcul AFC ----
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
  
  #----------------------------------- #2.3.INPUT > OUTPUT ----
  
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
                              subtitle ="L. Nahassia, Géographie-cité, 2018 | Sources : ToToPI, LAT, CITERES")
                
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
          caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, LAT, CITERES",
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
          caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, LAT, CITERES",
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
          caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, LAT, CITERES")+
        theme_fivethirtyeight()+
        theme_ln()
      
    )
    
  })
  
  ###########################################################################
  ##################### ONGLET 3 : ZONES ##############################
  ###########################################################################
  
  observe(priority = 12, {
    
    val_usage <- lapply(
      1:6, 
      function(i){
        substring(
          eval(parse(text=paste("input$picker_zones_",i,sep=""))), #récupère toutes les options cochées
          1,2 #ne garde que les deux premiers caractères (=les numéros)
        )
      }) %>% unlist %>% as.numeric
    
    tab <- OH_zones %>% filter(V_USAGE %in% val_usage)
    stitre <- paste("OH de valeur d'usage :", paste(val_usage, sep = '', collapse = ', '))
    
    output$plot_occupation <- renderPlot(plot_occupation_portees(tab,stitre))
    output$plot_densite <- renderPlot(plot_densite_portees(tab,stitre))
    
    
  })
  
  
}) # fin du serveur



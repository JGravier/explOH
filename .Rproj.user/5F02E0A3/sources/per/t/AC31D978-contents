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
    biplot +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
  )    
  
  
  #ZOOM PLOT AFC
  observeEvent(input$plot_biplot_dblclick, {
    brush <- input$plot_biplot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
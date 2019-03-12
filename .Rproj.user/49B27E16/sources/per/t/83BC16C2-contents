################################
# Shiny app pour afficher les objets selon le temps 
# L. Nahassia, aout 2018
# analyse factorielle et CAH
################################

#chargement des tableaux de contingence -------------------------------
tab_urb_50 <- read.table("./data/tab_AFC/tab_urb_50.csv", sep=";", dec=",")
tab_urb_25 <- read.table("./data/tab_AFC/tab_urb_25.csv", sep=";", dec=",")
tab_urb_100 <- read.table("./data/tab_AFC/tab_urb_100.csv", sep=";", dec=",")
tab_urb_exp <- read.table("./data/tab_AFC/tab_urb_exp.csv", sep=";", dec=",")
tab_usage_25 <- read.table("./data/tab_AFC/tab_usage_25.csv", sep=";", dec=",")
tab_usage_50 <- read.table("./data/tab_AFC/tab_usage_50.csv", sep=";", dec=",")
tab_usage_100 <- read.table("./data/tab_AFC/tab_usage_100.csv", sep=";", dec=",")
tab_usage_exp <- read.table("./data/tab_AFC/tab_usage_exp.csv", sep=";", dec=",")
tab_portee_25 <- read.table("./data/tab_AFC/tab_portee_25.csv", sep=";", dec=",")
tab_portee_50 <- read.table("./data/tab_AFC/tab_portee_50.csv", sep=";", dec=",")
tab_portee_100 <- read.table("./data/tab_AFC/tab_portee_100.csv", sep=";", dec=",")
tab_portee_exp <- read.table("./data/tab_AFC/tab_portee_exp.csv", sep=";", dec=",")


# summary.variance.dudi -------------------------------------------------------------------------
# Crée un tableau pour résumer la variance expliquée par chaque axe + mise en forme pour plotter
# Entrée : data = resultat dudi.coa
# Sortie : data.frame avec {n° de composante, variance, % de la variance, variance résultante, factor pour couleur}

summary.variance.dudi <- function(data){
  expected_inertia <- 100/max(data$rank)
  summaryAFC <- data.frame(
    COMP = seq(1,max(data$rank),1), 
    EIG = data$eig,
    PCTVAR = 100*data$eig / sum(data$eig),
    CUMPCTVAR = cumsum(100*data$eig / sum(data$eig)))
  summaryAFC$CUTSCOLOR <- cut(summaryAFC$PCTVAR, breaks=c(0,expected_inertia,Inf))
  return(summaryAFC)
}


# barplot.dudi.variance -----------------------------------------------------------------
# Crée un graphique barplot de la variance expliquée par chaque axe d'un résultat d'AFC
# Entrée : data = resultat dudi.coa + sumdata = résultat summary.variance.dudi
# Sortie : un objet ggplot

#paste(as.character(substitute(data)), # ligne pour récupérer le nom de la variable

barplot.dudi.variance <- function(data, sumdata, titre){
  expected_inertia <- 100/max(data$rank)
  plot <- ggplot(data=sumdata, aes(x=COMP, y=PCTVAR, fill=sumdata$CUTSCOLOR)) +
    geom_bar(stat="identity") +
    labs(x="Composantes",
         y="Part de la variance expliquée",
         title= titre,
         caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, LAT, CITERES")+
    scale_fill_manual(values=alpha(c("grey60","#F39C12"), 0.8)) +
    geom_hline(aes(yintercept=expected_inertia), colour="grey50", linetype="dashed")+
    geom_text(x=max(sumdata$COMP)+0.5, 
              y=expected_inertia+2,
              label=paste("inertie moyenne attendue par composante : ", round(expected_inertia, digits=2), "%", sep=""),
              vjust=0,
              hjust=1,
              colour="grey50")+
    theme_fivethirtyeight()+
    theme_ln()
  return(plot)
}



# ggdendrogramme2 -----------------------------------------------------------------
# Crée un dendrogramme à partir d'un résultat de CAH  / adapté à partir du package ggdendrogramme pour expand les limites
# Entrée : résultat d'une CAH
# Sortie : un objet ggplot

ggdendrogram2 <- function (data, segments = TRUE, labels = TRUE, leaf_labels = TRUE, 
          rotate = FALSE, theme_dendro = TRUE, ...) 
{
  dataClass <- if (inherits(data, "dendro")) 
    data$class
  else class(data)
  angle <- if (dataClass %in% c("dendrogram", "hclust")) {
    ifelse(rotate, 0, 90)
  }
  else {
    ifelse(rotate, 90, 0)
  }
  hjust <- if (dataClass %in% c("dendrogram", "hclust")) {
    ifelse(rotate, 0, 1)
  }
  else {
    0.5
  }
  if (!is.dendro(data)) 
    data <- dendro_data(data)
  p <- ggplot() + geom_blank()
  if (segments && !is.null(data$segments)) {
    p <- p + geom_segment(data = segment(data), aes_string(x = "x", 
                                                           y = "y", xend = "xend", yend = "yend"))
  }
  if (leaf_labels && !is.null(data$leaf_labels)) {
    p <- p + geom_text(data = leaf_label(data), aes_string(x = "x", 
                                                           y = "y", label = "label"), hjust = hjust, angle = angle, 
                       ...)
  }
  if (labels) {
    p <- p + scale_x_continuous(breaks = seq_along(data$labels$label), 
                                labels = data$labels$label,
                                expand = c(0.01,0.01))
  }
  if (rotate) {
    p <- p + coord_flip()
    p <- p + scale_y_continuous(expand = c(0.01,0))
  }
  else {
    p <- p + scale_y_continuous(expand = c(0.01,0))
  }
  if (theme_dendro) 
    p <- p + theme_dendro()
  p <- p + theme(axis.text.x = element_text(angle = angle, 
                                            hjust = 1)) + theme(axis.text.y = element_text(angle = angle, 
                                                                                           hjust = 1))
  p
}


# indep.classes.cah -----------------------------------------------------------------
# Crée le tableau des écarts standardisés à l'indépendance de Pearson
# pour les valeurs urbaines et les valeurs d'usage + transparence pour plot
# Entrée : tab_cont= tableau initial, CAH = résultat CAH, nb = nombre de classes
# Sortie : matrice

indep.classes.cah <- function(tab_cont_vurb, tab_cont_portee, CAH, nb){

    # Fonction pour abtenir les écarts standardisés
  TabEcartPearsonResidus <- function(x){
    x <- as.matrix(x)
    x <- suppressWarnings(chisq.test(x))
    x <- x$residuals 
    as.data.frame(x)
  }
  
  typochrono <- cutree(CAH, k=nb) 
  #caracterisation par valeur d'usage
  ecarts_vurb <- tab_cont_vurb %>% TabEcartPearsonResidus() %>%  mutate(Cluster = factor(typochrono, levels = 1:nb))
  ecarts_norm_vurb  <- ecarts_vurb %>% group_by(Cluster) %>% summarise_all(list(mean))
  #caracterisation par portée
  ecarts_portee <- tab_cont_portee %>% TabEcartPearsonResidus() %>%  mutate(Cluster = factor(typochrono, levels = 1:nb))
  ecarts_norm_portee  <- ecarts_portee %>% group_by(Cluster) %>% summarise_all(list(mean))
  #les deux groupés en 1 tableau
  tab_ecarts <- cbind(ecarts_norm_vurb,ecarts_norm_portee %>% select(-Cluster))

  
  return(tab_ecarts)
  
  
}


# classes.periodes.cah -------------------------------------------------------------------
# Crée le tableaux recençant les appartenances de chaque périodes aux classes de la CAH
# Entrée : CAH = résultats CAH, nb= nombre de classe
# Sortie : $entier = tableau dans son intégralité, $axe = sous-ensembles pour légendes

classes.periodes.cah <- function(CAH, nb){
  typochrono <- cutree(CAH, k=nb) 
  classes_periodes <- typochrono %>% as.data.frame()
  classes_periodes$periode <- row.names(classes_periodes) 
  classes_periodes$deb <- word(row.names(classes_periodes),1,sep = "\\-")%>% as.numeric()#sélection des dates avant "-" /!\ remettre -25 qui disparaît forcément
  classes_periodes[grep("^-25",row.names(classes_periodes)),]$deb <- -25 # -25 remis en premières date
  classes_periodes$fin <- word(row.names(classes_periodes),2,sep = "\\-")%>% as.numeric()#sélection des dates après "-" >/ erreur sur fin de la période commençant par -25
  classes_periodes[grep("^-25",row.names(classes_periodes)),]$fin <- word(row.names(classes_periodes),2,sep = "\\-25-")[1] %>% as.numeric() # première date de fin ajustée
  row.names(classes_periodes) <- NULL
  colnames(classes_periodes) <- c("classes","periode","deb","fin")
  classes_periodes$classes <- as.factor(classes_periodes$classes)
  
  dates_axe <- NULL
  
  for (i in 1:nb)
  {
    tab <- classes_periodes %>% filter(classes==i)
    tab$deb <- as.character(tab$deb)
    tab$fin <- as.character(tab$fin)
    tab <- tab %>% filter(!(deb %in% fin))
    dates_axe <- rbind(dates_axe,tab)
    
  }
  
  dates_axe$deb <- as.numeric(dates_axe$deb)
  dates_axe$fin <- as.numeric(dates_axe$fin)
  
  liste_tab <- list("entier"=classes_periodes, "axe"=dates_axe)
  
  return(liste_tab)
  
}




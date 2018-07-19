###############################################
## Analyses factorielles
## Descriptions élémentaires - chapitre 6
## L. Nahassia, 2018
##############################################


#chargement des tableaux de contingence
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


# biplot.AFC -----------------------------------------------------------------
# Crée un graphique biplot des résultats de l'AFC sur les 2 premiers axes (variables+individus)
# Entrée : data = resultat dudi.coa  ; variable_color = palette pour les variables ; liste_variable = nom des variables ; axes = liste des 2 axes à ploter
# Sortie : un objet ggplot

color_ramp_ind <- colorRampPalette(c("grey10", "gray60"))


biplot.AFC <- function(data, axe, variable_color, liste_variable, titre, soustitre){
  pctvar_A1 <- round(data$eig[axe[[1]]]*100/sum(data$eig))
  pctvar_A2 <- round(data$eig[axe[[2]]]*100/sum(data$eig))
  if(nrow(data$li) > 50){lab_periode <- row.names(data$li[seq(1,nrow(data$li),2),])} else {lab_periode <- row.names(data$li)} #selection d'une ligne sur 2 si nombre de périodes trop grand pour lisibilité
  
  ggplot()+
    geom_hline(aes(yintercept=0), colour="gray25")+
    geom_vline(aes(xintercept=0), colour="gray25")+
    labs(title=titre,
         subtitle=soustitre,
         x=paste("Composante n°", axe[[1]], " (",pctvar_A1, "% de variance expliquée)", sep=""),
         y=paste("Composante n°", axe[[2]]," (",pctvar_A2, "% de variance expliquée)", sep=""),
         caption="L. Nahassia, Géographie-cité, 2018 | Sources : ToToPI, LAT, CITERES"
    )+
    # élargissement des limites du plot
    expand_limits(x=c(min(data$li[,axe[[1]]])-0.1, max(data$li[,axe[[1]]])+0.1), y=c(min(data$li[,axe[[2]]])-0.1, max(data$li[,axe[[2]]])))+ 
    # points des variables  
    geom_point(data=data$co,
               mapping=aes(x=data$co[,axe[[1]]], y=data$co[,axe[[2]]]), #choix des composantes i et j
               shape=16,
               size=7,
               colour=variable_color)+
    #labels variables
    geom_text(data=data$co,
              mapping=aes(x=data$co[,axe[[1]]], y=data$co[,axe[[2]]],label=liste_variable),
              colour="white",
              fontface="bold")+
    # lignes reliant les individus
    geom_path(data=data$li, 
              mapping=aes(x=data$li[,axe[[1]]], y=data$li[,axe[[2]]]),
              linetype="dotted",
              colour= "gray18")+ #bcp trop compliqué de faire une line avec gradient
    #label des individus
    geom_text_repel(data=lab_periode,
                    mapping=aes(x=lab_periode[,axe[[1]]], y=lab_periode[,axe[[2]]],label=rownames(lab_periode)),
                    force=2,
                    # max.iter = 1000,
                    box.padding = unit(0.4, "lines"), 
                    size=3,
                    colour=color_ramp_ind(nrow(lab_periode)),
                    segment.color="grey60")+
    #points des individsu
    geom_point(data=data$li, 
               mapping=aes(x=data$li[,axe[[1]]], y=data$li[,axe[[2]]]),
               shape=18,
               size=4,
               colour= color_ramp_ind(nrow(data$li)))+
    theme_fivethirtyeight()+
    theme_ln()
  
}



# 
# #---- Plot analyse des contributions ----
# contrib_AFC_OH <- inertia.dudi(AFC, row.inertia = TRUE, col.inertia = TRUE)
# # contrib_AFC_OH
# # contrib_AFC_OH$TOT #inertie des axes
# # contrib_AFC_OH$row.abs #contribution des lignes à l'inertie des axes (part de la variance expliquée par l'individu), somme en colonne
# # contrib_AFC_OH$col.abs #idem colonne
# # contrib_AFC_OH$row.rel #contribution de l'axe à la variance de la ligne (cos²), qualité de représentation de l'individu, somme en ligne
# # contrib_AFC_OHcol.rel #idem colonne => pour les deux à mettre en absolu
# 
# #sortie table avec couleurs :
# #contribution lignes
# CTR_li <- contrib_AFC_OH$row.abs
# colnames(CTR_li) <- c("Axe 1", "Axe 2", "Axe 3", "Axe 4", "Axe 5")
# CTR_li <- CTR_li %>% select(c("Axe 1", "Axe 2")) #que les axes analysés
# #contrribution colonnes
# CTR_co <- contrib_AFC_OH$col.abs
# colnames(CTR_co) <- c("Axe 1", "Axe 2", "Axe 3", "Axe 4", "Axe 5")
# CTR_co <- CTR_co %>% select(c("Axe 1", "Axe 2")) #que les axes analysés
# #plot # Entrée data = tableau à préparer dessus, titre = titre, couleur = couleur, gestion date : si périodes tri dans l'ordre chronologique les vecteur
# pligne <- heat.contribvar(data=CTR_li, 
#                           titre="Contribution des périodes \n dans la variance des axes", 
#                           soustitre="",
#                           couleur="turquoise4", 
#                           gestion_dates=TRUE)
# pcol <- heat.contribvar(CTR_co, 
#                         titre="Contribution des valeurs urbaines \n dans la variance des axes", 
#                         soustitre="",
#                         couleur="turquoise4", 
#                         gestion_dates=FALSE)
# 
# #planche tous résultats
# arrangeGrob(
#   axes_barplot+labs(caption=""),
#   pligne+labs(caption=""), 
#   pcol+labs(caption=""),
#   plot_AFC,
#   nrow=3,
#   layout_matrix=rbind(c(1,1,1,2),
#                       c(4,4,4,2),
#                       c(4,4,4,3))) %>%
#   as_ggplot() +
#   draw_plot_label(label=c("A","B","C","D"), size = 11, x=c(0.05,0.05,0.75,0.75), y=c(0.99,0.66,0.99,0.35))
# 
# 
# 
# 
# # V_URBAINE
# #---- AFC100
# #la présence.absence des fonctions (sans prendre en compte leur localisation), met en évidence 6 périodes urbaines
# # 0-200, 200-500, 500-1100(défense), 1100-1500 (religieux et inhumation), 1500-1800 (religieux et civil), 1800-2015
# # Axe 1 : Constructions civiles (très tiré par époque contemporaine) VS structures défensives, qui caractérisent surtout le passage du bas empire que MA
# # AXE 2 : Religieux et innumation VS Aménagement et lieux de commerces
# #---- AFC 50 
# #>> même organisation, mêmes conglomérat d'individus
# #équivalent à *100, 0-50 qui se distingue + que les périodes d'après
# #---- AFC HIST 
# #
# 
# 
# #---- COS2 ligne (axe explique comment l'individu) ----
# #préparation des données hors fonction
# #suppression de la dernière colonne inutile
# cos2_li <- subset(abs(contrib_AFC_OH$row.rel), select = - con.tra)
# #mettre des noms corrects
# colnames(cos2_li) <- c("Axe 1", "Axe 2", "Axe 3", "Axe 4", "Axe 5")
# #COS2colonne (axe explique comment l'individu)
# cos2_col <- subset(abs(contrib_AFC_OH$col.rel), select = - con.tra)
# colnames(cos2_col) <- c("Axe 1", "Axe 2", "Axe 3", "Axe 4", "Axe 5")
# rownames(cos2_col) <-  c("voiries, aménagements","structures défensives et militaires","constructions civiles","édifices religieux","lieux d'inhumation","lieux de commerce, artisanat, production")
# heat.contribvar(cos2_col, "Part de la variance des variables-valeurs_urbaines exprimée par chaque axe", "turquoise")
# 
# 
# 
# 






################################
# Shiny app pour afficher les objets selon le temps 
# L. Nahassia, 2019
# plots analyse environnement
################################

facet_labels_occupation <- c("urbaine"="Zone \nd'occupation \nurbaine", "intermediaire"="Zone \nd'occupation \nintermédiaire", "non urbaine"="Zone \nd'occupation \nnon urbaine")
facet_labels_dens <- c("3"="Zone de \ndensité \nforte (3)", "2"="Zone de \ndensité \nmoyenne (2)", "1"="Zone de \nfaible \ndensité (1)", "0"="Zone \nd'occupation \nnon urbaine")
facet_labels_portee <- c("1"="1.Faible portée", "2"="2.Portée moyenne", "3"= "3.grande portée", "4"="4.Portée exceptionnelle")
####
#création d'un plot qui représente le nombre d'OH apparaissant dans chaque type d'occupation au cours du temps
#df = tableau OH_over_app 
#sous-titre = sous-titre
#wrap= 1 facettes (occupations) ou 2 (occupations*portées)
plot_occupation <- function(df, sous_titre, wrap) {
  ggplot(df, aes(x=date_debut)) +
    geom_bar(
      stat="count",
      width=6,
      fill="grey10") +
    geom_histogram( 
      binwidth=100,
      aes(alpha=0.7, fill=occupation)) +
    facet_grid(
      wrap,
      labeller=labeller(occupation=facet_labels_occupation, PORTEE=facet_labels_portee))+
    scale_x_continuous(breaks = c(1,seq(100,1900,100),2015))+
    scale_fill_manual(values=c("urbaine"="#a05050", "intermediaire"="#c8ab37", "non urbaine"="darkgrey"))+
    labs(title="Localisation des OH au moment de leur apparition par type d'occupation du sol",
         subtitle=sous_titre,
         x="année d'apparition",
         y="nombre d'OH",
         caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, LAT, CITERES")+
    theme_fivethirtyeight()+
    theme_ln() +
    theme(strip.text.y = element_text(size=10),
          axis.text=element_text(size=9),
          axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
}
####
#création d'un plot qui représente le nombre d'OH apparaissant dans chaque type de densité au cours du temps
#df = tableau OH_over_app 
#sous-titre = sous-titre
#wrap= 1 facettes (densite) ou 2 (densite*portées)
plot_densite <- function(df, sous_titre, wrap) {
  ggplot(df,aes(x=date_debut)) +
    geom_bar(
      stat="count",
      width=6,
      fill="grey10") +
    geom_histogram( 
      binwidth=100,
      aes(alpha=0.4, fill=densite)) +
    facet_grid(
      wrap,
      labeller=labeller(densite=facet_labels_dens, PORTEE=facet_labels_portee))+
    scale_x_continuous(breaks = c(1,seq(100,1900,100),2015))+
    scale_fill_manual(values=c("3"="#016957", "2"="#3ab29d", "1"="#aaddd4", "0"="darkgrey"))+
    labs(title="Localisation des OH au moment de leur apparition par type de densité du bâti",
         subtitle=sous_titre,
         x="année d'apparition",
         y="nombre d'OH",
         caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, LAT, CITERES")+
    theme_fivethirtyeight()+
    theme_ln() +
    theme(strip.text.y = element_text(size=10),
          axis.text=element_text(size=9),
          axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
}

####
#création d'un plot qui représente la part des apparitions par type d'occupation
#df = tableau OH_over_app 
#sous-titre = sous-titre
plot_part_occupation <- function(df, sous_titre) {
    ggplot(df, aes(x=date_debut)) +
    geom_histogram(
      position="fill",
      bins=20,
      alpha=0.6,
      aes( fill=occupation)) +
    scale_fill_manual(values=c("urbaine"="#a05050", "intermediaire"="#c8ab37", "non urbaine"="darkgrey"),
                      name="Apparition en zone d'occupation",
                      labels=c("urbaine","intermédiaire", "non urbaine"))+
    scale_x_continuous(breaks = c(1,seq(100,1900,100),2015))+
    scale_y_continuous(labels=scales::percent)+
    labs(title="Répartition des types de zones d'apparition des OH",
         subtitle=sous_titre,
         x="année",
         y="part des apparitions dans \nchaque type d'occupation du sol",
         caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, LAT, CITERES")+
    theme_fivethirtyeight()+
    theme_ln() +
    theme(
      legend.position="bottom",
      legend.direction = "vertical",
      strip.text.y = element_text(size=10),
      axis.text=element_text(size=9),
      axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
}
####
#création d'un plot qui représente la part des apparitions par type de densité
#df = tableau OH_over_app 
#sous-titre = sous-titre
plot_part_densite <- function(df, sous_titre) {
  ggplot(df, aes(x=date_debut)) +
    geom_histogram(
      position="fill",
      bins=20,
      alpha=0.6,
      aes( fill=densite)) +
    scale_fill_manual(values=c("3"="#016957", "2"="#3ab29d", "1"="#aaddd4", "0"="darkgrey"),
                      name="Apparition en zone de densité",
                      labels=c("forte","moyenne", "faible","non urbaine"))+
    scale_x_continuous(breaks = c(1,seq(100,1900,100),2015))+
    scale_y_continuous(labels=scales::percent)+
    labs(title="Répartition des types de zones d'apparition des OH",
         subtitle=sous_titre,
         x="année",
         y="part des apparitions dans \nchaque type d'occupation du sol",
         caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, LAT, CITERES")+
    theme_fivethirtyeight()+
    theme_ln() +
    theme(
      legend.position="bottom",
      legend.direction = "vertical",
      strip.text.y = element_text(size=10),
      axis.text=element_text(size=9),
      axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
}


###
#Création d'un plot qui représente les changements de zone d'occupation sur la durée de vie des OH
# df : tableau exi_ordre. Attention OHNUM2 = colonne réordonnée
# sous_titre : sous-titre
plot_exi_occupation <- function(df, sous_titre) {
  ggplot(df)+
    geom_segment(
      aes(y=OH_NUM2, yend=OH_NUM2, x=date_debut, xend=date_fin+1, color=factor(occupation,levels=c("non urbaine","intermediaire","urbaine"))), 
      size=1.5)+
    scale_color_manual(values=c("urbaine"="#a05050", "intermediaire"="#c8ab37", "non urbaine"="darkgrey"),
                       name="Localisation de l'OH",
                       labels=c("en zone non urbaine","en zone intermédiaire", "en zone urbaine"))+
    scale_x_continuous(breaks = c(1,seq(100,2015,100)))+
    labs(title="Séquences des types de localisation des OH au cours de leur existence (occupation du sol)",
         subtitle=sous_titre,
         x="années",
         y="OH",
         caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, LAT, CITERES")+
    theme_fivethirtyeight()+
    theme_ln()+
    theme(legend.position="bottom",
          panel.background = element_rect(fill="white"),
          panel.grid.major.y = element_blank(),
          axis.ticks.y =element_blank(),
          axis.text.y=element_blank())
  
}
###
#Création d'un plot qui représente les changements de zone de densité sur la durée de vie des OH
# df : tableau exi_ordre. Attention OHNUM2 = colonne réordonnée
# sous_titre : sous-titre
plot_exi_densite <- function(df, sous_titre) {
  ggplot(df)+
    geom_segment(
      aes(y=OH_NUM2, yend=OH_NUM2, x=date_debut, xend=date_fin+1, color=densite), 
      size=1.5)+
    scale_color_manual(values=c("3"="#016957", "2"="#3ab29d", "1"="#aaddd4", "0"="darkgrey"),
                       name="Localisation de l'OH",
                       labels=c("en zone de densité forte","en zone de densité moyenne", "en zone de densité faible", "en zone non urbaine"))+
    scale_x_continuous(breaks = c(1,seq(100,2015,100)))+
    labs(title="Séquences des types de localisation des OH au cours de leur existence (densite)",
         subtitle=sous_titre,
         x="années",
         y="OH",
         caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, LAT, CITERES")+
    theme_fivethirtyeight()+
    theme_ln()+
    theme(legend.position="bottom",
          legend.direction = "vertical",
          panel.background = element_rect(fill="white"),
          panel.grid.major.y = element_blank(),
          axis.ticks.y =element_blank(),
          axis.text.y=element_blank())
}

###
#Création d'un plot qui représente le nombre de transition entre 2 états au cours du temps
# df : tableau exi_transitions
# sous_titre : sous-titre
plot_transitions <- function(df, type_zone, sous_titre){
  if(type_zone=="occupation")
  {
    df <- df %>% filter(occupation!=lead_occ)
    titre <- "(occupation du sol)"
    wrap <- "trans_occ~."
    couleurs <- c("urbaine"="#a05050", "intermediaire"="#c8ab37", "non urbaine"="darkgrey")
  }
  else if (type_zone=="densite"){
    df <- df %>% filter(densite!=lead_dens)
    titre <- "(densite du bâti)"
    wrap <- "trans_dens~."
    couleurs <- c("3"="#016957", "2"="#3ab29d", "1"="#aaddd4", "0"="darkgrey")
    
  }
  ggplot(data=df, aes(x=date_transition)) +
    geom_bar(stat = "count",
             width=20,
             aes(fill=df[,type_zone]))+
    facet_grid(wrap)+
    scale_x_continuous(breaks = c(1,seq(100,1900,100),2015))+
    scale_fill_manual(values=couleurs)+
    labs(title=paste("Répartition des transitions d'un type de zone à l'autre",titre,sep=" "),
         subtitle=sous_titre,
         x="année",
         y="nombre de transitions",
         caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, LAT, CITERES")+
    theme_fivethirtyeight()+
    theme_ln()
  
}

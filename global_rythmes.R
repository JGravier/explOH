################################
# Shiny app pour afficher les objets selon le temps 
# L. Nahassia, 2019
# analyse rythmes : existence, apparition, disparition
################################

#labels---------------------------
labels_portees <- c("1"="1.portée courte",
                    "2"="2.portée moyenne",
                    "3"="3.grande portée",
                    "4"="4.portée exceptionnelle")

labels_vurb=c("1"="1.aménagements",
              "2"="2.structures défensives et militaires",
              "3"="3.constructions civiles",
              "4"="4.édifices regifieux",
              "5"="5.lieux d'inhumation",
              "6"="6.lieux de commerce, artisanat, production")


#calculs tableau nombre d'OH existants par année ---------------------------

#----création tableau V_URB
tableau.melt.vurb <-  function(OH_subset){
# structure du tableau
nb_vurb_an <- as.data.frame(matrix(nrow=2015+26,ncol=6))
colnames(nb_vurb_an) <-c(1:6)
nb_vurb_an$annee <- as.numeric(rownames(nb_vurb_an))-26
# rownames(nb_vurb_an) <- as.numeric(rownames(nb_vurb_an))-1
#remplissage du tableau
for (n in 1:6){ # pour toutes les colonnes valeurs urbaines existantes
  for (i in 1:(nrow(nb_vurb_an)+1)) { #pour toutes les lignes
    annee <- i-26
    nb_vurb_an[i,n] <- sum (OH_subset$V_URB==n & OH_subset$DATE_DEB<=annee & OH_subset$DATE_FIN>=annee)
  } # remplir cette colonne de la somme des individus ayant cette valeur urbaine et ayant existé dans les bornes données
}
nb_vurb_an <- nb_vurb_an %>%  filter(annee != 0) #enlever l'année 0
nb_vurb_an$somme <- rowSums(nb_vurb_an)-nb_vurb_an$annee #somme
pourc_vurb_an <- nb_vurb_an %>% mutate_at(vars(-somme, -annee), list(~./somme*100)) #pourcentage

m_nb_vurb_an <- nb_vurb_an %>% select(-somme) %>% melt(id.vars="annee")#melt pour plot sur valeurs absolues
levels(m_nb_vurb_an$variable) <-liste_vurb
m_pourc_vurb_an <-  pourc_vurb_an %>% select(-somme) %>% melt(id.vars="annee")#melt pour plot sur valeurs relatives
levels(m_pourc_vurb_an$variable) <-liste_vurb

tableaux <- list("melt_nb"=m_nb_vurb_an,"melt_pourc"=m_pourc_vurb_an)#liste des deux tableaux résultats
return(tableaux)

}

#----création tableau V_USAGE
tableau.melt.vusage <-  function(OH_subset){
  liste_vusage <- unique(OH_subset %>% select(V_USAGE)) 
  nb_usage_an <- as.data.frame(matrix(nrow=2015+26,ncol=nrow(liste_vusage)))
  colnames(nb_usage_an) <- sort(liste_vusage$V_USAGE)
  nb_usage_an$annee <- as.numeric(rownames(nb_usage_an))-26
  for (n in 1:nrow(liste_vusage)){
    for (i in 1:nrow(nb_usage_an)+1) {
      annee <- i-26
      v_usage <- colnames(nb_usage_an[n])
      nb_usage_an[i,n] <- sum (OH_subset$V_USAGE==v_usage & OH_subset$DATE_DEB<=annee & OH_subset$DATE_FIN>=annee)
    }
  }
  nb_usage_an <- nb_usage_an %>%  filter(annee != 0)
  nb_usage_an$somme <- rowSums(nb_usage_an)-nb_usage_an$annee
  pourc_usage_an <- nb_usage_an %>% mutate_at(vars(-somme, -annee), list(~./somme*100)) #attention au nombre de colonnes
  
  m_nb_usage_an <- nb_usage_an %>% 
    select(-somme) %>% 
    melt(id.vars="annee") %>% 
    mutate(v_urb=stri_sub(variable,-2,-2))
  levels(m_nb_usage_an$v_urb) <-liste_vurb
  
  m_pourc_usage_an<-  pourc_usage_an %>% 
    select(-somme) %>% 
    melt(id.vars="annee") %>% 
    mutate(v_urb=stri_sub(variable,-2,-2))
  levels(m_pourc_usage_an$v_urb) <-liste_vurb
  
  tableaux <- list("melt_nb"=m_nb_usage_an,"melt_pourc"=m_pourc_usage_an)#liste des deux tableaux résultats
  return(tableaux)
  
}

#----création tableau V_URB
tableau.melt.portee <-  function(OH_subset){
  nb_portee_an <- as.data.frame(matrix(nrow=max(OH_subset$DATE_FIN)+26,ncol=4))
  colnames(nb_portee_an) <-c(1:4)
  nb_portee_an$annee <- as.numeric(rownames(nb_portee_an))-26
  for (n in 1:4){
    for (i in 1:(nrow(nb_portee_an+1))) {
      annee <- i-26
      nb_portee_an[i,n] <- sum (OH_subset$PORTEE==n & OH_subset$DATE_DEB<=annee & OH_subset$DATE_FIN>=annee)
    } 
  }
  nb_portee_an <- nb_portee_an %>%  filter(annee != 0)
  nb_portee_an$somme <- rowSums(nb_portee_an)-nb_portee_an$annee
  pourc_portee_an <- nb_portee_an %>% mutate_at(vars(-somme, -annee), list(~./somme*100))
  
  
  m_nb_portee_an <- nb_portee_an %>% select(-somme) %>% melt(id.vars="annee")
  levels(m_nb_portee_an$variable) <-c("1.portée petite ","2.portée moyenne","3.grande portée","4.portée exceptionnelle")
  m_pourc_portee_an <-  pourc_portee_an%>% select(-somme) %>% melt(id.vars="annee")
  levels(m_pourc_portee_an$variable) <-c("1.portée petite ","2.portée moyenne","3.grande portée","4.portée exceptionnelle")
  
  tableaux <- list("melt_nb"=m_nb_portee_an,"melt_pourc"=m_pourc_portee_an)
  return(tableaux)
  
}


#plot.repartition -------------------------------
# fonction qui génère deux plots (répartition asbolue et relative) des OH dans le temps
# Entrée : dfnb = plot du nombre d'OH, dfpourc= table du pourcentage d'OH, titre = complément du titre, couleur=palette à utiliser
# Sortie : nb = plot de la répartition absolue, pourc = plot de la répartition relative
plot.repartition <- function(dfnb, dfpourc, titre, couleur) {
  
  plot1 <-  ggplot(dfnb, aes(x=annee, y=value))+
    geom_area(aes(fill=rev(dfnb$variable)))+
    geom_line(aes(color=rev(dfnb$variable)))+
    geom_hline(yintercept = 0, color ="grey75")+
    scale_color_manual(values=adjustcolor(rev(couleur), alpha=0.9))+
    scale_fill_manual(values=adjustcolor(rev(couleur), alpha=0.4))+
    scale_x_continuous(breaks = c(1,seq(200,2000,200)))+
    facet_wrap(~variable, ncol=2, scales = "free_x" )+
    labs(title=paste("Nombre d'OH par année et par",titre,sep=" "),
         caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, ToToPI, CITERES-LAT",
         x="année",
         y="nombre d'OH")+
    theme_fivethirtyeight()+
    theme_ln()+
    theme(
      axis.text.x=element_text(angle=90, vjust = 0.5, hjust = 1),
      plot.margin = unit(c(0.5,1,1.5,1), "lines"))
  
  plot2 <-  ggplot(dfpourc, aes(x=annee, y=value))+
    geom_area(aes(fill=rev(dfpourc$variable)))+
    geom_line(aes(color=rev(dfpourc$variable)))+
    geom_hline(yintercept = 0, color ="grey75")+
    scale_color_manual(values=adjustcolor(rev(couleur), alpha=0.9))+
    scale_fill_manual(values=adjustcolor(rev(couleur), alpha=0.4))+
    scale_x_continuous(breaks = c(1,seq(200,2000,200)))+
    facet_wrap(~variable, ncol=2, scales = "free_x" )+
    labs(title=paste("Part d'OH par année et par",titre,sep=" "),
         caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, ToToPI, CITERES-LAT",
         x="année",
         y="% d'OH")+
    theme_fivethirtyeight()+
    theme_ln()+
    theme(
      axis.text.x=element_text(angle=90, vjust = 0.5, hjust = 1),
      plot.margin = unit(c(0.5,1,1.5,1), "lines"))
  
  resultats <-list("nb"=plot1, "pourc"=plot2)
  
  return(resultats)
}


#plot.repartition.usage-------------------------------
# fonction qui génère deux plots (répartition asbolue et relative) des OH dans le temps spécifiquement pour les valeurs d'usage
# Entrée : dfnb = plot du nombre d'OH, dfpourc= table du pourcentage d'OH
# Sortie : nb = plot de la répartition absolue, pourc = plot de la répartition relative
plot.repartition.usage <- function(dfnb, dfpourc) {
  
  plot1 <-  ggplot(dfnb, aes(x=annee, y=value))+
    geom_area(aes(fill=rev(dfnb$v_urb)))+
    geom_line(aes(color=rev(dfnb$v_urb)))+
    geom_hline(yintercept = 0, color ="grey75")+
    scale_color_manual(values=adjustcolor(rev(couleurs_vurb), alpha=0.9))+
    scale_fill_manual(values=adjustcolor(rev(couleurs_vurb), alpha=0.4))+
    scale_x_continuous(breaks = c(1,seq(200,2000,200)))+
    facet_wrap(~variable, ncol=6, scales = "free_x" )+
    labs(title=paste("Nombre d'OH par année et par valeur d'usage "),
         caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, ToToPI, CITERES-LAT",
         x="année",
         y="nombre d'OH")+
    theme_fivethirtyeight()+
    theme_ln()+
    theme(
      axis.text.x=element_text(angle=90, vjust = 0.5, hjust = 1),
      plot.margin = unit(c(0.5,1,1.5,1), "lines"))
  
  plot2 <-  ggplot(dfpourc, aes(x=annee, y=value))+
    geom_area(aes(fill=rev(dfpourc$v_urb)))+
    geom_line(aes(color=rev(dfpourc$v_urb)))+
    geom_hline(yintercept = 0, color ="grey75")+
    scale_color_manual(values=adjustcolor(rev(couleurs_vurb), alpha=0.9))+
    scale_fill_manual(values=adjustcolor(rev(couleurs_vurb), alpha=0.4))+
    scale_x_continuous(breaks = c(1,seq(200,2000,200)))+
    facet_wrap(~variable, ncol=6, scales = "free_x" )+
    labs(title=paste("Part d'OH par année et par valeur d'usage"),
         x="année",
         caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, ToToPI, CITERES-LAT",
         y="% d'OH")+
    theme_fivethirtyeight()+
    theme_ln()+
    theme(
      axis.text.x=element_text(angle=90, vjust = 0.5, hjust = 1),
      plot.margin = unit(c(0.5,1,1.5,1), "lines"))
  
  resultats <-list("nb"=plot1,"pourc"=plot2)
  
  return(resultats)
}

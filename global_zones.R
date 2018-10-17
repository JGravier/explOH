################################
# Shiny app pour afficher les objets selon le temps 
# L. Nahassia, aout 2018
# plots analyse zone
################################

plot_occupation_portees <- function(df, sous_titre) {
  facet_labels_zonesurb <- c("A"="Zone \nd'occupation \nurbaine", "B"="Zone \nd'occupation \nintermédiaire", "C"="Zone \nd'occupation \nnon urbaine")
  facet_labels_portee <- c("1"="1.Faible portée", "2"="2.Portée moyenne", "3"= "3.Grande portée", "4"="4.Portée exceptionnelle")
    ggplot(df, aes(x=DATE_DEB)) +
      geom_bar(
        stat="count",
        width=6,
        fill="grey10") +
      geom_histogram( 
        binwidth=100,
        aes(alpha=0.7, fill=deb_zone_urb2)) +
      facet_grid(
        deb_zone_urb2~PORTEE,
        labeller=labeller(deb_zone_urb2=facet_labels_zonesurb, PORTEE=facet_labels_portee))+
      scale_x_continuous(breaks = c(1,seq(100,1900,100)))+
      scale_fill_manual(values=c("A"="#a05050", "B"="#c8ab37", "C"="darkgrey"))+
      labs(title="Localisation des OH au moment de leur apparition par type d'occupation (urbaine/intermédiaire/occupation non urbaine)",
           subtitle=sous_titre,
           x="année d'apparition",
           y="nombre d'OH",
           caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, LAT, CITERES")+
      theme_fivethirtyeight()+
      theme_ln() +
      theme(strip.text.y = element_text(size=10),
            axis.text=element_text(size=9),
            axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
}

plot_densite_portees <- function(df, sous_titre) {
  facet_labels_dens <- c("3"="Zone de \ndensité \nforte (3)", "2"="Zone de \ndensité \nmoyenne (2)", "1"="Zone de \nfaible \ndensité (1)", "0"="Zone \nd'occupation \nnon urbaine")
  facet_labels_portee <- c("1"="1.Faible portée", "2"="2.Portée moyenne", "3"= "3.grande portée", "4"="4.Portée exceptionnelle")
    ggplot(df,aes(x=DATE_DEB)) +
      geom_bar(
        stat="count",
        width=6,
        fill="grey10") +
      geom_histogram( 
        binwidth=100,
        aes(alpha=0.4, fill=deb_zone_densite)) +
      facet_grid(
        deb_zone_densite~PORTEE,
        labeller=labeller(deb_zone_densite=facet_labels_dens, PORTEE=facet_labels_portee))+
      scale_x_continuous(breaks = c(1,seq(100,1900,100)))+
      scale_fill_manual(values=c("3"="#016957", "2"="#3ab29d", "1"="#aaddd4", "0"="darkgrey"))+
      labs(title="Localisation des OH au moment de leur apparition par type d'espace (densité)",
           subtitle=sous_titre,
           x="année d'apparition",
           y="nombre d'OH",
           caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, LAT, CITERES")+
      theme_fivethirtyeight()+
      theme_ln() +
      theme(strip.text.y = element_text(size=10),
            axis.text=element_text(size=9),
            axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
}

# ================================================================================
# Etudiants : * Anis BOUHADIDA (Universite Paris 13)
#             * Radouane ELAYACHY (Universite Paris Descarte)

# Fichier : * Contient toutes les fonctions appelee par le serveur 
# ================================================================================

library(shiny)
library(tidyverse)
library(leaflet)




shinyServer(function(input, output) {
   
  output$dechetSelectOutput <- renderUI({
   switch(input$dechetSelectInput,
          "Groupe"= selectInput(
            inputId = "selectAttrDechet",label = "Selectionnez ",
            choices = select(ED_dimensionDechet,`GROUPE DE DECHETS`)%>% drop_na()),
          "Sous-groupe"= selectInput(
            inputId = "selectAttrDechet",label = "Selectionnez ",
            choices =select(ED_dimensionDechet,`SOUS-GROUPE DE DECHETS`)%>% drop_na()),
          "Famille" = selectInput(
                inputId = "selectAttrDechet",label = "Selectionnez ",
                choices =select(ED_dimensionDechet,`FAMILLE IN`)%>% drop_na()))})
  
  output$geo <- renderUI({
    switch(input$geoSelectInput,
           "Region"= selectInput(
             inputId = "selectInput",label = "Selectionner la region ",
             choices = sort(ED_dimensionGeo$NOM_REG)),
           "Departement"= selectInput(
             inputId = "selectInput",label = "Selectionner le departement ",
             choices = sort(ED_dimensionGeo$NOM_DEPT)),
           "Commune" = selectInput(
             inputId = "selectInput",label = "Selectionner la commune ",
             choices = sort(ED_dimensionGeo$NOM_COM)))})
  
  output$selectOutput <- renderDataTable({
    ED_dimensionDechet %>% 
      filter(`GROUPE DE DECHETS`==input$selectAttrDechet|
               `SOUS-GROUPE DE DECHETS`==input$selectAttrDechet|
               `FAMILLE IN`==input$selectAttrDechet) %>% 
      left_join(ED_faitRepartitionPoluant,by=c("id_dim_dechet")) %>% 
      left_join(ED_dimensionProducteurDechet,by=c("id_dim_producteur")) %>%
      select(`NOM DU SITE`,`GROUPE DE DECHETS`,`SOUS-GROUPE DE DECHETS`,
             `DESCRIPTION PHYSIQUE`,`FAMILLE IN`, `VOLUME EQUIVALENT CONDITIONNE`,
             `ACTIVITE ( Bq)`)})

# affichage de la carte avec tous les points  
  output$carte_ville <- renderLeaflet(
    left_join(ED_faitRepartitionPoluant, ED_dimensionGeo)%>%
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng=2.3037, lat=46.4317, zoom=5) %>%
      addMarkers(~as.numeric(lng), ~as.numeric(lat),
                 clusterOptions = markerClusterOptions(),
                 popup = ~ as.character(`ACTIVITE ( Bq)`), 
                 icon= makeIcon(iconUrl = "../img/radioactif.png", iconWidth = 40, iconHeight = 40)))
})
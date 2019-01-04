# ================================================================================
# Etudiants : * Anis BOUHADIDA (Universite Paris 13)
#             * Radouane ELAYACHY (Universite Paris Descarte)

# Fichier : * Contient toutes les fonctions appelee par le serveur 
# ================================================================================

library(shiny)
library(tidyverse)
library(leaflet)




shinyServer(function(input, output) {
  #permettre de renvoyer à l'util liste de choix pour les groupes 
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
  
  #permettre de renvoyer à l'util liste de choix pour les localisations
  output$geo <- renderUI({
    switch(input$geoSelectInput,
           "Region"= selectInput(
             inputId = "selectAttrGeo",label = "Selectionner la region ",
             choices = sort(ED_dimensionGeo$NOM_REG)),
           "Departement"= selectInput(
             inputId = "selectAttrGeo",label = "Selectionner le departement ",
             choices = sort(ED_dimensionGeo$NOM_DEPT)),
           "Commune" = selectInput(
             inputId = "selectAttrGeo",label = "Selectionner la commune ",
             choices = sort(ED_dimensionGeo$NOM_COM)))})
  
  # affichage de la table selon critères sélectionnés
  output$tableSelectOutput <- renderDataTable({
    if(input$dechetSelectInput != "Tous les groupes" & input$geoSelectInput !="France entière"){
      ED_dimensionDechet %>% 
        filter(`GROUPE DE DECHETS`==input$selectAttrDechet|
                 `SOUS-GROUPE DE DECHETS`==input$selectAttrDechet|
                 `FAMILLE IN`==input$selectAttrDechet) %>% 
        left_join(ED_faitRepartitionPoluant,by=c("id_dim_dechet")) %>% 
        left_join(ED_dimensionProducteurDechet,by=c("id_dim_producteur")) %>%
        left_join(ED_dimensionGeo, by=c("id_dim_geo")) %>%
        filter(`NOM_COM`==input$selectAttrGeo |
                 `NOM_REG`==input$selectAttrGeo |
                 `NOM_DEPT`==input$selectAttrGeo) %>%
        select(`NOM DU SITE`,`GROUPE DE DECHETS`,`SOUS-GROUPE DE DECHETS`,
               `DESCRIPTION PHYSIQUE`,`FAMILLE IN`, `VOLUME EQUIVALENT CONDITIONNE`,
               `ACTIVITE ( Bq)`, `NOM_REG`)} 
  
    else if(input$dechetSelectInput == "Tous les groupes" & input$geoSelectInput !="France entière"){
      ED_dimensionDechet %>% 
        left_join(ED_faitRepartitionPoluant,by=c("id_dim_dechet")) %>% 
        left_join(ED_dimensionProducteurDechet,by=c("id_dim_producteur")) %>%
        left_join(ED_dimensionGeo, by=c("id_dim_geo")) %>%
        filter(`NOM_COM`==input$selectAttrGeo |
                 `NOM_REG`==input$selectAttrGeo |
                 `NOM_DEPT`==input$selectAttrGeo) %>%
        select(`NOM DU SITE`,`GROUPE DE DECHETS`,`SOUS-GROUPE DE DECHETS`,
               `DESCRIPTION PHYSIQUE`,`FAMILLE IN`, `VOLUME EQUIVALENT CONDITIONNE`,
               `ACTIVITE ( Bq)`, `NOM_REG`)}
    
    else if(input$dechetSelectInput != "Tous les groupes" & input$geoSelectInput =="France entière"){
      ED_dimensionDechet %>% 
        filter(`GROUPE DE DECHETS`==input$selectAttrDechet|
                 `SOUS-GROUPE DE DECHETS`==input$selectAttrDechet|
                 `FAMILLE IN`==input$selectAttrDechet) %>% 
        left_join(ED_faitRepartitionPoluant,by=c("id_dim_dechet")) %>% 
        left_join(ED_dimensionProducteurDechet,by=c("id_dim_producteur")) %>%
        left_join(ED_dimensionGeo, by=c("id_dim_geo")) %>%
        select(`NOM DU SITE`,`GROUPE DE DECHETS`,`SOUS-GROUPE DE DECHETS`,
               `DESCRIPTION PHYSIQUE`,`FAMILLE IN`, `VOLUME EQUIVALENT CONDITIONNE`,
               `ACTIVITE ( Bq)`, `NOM_REG`)}
    else {
      ED_dimensionDechet %>% 
        left_join(ED_faitRepartitionPoluant,by=c("id_dim_dechet")) %>% 
        left_join(ED_dimensionProducteurDechet,by=c("id_dim_producteur")) %>%
        left_join(ED_dimensionGeo, by=c("id_dim_geo")) %>%
        select(`NOM DU SITE`,`GROUPE DE DECHETS`,`SOUS-GROUPE DE DECHETS`,
               `DESCRIPTION PHYSIQUE`,`FAMILLE IN`, `VOLUME EQUIVALENT CONDITIONNE`,
               `ACTIVITE ( Bq)`, `NOM_REG`)}
    })
    
    

# affichage de la carte selon critères sélectionnés  
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
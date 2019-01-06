# ================================================================================
# Etudiants : * Anis BOUHADIDA (Universite Paris 13)
#             * Radouane ELAYACHY (Universite Paris Descarte)

# Fichier : * Contient toutes les fonctions appelee par le serveur 
# ================================================================================

library(shiny)
library(tidyverse)
library(leaflet)

shinyServer(function(input, output) {
  
#permettre de renvoyer à l'utilisateur liste de choix pour les groupes 
  output$dechetSelectOutput <- renderUI({
   switch(input$dechetSelectInput,
          "Groupe"= {
                  selectInput(
                    inputId = "selectAttrDechet",label = h3("Selectionner un groupe de déchet "),
                    choices = select(ED_dimensionDechet,`GROUPE DE DECHETS`)%>% drop_na())
            },
          "Sous-groupe"= {
                  selectInput(
                    inputId = "selectAttrDechet",label = h3("Selectionner un sous-groupe de déchet "),
                    choices =select(ED_dimensionDechet,`SOUS-GROUPE DE DECHETS`)%>% drop_na())
            },
          "Famille" = {
                  selectInput(
                      inputId = "selectAttrDechet",label = h3("Selectionner une famille de déchet "),
                      choices =select(ED_dimensionDechet,`FAMILLE IN`)%>% drop_na())
            }
          )
    })
  
#permettre de renvoyer à l'utilisateur liste de choix pour les localisations
  output$geo <- renderUI({
    switch(input$geoSelectInput,
           "Region"= selectInput(
             inputId = "selectAttrGeo",label = h3("Selectionner la region "),
             choices = sort(data()$NOM_REG)),
           "Departement"= selectInput(
             inputId = "selectAttrGeo",label = h3("Selectionner le departement "),
             choices = sort(data()$NOM_DEPT)),
           "Commune" = selectInput(
             inputId = "selectAttrGeo",label = h3("Selectionner la commune "),
             choices = sort(data()$NOM_COM)),
           "Site"= selectInput(
             inputId = "selectAttrGeo",label = h3("Selectionner le site "),
             choices = sort(data()$`NOM DU SITE`)))})
  
# création reactive prenant en compte les choix de l'utilisateur pour l'affiche de la carte et de la table
  data <- reactive({
    ED_faitRepartitionPoluant %>% 
      left_join(ED_dimensionDechet) %>% 
      left_join(ED_dimensionProducteurDechet) %>%
      left_join(ED_dimensionGeo)%>%
      left_join(dep.sf) %>% 
      st_as_sf(sf_column_name = "geometry")})
  
  filtered_dechet <- reactive({
    switch (input$dechetSelectInput,
            "Groupe" = filtered_dechet <- data() %>% filter(`GROUPE DE DECHETS`==input$selectAttrDechet),
            "Sous-groupe" = filtered_dechet <- data() %>% filter(`SOUS-GROUPE DE DECHETS`==input$selectAttrDechet),
            "Famille" = filtered_dechet <- data() %>% filter(`FAMILLE IN`==input$selectAttrDechet))
  })
  filtered_geo <- reactive({
    switch (input$geoSelectInput,
            "Commune" = filtered_geo <- data() %>% filter(`NOM_COM`==input$selectAttrGeo ),
            "Region" = filtered_geo <- data() %>% filter(`NOM_REG`==input$selectAttrGeo ),
            "Departement" = filtered_geo <- data() %>% filter(`NOM_DEPT`==input$selectAttrGeo),
            "Site" = filtered_geo <- data() %>% filter(`NOM DU SITE`==input$selectAttrGeo ))
  })
  showed_result <- reactive({
    if(input$dechetSelectInput != "Tous les groupes" & input$geoSelectInput !="France entière"){
      showed_result <- filtered_dechet() %>% inner_join(filtered_geo())}
    
    else if(input$dechetSelectInput == "Tous les groupes" & input$geoSelectInput !="France entière"){
      showed_result <- filtered_geo()} 
    
    else if(input$dechetSelectInput != "Tous les groupes" & input$geoSelectInput =="France entière"){
      showed_result <- filtered_dechet()}
    
    else {showed_result<- data()}
    
    return (showed_result)})
  
  # affichage de la table
  output$tableSelectOutput <- renderDataTable({
    showed_result() %>%  select(`NOM DU SITE`,
                          `GROUPE DE DECHETS`,
                          `SOUS-GROUPE DE DECHETS`,
                          `DESCRIPTION PHYSIQUE`,
                          `FAMILLE IN`, 
                          `VOLUME EQUIVALENT CONDITIONNE`,
                          `ACTIVITE ( Bq)`, 
                          `NOM_REG`)})

# affichage de la carte répartition polluants   
  output$carte_ville <- renderLeaflet({

    leaflet(data=showed_result()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(~as.numeric(lng), 
                 ~as.numeric(lat),
                 clusterOptions = markerClusterOptions(),
                 popup = paste(
                   "<b>Site : ", showed_result()$`NOM DU SITE`, "</b><br/>",
                   "<b>Activité en Bq : ", showed_result()$`ACTIVITE ( Bq)`,"</b> <br/>", 
                   "Quantité en VEC :", showed_result()$`VOLUME EQUIVALENT CONDITIONNE`, "<br/>",
                   "Groupe de déchet :", showed_result()$`GROUPE DE DECHETS`, "<br/>"),
                 label = ~ as.character(`NOM_COM`),
                 #icon ne s'adapte pas encore à l'activité à cause des valeurs "-"
                 icon= makeIcon(iconUrl = "../img/radioactif.png", iconWidth = 50, iconHeight = 50))})

  #affichage de la carte des évènements  
  output$carte_incidence <- renderLeaflet({
    
    leaflet() %>% 
      addLegend(data=showed_result(), #légende à mettre en premier sinon ne sait plus quelle carte prendre
                pal = pal,
                values=~dep.sf$ratio, 
                opacity = 0.7,
                title = "Incidence/100.000 hab.") %>%
      addMeasure(       #addin pour faire des mesures sur la carte
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479")%>%
      addEasyButton(easyButton(    #bouton zoom réinitialiser à vérifier si marche lorsque choix de région
        icon="fa-globe", title="Zoom to France", #sinon changer titre
        onClick=JS("function(btn, map){ map.setZoom(2); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))%>%
      addMiniMap(
        tiles = providers$Esri.WorldStreetMap,
        toggleDisplay = TRUE)%>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(data=showed_result(),
                 ~as.numeric(lng), 
                 ~as.numeric(lat),
                 clusterOptions = markerClusterOptions(),
                 popup = paste(
                   "<b>Site : ", showed_result()$`NOM DU SITE`, "</b><br/>",
                   "<b>Activité en Bq : ", showed_result()$`ACTIVITE ( Bq)`,"</b> <br/>", 
                   "Quantité en VEC :", showed_result()$`VOLUME EQUIVALENT CONDITIONNE`, "<br/>",
                   "Groupe de déchet :", showed_result()$`GROUPE DE DECHETS`, "<br/>"),
                 label = ~ as.character(`NOM_COM`),
                 icon= makeIcon(iconUrl = "../img/radioactif.png", iconWidth = 50, iconHeight = 50))%>%
      addPolygons(data=showed_result() , color = "#444444", weight = 1, smoothFactor = 0.5,
                  fillColor = ~pal(ratio),
                  opacity = 1.0, fillOpacity = 0.7,
                  dashArray = "3",# limite en pointillé
                  label = str_c(showed_result()$NOM_DEPT),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
                  # pour la surbrillance du polygone lorsque souris dessus:
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE))})

  })
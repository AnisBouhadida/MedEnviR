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
      left_join(ED_dimensionGeo)})
  
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

# affichage de la carte   
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
})
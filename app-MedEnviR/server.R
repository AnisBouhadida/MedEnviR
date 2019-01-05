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
             choices = sort(re_temp1()$NOM_REG)),
           "Departement"= selectInput(
             inputId = "selectAttrGeo",label = h3("Selectionner le departement "),
             choices = sort(re_temp1()$NOM_DEPT)),
           "Commune" = selectInput(
             inputId = "selectAttrGeo",label = h3("Selectionner la commune "),
             choices = sort(re_temp1()$NOM_COM)),
           
           #intégration du site dans les localisations
           "Site"= selectInput(
             inputId = "selectAttrGeo",label = h3("Selectionner le site "),
             choices = sort(re_temp1()$`NOM DU SITE`)))})
  
# création reactive prenant en compte les choix de l'utilisateur pour l'affiche de la carte et de la table
  re_temp1 <- reactive({
    ED_faitRepartitionPoluant %>% 
      left_join(ED_dimensionDechet) %>% 
      left_join(ED_dimensionProducteurDechet) %>%
      left_join(ED_dimensionGeo)})
  
  re_temp <- reactive({
    if(input$dechetSelectInput != "Tous les groupes" & input$geoSelectInput !="France entière"){
      re_temp <- re_temp1() %>%
        filter(`GROUPE DE DECHETS`==input$selectAttrDechet|
                 `SOUS-GROUPE DE DECHETS`==input$selectAttrDechet|
                 `FAMILLE IN`==input$selectAttrDechet) %>% 
        filter(`NOM_COM`==input$selectAttrGeo |
                 `NOM_REG`==input$selectAttrGeo |
                 `NOM_DEPT`==input$selectAttrGeo|
                 `NOM DU SITE`==input$selectAttrGeo)}
    
    else if(input$dechetSelectInput == "Tous les groupes" & input$geoSelectInput !="France entière"){
      switch (input$geoSelectInput,
              "Commune" = re_temp <- re_temp1() %>% filter(`NOM_COM`==input$selectAttrGeo ),
              "Region" = re_temp <- re_temp1() %>% filter(`NOM_REG`==input$selectAttrGeo ),
              "Departement" = re_temp <- re_temp1() %>% filter(`NOM_DEPT`==input$selectAttrGeo ),
              "Site" = re_temp <- re_temp1() %>% filter(`NOM DU SITE`==input$selectAttrGeo ))}
    
    else if(input$dechetSelectInput != "Tous les groupes" & input$geoSelectInput =="France entière"){
      switch (input$dechetSelectInput,
              "Groupe" = re_temp <- re_temp1() %>% filter(`GROUPE DE DECHETS`==input$selectAttrDechet),
              "Sous-groupe" = re_temp <- re_temp1() %>% filter(`SOUS-GROUPE DE DECHETS`==input$selectAttrDechet),
              "Sous-groupe" = re_temp <- re_temp1() %>% filter(`FAMILLE IN`==input$selectAttrDechet))}
    
    else {re_temp<- re_temp1()}
    
    return (re_temp)})
  
  # affichage de la table
  output$tableSelectOutput <- renderDataTable({
    re_temp() %>%  select(`NOM DU SITE`,
                          `GROUPE DE DECHETS`,
                          `SOUS-GROUPE DE DECHETS`,
                          `DESCRIPTION PHYSIQUE`,
                          `FAMILLE IN`, 
                          `VOLUME EQUIVALENT CONDITIONNE`,
                          `ACTIVITE ( Bq)`, 
                          `NOM_REG`)})

# affichage de la carte   
  output$carte_ville <- renderLeaflet({
    leaflet(data=re_temp()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(~as.numeric(lng), 
                 ~as.numeric(lat),
                 clusterOptions = markerClusterOptions(),
                 popup = paste(
                   "<b>Site : ", re_temp()$`NOM DU SITE`, "</b><br/>",
                   "<b>Activité en Bq : ", re_temp()$`ACTIVITE ( Bq)`,"</b> <br/>", 
                   "Quantité en VEC :", re_temp()$`VOLUME EQUIVALENT CONDITIONNE`, "<br/>",
                   "Groupe de déchet :", re_temp()$`GROUPE DE DECHETS`, "<br/>"),
                 label = ~ as.character(`NOM_COM`),
                 #icon ne s'adapte pas encore à l'activité à cause des valeurs "-"
                 icon= makeIcon(iconUrl = "../img/radioactif.png", iconWidth = 50, iconHeight = 50))})
})
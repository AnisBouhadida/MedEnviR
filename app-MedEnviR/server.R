# ================================================================================
# Etudiants : * Anis BOUHADIDA (Universite Paris 13)
#             * Radouane ELAYACHY (Universite Paris Descarte)

# Fichier : * Contient toutes les fonctions appelee par le serveur 
# ================================================================================

library(shiny)
library(tidyverse)
library(leaflet)

temp <-inner_join(ED_faitRepartitionPoluant, ED_dimensionGeo) #jointure des faits avec les coordonnées
#PROBLEME DANS LA TABLE DIM GEO PLUSIEURS MEMES ID POUR DIFF INSEE
temp_sep<-temp %>% separate(`Geo Point`,c("lat","lng"),sep = ",")

shinyServer(function(input, output) {
   
  output$dechetSelectOutput <- renderUI({
    
   switch(input$dechetSelectInput,
          
          "Groupe"= selectInput(
            inputId = "selectInput",label = "Selectionnez ",
            choices = select(ED_dimensionDechet,`GROUPE DE DECHETS`)%>% drop_na()),
          
          "Sous-groupe"= selectInput(
            inputId = "selectInput",label = "Selectionnez ",
            choices =select(ED_dimensionDechet,`SOUS-GROUPE DE DECHETS`)%>% drop_na()),
         
          "Famille" = selectInput(
                inputId = "selectInput",label = "Selectionnez ",
                choices =select(ED_dimensionDechet,`FAMILLE IN`)%>% drop_na())
          )
    })
  output$selectOutput <- renderDataTable({
    ED_dimensionDechet %>% 
      filter(`GROUPE DE DECHETS`==input$selectInput|
               `SOUS-GROUPE DE DECHETS`==input$selectInput|
               `FAMILLE IN`==input$selectInput) %>% 
      left_join(ED_faitRepartitionPoluant,by=c("id_dim_dechet")) %>% 
      left_join(ED_dimensionProducteurDechet,by=c("id_dim_producteur")) %>%
      select(`NOM DU SITE`,`GROUPE DE DECHETS`,`SOUS-GROUPE DE DECHETS`,
             `DESCRIPTION PHYSIQUE`,`FAMILLE IN`, `VOLUME EQUIVALENT CONDITIONNE`,
             `ACTIVITE ( Bq)`)
  })
  city <- reactive({
    ED_faitRepartitionPoluant %>%
      filter(id_dim_geo %in% filter(ED_dimensionGeo, NOM_COM==input$select))})
  
  
  output$carte_ville <- renderLeaflet(
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng=2.3037, lat=46.4317, zoom=5) %>%
      addMarkers(lat=as.numeric(temp_sep$lat), lng = as.numeric(temp_sep$lng),
                 clusterOptions = markerClusterOptions(),
                 popup = temp_sep$`ACTIVITE ( Bq)`, 
                 icon= makeIcon(iconUrl = "../img/radioactif.png", iconWidth = 40, iconHeight = 40)))
})
#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)


temp <-inner_join(ED_faitRepartitionPoluant, ED_dimensionGeo) #jointure des faits avec les coordonnées
#PROBLEME DANS LA TABLE DIM GEO PLUSIEURS MEMES ID POUR DIFF INSEE
temp_sep<-temp %>% separate(`Geo Point`,c("lat","lng"),sep = ",")

# serveur
shinyServer(function(input, output) {
   
  city <- reactive({
    ED_faitRepartitionPoluant %>%
      filter(id_dim_geo %in% filter(ED_dimensionGeo, NOM_COM==input$select))})
  
 
  output$carte_ville <- renderLeaflet(
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng=2.3037, lat=46.4317, zoom=5) %>%
      addMarkers(lat=as.numeric(temp_sep$lat), lng = as.numeric(temp_sep$lng),
                 clusterOptions = markerClusterOptions(),
                 popup = temp_sep$`ACTIVITE ( Bq)`, icon= makeIcon(iconUrl = "../img/radioactif.png", iconWidth = 40, iconHeight = 40))
      
    
  )
  
})

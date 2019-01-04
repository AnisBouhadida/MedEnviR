# ================================================================================
# Etudiants : * Anis BOUHADIDA (Universite Paris 13)
#             * Radouane ELAYACHY (Universite Paris Descarte)

# Fichier : * Contient toutes les fonctions appelee par le serveur 
# ================================================================================

library(shiny)
library(tidyverse)  

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
})

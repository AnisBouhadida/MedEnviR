# ================================================================================
# Etudiants : * Anis BOUHADIDA (Universite Paris 13)
#             * Radouane ELAYACHY (Universite Paris Descarte)

# Fichier : * Contient tous les elements de l'interface 
# ================================================================================

library(shiny)
library(tidyverse)
library(leaflet)

# UI de l'application qui explore l'entrepot de donnees:
shinyUI(fluidPage(
  
  titlePanel("Selectionner"),
  

  # Sidebar avec bouttons radio et un selectInput : 
  sidebarLayout(
    sidebarPanel(
       radioButtons(inputId = "dechetSelectInput" ,label = "Dechets selon :",
                    choices = list("Groupe","Sous-groupe","Famille")),
       uiOutput(outputId = "dechetSelectOutput"),
       selectInput("select", label = h3("Selectionner Ville"), 
                   choices = sort(ED_dimensionGeo$NOM_COM), selected = 1),
       selectInput("select", label = h3("Selectionner Region"), 
                   choices = sort(ED_dimensionGeo$NOM_COM), selected = 1))
    ),
    
    # mainPanel avec 2 tabPanel, 1 pour la carte et l'autre pour le tableau:
    mainPanel(
      tabsetPanel(tabPanel("Carte", leafletOutput("carte_ville")),
                  tabPanel("Tableau",dataTableOutput(outputId = "selectOutput"))
                  )
      )
  )
)
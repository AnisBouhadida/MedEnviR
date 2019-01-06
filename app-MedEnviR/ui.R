# ================================================================================
# Etudiants : * Anis BOUHADIDA (Universite Paris 13)
#             * Radouane ELAYACHY (Universite Paris Descarte)

# Fichier : * Contient tous les elements de l'interface 
# ================================================================================

library(shiny)
library(tidyverse)
library(leaflet)
library(shinythemes)
library(DT)

# UI de l'application qui explore l'entrepot de donnees:
shinyUI(fluidPage(
  
  titlePanel(h1("Déchets radioactif et émission naturelle de Radon en France")),

  # Sidebar avec bouttons radio et un selectInput : 
  sidebarLayout(
    sidebarPanel(
        
       radioButtons(inputId = "dechetSelectInput" ,label = h2("Dechets selon :"),
                    choices = list("Tous les groupes","Groupe","Sous-groupe","Famille")),
       uiOutput(outputId = "dechetSelectOutput"),
       
       radioButtons(inputId = "geoSelectInput" ,label = h2("Afficher selon :"),
                    choices = list("France entière","Region","Departement","Commune","Site")),
       uiOutput("geo")),
    
    # mainPanel avec 4 tabPanel:
    mainPanel(
      tabsetPanel(id="dataViz",
                  tabPanel("Carte",value = "a", leafletOutput("carte_ville")),
                  tabPanel("Tableau",value = "b",dataTableOutput(outputId = "tableSelectOutput")),
                  tabPanel("Carte incidence",value = "c",plotOutput("carte_cartography"))
                  )
      )
  )
))

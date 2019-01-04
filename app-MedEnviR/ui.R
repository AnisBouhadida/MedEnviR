# ================================================================================
# Etudiants : * Anis BOUHADIDA (Universite Paris 13)
#             * Radouane ELAYACHY (Universite Paris Descarte)

# Fichier : * Contient tous les elements de l'interface 
# ================================================================================

library(shiny)
# UI de l'application qui explore l'entrepot de donnees:
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Polluants en France"),
  
  # Sidebar avec bouttons radio et un selectInput : 
  sidebarLayout(
    sidebarPanel(
       radioButtons(inputId = "dechetSelectInput" ,label = "Dechets selon :",
                    choices = list("Groupe","Sous-groupe","Famille")),
       uiOutput(outputId = "dechetSelectOutput")
    ),
    
    # mainPanel avec 2 tabPanel, 1 pour la carte et l'autre pour le tableau:
    mainPanel(
      tabsetPanel(tabPanel("Carte"),
                  tabPanel("Tableau",dataTableOutput(outputId = "selectOutput"))
                  )
      )
  )
))
# ================================================================================
# Etudiants : * Anis BOUHADIDA (Universite Paris 13)
#             * Radouane ELAYACHY (Universite Paris Descarte)

# Fichier : * Contient tous les elements de l'interface 
# ================================================================================

# UI de l'application qui explore l'entrepot de donnees:
shinyUI(fluidPage(
  
  #choix du thème
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://bootswatch.com/4/sketchy/bootstrap.css")
  ),
  
  #titre de la page
  headerPanel(h1("Déchets radioactifs et émission naturelle de Radon en France")),
  
  # Sidebar pour les choix de déchets et de région : 
  sidebarLayout(
    sidebarPanel(
        
       radioButtons(inputId = "dechetSelectInput" ,label = h2("Dechets selon :"),
                    choices = list("Tous les groupes","Groupe","Sous-groupe","Famille")),
       uiOutput(outputId = "dechetSelectOutput"),
       
       radioButtons(inputId = "geoSelectInput" ,label = h2("Afficher selon :"),
                    choices = list("France entière","Region","Departement","Commune","Site")),
       uiOutput("geo")),
      

    # mainPanel avec 3 tabPanel:
    mainPanel(
      tabsetPanel(id="dataViz",
                  tabPanel("Carte",value = "a", leafletOutput("carte_ville")),
                  tabPanel("Tableau",value = "b",
                           #affichage des colonnes à sélectionner
                           checkboxGroupInput(inputId="show_vars", label="Colonnes à afficher :",
                                                               choices=names(data_test), selected = names(data_test),
                                                               inline = TRUE),
                           DT::dataTableOutput(outputId = "tableSelectOutput")
                  ),
                  tabPanel("Carte incidence",value = "c",plotOutput("carte_cartography"))#,
                  # tabPanel("Valeurs du test de Stone",value = "d",
                  #          DT::dataTableOutput(outputId = "table2SelectOutput"))
                  )
      )
  )
))



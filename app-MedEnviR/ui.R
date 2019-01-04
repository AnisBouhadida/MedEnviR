#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  titlePanel("Selectionner"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("select", label = h3("Selectionner Ville"), 
                  choices = sort(ED_dimensionGeo$NOM_COM), selected = 1),
      selectInput("select", label = h3("Selectionner Region"), 
                  choices = sort(ED_dimensionGeo$NOM_COM), selected = 1)),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Carte", leafletOutput("carte_ville")),
        tabPanel("Table"))
    )
  )
))



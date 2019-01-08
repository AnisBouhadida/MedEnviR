# ================================================================================
# Etudiants : * Anis BOUHADIDA (Universite Paris 13)
#             * Radouane ELAYACHY (Universite Paris Descarte)

# Fichier : * Contient toutes les fonctions appelee par le serveur 
# ================================================================================
library(DT)

shinyServer(function(input, output,session) {


#permettre de renvoyer à l'utilisateur liste de choix pour les groupes 
  output$dechetSelectOutput <- renderUI({
   switch(input$dechetSelectInput,
          "Groupe"= {
                  selectInput(
                    inputId = "selectAttrDechet",label = h3("Selectionner un groupe de déchet "),
                    choices = dplyr::select(data_test,`GROUPE DE DECHETS`)%>% drop_na())
            },
          "Sous-groupe"= {
                  selectInput(
                    inputId = "selectAttrDechet",label = h3("Selectionner un sous-groupe de déchet "),
                    choices =dplyr:: select(data_test,`SOUS-GROUPE DE DECHETS`)%>% drop_na())
            },
          "Famille" = {
                  selectInput(
                      inputId = "selectAttrDechet",label = h3("Selectionner une famille de déchet "),
                      choices =dplyr::select(data_test,`FAMILLE IN`)%>% drop_na())
            }
          )
    })
  
  observe({
    if(input$dataViz == "c"){
      updateRadioButtons(session,inputId = "geoSelectInput",
                         choices = list("France entière","Region"))
    }
    if(input$dataViz == "a" | input$dataViz == "b"){
      updateRadioButtons(session,inputId = "geoSelectInput",
                         choices = list("France entière","Region","Departement","Commune","Site"))
    }
  })
  
  
#permettre de renvoyer à l'utilisateur liste de choix pour les localisations
  output$geo <- renderUI({
    switch(input$geoSelectInput,
           "Region"= selectInput(
             inputId = "selectAttrGeo",label = h3("Selectionner la region "),
             choices = sort(data_departement$NOM_REG)),
           "Departement"= selectInput(
             inputId = "selectAttrGeo",label = h3("Selectionner le departement "),
             choices = sort(data_departement$NOM_DEPT)),
           "Commune" = selectInput(
             inputId = "selectAttrGeo",label = h3("Selectionner la commune "),
             choices = sort(data_departement$NOM_COM)),
           "Site"= selectInput(
             inputId = "selectAttrGeo",label = h3("Selectionner le site "),
             choices = sort(data_departement$`NOM DU SITE`)))})
  
# création reactive prenant en compte les choix de l'utilisateur pour l'affiche de la carte et de la table
  data <- reactive({
    data_test
    })
  
  filtered_dechet <- reactive({
    # req permet d'éviter message d'erreur et ne montre la carte que si sélection faite
    req(input$selectAttrDechet, cancelOutput = FALSE) 
    switch (input$dechetSelectInput,
            "Groupe" = filtered_dechet <- data() %>% filter(`GROUPE DE DECHETS`==input$selectAttrDechet),
            "Sous-groupe" = filtered_dechet <- data() %>% filter(`SOUS-GROUPE DE DECHETS`==input$selectAttrDechet),
            "Famille" = filtered_dechet <- data() %>% filter(`FAMILLE IN`==input$selectAttrDechet))
  })
  
  filtered_geo <- reactive({
    req(input$selectAttrGeo, cancelOutput = FALSE)   
    switch (input$geoSelectInput,
            "Commune" = filtered_geo <- data() %>% filter(`NOM_COM`==input$selectAttrGeo ),
            "Region" = filtered_geo <- data() %>% filter(`NOM_REG`==input$selectAttrGeo ),
            "Departement" = filtered_geo <- data() %>% filter(`NOM_DEPT`==input$selectAttrGeo),
            "Site" = filtered_geo <- data() %>% filter(`NOM DU SITE`==input$selectAttrGeo ))
  })
  
  showed_result <- reactive({
    if(input$dechetSelectInput != "Tous les groupes" & input$geoSelectInput !="France entière"){
      showed_result <- filtered_dechet() %>% inner_join(filtered_geo())
      #message si absence de données correspondantes
      validate(
        need(is_empty(showed_result)==TRUE , "Absence de données pour les critères choisis"))}
    
    else if(input$dechetSelectInput == "Tous les groupes" & input$geoSelectInput !="France entière"){
      showed_result <- filtered_geo()} 
    
    else if(input$dechetSelectInput != "Tous les groupes" & input$geoSelectInput =="France entière"){
      showed_result <- filtered_dechet()}
    
    else {showed_result<- data()}
  })
  
# affichage de la table
  output$tableSelectOutput <- DT::renderDataTable({
    
    # showed_result() %>%  dplyr::select(`NOM DU SITE`,`GROUPE DE DECHETS`,`SOUS-GROUPE DE DECHETS`,
    #                             `DESCRIPTION PHYSIQUE`,`FAMILLE IN`, `VOLUME EQUIVALENT CONDITIONNE`,
    #                             `ACTIVITE ( Bq)`, `NOM_REG`, classe_potentiel) %>%
      DT::datatable(showed_result()[, input$show_vars, drop = FALSE], options = list(orderClasses = TRUE))
  })
  
# affichage de la carte répartition polluants   
  output$carte_ville <- renderLeaflet({
    
    
    pal <- colorBin("YlOrRd", domain = data_radon_carte$classe_potentiel)
    #data_radon_carte <-data_radon_carte%>% left_join(filtered_geo()) %>% st_transform("+proj=longlat +datum=WGS84")
    leaflet(data=showed_result()) %>%
      addMeasure(       #addin pour faire des mesures sur la carte
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479")%>%
      
      addEasyButton(
        easyButton(    
          icon="fa-globe", title="Zoom réinitialisé", 
          onClick=JS("function(btn, map){ map.setZoom(2); }"))) %>%
      
      addEasyButton(
        easyButton(
            icon="fa-crosshairs", title="Locate Me",
            onClick=JS("function(btn, map){ map.locate({setView: true}); }")))%>%
      
      addMiniMap(
        tiles = providers$Esri.WorldStreetMap,
        toggleDisplay = TRUE) %>%
      
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
                 icon= makeIcon(iconUrl = "../img/radioactif.png", iconWidth = 50, iconHeight = 50))
    
    # #représentation du chloropath du radon
    # addPolygons(data= data_radon_carte , color = "#444444", weight = 1, smoothFactor = 0.5,
    #             fillColor = ~pal(data_radon_carte$classe_potentiel),
    #             opacity = 1.0, fillOpacity = 0.7,
    #             dashArray = "3")#, limite en pointillé
                #label = str_c(data_departement$NOM_DEPT),
                #labelOptions = labelOptions(
                #  style = list("font-weight" = "normal", padding = "3px 8px"),
                #  textsize = "15px",
                #  direction = "auto"),
                #pour la surbrillance du polygone lorsque souris dessus:
               # highlightOptions = highlightOptions(color = "white", weight = 2,
               #                                     bringToFront = TRUE))
    })

  
# Affichage des donnees evenements sur la carte en utilisant cartography:
  #Filtration des données carto selon les données sélectionnées
  data_carto <- reactive({
    
    if(input$geoSelectInput !="France entière"){
      req(filtered_geo(),cancelOutput = FALSE) 
      data_carto <- data_departement %>% filter(NOM_REG==unique(filtered_geo()$NOM_REG))
    }
    else {data_carto <- data_departement}
  })
  
  #Affichage de la carte
  output$carte_cartography <- renderPlot({
    
    choroLayer(data_carto(),
               var = 'ratio',
               method = 'quantile',
               col = carto.pal("orange.pal", 9), 
               border = "black",
               lwd = 1.5, 
               legend.values.rnd = 1, 
               legend.pos = 'left', 
               legend.title.txt = 'Incidence/100000 hab.') 
    
    layoutLayer(title = "Incidence en 2016 de la maladie X et répartition des déchets radioactif",
                scale = 0, 
                frame = FALSE,
                col = "#688994")
    
    
    # Creation des points dechets radioactifs selon sélection utilisateur :
    showed_result() %>% dplyr::select(lng,lat) %>% drop_na() %>% st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
      st_transform(crs = 2154) %>%plot(pch = 20, add = TRUE, col="blue") 
    
  })
  
#table statistique
  # output$table2SelectOutput <- DT::renderDataTable({
  #   DT::datatable([, input$show_vars, drop = FALSE], options = list(orderClasses = TRUE))
  # })
})
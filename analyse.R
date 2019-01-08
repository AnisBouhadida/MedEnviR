library(epitools)
library(ggmap)
library(cartography)
library(tidyverse)
library(sf)
library(sp)
library(htmltools)
library(htmlwidgets)
library(leaflet)
library(questionr)


# Affichage des données évènements sur la carte en utilisant cartography ----
choroLayer(data_departement,
           var = 'ratio',
           method = 'quantile', # méthode de discrétisation
           col = carto.pal("orange.pal", 9), 
           border = "black",
           lwd = 1.5, 
           legend.values.rnd = 1, 
           legend.pos = 'left', 
           legend.title.txt = 'Incidence/100000 hab.') 
layoutLayer(title = "Incidence en 2016 de la maladie X et répartition des déchets radioactif",
            sources = "Sources : ",
            author = "Auteur : ",
            scale = 0, 
            frame = FALSE,
            col = "#688994") 

# Création des points déchets radioactifs ----
dechet.coor <- data_test%>%
  dplyr::select(lng,lat)%>%   #ajout ID fait 
  drop_na()%>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(crs = 2154)

# Ajout des points à la carte évènement
plot(dechet.coor, pch = 20, add = TRUE, col="darkblue") 


# Pour l'affichage d'une seule région exemple ici de l'Ile-de-France----
dep_idf <- data_departement %>% 
  filter(NOM_REG=='ILE-DE-FRANCE') 

choroLayer(dep_idf,
           var = 'ratio',
           method = 'quantile', # méthode de discrétisation
           col = carto.pal("orange.pal", 9), 
           border = "black",
           lwd = 1.5, 
           legend.values.rnd = 1, 
           legend.pos = 'left', 
           legend.title.txt = 'Incidence/100000 hab.') 
layoutLayer(title = paste("Incidence en 2016 de la maladie X et répartition des déchets radioactif en", 
                          str_c(dep_idf$NOM_REG)),
            sources = "Sources : ",
            author = "Auteur : ",
            scale = 0, 
            frame = FALSE,
            col = "#688994") 


dechet_IDF.coor <- data_test %>%
  select(lng,lat,NOM_REG) %>%
  filter(NOM_REG=="ILE-DE-FRANCE") %>%
  drop_na() %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(crs = 2154)

plot(dechet_IDF.coor, pch = 20, add = TRUE, col="darkblue") 

# affichage avec leaflet----

#transformation dans le mode polygon accepté par Leaflet
data_departement1<-st_transform(data_departement, "+proj=longlat +datum=WGS84") 

#palette couleur
pal <- colorBin("YlOrRd", domain = data_departement$ratio)#palette couleur

leaflet() %>% 
  addLegend(data=data_departement1, #légende à mettre en premier sinon ne sait pas quelle carte prendre
            pal = pal,
            values=~data_departement$ratio, 
            opacity = 0.7,
            title = "Incidence/100.000 hab.") %>%
  
  #addin pour faire des mesures sur la carte
  addMeasure(       
    position = "bottomleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479")%>%
  
  #bouton zoom réinitialisé 
  addEasyButton(easyButton(    
    icon="fa-globe", title="Zoom réinitialisé", 
    onClick=JS("function(btn, map){ map.setZoom(5); }"))) %>%
  addEasyButton(easyButton(
    icon="fa-crosshairs", title="Locate Me",
    onClick=JS("function(btn, map){ map.locate({setView: true}); }")))%>%
  
  #addin pour la minicarte dans le coin
  addMiniMap(
    tiles = providers$Esri.WorldStreetMap,
    toggleDisplay = TRUE)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  #représentation des points polluants
  addMarkers(data=data_test,
             ~as.numeric(lng), 
             ~as.numeric(lat),
             clusterOptions = markerClusterOptions(),
             popup = paste(
               "<b>Site : ", data_test$`NOM DU SITE`, "</b><br/>",
               "<b>Activité en Bq : ", data_test$`ACTIVITE ( Bq)`,"</b> <br/>", 
               "Quantité en VEC :", data_test$`VOLUME EQUIVALENT CONDITIONNE`, "<br/>",
               "Groupe de déchet :", data_test$`GROUPE DE DECHETS`, "<br/>"),
             label = ~ as.character(`NOM_COM`),
             icon= makeIcon(iconUrl = "../img/radioactif.png", iconWidth = 50, iconHeight = 50))%>%
  
  #représentation du chloropath
  addPolygons(data= data_departement1, color = "#444444", weight = 1, smoothFactor = 0.5,
              fillColor = ~pal(ratio),
              opacity = 1.0, fillOpacity = 0.7,
              dashArray = "3",# limite en pointillé
              label = str_c(data_departement$NOM_DEPT),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              #pour la surbrillance du polygone lorsque souris dessus:
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))



# Analyses spatiales ----
# Nous avons donc des évènements (maladie X) par age, sexe, et la localisation (DEPARTEMENT)
# et des déchets radioactifs selon leur localisation (X,Y)
# On recherche donc des agrégats de maladie autour d'un point source 
# --> TEST DE CONCENTRATION
# agrégation des individus en sous-groupe département (données agrégées) 
library(sp)
library(DCluster)



#test de permutation qu'on utilise lorsque l'on ne peut pas connaitre la distribution sous l'hypothèse nulle
# Test de Stones

region<-which(data_departement$NOM_DEPT=="HAUTE-LOIRE") #a row number  

stone.stat(data_departement, region=region, lambda=NULL)
stone.test(Observed~offset(log(Expected)), data_departement, model="poisson", R=99, 
           region=region, lambda=NULL)


for (i in data_departement$NOM_DEPT){
  region<-which(data_departement$NOM_DEPT==i)
  stone.stat(data_departement, region=region, lambda=NULL)
  s<-stone.test(Observed~offset(log(Expected)), data_departement, model="poisson", R=99, 
             region=region, lambda=NULL)
  print(i)
  print(s)
}

# compute Moran statistics 


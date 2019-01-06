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


# Importation de l'effectif en France ----
effectif_france <- read_delim("./data/effectif.france.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Importation de la table avec le nombre d'évènements ----
evenements <- read_delim("./data/evenements.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Importation de la table avec l'effectif par département ----
effectif_departement <- read_delim("./data/effectif.departement.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Ajustement direct de toute la table selon effectif par département et France entière ----
ratio.vector <- vector()
for (i in colnames(evenements[,-1])) {
  ratio.vector <- append(ratio.vector, round(ageadjust.direct(count = evenements[,i], 
                                                              pop = effectif_departement[,i], 
                                                              stdpop = effectif_france[,2])["adj.rate"] *10^5, 2))} 
                                                             # 10^5 : pour 100000 habitants / 2 : 2 décimales
standdirect_dep <- tibble(dep = colnames(evenements[,-1]), 
                          ratio = ratio.vector)

# Importations données géographiques de GEOFLA des départements (car évènement/dpt)----
dep <- st_read("data/cartes/DEPARTEMENT.shp")


# jointure données carto et les taux standardisés d'évènements par département----
dep_ordre <- sort(dep$NOM_DEPT)
standdirect_dep <- standdirect_dep %>% 
  arrange(dep) %>% 
  mutate(dep = dep_ordre)
dep <- left_join(dep, standdirect_dep, by = c('NOM_DEPT' = 'dep')) 

# Affichage des données évènements sur la carte en utilisant cartography ----
choroLayer(dep,
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
dechet<-ED_faitRepartitionPoluant %>% 
  left_join(ED_dimensionDechet) %>% 
  left_join(ED_dimensionProducteurDechet) %>%
  left_join(ED_dimensionGeo) 
dechet.coor <- dechet%>%
  dplyr::select(lng,lat)%>%
  drop_na()%>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(crs = 2154)

# Ajout des points à la carte évènement
plot(dechet.coor, pch = 20, add = TRUE, col="darkblue") 


# Pour l'affichage d'une seule région exemple ici de l'Ile-de-France----
dep_idf <- dep %>% 
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

dechet<-ED_faitRepartitionPoluant %>% 
  left_join(ED_dimensionDechet) %>% 
  left_join(ED_dimensionProducteurDechet) %>%
  left_join(ED_dimensionGeo) 
dechet_IDF.coor <- dechet %>%
  select(lng,lat,NOM_REG) %>%
  filter(NOM_REG=="ILE-DE-FRANCE") %>%
  drop_na() %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(crs = 2154)

plot(dechet_IDF.coor, pch = 20, add = TRUE, col="darkblue") 

# affichage avec leaflet----
re_temp<- ED_faitRepartitionPoluant %>% 
  left_join(ED_dimensionDechet) %>% 
  left_join(ED_dimensionProducteurDechet) %>%
  left_join(ED_dimensionGeo)

dep.sf<-st_transform(dep, "+proj=longlat +datum=WGS84") #transformation dans le mode polygon accepté par Leaflet


couleurs <- colorNumeric("YlOrRd", dep.sf$ratio, n = 10) #palette couleur
pal <- colorBin("YlOrRd", domain = dep.sf$ratio)
leaflet() %>% #attention dans cette partie "()" du re_temp enlevée
  addLegend(data=dep.sf, #légende à mettre en premier sinon ne sait plus quelle carte prendre
            pal = pal,
            values=~dep.sf$ratio, 
            opacity = 0.7,
            title = "Incidence/100.000 hab.") %>%
  addMeasure(       #addin pour faire des mesures sur la carte
    position = "bottomleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479")%>%
  addEasyButton(easyButton(    #bouton zoom réinitialiser à vérifier si marche lorsque choix de région
    icon="fa-globe", title="Zoom to France", #sinon changer titre
    onClick=JS("function(btn, map){ map.setZoom(5); }"))) %>%
  addEasyButton(easyButton(
    icon="fa-crosshairs", title="Locate Me",
    onClick=JS("function(btn, map){ map.locate({setView: true}); }")))%>%
  
  addMiniMap(
    tiles = providers$Esri.WorldStreetMap,
    toggleDisplay = TRUE)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(data=re_temp,
             ~as.numeric(lng), 
             ~as.numeric(lat),
             clusterOptions = markerClusterOptions(),
             popup = paste(
               "<b>Site : ", re_temp$`NOM DU SITE`, "</b><br/>",
               "<b>Activité en Bq : ", re_temp$`ACTIVITE ( Bq)`,"</b> <br/>", 
               "Quantité en VEC :", re_temp$`VOLUME EQUIVALENT CONDITIONNE`, "<br/>",
               "Groupe de déchet :", re_temp$`GROUPE DE DECHETS`, "<br/>"),
             label = ~ as.character(`NOM_COM`),
             icon= makeIcon(iconUrl = "./img/radioactif.png", iconWidth = 50, iconHeight = 50))%>%
  addPolygons(data= dep.sf, color = "#444444", weight = 1, smoothFactor = 0.5,
              fillColor = ~pal(ratio),
              opacity = 1.0, fillOpacity = 0.7,
              dashArray = "3",# limite en pointillé
              label = str_c(dep.sf$NOM_DEPT),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              # pour la surbrillance du polygone lorsque souris dessus:
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))


# Nous avons donc des évènements (maladie X) par age, sexe, et la localisation (DEPARTEMENT)
# et des déchets radioactifs selon leur localisation (X,Y)
# On recherche donc des agrégats de maladie autour d'un point source 
# --> TEST DE CONCENTRATION
# agrégation des individus en sous-groupe département (données agrégées) 
library(sp)
library(DCluster)

#création d'une table avec nombre observé et nombre attendu par département
nbre.vector <- vector()
for (i in colnames(evenements[,-1])) {
  nbre.vector <- append(nbre.vector, sum(evenements[,i]))}
eff.vector<-vector()
for (i in colnames(effectif_departement[,-1])) {
  eff.vector <- append(eff.vector, sum(effectif_departement[,i]))}
a <-tibble(dep = colnames(evenements[,-1]), 
           ratio = ratio.vector, 
           nbre_observé = nbre.vector, 
           effectif = eff.vector,
           nbre_attendu= ratio.vector*eff.vector/10^5)
dep_ordre <- sort(dep$NOM_DEPT)
a <- a %>% 
  arrange(dep) %>% 
  mutate(dep = dep_ordre)
dep <- left_join(dep, a, by = c('NOM_DEPT' = 'dep')) 
dep <- rename.variable(dep, "ratio.x", "ratio")


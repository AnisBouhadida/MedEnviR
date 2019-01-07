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


# # Importation de l'effectif en France ----
# effectif_france <- read_delim("./data/effectif.france.csv", ";", escape_double = FALSE, trim_ws = TRUE)
# 
# # Importation de la table avec le nombre d'évènements ----
# evenements <- read_delim("./data/evenements.csv", ";", escape_double = FALSE, trim_ws = TRUE)
# 
# # Importation de la table avec l'effectif par département ----
# effectif_departement <- read_delim("./data/effectif.departement.csv", ";", escape_double = FALSE, trim_ws = TRUE)
# 
# # Ajustement direct de toute la table selon effectif par département et France entière ----
# ratio.vector <- vector()
# for (i in colnames(evenements[,-1])) {
#   ratio.vector <- append(ratio.vector, round(ageadjust.direct(count = evenements[,i], 
#                                                               pop = effectif_departement[,i], 
#                                                               stdpop = effectif_france[,2])["adj.rate"] *10^5, 2))} 
#                                                              # 10^5 : pour 100000 habitants / 2 : 2 décimales
# standdirect_dep <- tibble(dep = colnames(evenements[,-1]), 
#                           ratio = ratio.vector)
# 
# # Importations données géographiques de GEOFLA des départements (car évènement/dpt)----
# dep <- st_read("../data/cartes/DEPARTEMENT.shp")
# 
# 
# # jointure données carto et les taux standardisés d'évènements par département----
# dep_ordre <- sort(dep$NOM_DEPT)
# standdirect_dep <- standdirect_dep %>% 
#   arrange(dep) %>% 
#   mutate(dep = dep_ordre)
# dep <- left_join(dep, standdirect_dep, by = c('NOM_DEPT' = 'dep')) 

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
dechet<-ED_faitRepartitionPoluant %>% 
  left_join(ED_dimensionDechet) %>% 
  left_join(ED_dimensionProducteurDechet) %>%
  left_join(ED_dimensionGeo) 
dechet.coor <- dechet%>%
  dplyr::select(lng,lat,id_fait)%>%   #ajout ID fait 
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


dechet_IDF.coor <- dechet %>%
  select(lng,lat,NOM_REG) %>%
  filter(NOM_REG=="ILE-DE-FRANCE") %>%
  drop_na() %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(crs = 2154)

plot(dechet_IDF.coor, pch = 20, add = TRUE, col="darkblue") 

# affichage avec leaflet----
data_departement<-st_transform(data_departement, "+proj=longlat +datum=WGS84") #transformation dans le mode polygon accepté par Leaflet

pal <- colorBin("YlOrRd", domain = data_departement$ratio)#palette couleur

leaflet() %>% #attention dans cette partie "()" du re_temp enlevée
  addLegend(data=data_departement, #légende à mettre en premier sinon ne sait plus quelle carte prendre
            pal = pal,
            values=~data_departement$ratio, 
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
  addMarkers(data=dechet,
             ~as.numeric(lng), 
             ~as.numeric(lat),
             clusterOptions = markerClusterOptions(),
             popup = paste(
               "<b>Site : ", dechet$`NOM DU SITE`, "</b><br/>",
               "<b>Activité en Bq : ", dechet$`ACTIVITE ( Bq)`,"</b> <br/>", 
               "Quantité en VEC :", dechet$`VOLUME EQUIVALENT CONDITIONNE`, "<br/>",
               "Groupe de déchet :", dechet$`GROUPE DE DECHETS`, "<br/>"),
             label = ~ as.character(`NOM_COM`),
             icon= makeIcon(iconUrl = "../img/radioactif.png", iconWidth = 50, iconHeight = 50))%>%
  addPolygons(data= data_departement, color = "#444444", weight = 1, smoothFactor = 0.5,
              fillColor = ~pal(ratio),
              opacity = 1.0, fillOpacity = 0.7,
              dashArray = "3",# limite en pointillé
              label = str_c(data_departement$NOM_DEPT),
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

# #création d'une table avec nombre observé et nombre attendu par département
# nbre.vector <- vector()
# for (i in colnames(evenements[,-1])) {
#   nbre.vector <- append(nbre.vector, sum(evenements[,i]))}
# eff.vector<-vector()
# for (i in colnames(effectif_departement[,-1])) {
#   eff.vector <- append(eff.vector, sum(effectif_departement[,i]))}
# a <-tibble(NOM_DEPT = colnames(evenements[,-1]), 
#            Observed = nbre.vector, 
#            effectif = eff.vector,
#            Expected= ratio_vector*eff.vector/10^5)
# 
# 
# nom_depback <- a$NOM_DEPT
# a$NOM_DEPT<-a$NOM_DEPT%>% iconv(from = "UTF-8", to="ASCII//TRANSLIT" ) %>%gsub(pattern = "\\W", "", .) %>% toupper()
# data_departement$NOM_DEPT<-data_departement$NOM_DEPT %>% gsub(pattern = "\\W", "", .)
# 
# data_departement <- right_join(data_departement, a, by = 'NOM_DEPT')


# sids<-data.frame(Observed=nc.sids$SID74)
# sids<-cbind(sids, Expected=nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74))
# sids<-cbind(sids, x=nc.sids$x, y=nc.sids$y)

#Compute Stones statistic around a location, 
#test de permutation qu'on utilise lorsque l'on ne peut pas 
#connaitre la distribution
#sous l'hypothèse nulle
dechets_test3 <- left_join(dechet,data_departement) 
dechets_test3<- dechets_test3select_()
dechets_test3 <-dechets_test3%>% drop_na_(vars = "x")

#dechets_test1<-cbind(dechets_test1,x=dechets_test$X_CENTROID, y=dechets_test$Y_CENTROID)
#region<-which(row.names(nc.sids)=="Robeson")
region<-which(data_departement$NOM_DEPT=="HAUTE-LOIRE") #a row number     #choix dechets_test2 ou dechet ou data_dept?!
#stone.stat(sids, region=region, lambda=1)
stone.stat(data_departement, region=region, lambda=NULL)
s<-stone.test(Observed~offset(log(Expected)), data_departement, model="poisson", R=99, 
           region=region, lambda=NULL)
print(s)

for (i in data_departement$NOM_DEPT){
  region<-which(data_departement$NOM_DEPT==i)
  stone.stat(data_departement, region=region, lambda=NULL)
  s<-stone.test(Observed~offset(log(Expected)), data_departement, model="poisson", R=99, 
             region=region, lambda=NULL)
  print(i)
  print(s)
}

for (i in data_departement$NOM_DEPT){print((i))}

# compute Moran statistics
col.W <- nb2listw(ncCR85.nb, zero.policy=TRUE)
sids<-data.frame(Observed=nc.sids$SID74)
sids<-cbind(sids, Expected=nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74) )
moranI.stat(data=sids, listw=col.W, n=length(ncCR85.nb), S0=Szero(col.W) )
moranI.stat(data=sids, applyto="residuals", listw=col.W, n=length(ncCR85.nb),
            S0=Szero(col.W) )
moranI.test(Observed~offset(log(Expected)), sids, model="poisson", R=99,
            listw=col.W, n=length(ncCR85.nb), S0=Szero(col.W) )

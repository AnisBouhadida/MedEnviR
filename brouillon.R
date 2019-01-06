# rechercher doublons
test0 <- duplicated(data_dechet)
length(test0)
for (i in 1:length(test0)){
  if(test0[[i]]){dup<-TRUE } else {dup<-FALSE}
}

######
skim(data_dechet)
data_dechet  <- na.omit(data_dechet)
data_dechet_clean <- data_dechet[complete.cases(data_dechet),]

skim(data_INSEE_clean)

data_INSEE_clean <- data_INSEE[complete.cases(data_INSEE),]

# Entrepot de donnees:

ED_faitRepartitionPoluant <- data.frame() %>% tbl_df()
ED_faitRepartitionPoluant <- select(ED_dimensionDechet,id_dim_dechet)
ED_faitRepartitionPoluant <-add_column(ED_faitRepartitionPoluant,id_fait=round(runif(nrow(ED_faitRepartitionPoluant), min=200, max=7000)))
ED_faitRepartitionPoluant <- select(ED_dimensionRadioActivite,id_dim_radio)%>% bind_cols(ED_faitRepartitionPoluant)
ED_faitRepartitionPoluant <- select(ED_dimensionProducteurDechet,id_dim_producteur)%>% bind_cols(ED_faitRepartitionPoluant)

test <-select(ED_dimensionGeo,INSEE_COM,id_dim_geo) %>% tbl_df()

test1 <- inner_join(test, data_dechet, by = c("INSEE_COM" = "CODE.INSEE")) %>% 
  drop_na() %>% distinct(INSEE_COM,NOM.DU.SITE,DESCRIPTION.PHYSIQUE,VOLUME.EQUIVALENT.CONDITIONNE, .keep_all = TRUE)


test2 <- inner_join(test1, ED_dimensionDechet, by = NULL, copy = FALSE) %>% 
  drop_na() %>% distinct(INSEE_COM,NOM.DU.SITE,DESCRIPTION.PHYSIQUE,VOLUME.EQUIVALENT.CONDITIONNE, .keep_all = TRUE)

test3 <- inner_join(test2, ED_faitRepartitionPoluant, by = c("id_dim_dechet"), copy = FALSE) %>% 
  drop_na() %>% distinct(INSEE_COM,NOM.DU.SITE,DESCRIPTION.PHYSIQUE,VOLUME.EQUIVALENT.CONDITIONNE, .keep_all = TRUE)

test4 <- select(test3,id_dim_dechet,id_dim_producteur, id_dim_radio, id_dim_geo) %>% 
  tbl_df() %>% add_column(id_fait=round(runif(5633, min=200, max=7000)))


#essai avec leaflet----
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



# Pour l'affichage d'une seule region:
dep_idf <- data_departement %>% filter(NOM_REG=='ILE-DE-FRANCE') 

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

dechet_IDF.coor <- dechet %>% select(lng,lat,NOM_REG) %>% filter(NOM_REG=="ILE-DE-FRANCE") %>% drop_na() %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% st_transform(crs = 2154)

plot(dechet_IDF.coor, pch = 20, add = TRUE, col="darkblue") 



#code pour les polygones sur shiny carte poluants
#addPolygons(data= data_departement.sf,color = "#444444", weight = 1, smoothFactor = 0.5,
#            opacity = 1.0, fillOpacity = 0.1,
#            highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                bringToFront = TRUE))%>%
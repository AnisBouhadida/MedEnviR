# ================================================================================
# Etudiants : * Anis BOUHADIDA (Universite Paris 13)
#             * Radouane ELAYACHY (Universite Paris Descarte)

# Fichier : * Importation et nettoyage des donnees d'effectif et des donnees geographiques  
#           * Affichage des données evenements sur la carte  
# ================================================================================
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

effectif_france <- read_delim("./data/effectif.france.csv", ";",na=c("","NA"), #encodage UTF-8 pour ces fichiers !!!
                              escape_double = FALSE, trim_ws = TRUE)

evenements <- read_delim("./data/evenements.csv", ";",na=c("","NA"),  #encodage UTF-8 pour ces fichiers !!!
                         escape_double = FALSE, trim_ws = TRUE)


effectif_departement <- read_delim("./data/effectif.departement.csv", ";",na=c("","NA"),  #encodage UTF-8 pour ces fichiers !!!
                                   escape_double = FALSE, trim_ws = TRUE)

data_departement <- st_read("./data/cartes/DEPARTEMENT.shp") %>% dplyr::select(ID_GEOFLA,CODE_DEPT,NOM_DEPT,NOM_REG,geometry,X_CENTROID, Y_CENTROID)
                                                                    #ajout des centroides pour stats

# Ajustement direct de toute la table selon effectif par departement et France entiere:
ratio_vector <- vector()
for (i in colnames(evenements[,-1])) {
  ratio_vector <- append(ratio_vector, 
                         round(ageadjust.direct(count = evenements[,i], 
                                                pop = effectif_departement[,i], 
                                                stdpop = effectif_france[,2])["adj.rate"] *10^5, 2))} 
standdirect_dep <- tibble(data_departement = colnames(evenements[,-1]), ratio = ratio_vector)

# jointure donnees carto et les taux standardises d'evenements par departement:
data_departement_ordre <- sort(data_departement$NOM_DEPT)
standdirect_dep <- standdirect_dep %>% arrange(data_departement) %>% mutate(data_departement = data_departement_ordre)

rm(data_departement_ordre)
data_departement <- left_join(data_departement, standdirect_dep, by = c('NOM_DEPT' = 'data_departement')) 

#création d'une table avec nombre observé (Observed) et nombre attendu (Expected) par département selon le ratio
#et l'effectif de chaque département
nbre.vector <- vector()
for (i in colnames(evenements[,-1])) {
  nbre.vector <- append(nbre.vector, sum(evenements[,i]))}
eff.vector<-vector()
for (i in colnames(effectif_departement[,-1])) {
  eff.vector <- append(eff.vector, sum(effectif_departement[,i]))}
a <-tibble(NOM_DEPT = colnames(evenements[,-1]), 
           Observed = nbre.vector, 
           effectif = eff.vector,
           Expected= ratio_vector*eff.vector/10^5)

#nomage identique des noms de département pour pouvoir faire la jointure correctement
nom_depback <- sort(data_departement$NOM_DEPT)
a$NOM_DEPT<-a$NOM_DEPT%>% iconv(from = "UTF-8", to="ASCII//TRANSLIT" ) %>%gsub(pattern = "\\W", "", .) %>% toupper()
data_departement$NOM_DEPT<-data_departement$NOM_DEPT %>% gsub(pattern = "\\W", "", .)
#jointure de la table créée avec les data département
data_departement <- right_join(data_departement, a, by = 'NOM_DEPT')
data_departement$NOM_DEPT=nom_depback #réafectation des noms d'origine
#spécification des colonnes x et y pour tests statistiques à partir des coordonées des centroides des départements
data_departement<-cbind(data_departement, x=data_departement$X_CENTROID, y=data_departement$Y_CENTROID)

#simplification des données que l'on utilise dans l'appli shiny pour améliorer performance
data_test <- ED_faitRepartitionPoluant %>% 
  left_join(ED_dimensionDechet) %>% 
  left_join(ED_dimensionProducteurDechet) %>%
  left_join(ED_dimensionGeo)%>%
  select(`VOLUME EQUIVALENT CONDITIONNE`,
         `ACTIVITE ( Bq)`,
         `GROUPE DE DECHETS`,
         `SOUS-GROUPE DE DECHETS`,
         `DESCRIPTION PHYSIQUE`,
         `FAMILLE IN`,
         `NOM DU SITE`,
         lat,
         lng,
         NOM_COM,
         NOM_DEPT,
         NOM_REG,
         classe_potentiel)
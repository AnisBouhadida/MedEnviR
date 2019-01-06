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


# Nous avons donc des évènements (maladie X) par age, sexe, et la localisation (DEPARTEMENT)
# et des déchets radioactifs selon leur localisation (X,Y)
# On recherche donc des agrégats de maladie autour d'un point source 
# --> TEST DE CONCENTRATION
# mais la localisation de la maladie est par département
library(sp)
library(DCluster)
nc$expect74=nc$BIR74*sum( nc$SID74 )/sum(nc$BIR74 )



nc$SMR74=nc$SID74/nc$expect74
brks=seq(0,5,1)
spplot(nc,"SMR74",at=brks,col.regions=grey.colors(5,start=0.9,end=0.1))

data(nc.sids)
sids<-data.frame(Observed=nc.sids$SID74)
sids<-cbind(sids, Expected=nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74))
sids<-cbind(sids, x=nc.sids$x, y=nc.sids$y)

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

#Compute Stones statistic around a location, 
#test de permutation qu'on utilise lorsque l'on ne peut pas 
#connaitre la distribution
#sous l'hypothèse nulle
region<- dechet %>% dplyr::filter(dechet$NOM_DEPT =="Oise")
stone.stat(dep, region=region, lambda=1)
stone.test(Observed=dep$nbre_observé.x~offset(log(dep$nbre_attendu.x)), as_data_frame(dep), model="poisson", R=99, #loi de poisson car données de compte par comté
           region=region, lambda=1)

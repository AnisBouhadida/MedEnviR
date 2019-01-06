# ================================================================================
# Etudiants : * Anis BOUHADIDA (Universite Paris 13)
#             * Radouane ELAYACHY (Universite Paris Descarte)

# Fichier : * Importation et nettoyage des donnees d'effectif et des donnees geographiques  
#           * Affichage des données evenements sur la carte  
# ================================================================================

effectif_france <- read_delim("./data/effectif.france.csv", ";", 
                              locale = locale(encoding = "ISO-8859-1"),na=c("","NA"), 
                              escape_double = FALSE, trim_ws = TRUE)

evenements <- read_delim("./data/evenements.csv", ";", 
                         locale = locale(encoding = "ISO-8859-1"),na=c("","NA"),
                         escape_double = FALSE, trim_ws = TRUE)

effectif_departement <- read_delim("./data/effectif.departement.csv", ";", 
                                   locale = locale(encoding = "ISO-8859-1"),na=c("","NA"),
                                   escape_double = FALSE, trim_ws = TRUE)

data_departement <- read_sf("data/cartes/DEPARTEMENT.shp") %>% select(ID_GEOFLA,CODE_DEPT,NOM_DEPT,NOM_REG,geometry)

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

rm(ratio_vector,i,data_departement_ordre)
data_departement <- left_join(data_departement, standdirect_dep, by = c('NOM_DEPT' = 'data_departement')) 
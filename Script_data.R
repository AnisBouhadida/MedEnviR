# ================================================================================
# Etudiants : * Anis BOUHADIDA (Universite Paris 13)
#             * Radouane ELAYACHY (Universite Paris Descarte)

# Fichier : * Importation et nettoyage des donnees initiales 
#           * Exportation de l'entrepot au format .csv  
# ================================================================================

# initialisation des packages:
library(tidyverse)

# Importation des donnees et Transformation en tibble:
  data_dechet <- read_delim(file = "./data/dechets-declares-au-31-12-2016.csv", ";",
                            locale = locale(encoding = "ISO-8859-1"),na=c("","NA"), 
                            escape_double = F, trim_ws = F) %>% tbl_df()
  
  data_INSEE <- read_delim(file = "./data/code-postal-code-insee-2015.csv", ";", 
                           locale = locale(encoding = "ISO-8859-1"),na=c("","NA"), 
                           escape_double = F, trim_ws = F) %>% tbl_df()
  
# Nettoyage des donnees:
  data_dechet_clean <- unique(data_dechet)
  data_INSEE_clean <- unique(data_INSEE)

# Extraction des variables interessantes et creation des dimensions:
  ED_dimensionGeo <-select(data_INSEE_clean,`Geo Point`,INSEE_COM,
                           NOM_COM,NOM_DEPT,NOM_REG,Code_postal)%>% tbl_df() %>% 
    rowid_to_column("id_dim_geo") %>% distinct(`Geo Point`,INSEE_COM,NOM_COM,.keep_all = TRUE) %>% 
    separate(`Geo Point`,c("lat","lng"),sep=",")
  
  ED_dimensionDechet <- select(data_dechet_clean,`GROUPE DE DECHETS`,
                              `SOUS-GROUPE DE DECHETS`,`DESCRIPTION PHYSIQUE`,
                               CATEGORIE,`FAMILLE IN`) %>% tbl_df() %>% rowid_to_column("id_dim_dechet")%>%
    distinct(`GROUPE DE DECHETS`,`SOUS-GROUPE DE DECHETS`,
             `DESCRIPTION PHYSIQUE`,CATEGORIE,`FAMILLE IN`,.keep_all = TRUE)
  
  ED_dimensionRadioActivite <- select(data_dechet_clean,`PRINCIPAUX RADIONUCLEIDES`) %>% 
    tbl_df() %>% rowid_to_column("id_dim_radio")%>%
    distinct(`PRINCIPAUX RADIONUCLEIDES`,.keep_all = TRUE)
  
  ED_dimensionProducteurDechet <-select(data_dechet_clean,`NOM DU SITE`) %>% 
    tbl_df() %>% 
    rowid_to_column("id_dim_producteur")%>%
    distinct(`NOM DU SITE`,.keep_all = TRUE)
  
  # Matching des id avec la table dechet:

  table_fait0 <- left_join(data_dechet_clean,ED_dimensionGeo, 
                           by= c("CODE INSEE"="INSEE_COM"))
  
  table_fait1 <- left_join(table_fait0,ED_dimensionDechet, 
                           by= c("GROUPE DE DECHETS","SOUS-GROUPE DE DECHETS",
                                 "DESCRIPTION PHYSIQUE","CATEGORIE","FAMILLE IN"))
  
  table_fait2 <- left_join(table_fait1,ED_dimensionRadioActivite, 
                           by= c("PRINCIPAUX RADIONUCLEIDES"))
  
  table_fait3 <- left_join(table_fait2,ED_dimensionProducteurDechet, 
                           by= c("NOM DU SITE"))
  
# Creation du fait :
  ED_faitRepartitionPoluant <- select(table_fait3,
                                      id_dim_geo,id_dim_dechet,
                                      id_dim_radio,id_dim_producteur,`VOLUME EQUIVALENT CONDITIONNE`,`ACTIVITE ( Bq)`,MAJORATION)
  ED_faitRepartitionPoluant <-add_column(ED_faitRepartitionPoluant,
                                         id_fait=runif(nrow(ED_faitRepartitionPoluant), 
                                                             min=200, max=7000))

# Exportation de l'entrepot de donnees : 
  if (!file.exists("EntrepotDeDonnees")){
    dir.create("EntrepotDeDonnees")
    }
  write.csv(ED_dimensionDechet,file = "./EntrepotDeDonnees/ED_dimensionDechet.csv", 
            row.names = FALSE)
  write.csv(ED_dimensionGeo,file = "./EntrepotDeDonnees/ED_dimensionGeo.csv", 
            row.names = FALSE)
  write.csv(ED_dimensionProducteurDechet,file = "./EntrepotDeDonnees/ED_dimensionProducteurDechet.csv", 
            row.names = FALSE)
  write.csv(ED_dimensionRadioActivite,file = "./EntrepotDeDonnees/ED_dimensionRadioActivite.csv", 
            row.names = FALSE)
  write.csv(ED_faitRepartitionPoluant,file = "./EntrepotDeDonnees/ED_faitRepartitionPoluant.csv", 
            row.names = FALSE)

# Nettoyage de l'environnement et des objets temporaires:
rm(data_dechet,data_INSEE)
rm(table_fait0,table_fait1,table_fait2,table_fait3)
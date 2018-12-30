library(tidyverse)

# Importation des donnees et Transformation en tibble pour un manipulation plus facile:
  # Donnees dechets poluants: 
  data_dechet <- read.csv(file = "./data/dechets-declares-au-31-12-2016.csv", 
                          header = TRUE, sep = ";", stringsAsFactors = FALSE ) %>% 
    tbl_df() %>% drop_na() 
  
  data_INSEE <- read.csv(file = "./data/code-postal-code-insee-2015.csv", 
                         header = TRUE, sep = ";", stringsAsFactors = FALSE ) %>% 
    tbl_df() %>% drop_na() 

# Extraction des variables et creation des dimensions:
  ED_dimensionGeo <-select(data_INSEE,
                           Geo.Point,INSEE_COM,NOM_COM,NOM_DEPT,NOM_REG,Code_postal) %>% 
    tbl_df() %>% add_column(id_dim_geo=round(runif(nrow(data_INSEE), min=1, max=40000)))%>%
    distinct(Geo.Point,INSEE_COM,.keep_all = TRUE)
  ###
  ED_dimensionDechet <-select(data_dechet,
                              GROUPE.DE.DECHETS,SOUS.GROUPE.DE.DECHETS,
                              DESCRIPTION.PHYSIQUE,CATEGORIE,FAMILLE.IN,
                              VOLUME.EQUIVALENT.CONDITIONNE) %>% tbl_df() %>%
    add_column(id_dim_dechet=round(runif(nrow(data_dechet), min=1, max=7000))) %>%
    distinct(GROUPE.DE.DECHETS,SOUS.GROUPE.DE.DECHETS,
             DESCRIPTION.PHYSIQUE,CATEGORIE,FAMILLE.IN,
             VOLUME.EQUIVALENT.CONDITIONNE,.keep_all = TRUE)
  ###
  ED_dimensionRadioActivite <- select(data_dechet,ACTIVITE...Bq.,
                                      MAJORATION,PRINCIPAUX.RADIONUCLEIDES) %>% 
    tbl_df() %>% add_column(id_dim_radio=round(runif(nrow(data_dechet), min=1, max=7000)))%>%
    distinct(ACTIVITE...Bq.,PRINCIPAUX.RADIONUCLEIDES,MAJORATION,.keep_all = TRUE)
  ###
  ED_dimensionProducteurDechet <-select(data_dechet,NOM.DU.SITE) %>% tbl_df() %>% 
    add_column(id_dim_producteur=round(runif(nrow(data_dechet), min=1, max=7000)))%>%
    distinct(NOM.DU.SITE,.keep_all = TRUE)

  
  
  
  
  
  
  
  
  
  
  
# Entrepot de donnees:
  
  ED_faitRepartitionPoluant <- data.frame() %>% tbl_df()
  ED_faitRepartitionPoluant <- select(ED_dimensionDechet,id_dim_dechet)
  ED_faitRepartitionPoluant <-add_column(ED_faitRepartitionPoluant,id_fait=round(runif(6226, min=200, max=7000)))
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
  
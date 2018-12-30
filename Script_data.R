library(tidyverse)

# Importation des donnees et Transformation en tibble pour un manipulation plus facile:
  # Donnees dechets poluants: 
  data_dechet <- read.csv(file = "./data/dechets-declares-au-31-12-2016.csv", 
                          header = TRUE, sep = ";", stringsAsFactors = FALSE )
  data_dechet <- tbl_df(data_dechet)
  # Donnees geographique:
  data_INSEE <- read.csv(file = "./data/code-postal-code-insee-2015.csv", 
                         header = TRUE, sep = ";", stringsAsFactors = FALSE )
  data_INSEE <- tbl_df(data_INSEE)
  
# Extraction des variables et creation des dimensions:
  ED_dimensionDechet <-select(data_dechet,GROUPE.DE.DECHETS,
         SOUS.GROUPE.DE.DECHETS,DESCRIPTION.PHYSIQUE,
         CATEGORIE,FAMILLE.IN,VOLUME.EQUIVALENT.CONDITIONNE) %>% tbl_df()
  ED_dimensionDechet <- add_column(ED_dimensionDechet,
                                   id_dim_dechet=round(runif(6226, min=1, max=7000)))
  ED_dimensionDechet  %>% drop_na()
  
  ###
  ED_dimensionRadioActivite <- select(data_dechet,ACTIVITE...Bq.,
                                      MAJORATION,PRINCIPAUX.RADIONUCLEIDES) %>% tbl_df()
  ED_dimensionRadioActivite <- add_column(ED_dimensionRadioActivite,
                                          id_dim_radio=round(runif(6226, min=1, max=7000)))
  
  ###
  ED_dimensionProducteurDechet <-select(data_dechet,NOM.DU.SITE) %>% tbl_df()
  ED_dimensionProducteurDechet <- add_column(ED_dimensionProducteurDechet,
                                             id_dim_producteur=round(runif(6226, min=1, max=7000)))
  
  
  ###
  ED_dimensionGeo <-select(data_INSEE,Geo.Point,INSEE_COM,NOM_COM,NOM_DEPT,NOM_REG,Code_postal) %>% tbl_df()
  ED_dimensionGeo <- add_column(ED_dimensionGeo,id_dim_geo=round(runif(39711, min=1, max=40000)))
  
  
# Entrepot de donnees:
  
  ED_faitRepartitionPoluant <- data.frame() %>% tbl_df()
  ED_faitRepartitionPoluant <- select(ED_dimensionDechet,id_dim_dechet)
  ED_faitRepartitionPoluant <-add_column(ED_faitRepartitionPoluant,id_fait=round(runif(6226, min=200, max=7000)))
  ED_faitRepartitionPoluant <- select(ED_dimensionRadioActivite,id_dim_radio)%>% bind_cols(ED_faitRepartitionPoluant,)
  ED_faitRepartitionPoluant <- select(ED_dimensionProducteurDechet,id_dim_producteur)%>% bind_cols(ED_faitRepartitionPoluant,)

  test <-select(ED_dimensionGeo,INSEE_COM,id_dim_geo) %>% tbl_df()
  
  test1 <- left_join(test, data_dechet, by = c("INSEE_COM" = "CODE.INSEE"))
  test1 <- drop_na(test1)
  test2 <- inner_join(test1, ED_dimensionDechet, by = NULL, copy = FALSE)
  test2 <- drop_na(test2)

  ?left_join  
  
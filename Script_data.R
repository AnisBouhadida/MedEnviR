# ================================================================================
# Etudiants : * Anis BOUHADIDA (Universite Paris 13)
#             * Radouane ELAYACHY (Universite Paris Descarte)

# Fichier : * Importation et nettoyage des donnees initiales 
#           * Exportation de l'entrepot au format .csv  
# ================================================================================

# initialisation des packages:
library(tidyverse)
library(naniar)

# Importation des donnees et Transformation en tibble:
  data_dechet <- read_delim(file = "./data/dechets-declares-au-31-12-2016.csv", ";",
                            locale = locale(encoding = "ISO-8859-1"),na=c("","NA"), 
                            escape_double = F, trim_ws = F) %>% tbl_df()
  
  data_INSEE <- read_delim(file = "./data/code-postal-code-insee-2015.csv", ";", 
                           locale = locale(encoding = "ISO-8859-1"),na=c("","NA"), 
                           escape_double = F, trim_ws = F) %>% tbl_df()
  
  data_radon <- read_delim(file = "./data/radon.csv", ";", 
                           locale = locale(encoding = "ISO-8859-1"),na=c("","NA"), 
                           escape_double = F, trim_ws = F) %>% tbl_df()
  
# Nettoyage des donnees:
  data_dechet_clean <- unique(data_dechet)
  data_INSEE_clean <- unique(data_INSEE)
  data_radon_clean <- unique(data_radon)
  
  # Transformation des codes INSEE: 
  for (i in 1:nrow(data_dechet_clean))
    { 
    #DOM/TOM sauf GUYANNE:
      if(str_length(data_dechet_clean$`CODE INSEE`[i] )>5){
          data_dechet_clean$`CODE INSEE`[i]<- data_dechet_clean$`CODE INSEE`[i] %>% str_replace("0","")}
    
    #PARIS-LYON-MARSEILLE:
      data_dechet_clean$DEPARTEMENT[i]<- as.character(data_dechet_clean$DEPARTEMENT[i])
      
      switch (data_dechet_clean$DEPARTEMENT[i],
              "75" = {
                num_arr <- data_dechet_clean$VILLE[i] %>% str_extract_all("\\d") %>% 
                  unlist() %>% paste(collapse='')
                data_dechet_clean$`CODE INSEE`[i]<- data_dechet_clean$`CODE INSEE`[i] %>% 
                  str_replace("056","") %>% 
                  paste("1",sep = "")%>% 
                  paste(num_arr,sep = "")
              },
              "69" = {
                data_dechet_clean$`CODE INSEE`[i]<- data_dechet_clean$`CODE INSEE`[i] %>% 
                  str_replace("69123","69381")
              },
              "13" = {
                data_dechet_clean$`CODE INSEE`[i]<- data_dechet_clean$`CODE INSEE`[i] %>% 
                  str_replace("13055","13201")
              })
  }

# Extraction des variables interessantes et creation des dimensions:
  Radon_cas <- select(data_radon_clean, insee_com , classe_potentiel)%>% 
    tbl_df() %>%unique()
  
  ED_dimensionGeo <-select(data_INSEE_clean,`Geo Point`,INSEE_COM,
                           NOM_COM,NOM_DEPT,NOM_REG,Code_postal, CODE_DEPT)%>% tbl_df() %>% 
                           rowid_to_column("id_dim_geo") %>% 
                           distinct(`Geo Point`,INSEE_COM,NOM_COM,.keep_all = TRUE) %>% 
                           separate(`Geo Point`,c("lat","lng"),sep=",") %>%
                           left_join(Radon_cas,by=c("INSEE_COM"="insee_com"))
  
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
                           by=c("CODE INSEE"="INSEE_COM"))
  
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
                                      id_dim_radio,id_dim_producteur,
                                      `VOLUME EQUIVALENT CONDITIONNE`,
                                      `ACTIVITE ( Bq)`) %>% 
                               drop_na(id_dim_geo) %>%
                               rowid_to_column("id_fait") %>% 
                               replace_with_na(replace = list(`VOLUME EQUIVALENT CONDITIONNE`="-",
                                                              `ACTIVITE ( Bq)`="-"))
  
# Exportation de l'entrepot de donnees : 
  if (!file.exists("EntrepotDeDonnees")){dir.create("EntrepotDeDonnees")}
  
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
rm(data_dechet,data_INSEE,data_radon)
rm(Radon_cas,table_fait0,table_fait1,table_fait2,table_fait3,i,num_arr)

save(ED_dimensionDechet, ED_dimensionGeo, ED_dimensionProducteurDechet,
     ED_dimensionRadioActivite, ED_faitRepartitionPoluant, file = "fichier.RData")
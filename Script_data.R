# Importation des donnees :
  data_dechet <- read.csv(file = "./data/dechets-declares-au-31-12-2016.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE )
  data_INSEE <- read.csv(file = "./data/code-postal-code-insee-2015.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE )

# Entrepot de donnees:
  ED_faitRepartitionPoluant <- data.frame(
    setNames(
      replicate(5,numeric(0), simplify = F),
      c("id_fait","id_dim_dechet","id_dim_radio","id_dim_geo","id_dim_prod")))
  
  ED_dimensionDechet <- data.frame(
    setNames(
    replicate(7,numeric(0), simplify = F),
    c("id_dim_dechet","groupe_dechet","s_groupe_dechet",
      "descript_dechet","categorie_dechet","famille_dechet",
      "volume_dechet")))
  
  ED_dimensionRadioActivite <- data.frame(
    setNames(
    replicate(4,numeric(0), simplify = F),
    c("id_dim_radio","activite","majoration","p_radioNUC")))
  
  ED_dimensionProducteurDechet <- data.frame(
    setNames(
    replicate(2,numeric(0), simplify = F),
    c("id_dim_prod","nom_prod")))
  
  ED_dimensionGeo <- data.frame(
    setNames(
    replicate(5,numeric(0), simplify = F),
    c("id_dim_geo","geo_point","nom_dept","nom_region","nom_commune")))
  
  id_dechet <- 001
  
for (row in 1:nrow(data_dechet)) {
  for (roww in 1:nrow(data_INSEE)) {
    if(!is.null(data_dechet[row]$CODE.INSEE) && 
       !is.null(data_INSEE[roww]$INSEE_COM) && 
       data_dechet[row]$CODE.INSEE == data_INSEE[roww]$INSEE_COM){
      rbind(ED_dimensionDechet,list(id_dechet,
                                    data_dechet[row]$GROUPE.DE.DECHETS ,
                                    data_dechet[row]$SOUS.GROUPE.DECHETS,
                                    data_dechet[row]$DESCRIPTION.PHYSIQUE,
                                    data_dechet[row]$CATEGORIE,
                                    data_dechet[row]$FAMILLE.IN,
                                    data_dechet[row]$VOLUME.EQUIVALENT.CONDINIONNE))
      id_dechet <- id_dechet + 1
    }
  }
}


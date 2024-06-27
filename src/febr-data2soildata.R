# title: Load and publish FEBR soil data to the SoilData Dataverse instance
# author: Alessandro Samuel-Rosa
# description: This script loads datasets from the FEBR repository and publishes them to the
# SoilData Dataverse instance.

rm(list = ls())

# Load required packages, install if necessary
# - dataverse: provides an R interface to the Dataverse API
# - data.table: provides an R interface to the data.table package
if (!require("dataverse")) {
  install.packages("dataverse")
}
if (!require("data.table")) {
  install.packages("data.table")
}

# Set the environment variable DATAVERSE_SERVER
Sys.setenv("DATAVERSE_SERVER" = "https://soildata.mapbiomas.org")

# Get the contents of the SoilData Dataverse instance
soildata_contents <- dataverse::dataverse_contents(dataverse = "soildata")

# Get metadata for all datasets in the SoilData Dataverse instance
# We want to know if there is a field named 'otherId' in the metadata of the datasets. If there is,
# we want to know the value of this field. We also want to know if the dataset has any files.
# The 'otherId' field is used to store the FEBR dataset identifier. If the dataset has files, it is
# already published in the SoilData Dataverse instance. We will use this information to check which 
# datasets in the FEBR index are already published in the SoilData Dataverse instance.
soildata_metadata <- lapply(soildata_contents, function(x) {
  dts <- dataverse::get_dataset(x, dataverse = "soildata")
  mtd <- dts$metadataBlocks$citation$fields
  idx <- mtd[["typeName"]] == "otherId"
  if (any(idx)) {
    ctb <- mtd[idx, "value"][[1]][["otherIdValue"]][["value"]]
  } else {
    ctb <- NA_character_
  }
  fls <- dts$files
  if (length(fls) > 0) {
    has_files <- TRUE
  } else {
    has_files <- FALSE
  }
  return(data.frame(ctb = ctb, has_files = has_files))
})
soildata_metadata <- data.table::rbindlist(soildata_metadata)

# Filter out records with NA in the 'ctb' field and records with 'has_files' equal to TRUE
soildata_metadata <- soildata_metadata[!is.na(ctb) & !has_files]

# Read FEBR index
febr_index <- data.table::fread("/home/alessandro/ownCloud/febr-repo/publico/febr-indice.txt")

# Filter out the FEBR records that are not in the SoilData Dataverse instance. Use the dados_id
# field to match the records. Keep only the fields dados_id e dados_acesso.
febr_index <- febr_index[dados_id %in% soildata_metadata$ctb, .(dados_id, dados_acesso)]

# title: Load and publish FEBR soil data to the SoilData Dataverse instance
# author: Alessandro Samuel-Rosa
# description: This script loads datasets from the FEBR repository and publishes them to the
# SoilData Dataverse instance.

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
dtv_contents <- dataverse::dataverse_contents(dataverse = "soildata")

# Get metadata for all datasets in the SoilData Dataverse instance
# The metadata is stored in the 'fields' field of the returned list. The 'fields' field is a data.frame with 19 observations of  4 variables:
# - typeName (chr): the field names
# - multiple (logi): whether the field can have multiple values
# - typeClass (chr): the field type
# - value (List of 19): the field value
# Our field of interest is named 'otherId'
dtv_metadata <- lapply(dtv_contents[1:2], function(x) {
  mtd <- dataverse::dataset_metadata(dataset = x, dataverse = "soildata", block = "citation")$fields
  idx <- mtd[["typeName"]] == "otherId"
  if (any(idx)) {
    ctb <- mtd[idx, "value"][[1]][["otherIdValue"]][["value"]]
  } else {
    ctb <- NA_character_
  }
  return(ctb)
})


# Now access the SoilData Dataverse instance and get the contents. We need to check which of the
# datasets in the FEBR index are already published in the SoilData Dataverse instance. The
# information is stored in the "dados_id" column of the FEBR index file. If a dataset is already
# published, we will identify it in the field 'Other Identifier' with the pattern
# `febr:<dados_id>`.

# Read FEBR index
febr_index <- data.table::fread("/home/alessandro/ownCloud/febr-repo/publico/febr-indice.txt")
febr_index[, dados_id]
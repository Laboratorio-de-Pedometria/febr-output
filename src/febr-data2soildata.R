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
  return(data.frame(ctb = ctb, has_files = has_files, doi = x[["persistentUrl"]]))
})
soildata_metadata <- data.table::rbindlist(soildata_metadata)

# Check if there are any duplicated records in the SoilData metadata
soildata_metadata[!is.na(ctb), ][duplicated(ctb), ]

# Read FEBR index
febr_index <- data.table::fread("/home/alessandro/ownCloud/febr-repo/publico/febr-indice.txt")

# Check if there are any duplicated records in the FEBR index
febr_index[duplicated(dados_id), .(dados_id, dados_acesso)]

# Check if any FEBR datasets (dados_id) is not published in SoilData (ctb)
febr_index[!dados_id %in% soildata_metadata$ctb, .(dados_id, dados_acesso)]

# Check if any SoilData datasets (ctb) is not in the FEBR index
soildata_metadata[!is.na(ctb), ][!ctb %in% febr_index$dados_id, .(ctb)]

# Filter out the records with NA in the 'ctb' field. These records are not included in FEBR.
# Filter out the records with 'has_files' equal to TRUE. These records are already published in the
# SoilData Dataverse instance.
soildata_metadata <- soildata_metadata[!is.na(ctb) & !has_files]

# Loop over soildata_metadata ctb values and publish their files to SoilData
febr_dir <- "/home/alessandro/ownCloud/febr-repo/publico/"
febr_filenames <- c("full", "camada", "identificacao", "metadado", "versionamento", "observacao")
dts_description <- list(
  camada = "Dados químicos e físicos das amostras obtidas de camadas e horizontes do solo",
  identificacao = "Dados de identificação dos autores e dos dados como um todo",
  metadado = "Descrição dos métodos utilizados em campo e laboratório para obtenção dos dados de solo",
  versionamento = "Dados do histórico de modificação dos dados",
  observacao = "Dados ambientais e de referência espacial e temporal dos pontos de observação e amostragem do solo"
)
for (i in soildata_metadata[1, ctb]) {
  for (j in febr_filenames) {
    if (j == "full") {
      file <- paste0(febr_dir, i, "/", i, ".xlsx")
      description <- "Planilha com os arquivos TXT individuais incluídos como abas para compartilhamento facilitado"
    } else {
      file <- paste0(febr_dir, i, "/", i, "-", j, ".txt")
      description <- dts_description[[j]]
    }
    cat("Adding file", file, "to dataset", soildata_metadata[ctb == i, doi], "\n")
    cat("Description:", description, "\n")
    dataverse::add_dataset_file(
      file = file,
      dataset = soildata_metadata[ctb == i, doi]
      description = description
    )
  }
  dataverse::publish_dataset(
    dataset = soildata_metadata[ctb == i, doi],
    minor = FALSE
  )
}

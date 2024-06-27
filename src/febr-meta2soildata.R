# title: Load and publish FEBR metadata to the SoilData Dataverse instance
# author: Alessandro Samuel-Rosa
# description: This script loads datasets from the FEBR repository and publishes them to the
# SoilData Dataverse instance. The script reads the FEBR index file, which contains metadata
# for each dataset, and uses a template JSON file to create the metadata for the datasets.
# The script then iterates over the datasets, updating the metadata in the JSON file and
# publishing the dataset to the SoilData Dataverse instance using the Dataverse API.
# note: The script DOES NOT publish the actual data files, only the metadata.

# Install and load required packages
if (!require("dataverse")) {
  install.packages("dataverse")
}
if (!require("RJSONIO")) {
  install.packages("RJSONIO")
}

# # Set environment variable DATAVERSE_SERVER
# Sys.setenv("DATAVERSE_SERVER" = "https://solo.mapbiomas.org")

# dataverse::get_dataverse(dataverse = "soildata")
# dtv <- dataverse::dataverse_contents(dataverse = "soildata")

# # Get metadata for a dataset
# dnos <- dataverse::get_dataset(
#   dataset = "https://doi.org/10.60502/SoilData/RCYUYZ",
#   dataverse = "soildata"
# )
# str(dnos$license)

# Read FEBR index
index <- data.table::fread("/home/alessandro/ownCloud/febr-repo/publico/febr-indice.txt")
index <- index[-3, ] # remove datasets already published
index[, dados_licenca := gsub(" Atribuição-NãoComercial", "", index[, dados_licenca])]
index[, dados_licenca := gsub(" Atribuição", "", index[, dados_licenca])]
index[, dados_licenca := gsub("-CompartilhaIgual", "", index[, dados_licenca])]
index[
  dados_licenca == "CC BY 4.0",
  dados_licenca_uri := "http://creativecommons.org/licenses/by/4.0"
]
index[
  dados_licenca == "CC BY-NC 4.0",
  dados_licenca_uri := "http://creativecommons.org/licenses/by-nc/4.0/"
]
index[
  dados_licenca == "CC BY-NC-SA 4.0",
  dados_licenca_uri := "http://creativecommons.org/licenses/by-nc-sa/4.0/"
]

# Read template JSON file with metadata
meta <- RJSONIO::fromJSON("febr-output/src/dataset-create-new-all-default-fields.json")

i <- 250
i <- i-1
# for (i in seq_len(nrow(index))) {
  meta$datasetVersion$license[1] <- index[i, dados_licenca]
  meta$datasetVersion$license[2] <- index[i, dados_licenca_uri]
  meta$datasetVersion$metadataBlocks$citation$fields[[1]]$value <- index[i, dados_titulo]
  meta$datasetVersion$metadataBlocks$citation$fields[[2]]$value <- index[i, dados_acesso]
  meta$datasetVersion$metadataBlocks$citation$fields[[3]]$value[[1]]$otherIdValue$value <-
    index[i, dados_id]
  authors <- strsplit(index[i, dados_autor], "; ")[[1]]
  meta$datasetVersion$metadataBlocks$citation$fields[[4]]$value <-
    meta$datasetVersion$metadataBlocks$citation$fields[[4]]$value[rep(1, length(authors))]
  for (k in seq_along(authors)) {
    meta$datasetVersion$metadataBlocks$citation$fields[[4]]$value[[k]]$authorName$value <-
      authors[k]
    if (k == 1) {
      if (is.na(index[i, organizacao_nome])) {
        index[i, organizacao_nome := ""]
      }
      index[i, organizacao_nome := gsub(" (EMBRAPA)$", "", index[i, organizacao_nome])]
      index[i, organizacao_nome := gsub("^Embrapa$", "Empresa Brasileira de Pesquisa Agropecuária",
        index[i, organizacao_nome],
        perl = TRUE
      )]
      index[i, organizacao_nome := gsub(
        "EMBRAPA SOLOS", "Embrapa Solos",
        index[i, organizacao_nome]
      )]
      index[i, organizacao_nome := gsub(" (IBGE)$", "", index[i, organizacao_nome])]
      meta$datasetVersion$metadataBlocks$citation$fields[[4]]$value[[k]]$authorAffiliation$value <-
        index[i, organizacao_nome]
    } else {
      meta$datasetVersion$metadataBlocks$citation$fields[[4]]$value[[k]]$authorAffiliation$value <-
        ""
    }
  }
  # datasetContact
  # meta$datasetVersion$metadataBlocks$citation$fields[[5]]
  # dsDescription
  if (is.na(index[i, dados_descricao])) {
    index[i, dados_descricao := index[i, dados_titulo]]
  }
  meta$datasetVersion$metadataBlocks$citation$fields[[6]]$value[[1]]$dsDescriptionValue$value <-
    index[i, dados_descricao]
  # subject
  # meta$datasetVersion$metadataBlocks$citation$fields[[7]]
  # keyword
  keywords <- strsplit(index[i, palavras_chave], "; ")[[1]]
  if (is.na(keywords[1])) {
    keywords <- ""
  }
  meta$datasetVersion$metadataBlocks$citation$fields[[8]]$value <-
    meta$datasetVersion$metadataBlocks$citation$fields[[8]]$value[rep(1, length(keywords))]
  for (k in seq_along(keywords)) {
    meta$datasetVersion$metadataBlocks$citation$fields[[8]]$value[[k]]$keywordValue$value <-
      keywords[k]
  }
  # topicClassification
  if (is.na(index[i, area_conhecimento])) {
    index[i, area_conhecimento := ""]
  }
  meta$datasetVersion$metadataBlocks$citation$fields[[9]]$value[[1]]$topicClassValue$value <-
    index[i, area_conhecimento]
  # publication
  publication <- strsplit(index[i, dados_publicacao], "; ")[[1]]
  if (is.na(publication[1])) {
    publication <- ""
  }
  meta$datasetVersion$metadataBlocks$citation$fields[[10]]$value <-
    meta$datasetVersion$metadataBlocks$citation$fields[[10]]$value[rep(1, length(publication))]
  for (k in seq_along(publication)) {
    meta$datasetVersion$metadataBlocks$citation$fields[[10]]$value[[k]]$publicationCitation$value <-
      publication[k]
  }
  # language
  # meta$datasetVersion$metadataBlocks$citation$fields[[11]] 

  # grantNumber
  if (is.na(index[i, fonte_financeira])) {
    index[i, fonte_financeira := ""]
  }
  meta$datasetVersion$metadataBlocks$citation$fields[[12]]$value[[1]]$grantNumberAgency$value <-
    index[i, fonte_financeira]
  
  # depositor
  # meta$datasetVersion$metadataBlocks$citation$fields[[13]]

  # otherGeographicCoverage
  if (is.na(index[i, extensao_espacial])) {
    index[i, extensao_espacial := ""]
  }
  meta$datasetVersion$metadataBlocks$geospatial$fields[[1]]$value[[1]]$otherGeographicCoverage$value <-
    index[i, extensao_espacial]

  # write JSON file with metadata
  writeLines(RJSONIO::toJSON(meta), paste0("febr-output/res/", index[i, dados_id], ".json"))
# }


# Set environmental variables
API_TOKEN <- " "
PARENT <- "soildata"
SERVER_URL <- "https://solo.mapbiomas.org"
FILE <- paste0("febr-output/res/", index[i, dados_id], ".json")

# Build curl command to create dataset
cmd <- paste0(
  "curl -H 'X-Dataverse-key:", API_TOKEN,
  "' -X POST '", SERVER_URL, "/api/dataverses/", PARENT,
  "/datasets' --upload-file '", FILE, "' -H 'Content-type:application/json'"
)
# Run curl command
system(cmd)

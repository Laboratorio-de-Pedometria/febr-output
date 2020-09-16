# title: Repositório Brasileiro Livre para Dados Abertos do Solo
# subtitle: Esquema de Metadados Dataverse v4.x
# author: Alessandro Samuel-Rosa
# source: https://github.com/GlobalDataverseCommunityConsortium/dataverse-language-packs

# citation ####

# descarregar e processar arquivo citation_br.properties
url <- 
  'https://raw.githubusercontent.com/GlobalDataverseCommunityConsortium/dataverse-language-packs/develop/pt_BR/citation_br.properties'
cite_prop <- readLines(url)[-(1:2)]
cite_prop <- lapply(cite_prop, function (x) {
  strsplit(x, split = '.', fixed = TRUE)[[1]][1:3]
})
cite_prop <- do.call(rbind, cite_prop)
tmp <- lapply(cite_prop[, 3], function (x) {
  strsplit(x, split = '=', fixed = TRUE)[[1]][1:2]
})
tmp <- do.call(rbind, tmp)
cite_prop <- cbind(cite_prop[, 1:2], tmp)
cite_prop <- cite_prop[cite_prop[, 1] == 'datasetfieldtype', -1]
cite_prop <- cbind(
  cite_prop[cite_prop[, 2] == 'title', c(1, 3)],
  cite_prop[cite_prop[, 2] == 'description', 3],
  cite_prop[cite_prop[, 2] == 'watermark', 3])
colnames(cite_prop) <- c('name', 'title', 'description', 'watermark')
cite_prop[, 'watermark'] <- sub(' [en]', '[en]', cite_prop[, 'watermark'], fixed = TRUE)

# descarregar e processar arquivo `metadatablocks/citation.tsv`
url <-
  'https://raw.githubusercontent.com/GlobalDataverseCommunityConsortium/dataverse/develop/scripts/api/data/metadatablocks/citation.tsv'
cite <- readLines(url)[-(1:2)]
cite[1] <- sub('#datasetField', '', cite[1], fixed = TRUE)
cite <- strsplit(cite[1:(nrow(cite_prop) + 1)], split = '\t')
cite <- do.call(rbind, cite)
colnames(cite) <- cite[1, ]
cite <- cite[-1, ]

# fundir dados
citation <- cbind(cite_prop, cite[, 6:ncol(cite)])

# geospatial ####

# descarregar e processar arquivo geospatial_br.properties
url <-
  'https://raw.githubusercontent.com/GlobalDataverseCommunityConsortium/dataverse-language-packs/develop/pt_BR/geospatial_br.properties'
geo_prop <- readLines(url)[-(1:2)]
geo_prop <- lapply(geo_prop, function (x) {
  strsplit(x, split = '.', fixed = TRUE)[[1]][1:3]
})
geo_prop <- do.call(rbind, geo_prop)
tmp <- lapply(geo_prop[, 3], function (x) {
  strsplit(x, split = '=', fixed = TRUE)[[1]][1:2]
})
tmp <- do.call(rbind, tmp)
geo_prop <- cbind(geo_prop[, 1:2], tmp)
geo_prop <- geo_prop[geo_prop[, 1] == 'datasetfieldtype', -1]
geo_prop <- cbind(
  geo_prop[geo_prop[, 2] == 'title', c(1, 3)],
  geo_prop[geo_prop[, 2] == 'description', 3],
  geo_prop[geo_prop[, 2] == 'watermark', 3])
colnames(geo_prop) <- c('name', 'title', 'description', 'watermark')
geo_prop[, 'watermark'] <- sub(' [en]', '[en]', geo_prop[, 'watermark'], fixed = TRUE)

# descarregar e processar arquivo `metadatablocks/geospatial.tsv`
url <-
  'https://raw.githubusercontent.com/GlobalDataverseCommunityConsortium/dataverse/develop/scripts/api/data/metadatablocks/geospatial.tsv'
geo <- readLines(url)[-(1:2)]
geo[1] <- sub('#datasetField', '', geo[1], fixed = TRUE)
geo <- strsplit(geo[1:(nrow(geo_prop) + 1)], split = '\t')
geo <- do.call(rbind, geo)
colnames(geo) <- geo[1, ]
geo <- geo[-1, ]

# fundir dados
geospatial <- cbind(geo_prop, geo[, 6:ncol(geo)], termURI = NA)

# metadatablock ####
metadatablock <- as.data.frame(rbind(citation, geospatial), stringsAsFactors = FALSE)
metadatablock <- metadatablock[, c(15, 1:2, 14, 3:13, 16)]
colnames(metadatablock) <- sub(' ', '', colnames(metadatablock))

# parent
idx <- match(metadatablock$parent, metadatablock$name)

# title
metadatablock$title <- paste0(metadatablock$title[idx], ": ", metadatablock$title)
metadatablock$title <- sub('NA: ', '', metadatablock$title)

# description
metadatablock$description <- paste0(metadatablock$description[idx], ". ", metadatablock$description)
metadatablock$description <- sub('NA. ', '', metadatablock$description)

# watermark
metadatablock$watermark <- sub('[en]', '', metadatablock$watermark, fixed = TRUE)

# parent
metadatablock <- metadatablock[-na.exclude(idx), ]

# schema
metadatablock <- cbind(schema = 'Dataverse v4.x', metadatablock)

# DataCite ####
# Documentação disponível em 
# https://github.com/datacite/schema/blob/master/source/meta/kernel-4.3/metadata.xsd
# https://schema.datacite.org/meta/kernel-4.3/doc/DataCite-MetadataKernel_v4.3.pdf
url <- 
  'https://raw.githubusercontent.com/datacite/schema/master/source/json/kernel-4.3/datacite_4.3_schema.json'
datacite <- jsonlite::read_json(path = url, simplifyVector = TRUE)
datacite <- datacite$properties[c("version", "rightsList")]
datacite <- data.frame(
  schema = 'DataCite 4.3',
  metadatablock_id = 'file',
  name = c(names(datacite)[1], names(datacite$rightsList$items$properties)),
  parent = c(rep('', 2), rep(names(datacite)[2], 5))
)[-7, ]
datacite$title <- c(
  'Número da versão',
  'Direitos: Licença',
  'Direitos: URL da Licença',
  'Direitos: Identificador da Licença',
  'Direitos: Esquema do Identificador',
  'Direitos: URL do Esquema'
)
datacite$description <- c(
  'Version number of the dataset. If the primary dataset has changed the version number increases. Suggested practice: track major_version.minor_version.',
  'Free text. Any rights information for this dataset. Provide a rights management statement for the resource or reference a service providing such information. Use the complete title of a license and include version information if applicable. Example: Creative Commons Attribution 3.0 Germany License',
  'The URI of the license. Example: http://creativecommons.org/licenses/by/3.0/de/deed.en.',
  'A short, standardized version of the license name. Example: CC-BY-3.0Note: It’s suggested to use the identifiers from the SPDX licence list (https://spdx.org/licenses/).',
  'The name of the scheme. Example: SPDX',
  'The URI of the rightsIdentifierScheme. Example: https://spdx.org/licenses/'
)
datacite$watermark <- rep(c('', '', 'Digite a URL completa, começando com http://'), 2)
datacite$fieldType <- rep(c('text', 'text', 'url'), 2)
datacite$required <- TRUE
datacite$allowmultiples <- FALSE
miss <- colnames(metadatablock)[!(colnames(metadatablock) %in% colnames(datacite))]
plus <- matrix(NA, nrow = nrow(datacite), ncol = length(miss))
colnames(plus) <- miss
datacite <- cbind(datacite, plus)
datacite <- datacite[, match(colnames(datacite), colnames(metadatablock))]

# fundir
metadatablock <- rbind(metadatablock, datacite)
metadatablock$displayOrder <- 1:nrow(metadatablock)
rownames(metadatablock) <- NULL

# salvar dados
write.table(
  x = metadatablock[, c('schema', "metadatablock_id", "parent", "name", "title", "watermark") ], 
  file = 'tmp.txt', sep = '\t', row.names = FALSE)

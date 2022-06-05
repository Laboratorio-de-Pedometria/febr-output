# title: Instantâneo de Junho de 2022 - eventos
# subtitle: Repositório de Dados do Solo Brasileiro
# author: Alessandro Samuel-Rosa
# 
# Documentos importantes:
# - FEBR Dicionário de Dados v2: https://goo.gl/hi77sB
# 
# Instalar última versão do pacote febr diretamente o GitHub
if (!require(remotes)) {
  install.packages(pkgs = "remotes")
}
remotes::install_github(repo = "laboratorio-de-pedometria/febr-package")
library(febr)
febr_repo <- "~/ownCloud/febr-repo/publico"
#
## 2022-06-eventos.txt #############################################################################
snapshot <- "2022-06"
# Campos exportados da tabela 'observacao':
eventos_cols <- c(
  "dados_id",
  "evento_id_febr",
  # Identificador único exclusivo do evento nos dados publicados no Repositório de Dados do Solo
  # Brasileiro.
  "evento_id_sisb",
  # Identificador único exclusivo do evento nos dados publicados no Sistema de Informações de Solos
  # Brasileiros, mantido pela Embrapa Solos e Embrapa Informática Agropecuária e acessível via
  # https://www.sisolos.cnptia.embrapa.br/.
  "evento_id_ibge",
  # Identificador único exclusivo do evento nos dados publicados no banco de dados da Diretoria de
  # Geociências do Instituto Brasileiro de Geografia e Estatística, acessível via
  # https://downloads.ibge.gov.br/downloads_geociencias.htm.
  "data_coleta_ano",
  # Ano do calendário gregoriano em que o evento foi coletado.
  "coord_longitude_grau",
  # Longitude (grau) do par de coordenadas espaciais onde o evento foi coletado.
  "coord_latitude_grau",
  # Latitude (grau) do par de coordenadas espaciais onde o evento foi coletado.
  "coord_datum_epsg",
  # Código do sistema de referência de coordenadas (SRC) espaciais utilizado para georreferenciar o
  # evento, conforme estabelecido pelo European Petroleum Survey Group (EPSG), acessível via
  # https://www.epsg.org.
  "coord_precisao",
  # Precisão com que o par de coordenadas espaciais do evento foram determinadas.
  "coord_fonte",
  # Fonte ou método de obtenção do par de coordenadas espaciais do evento.
  "coord_estado_sigla",
  # Sigla da unidade da federação brasileira onde a observação do solo foi realizada.
  "coord_municipio_nome",
  # Nome completo do município (na data de coleta) onde o evento foi coletado.
  "subamostra_quanti",
  # Quantidade de pontos amostrais que compõem o evento.
  "amostra_area",
  # Área superficial utilizada para amostragem do solo.
  "sibcs_20xx_ordem",
  # Classificação do perfil de solo no primeiro nível categórico (ordem) conforme edição do Sistema
  # Brasileiro de Classificação do Solo (SiBCS) posterior ao ano 2000.
  "terra_uso_descricao",
  # Descrição textual das formas de utilização humana da terra no local e imediações do evento.
  "fito_primaria_descricao",
  # Descrição da vegetação primária (vegetação natural de máxima expressão local) do local do
  # evento.
  "sitio_descricao"
  # Descrição textual do sítio do evento.
)
# Carregar pacotes necessários
if (!require(sf)) {
  install.packages(pkgs = "sf", dependencies = TRUE)
}
# Descarregar dados utilizando o nível 3 de harmonização
# vars <- c("evento_id", "sitio_descricao", "taxon_", "terra_", "fito_", "observacao_autoria")
vars <- c("evento_id", "sitio_descricao", "sibcs_", "terra_", "fito_", "observacao_autoria")
observacao <- febr::observation(
  data.set = "all",
  variable = vars,
  stack = TRUE,
  standardization = list(
    crs = "EPSG:4674", # SIRGAS 2000
    # time.format = "%Y-%m-%d",
    # time.format = "%d-%m-%Y",
    units = FALSE, round = FALSE  # somente variáveis categóricas sendo processadas
  ),
  harmonization = list(harmonize = TRUE, level = 3),
  febr.repo = febr_repo,
  verbose = TRUE)
# Processar data do evento, mantendo apenas o ano
# https://stackoverflow.com/a/43230524/3365410 -> as.Date(42705, origin = "1899-12-30")
observacao[["data_coleta_original"]] <- observacao[["data_coleta"]]
n <- which(nchar(observacao[["data_coleta_original"]]) == 5)
observacao[n, "data_coleta"] <-
  as.character(as.Date(as.integer(observacao[n, "data_coleta_original"]), origin = "1899-12-30"))
has_bar <-  which(grepl("/", observacao[["data_coleta"]], fixed = TRUE))
if (any(has_bar)) {
  observacao[has_bar, "data_coleta"] <- gsub("/", "-", observacao[has_bar, "data_coleta"])
}
has_dash <- which(grepl("-", observacao[["data_coleta"]]))
observacao[["data_coleta_ano"]] <- NA_integer_
for (i in has_dash) {
  y <- strsplit(observacao[i, "data_coleta"], "-")[[1]]
  n <- nchar(y)
  if (any(n == 4)) {
    is_year <- which(n == 4)
    observacao[i, "data_coleta_ano"] <- as.integer(y[is_year])
  }
}
# Processar classificação taxonômica
# 1. Fusão das colunas com a classificação taxonômica do solo nas diferentes versões do SiBCS.
# Prioridade é dada à classificação mais recente. Classificações até 1999 são ignoradas,
# pois o número de classes e a nomenclatura utilizada são diferentes da versão atual. Nesse caso,
# observações apenas com a classificação taxonômica antiga do SiBCS ficam sem dados
# (`NA_character`). O código de identificação da coluna resultante da fusão das colunas de
# cada uma das versões do SiBCS é `sibcs_20xx`.
# O processo de fusão dos dados é realizado usando procedimento conhecido como coalescer:
# * https://dplyr.tidyverse.org/reference/coalesce.html
# * https://www.w3schools.com/SQL/func_sqlserver_coalesce.asp
obs_cols <- colnames(observacao)
# taxon_cols <- grepl("^taxon_sibcs_20(.)|^taxon_sibcs_1999", obs_cols)
taxon_cols <- grepl("^sibcs_20(.)|^sibcs_1999", obs_cols)
taxon_cols <- sort(obs_cols[taxon_cols], decreasing = TRUE)
observacao[, "sibcs_20xx_ordem"] <- NA_character_
has_taxon <- which(rowSums(is.na(observacao[, taxon_cols])) < length(taxon_cols))
for (i in has_taxon) {
  idx_not_na <- which(!is.na(observacao[i, taxon_cols]))[1] # reter o primeiro
  observacao[i, "sibcs_20xx_ordem"] <- observacao[i, taxon_cols][idx_not_na]
}
# 2. Substituição da classificação taxonômica registrada na forma de sigla pelo nome correspondente
# por extenso. O código da planilha do Google Sheets contendo a tabela de mapeamento entre sigla e
# nome é 1yJ_XnsJhnhJSfC3WRimfu_3_owXxpfSKaoxCiMD2_Z0.
# Inicia-se identificando quais eventos possuem a classificação taxonômica na forma curta, ou seja,
# na forma curta. Isso é feito avaliando o comprimento das cadeias de caracteres.
# O mapeamento é feito após identificação da correspondência entre os dados e o vocabulário
# controlado usando match().
# No futuro, é recomendado que o mapeamento seja realizado para cada versão do SiBCS, antes da fusão
# dos dados realizada no passo anterior. Esse procedimento deve ser implementado na função
# febr::taxonomy().
sibcs_tabela <- febr::readVocabulary()
# idx_taxon_sibcs <- grepl("^taxon_sibcs_20(.)", sibcs_tabela[["campo_id"]])
idx_taxon_sibcs <- grepl("^sibcs_20(.)", sibcs_tabela[["campo_id"]])
idx_taxon_sibcs <- which(idx_taxon_sibcs)
acronym_length <- nchar(sibcs_tabela[["campo_valorcurto"]][idx_taxon_sibcs])
acronym_length <- max(acronym_length)
idx_acronym <- nchar(observacao[["sibcs_20xx_ordem"]])
idx_acronym <- which(idx_acronym <= acronym_length)
idx_match <- match(x = observacao[["sibcs_20xx_ordem"]][idx_acronym],
  table = sibcs_tabela[["campo_valorcurto"]][idx_taxon_sibcs])
observacao[["sibcs_20xx_ordem"]][idx_acronym] <- sibcs_tabela[["campo_valorlongo"]][idx_match]
# 3. O terceiro passo consiste na eliminação de níveis categóricos inferiores, mantendo-se apenas o
# primeiro (ordem). O processamento é realizado usando a função febr::taxonomy(), que retorna os
# termos todos em caixa alta (primeiro nível categórico).
has_taxon <- is.na(observacao[["sibcs_20xx_ordem"]])
has_taxon <- which(!has_taxon)
observacao[has_taxon, "sibcs_20xx_ordem"] <-
  febr::taxonomy(observacao[has_taxon, "sibcs_20xx_ordem"])[, "ordem"]
# 
# Processar uso da terra
# * Dados de uso da terra (terra_uso_descricao), nome da cultura agrícola (terra_cultura) e sistema
#   de manejo do solo (terra_manejo) são agregados.
# * Alguma limpeza é realizada, removendo espaços, quebras de linha e pontuação desnecessários. No
#   caso de ponto final, o mesmo é removido apenas quando a cadeia de caracteres possui comprimento
#   menor ou igual a 50.
observacao[, "terra_uso_original"] <- observacao[, "terra_uso_descricao"]
isna_uso <- is.na(observacao[["terra_uso_original"]])
isna_cultura <- is.na(observacao[["terra_cultura"]])
isna_manejo <- is.na(observacao[, "terra_manejo"])
observacao[["terra_uso_descricao"]] <- paste0(
  ifelse(isna_uso, "", observacao[["terra_uso_original"]]),
  ifelse(isna_cultura, "", paste0(" ", observacao[["terra_cultura"]])),
  ifelse(isna_manejo, "", paste0(" ", observacao[["terra_manejo"]]))
)
observacao[["terra_uso_descricao"]] <- gsub("  ", " ", observacao[["terra_uso_descricao"]])
observacao[["terra_uso_descricao"]] <-
  gsub("&#10;", " ", observacao[["terra_uso_descricao"]], fixed = TRUE)
observacao[["terra_uso_descricao"]] <- gsub("^ ", "", observacao[["terra_uso_descricao"]])
observacao[["terra_uso_descricao"]] <- gsub(" $", "", observacao[["terra_uso_descricao"]])
observacao[["terra_uso_descricao"]] <-
  gsub(" .", "", observacao[["terra_uso_descricao"]], fixed = TRUE)
idx_short <- which(nchar(observacao[["terra_uso_descricao"]]) <= 50)
observacao[idx_short, "terra_uso_descricao"] <-
  gsub(".", "", observacao[idx_short, "terra_uso_descricao"], fixed = TRUE)
# 
# Atualizar campos coord_latitude e coord_longitude
which_cols <- match(c("coord_latitude", "coord_longitude"), colnames(observacao))
colnames(observacao)[which_cols] <- c("coord_latitude_grau", "coord_longitude_grau")
observacao[["coord_latitude_grau"]] <- as.numeric(observacao[["coord_latitude_grau"]])
observacao[["coord_longitude_grau"]] <- as.numeric(observacao[["coord_longitude_grau"]])
# 
# Atualizar campos dataset_id -> dados_id
which_cols <- match("dataset_id", colnames(observacao))
colnames(observacao)[which_cols] <- "dados_id"
# Escrever tabela em disco
file_name <- paste0("febr-output/tmp/", snapshot, "-eventos.txt")
write.table(observacao[, eventos_cols], file = file_name, sep = ";", dec = ",", row.names = FALSE)
# colnames(observacao[, eventos_cols])
# all(observacao[["coord_datum_epsg"]] == observacao[["coord_datum"]])
# eventos_cols[-which(eventos_cols %in% colnames(observacao))]

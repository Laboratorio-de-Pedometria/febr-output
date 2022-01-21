# title: Instância de dezembro de 2021
# subtitle: Repositório de Dados do Solo Brasileiro
# author: Alessandro Samuel-Rosa
# 
# Instalar última versão do pacote febr diretamente o GitHub
if (!require(remotes)) {
  install.packages(pkgs = "remotes")
}
remotes::install_github(repo = "laboratorio-de-pedometria/febr-package")
library(febr)
febr_repo <- "~/ownCloud/febr-repo/publico"
# 
## 2021-12-fontes.txt ##############################################################################
# Campos exportados da tabela 'identificacao':
# * dados_id: código de identificação dos dados no FEBR
# * dados_licenca: licença de uso e distribuição dos dados definida pelos autores ou organização
#   responsável
ide <- febr::identification(data.set = "all", febr.repo = febr_repo)
ide <- lapply(ide, function(x){
  y <- as.list(x[["valor"]])
  names(y) <- x[["campo"]]
  data.frame(y)
})
ide <- do.call(rbind, ide)
ide[["dados_licenca"]] <- gsub(" 4.0 Atribuição-NãoComercial-CompartilhaIgual", "",
  ide[["dados_licenca"]])
ide[["dados_licenca"]] <- gsub(" 4.0 Atribuição-NãoComercial", "", ide[["dados_licenca"]])
ide[["dados_licenca"]] <- gsub(" 4.0 Atribuição", "", ide[["dados_licenca"]])
ide_cols <- c("dados_id", "dados_licenca")
ide <- ide[, ide_cols]
# Campos exportados da tabela 'versionamento':
# * dados_versao: número da última versão dos dados no FEBR
ver <- febr::readFEBR(data.set = "all", data.table = "versionamento")
latest_version <- character(length(ver))
for (i in seq_along(ver)) {
  which_row <- dim(ver[[i]])[1]  
  latest_version[i] <- ver[[i]][which_row, "dados_versao"][[1]]
}
# Escrever table em disco
ide <- cbind(ide, dados_versao = latest_version)
write.table(ide, "febr-output/tmp/2021-12-fontes.txt", sep = ";", dec = ",", row.names = FALSE)
# 
## 2021-12-eventos.txt #############################################################################
# Campos exportados da tabela 'observacao':
eventos_cols <- c(
  "evento_id_febr", # código de identificação do evento nos dados
  "evento_id_sisb", # código de identificação do evento no repositório da EMBRAPA (quando existente)
  "evento_id_ibge", # código de identificação do evento no repositório do IBGE (quando existente)
  "evento_data_ano" # data (ano) quando o evento foi observado, descrito e amostrado
)
# Carregar pacotes necessários
if (!require(sf)) {
  install.packages(pkgs = "sf", dependencies = TRUE)
}
# Descarregar dados utilizando o nível 3 de harmonização
vars <- c("sisb_id", "taxon_", "terra_", "fito_")
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
  febr.repo = febr_repo, verbose = FALSE)
# Processar data do evento, mantendo apenas o ano
# https://stackoverflow.com/a/43230524/3365410
# as.Date(42705, origin = "1899-12-30")
observacao[["evento_data_original"]] <- observacao[["evento_data"]]
n <- which(nchar(observacao[["evento_data_original"]]) == 5)
observacao[n, "evento_data"] <-
  as.character(as.Date(as.integer(observacao[n, "evento_data_original"]), origin = "1899-12-30"))
has_bar <-  which(grepl("/", observacao[["evento_data"]], fixed = TRUE))
if (any(has_bar)) {
  observacao[has_bar, "evento_data"] <- gsub("/", "-", observacao[has_bar, "evento_data"])
}
has_dash <- which(grepl("-", observacao[["evento_data"]]))
observacao[["evento_data_ano"]] <- NA_integer_
for (i in has_dash) {
  y <- strsplit(observacao[i, "evento_data"], "-")[[1]]
  n <- nchar(y)
  if (any(n == 4)) {
    is_year <- which(n == 4)
    observacao[i, "evento_data_ano"] <- as.integer(y[is_year])
  }
}
# Escrever tabela em disco
file_name <- "febr-output/tmp/2021-12-eventos.txt"
write.table(observacao[, eventos_cols], file = file_name, sep = ";", dec = ",", row.names = FALSE)

colnames(observacao)

# 
# Apresentação
# 
# Processamento de dados provenientes de conjuntos de dados publicados no FEBR.
# Os dados passam por limpeza, padronização e, quando possível, harmonização.
# O conjunto de dados resultante é disponibilizado num arquivo TXT único para reúso.
# 
# Variáveis incluídas
# 
# Tabela observacao:
# 1. taxon_sibcs: classificação taxonômica pelo Sistema Brasileiro de Classificação de Solos;
# 2. taxon_st: classificação taxonômica pelo Soil Taxonomy;
# 3. taxon_wrb: classificação taxonômica pelo World Reference Base for Soil Resources;
# 
# Tabela camada:
# 1. terrafina: conteúdo da fração terra fina (< 2 mm) no solo inteiro, em g/kg;
# 2. carbono: conteúdo de carbono orgânico na fração terra fina, em g/kg;
# 3. argila: conteúdo de argila total na fração terra fina, em g/kg;
# 4. silte: conteúdo de silte total na fração terra fina, em g/kg;
# 5. areia: conteúdo de areia total na fração terra fina, em g/kg;
# 6. ctc: capacidade de troca de cátions da fração terra fina, em cmol_c/kg;
# 7. ph: pH em água da fração terra fina, sem unidade de medida;
# 8. ce: condutividade elétrica de fração terra fina, em mS/cm;
# 9. densidade: densidade do solo inteiro, em kg/dm^3.
# 
# Dados adicionais:
# - profundidade superior (profund_sup) e inferior (profund_inf) da camada amostrada, em cm;
# - data de observação ou amostragem do solo (evento_data);
# - coordenadas do local de observaćão ou amostragem do solo (coord_x e coord_y), em graus decimais,
#   usando SIRGAS 2000 (EPSG:4674) como sistema de referência de coordenadas.
#   
# Os procedimentos de limpeza dos dados são descritos abaixo. Já os procedimentos de padronização e
# harmonização são descritos no pacote febr para o R (https://www.pedometria.org/software/febr/).





## Tabela  ############################################################################

# 



# Processar uso e cobertura da terra
# * Dados de uso da terra (terra_usoatual), nome da cultura agrícola (terra_cultura) e sistema de
#   manejo do solo (terra_manejo) são agregados.
# * Termos em inglês são traduzidos livremente para o português.
# * Alguma limpeza é realizada, removendo espaços, quebras de linha e pontuação desnecessários. No
#   caso de ponto final, o mesmo é removido apenas quando a cadeia de caracteres possui comprimento
#   menor ou igual a 50.
observacao[, "terra_usoatual_original"] <- observacao[, "terra_usoatual"]
isna_uso <- is.na(observacao[["terra_usoatual_original"]])
isna_cultura <- is.na(observacao[["terra_cultura"]])
isna_manejo <- is.na(observacao[, "terra_manejo"])
observacao[["terra_usoatual"]] <- paste0(
  ifelse(isna_uso, "", observacao[["terra_usoatual_original"]]),
  ifelse(isna_cultura, "", paste0(" ", observacao[["terra_cultura"]])),
  ifelse(isna_manejo, "", paste0(" ", observacao[["terra_manejo"]]))
)
observacao[["terra_usoatual"]] <- gsub("  ", " ", observacao[["terra_usoatual"]])
observacao[["terra_usoatual"]] <- gsub("&#10;", " ", observacao[["terra_usoatual"]], fixed = TRUE)
observacao[["terra_usoatual"]] <- gsub("^ ", "", observacao[["terra_usoatual"]])
observacao[["terra_usoatual"]] <- gsub(" $", "", observacao[["terra_usoatual"]])
observacao[["terra_usoatual"]] <- gsub(" .", "", observacao[["terra_usoatual"]], fixed = TRUE)
idx_short <- which(nchar(observacao[["terra_usoatual"]]) <= 50)
observacao[idx_short, "terra_usoatual"] <- gsub(".", "", observacao[idx_short, "terra_usoatual"], fixed = TRUE)
observacao[["terra_usoatual"]] <- gsub("shrubland", "capoeira", observacao[["terra_usoatual"]])
observacao[["terra_usoatual"]] <- gsub("forestry", "silvicultura", observacao[["terra_usoatual"]])
observacao[["terra_usoatual"]] <- gsub("native forest", "floresta nativa", observacao[["terra_usoatual"]])
observacao[["terra_usoatual"]] <- gsub("crop agriculture", "cultivo agrícola", observacao[["terra_usoatual"]])
observacao[["terra_usoatual"]] <- gsub("animal husbandry", "criação animal", observacao[["terra_usoatual"]])

####################################################################################################
# Processar classificação taxonômica
# 1. Fusão das colunas com a classificação taxonômica do solo nas diferentes versões do SiBCS.
# Prioridade é dada à classificação mais recente. Classificações até 1999 são ignoradas,
# pois o número de classes e a nomenclatura utilizada são diferentes da versão atual. Nesse caso,
# observações apenas com a classificação taxonômica antiga do SiBCS ficam sem dados
# (`NA_character`). O código de identificação da coluna resultante da fusão das colunas de
# cada uma das versões do SiBCS é `taxon_sibcs`.
# O processo de fusão dos dados é realizado usando procedimento conhecido como coalescer:
# * https://dplyr.tidyverse.org/reference/coalesce.html
# * https://www.w3schools.com/SQL/func_sqlserver_coalesce.asp
obs_cols <- colnames(observacao)

# taxon_cols <- grepl("^taxon_sibcs_(.)", obs_cols)
# taxon_cols <- sort(obs_cols[taxon_cols], decreasing = TRUE)
# idx <- is.na(observacao[["taxon_sibcs_20xx"]])
# idx <- which(!idx)
# unique(observacao[idx, "dataset_id"])


taxon_cols <- grepl("^taxon_sibcs_20(.)|^taxon_sibcs_1999", obs_cols)
taxon_cols <- sort(obs_cols[taxon_cols], decreasing = TRUE)
observacao[, "taxon_sibcs"] <- NA_character_
has_taxon <- which(rowSums(is.na(observacao[, taxon_cols])) < length(taxon_cols))
for (i in has_taxon) {
  idx_not_na <- which(!is.na(observacao[i, taxon_cols]))[1] # reter o primeiro
  observacao[i, "taxon_sibcs"] <- observacao[i, taxon_cols][idx_not_na]
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
idx_taxon_sibcs <- grepl("^taxon_sibcs_20(.)", sibcs_tabela[["campo_id"]])
idx_taxon_sibcs <- which(idx_taxon_sibcs)
acronym_length <- nchar(sibcs_tabela[["campo_valorcurto"]][idx_taxon_sibcs])
acronym_length <- max(acronym_length)
idx_acronym <- nchar(observacao[["taxon_sibcs"]])
idx_acronym <- which(idx_acronym <= acronym_length)
idx_match <- match(x = observacao[["taxon_sibcs"]][idx_acronym],
  table = sibcs_tabela[["campo_valorcurto"]][idx_taxon_sibcs])
observacao[["taxon_sibcs"]][idx_acronym] <- sibcs_tabela[["campo_valorlongo"]][idx_match]
# 3. O terceiro passo consiste na eliminação de níveis categóricos inferiores, mantendo-se apenas o
# primeiro (ordem). O processamento é realizado usando a função febr::taxonomy(), que retorna os
# termos todos em caixa alta (primeiro nível categórico).
has_taxon <- is.na(observacao[["taxon_sibcs"]])
has_taxon <- which(!has_taxon)
observacao[has_taxon, "taxon_sibcs"] <-
  febr::taxonomy(observacao[has_taxon, "taxon_sibcs"])[, "ordem"]


# As colunas são organizadas de maneira a:
# 
# 1. Descartar as colunas taxon_sibcs_<...>, taxon_st_<...> e taxon_wrb_<...>, processadas acima e 
#    substituídas pelas colunas taxon_sibcs, taxon_st e taxon_wrb, respectivamente.
# 2. Descartar a colunas coord_sistema`, uma vez que as coordenadas espaciais de todas as observações foram
#    padronizadas para o sistema de referência de coordenadas SIRGAS 2000 (EPSG:4674).
# 3. Posicionar as colunas com dados de identificação -- `observacao_id_`, `sisb_id` e `ibge_id` lado-a-lado.

# O tipo de dados das colunas também é definido aqui:

# 1. Os dados da coluna `coord_precisao` são definidos como do tipo real usando `as.numeric()`.

# Reter classificação taxonômica mais atual





observacao <- 
  observacao %>% 
  dplyr::mutate(
    taxon_sibcs = dplyr::coalesce(
      taxon_sibcs_2013, taxon_sibcs_2009, taxon_sibcs_2006, taxon_sibcs_2003),
      # taxon_sibcs_1999,
      # taxon_sibcs_19xx, taxon_sibcs_xxx, taxon_sibcs_xxx_1, taxon_sibcs_xxx_2, taxon_sibcs_xxxx),
    taxon_sibcs = dplyr::recode(.data$taxon_sibcs, !!!sibcs_tabela),
    taxon_sibcs = pruneStrings(taxon_sibcs, n = 2),
    taxon_sibcs = toupper(taxon_sibcs),
    taxon_sibcs = gsub(pattern = ".", "", .data$taxon_sibcs, fixed = TRUE),
    taxon_sibcs = chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", .data$taxon_sibcs),
    taxon_sibcs = chartr("âêîôûÂÊÎÔÛ", "aeiouAEIOU", .data$taxon_sibcs),
    taxon_st = dplyr::coalesce(taxon_st_2010, taxon_st_1999, taxon_st_xxx),
    taxon_st = toupper(taxon_st),
    taxon_st = gsub(pattern = ".", "", .data$taxon_st, fixed = TRUE),
    taxon_wrb = dplyr::coalesce(taxon_wrb_2006, taxon_wrb_1998, taxon_wrb_xxx),
    taxon_wrb = toupper(taxon_wrb),
    taxon_wrb = gsub(pattern = ".", "", .data$taxon_wrb, fixed = TRUE),
    coord_precisao = as.numeric(coord_precisao),
    coord_x = round(coord_x, 8),
    coord_y = round(coord_y, 8),
    amostra_tipo = toupper(amostra_tipo),
    amostra_quanti = as.integer(amostra_quanti),
    municipio_id = gsub(pattern = ".", "", .data$municipio_id, fixed = TRUE)
  ) %>% 
  dplyr::select(
    dataset_id, observacao_id, sisb_id, ibge_id, evento_data, coord_x, coord_y, coord_precisao, 
    coord_fonte, pais_id, estado_id, municipio_id, amostra_tipo, amostra_quanti, amostra_area, taxon_sibcs, 
    taxon_st, taxon_wrb)



### Análise exploratória

# observacao <- read.table(glue::glue("../data/febr-observacao.txt"), sep = ";", dec = ",", header = TRUE)

tmp <-
  observacao %>% 
  dplyr::summarise(
    Total = n(),
    `Lat/Long` = sum(!is.na(coord_x)),
    Precisão = median(coord_precisao, na.rm = TRUE),
    GPS = sum(coord_fonte == "GPS", na.rm = TRUE),
    MAPA = sum(coord_fonte == "MAPA", na.rm = TRUE),
    WEB = sum(coord_fonte == "WEB", na.rm = TRUE)
  ) %T>% 
  print()

# O ___febr___ dispõe de um total de 15158 observações do solo. Destas, 11970 possuem coordenadas espaciais, ou
# seja, 11970 / 15158 * 100 = 78.9682%. A precisão mediana das coordenadas espaciais é de 100 m. A fonte de boa
# parte das coordenadas é desconhecida, sendo o GPS a fonte mais comum (5309). Boa parte das coordenadas foi
# estimada usando serviços de mapas online (693), ou então foram estimadas usando, por exemplo, mapas base
# (693).

br <- sf::read_sf("../data/vector/br.shp")
tmp <- 
  observacao$estado_id %>% 
  table()
idx <- match(br$SigUF, names(tmp))
dens <- tmp[idx] / br$area
dens <- dens[order(dens)]
png("../res/fig/febr-observacao-espaco.png", width = 480 * 2, height = 480 * 2, res = 72 * 2)
# png("../res/fig/febr-observacao-espaco-saopaulo.png", width = 480 * 3, height = 480 * 2, res = 72 * 2)
# png("../res/fig/febr-observacao-espaco-goias.png", width = 480 * 3, height = 480 * 2, res = 72 * 2)
plot(
  br[1], graticule = TRUE, axes = TRUE, 
  sub = Sys.Date(), 
  # main = "Distribuição espacial das observações",
  # main = "Spatial distribution of observations",
  main = "",
  col = 0, reset = FALSE)
observacao %>%
  dplyr::filter(!is.na(coord_x) & !is.na(coord_y)) %>%
  sf::st_as_sf(coords = c('coord_x', 'coord_y'), crs = 4326) %>%
  # dplyr::select(estado_id) %>%
  # plot(
  # cex = ifelse(.$estado_id == "GO", 0.5, 0.3),
  # pch = ifelse(.$estado_id == "GO", 20, 1),
  # col = ifelse(.$estado_id == "GO", "firebrick1", "lightgray"), add = TRUE)
  # plot(
  # cex = ifelse(.$estado_id == "SP", 0.5, 0.3),
  # pch = ifelse(.$estado_id == "SP", 20, 1),
  # col = ifelse(.$estado_id == "SP", "firebrick1", "lightgray"), add = TRUE)
  plot(cex = 0.5, col = "firebrick1", add = TRUE)
dev.off()

tmp2 <- 
  data.frame(
    n_max = max(tmp),
    n_min = min(tmp),
    d_max = max(dens),
    d_min = min(dens)
  ) %T>% 
  print()

# A distribuição espacial das observações do solo é bastante heterogênea, com inúmeros agrupamentos de 
# observações aparecendo em várias partes do território nacional. Consequentemente, amplos vazios de observações
# aparecem. O estado de Rondônia concentra o maior número de observações. Enquanto isso, o menor número de
# observações parece no estado do Tocantis. Devido ao seu relativamente grande território, o estado do Tocantis
# também possui a menor densidade de observação. A maior densidade de observação é no Distrito Federal. Em geral,
# os estados das regiões Centro-Oeste, Norte e Nordeste são aqueles com as menores densidades de observação,
# refletindo o histórico de ocupação do território brasileiro e os investimentos feitos no ensino e pesquisa via
# universidades e centros de pesquisa agronômica.

png("../res/fig/febr-observacao-uf.png", width = 480 * 2, height = 480 * 2, res = 72 * 2)
# png("../res/fig/febr-observacao-uf-saopaulo.png", width = 480 * 3, height = 480 * 2, res = 72 * 2)
# png("../res/fig/febr-observacao-uf-goias.png", width = 480 * 3, height = 480 * 2, res = 72 * 2)
layout(matrix(1:2, nrow = 1), widths = c(2, 2), heights = 1, respect = FALSE)
par(oma = c(0, 0, 1, 0), las = 1, mar = c(4, 4, 1, 1))
tmp %>% 
  sort() %>% 
  barplot(
    col = "firebrick1", border = "firebrick1",
    # col = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    # col = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    ylab = "Unidade da federação", xlab = "Número",
    # ylab = "Federative unit", xlab = "Number",
    horiz = TRUE, ann = TRUE)
grid()
dens %>% 
  barplot(
    col = "firebrick1", border = "firebrick1",
    # col = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    # col = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    ylab = "Unidade da federação", xlab = "Densidade (por 1000 km²)",
    # ylab = "Federative unit", xlab = "Density (per 1000 km²)",
    horiz = TRUE, ann = TRUE)
title(
  main = "Distribuição das observações pelas unidades da federação",
  # main = "Observations by federative unit",
  sub = Sys.Date(), outer = TRUE)
grid()
dev.off()

# Das observações sem coordenadas espaciais, a maioria se encontra nos estados da Bahia, Minas Gerais, Paraná e
# Pará. Três conjuntos de dados concentram parte considerável dessas observações -- ctb0657, ctb0831 e ctb0775
# --, cada um com mais de 100 observações sem coordenadas espaciais. Outros 14 conjuntos de dados possuem entre
# 50 e 100 observações sem coordenadas espaciais.

png("../res/fig/febr-observacao-sem-coordenadas.png", width = 480 * 2, height = 480 * 2, res = 72 * 2)
# png("../res/fig/febr-observacao-sem-coordenadas-goias.png", width = 480 * 3, height = 480 * 2, res = 72 * 2)
# png("../res/fig/febr-observacao-sem-coordenadas-saopaulo.png", width = 480 * 3, height = 480 * 2, res = 72 * 2)
layout(matrix(1:2, nrow = 1), widths = c(2, 2), heights = 1, respect = FALSE)
par(oma = c(0, 0, 1, 0), las = 1, mar = c(4, 4, 1, 1))
observacao %>% 
  dplyr::filter(is.na(coord_x)) %>% 
  dplyr::select(estado_id) %>% 
  dplyr::mutate(estado_id = as.factor(estado_id)) %>% 
  table() %>% 
  sort() %>% 
  barplot(
    col = "firebrick1", border = "firebrick1",
    # col = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    # col = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    ylab = "Unidade da federação", xlab = "Número de observações",
    # ylab = "Federative unit", xlab = "Number",
    horiz = TRUE)
grid()
observacao %>% 
  dplyr::filter(is.na(coord_x)) %>% 
  dplyr::select(dataset_id) %>% 
  dplyr::mutate(dataset_id = as.factor(dataset_id)) %>% 
  table() %>% 
  sort(decreasing = TRUE) %>% 
  head(27L) %>% 
  sort() %>% 
  barplot(
    col = "firebrick1", border = "firebrick1",
    # col = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    # col = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    names.arg = gsub("ctb0", "", names(.)),
    ylab = "Conjunto de dados", xlab = "Número de observações",
    # ylab = "Dataset ID", xlab = "Number",
    horiz = TRUE)
grid()
title(
  main = "Distribuição das observações sem coordenadas espaciais",
  # main = "Observations without spatial coordinates",
  sub = Sys.Date(), outer = TRUE)
dev.off()

observacao %>% 
  dplyr::mutate(evento_data = as.Date(evento_data) %>% format("%Y")) %>% 
  dplyr::summarise(
    Total = dplyr::n(),
    Data = sum(!is.na(evento_data)),
    `<1960` = sum(evento_data < 1960, na.rm = TRUE),
    `1960-1990` = (sum(evento_data < 1990, na.rm = TRUE) - `<1960`),
    `1990-2010` = (sum(evento_data < 2010, na.rm = TRUE) - `<1960` - `1960-1990`),
    `2010-2019` = (sum(evento_data < 2019, na.rm = TRUE) - `<1960` - `1960-1990` - `1990-2010`)
    ) %>% 
  dplyr::mutate(
    `<1960` = round(`<1960` / Data * 100, 2),
    `1960-1990` = round(`1960-1990` / Data * 100, 2),
    `1990-2010` = round(`1990-2010` / Data * 100, 2),
    `2010-2019` = round(`2010-2019` / Data * 100, 2)
  )

# Apenas cerca de 10087 / 15158 * 100 = 66.54572% das observações do solo possuem registro da data de observação.
# Destas, metadade -- 49.26% -- foram obtidas entre 1990 e 2010. Isso se deve a um pico no ano de 1997, ano em
# que foram obtidas as observações do solo do maior conjunto de dados disponível no ___febr___. Trata-se do
# conjunto de dados do Zoneamento Agroecológico do Estado de Rondônia, que possui mais de 2000 observações do
# solo. Das observações sem data de observação, acredita-se que a maioria foi produzida na década de 1970,
# período de maior realização de levantamentos de solo no Brasil.

# A distribuição das observações do solo no tempo mostra que há um vazio entre as décadas de 1980 e 1990. Esse
# período foi marcado pelo término dos principas projetos/programas de mapeamento do solo em larga escala no 
# Brasil. Contudo, como parte considerável das observações do solo não possui informação sobre a data de 
# observação, não é possível identificar a razão do vazio com precisão.

observacao[!is.na(observacao$coord_x), c("coord_x", "coord_y", "evento_data")] %>% 
  duplicated() %>% 
  sum()

png("../res/fig/febr-observacao-tempo.png", width = 480 * 2, height = 480 * 2, res = 72 * 2)
# png("../res/fig/febr-observacao-tempo-saopaulo.png", width = 480 * 3, height = 480 * 2, res = 72 * 2)
# png("../res/fig/febr-observacao-tempo-goias.png", width = 480 * 3, height = 480 * 2, res = 72 * 2)
par(oma = c(0, 0, 0, 0), las = 1, mar = c(4, 4, 2, 1))
tmp <- 
  observacao %>% 
  select(evento_data) %>% 
  mutate(
    evento_data = as.Date(evento_data),
    evento_data = format(evento_data, "%Y")) %>% 
  table()
barplot(tmp, col = "lightgray", border = "lightgray")
# barplot(tmp, col = "firebrick1", border = "firebrick1")
grid()
tmp %>% 
  barplot(
    # col = "firebrick1", border = "firebrick1",
    col = "lightgray", border = "lightgray",
    # sub = glue::glue("Versão {Sys.Date()}"),
    xlab = "Ano", ylab = "Número de observações",
    # xlab = "Year", ylab = "Number",
    main = "Distribuição temporal das observações",
    # main = "Temporal distribution of observations",
    add = TRUE, ann = TRUE)
# tmp2 <-
# observacao %>%
#   # filter(estado_id == "GO") %>%
#   filter(estado_id == "SP") %>%
#   select(evento_data) %>%
#   mutate(
#     evento_data = as.Date(evento_data),
#     evento_data = format(evento_data, "%Y")) %>%
#   table()
# id <- which(names(tmp) %in% names(tmp2))
# tmp[id] <- tmp2
# tmp[-id] <- NA
# barplot(tmp, col = "firebrick1", border = "firebrick1", add = TRUE, xaxt = "n", yaxt = "n")
dev.off()

# A maioria das observações com data de observação desconhecida pertencem a conjuntos de dados que abrangem os
# estados do Amazonas, Santa Catarina, Bahia, Rio Grande do Sul, Pará, Minas Gerais e Paraná. Dentre estes, três
# conjuntos de dados se destacam pelo grande número de observações sem data. São eles: ctb0572, ctb0770 e
# ctb0657.

png("../res/fig/febr-observacao-sem-data.png", width = 480 * 2, height = 480 * 2, res = 72 * 2)
# png("../res/fig/febr-observacao-sem-data-goias.png", width = 480 * 2, height = 480 * 2, res = 72 * 2)
# png("../res/fig/febr-observacao-sem-data-saopaulo.png", width = 480 * 3, height = 480 * 2, res = 72 * 2)
layout(matrix(1:2, nrow = 1), widths = c(2, 2), heights = 1, respect = FALSE)
par(oma = c(0, 0, 1, 0), las = 1, mar = c(4, 4, 1, 1))
observacao %>% 
  dplyr::filter(is.na(evento_data)) %>% 
  dplyr::select(estado_id) %>% 
  dplyr::mutate(estado_id = as.factor(estado_id)) %>% 
  table() %>% 
  sort() %>% 
  barplot(
    col = "firebrick1", border = "firebrick1",
    # col = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    # col = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    ylab = "Unidade da federação", xlab = "Número de observações",
    # ylab = "Federative unit", xlab = "Number",
    horiz = TRUE)
grid()
observacao %>% 
  dplyr::filter(is.na(evento_data)) %>% 
  dplyr::select(dataset_id) %>% 
  dplyr::mutate(dataset_id = as.factor(dataset_id)) %>% 
  table() %>% 
  sort(decreasing = TRUE) %>% 
  head(27L) %>% 
  sort() %>% 
  barplot(
    col = "firebrick1", border = "firebrick1",
    # col = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    # col = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    names.arg = gsub("ctb0", "", names(.)),
    ylab = "Conjunto de dados", xlab = "Número de observações",
    # ylab = "Dataset ID", xlab = "Number",
    horiz = TRUE)
grid()
title(
  main = "Distribuição das observações com data desconhecida",
  # main = "Observations without temporal coordinate",
  sub = Sys.Date(), outer = TRUE)
dev.off()

### Salvar dados

# Salvar os dados no formato TXT.

write.table(
  observacao, file = glue::glue("../data/febr-observacao.txt"), sep = ";", dec = ",", row.names = FALSE)

## Tabelas _camada_

vars <- c(
  "carbono",
  "argila", "areia", "areiagrossa2", "areiafina2", "silte",
  "terrafina", "cascalho", "calhau",
  "dsi",
  "ctc",
  "ph",
  "ce")
camada <- febr::layer(
  dataset = "all",
  variable = glue::glue("{vars}_"),
  stack = TRUE,
  harmonization = list(harmonize = TRUE, level = 2),
  standardization = list(
    plus.sign = "add",
    plus.depth = 20,
    lessthan.sign = "remove",
    # lessthan.frac = 0.5,
    repetition = "combine", 
    combine.fun = "mean",
    transition = "smooth",
    smoothing.fun = "mean",
    units = TRUE, round = TRUE))
# camada %>%
#   lapply(function (x) {
#     classe <-
#       x %>%
#       dplyr::select(dplyr::starts_with("terrafina")) %>%
#       unlist() %>%
#       class()
#     c(dataset = x$dataset_id[1], classe = classe)
#   }) %>%
#   do.call(rbind, .)
```

### Processamento

# O processamento dos dados das variáveis prioritárias das tabelas `camada` segue os seguintes passos:

# Em seguida, as colunas dos dados de cada variável prioritária determinadas usando métodos parcialmente
# distintos são fundidas usando usando `dplyr::coalesce()`.

camada %<>% 
  dplyr::mutate(
    carbono = dplyr::coalesce(carbono_cromo, carbono_xxx, carbono_forno),
    argila = dplyr::coalesce(argila_naoh, argila_xxx),
    silte = dplyr::coalesce(silte_naoh, silte_xxx),
    areia = dplyr::coalesce(areia_naoh, areia_xxx),
    areiagrossa = dplyr::coalesce(areiagrossa2_naoh, areiagrossa2_xxx),
    areiafina = dplyr::coalesce(areiafina2_naoh, areiafina2_xxx),
    ctc = dplyr::coalesce(ctc_soma, ctc_xxx),
    dsi = dplyr::coalesce(dsi_cilindro, dsi_xxx),
    ph = dplyr::coalesce(ph_h2o),
    terrafina = dplyr::coalesce(terrafina_peneira, terrafina_xxx),
    ce = dplyr::coalesce(ce_pastasat),
    cascalho = dplyr::coalesce(cascalho_peneira, cascalho_olho, cascalho_xxx),
    calhau = dplyr::coalesce(calhau_peneira, calhau_xxx)
  ) %T>% 
  print()

# Após a fusão das colunas com dados da mesma variável, faz-se ajustes nos dados das frações granulométricas. 
# Primeiro, na falta de uma das duas frações granulométricas finas, `argila` ou `silte`, calcula-se seu valor 
# como sendo a diferença entre 1000 g/kg e a soma das duas outras frações granulométricas. Na falta dos dados do
# conteúdo de areia total -- `areia`, primeiro se usa (quando disponível) a soma dos dados do conteúdo de areia
# grossa -- `areiagrossa` -- e areia fina `areiafina`. Caso não estejam disponíveis, então se usa a estratégia 
# anterior. A seguir, verifica-se se a soma dos valores inteiros das três frações é igual a 1000 g/kg. Caso não 
# seja -- a diferença geralmente é de 1 g/kg para menos ou para mais --, então altera-se os dados do conteúdo de
# silte total de maneira que a soma seja igual a 1000 g/kg. Finalmente, faz-se o processamento dos dados das
# frações granulométricas grossas calhau -- `calhau` --, cascalho -- `cascalho` -- e terra fina -- `terrafina`.
# na ausência da última, utiliza-se as demais para sua estimativa. Caso todas as frações estejam faltando, então
# assume-se que o conteúdo da fração terra fina seja igual a 1000 g/kg. Isso porque é comum os trabalhos omitirem
# o conteúdo de terra fina quando a mesma corresponde a totalidade do solo.

correct_depth <-
  function (profund.sup, profund.inf) {
    res <- as.matrix(data.frame(profund.sup, profund.inf))
    if (any(profund.inf < profund.sup, na.rm = TRUE)) {
      id <- which(profund.inf < profund.sup)
      plus_depth <- max(res[id, 1])
      res[id,  1:2] <- abs(res[id,  1:2] - plus_depth)
      res[-id, 1:2] <- abs(res[-id, 1:2] + plus_depth)
    }
    as.data.frame(res)
  }
camada %<>%
  dplyr::mutate(
    argila_in = !is.na(argila),
    silte_in = !is.na(silte),
    areia_in = !is.na(areia),
    dtp_in = argila_in + silte_in + areia_in,
    argila = ifelse(dtp_in == 2 & !argila_in, 1000 - areia - silte, argila),
    silte = ifelse(dtp_in == 2 & !silte_in, 1000 - argila - areia, silte),
    areia = ifelse(!areia_in, areiagrossa + areiafina, areia),
    areia_in = !is.na(areia),
    areia = ifelse(dtp_in == 2 & !areia_in, 1000 - argila - silte, areia),
    dtp = areia + argila + silte,
    argila = round((argila / dtp) * 1000),
    silte = round((silte / dtp) * 1000),
    areia = round((areia / dtp) * 1000),
    dtp = areia + argila + silte,
    silte = ifelse(dtp != 1000, 1000 - areia - argila, silte),
    terrafina_in = !is.na(terrafina),
    cascalho_in = !is.na(cascalho),
    calhau_in = !is.na(calhau),
    terrafina = ifelse(!terrafina_in & cascalho_in & calhau_in, 1000 - cascalho - calhau, terrafina),
    terrafina = ifelse(!terrafina_in & cascalho_in & !calhau_in, 1000 - cascalho, terrafina),
    terrafina = ifelse(!terrafina_in & !cascalho_in & calhau_in, 1000 - calhau, terrafina),
    terrafina = ifelse(!terrafina_in & !cascalho_in & !calhau_in, 1000, terrafina) %>% round(),
    ph = ifelse(ph > 14, NA_real_, ph),
    carbono = round(carbono, 2),
    dsi = round(dsi, 2),
    profund_sup = ifelse(is.na(profund_inf), NA_real_, abs(profund_sup)),
    profund_inf = ifelse(is.na(profund_sup), NA_real_, abs(profund_inf))
  ) %>% 
  dplyr::group_by(dataset_id, observacao_id) %>%
  dplyr::mutate(
    sup = correct_depth(profund.sup = profund_sup, profund.inf = profund_inf)$profund.sup,
    inf = correct_depth(profund.sup = profund_sup, profund.inf = profund_inf)$profund.inf,
    profund_sup = sup,
    profund_inf = inf
  ) %>% 
  dplyr::ungroup()

# A última etapa de processamento lida com os dados da profundidade do solo, especificamente, com as camadas
# compostas por material orgânico. Em geral, o registro dessas camadas é realizado fixando o limite inferior como
# sendo igual a zero. Isso gera uma sequência invertida de valores de profundidade. Por exemplo, uma camada com 
# profundidade de 3--0 cm representa uma camada orgânica de três centímetros de espessura acima das camadas de
# material mineral do solo. Em alguns casos a profundidade superior pode ser negativa, por exemplo, -3--0 cm. O
# processamento desses dados consiste em ajustar a profundidade superior do solo a profundidade superior da 
# camada de material orgânico do solo, definida assim como sendo igual a zero centímetros de profundidade.

camada %<>% 
  dplyr::select(
    dataset_id, observacao_id, camada_id, amostra_id, camada_nome, profund_sup, profund_inf,
    terrafina, argila, silte, areia, carbono, ctc, ph, dsi, ce) %T>% 
  print()

### Análise exploratória

# Verificar a distribuição empírica dos dados, procurando por insconsistências nos dados.

png("../res/fig/febr-camada.png", width = 480 * 2, height = 480 * 2, res = 72 * 2)
par(oma = c(0, 0, 1, 0), las = 1)
camada %>% 
  dplyr::mutate(fragmentos = 1000 - terrafina) %>% 
  dplyr::rename(profund = profund_inf) %>%
  dplyr::select(
    'profund\n(cm)' = profund,
    'carbono\n(g/kg)' = carbono,
    'argila\n(g/kg)' = argila,
    'areia\n(g/kg)' = areia,
    'silte\n(g/kg)' = silte,
    'terrafina\n(g/kg)' = terrafina,
    'dsi\n(kg/dm³)' = dsi,
    'ctc\n(cmolc/kg)' = ctc,
    'ph\n(-)' = ph,
    'ce\n(mS/cm)' = ce) %>%
  # dplyr::select(
  #   'Depth\n(cm)' = profund, 
  #   'Carbon\n(g/kg)' = carbono,
  #   'Clay\n(g/kg)' = argila, 
  #   'Sand\n(g/kg)' = areia, 
  #   'Silt\n(g/kg)' = silte, 
  #   'Coarse\n(g/kg)' = fragmentos, 
  #   'BD\n(kg/dm³)' = dsi, 
  #   'CEC\n(cmolc/kg)' = ctc,
  #   'pH\n(-)' = ph,
  #   'EC\n(mS/cm)' = ce) %>%
  plot(
    cex = 0.5, col = "firebrick1", 
    main = "Distribuição empírica dos dados",
    # main = "Empirical data distribuion",
    sub = glue::glue("{Sys.Date()}-febr-camada"))
dev.off()

png("../res/fig/febr-camada-200cm.png", width = 480 * 2, height = 480 * 2, res = 72 * 2)
par(oma = c(0, 0, 1, 0), las = 1)
camada %>% 
  dplyr::mutate(fragmentos = 1000 - terrafina) %>% 
  dplyr::rename(profund = profund_inf) %>%
  dplyr::filter(profund <= 200) %>% 
  dplyr::select(
    'profund\n(cm)' = profund,
    'carbono\n(g/kg)' = carbono,
    'argila\n(g/kg)' = argila,
    'areia\n(g/kg)' = areia,
    'silte\n(g/kg)' = silte,
    'terrafina\n(g/kg)' = terrafina,
    'dsi\n(kg/dm³)' = dsi,
    'ctc\n(cmolc/kg)' = ctc,
    'ph\n(-)' = ph,
    'ce\n(mS/cm)' = ce) %>%
  # dplyr::select(
  #   'Depth\n(cm)' = profund, 
  #   'Carbon\n(g/kg)' = carbono,
  #   'Clay\n(g/kg)' = argila, 
  #   'Sand\n(g/kg)' = areia, 
  #   'Silt\n(g/kg)' = silte, 
  #   'Coarse\n(g/kg)' = fragmentos, 
  #   'BD\n(kg/dm³)' = dsi, 
  #   'CEC\n(cmolc/kg)' = ctc,
  #   'pH\n(-)' = ph,
  #   'EC\n(mS/cm)' = ce) %>%
  plot(
    cex = 0.5, col = "firebrick1",
    main = "Distribuição empírica dos dados (< 200 cm)",
    # main = "Empirical data distribuion (< 200 cm)",
    sub = glue::glue("{Sys.Date()}-febr-camada"))
dev.off()

### Salvar dados

# Salvar os dados no formato TXT.

write.table(camada, file = glue::glue("../data/febr-camada.txt"), sep = ";", dec = ",", row.names = FALSE)

## Tabelas _dataset_ + _observacao_ + _camada_

febr <- 
  merge(dataset, observacao, by = "dataset_id") %>% 
  merge(camada, by = c("dataset_id", "observacao_id"))

### Salvar dados

# Salvar os dados no formato TXT.

write.table(febr, file = "../data/febr-superconjunto.txt", sep = ";", dec = ",", row.names = FALSE)

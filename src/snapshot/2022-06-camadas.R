# title: Instantâneo de Junho de 2022 - camadas
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
## 2022-06-camadas.txt #############################################################################
snapshot <- "2022-06"
# Campos exportados da tabela 'camada':
camada_cols <- c(
  "dados_id",
  "evento_id_febr",
  "camada_id_febr"
)
# Descarregar dados utilizando o nível 3 de harmonização
vars <- c("argila", "silte", "areia")
  # "carbono_", "argila_", "silte_")
  # , "areia_", "areiagrossa2_", "areiafina2_", ,
  # "terrafina_", "cascalho_", "calhau_",
  # "densidade_",
  # "ctc_", "ph_", "ce_",
  # "camada_nome")
camada <- febr::layer(
  data.set = "all",
  # data.set = "ctb0768",
  variable = vars,
  # variable = "argila",
  stack = TRUE,
  # harmonization = list(harmonize = TRUE, level = 2),
  harmonization = list(harmonize = FALSE),
  standardization = list(
    plus.sign = "add",
    plus.depth = 20,
    lessthan.sign = "remove",
    repetition = "combine",
    combine.fun = "mean",
    transition = "smooth",
    smoothing.fun = "mean",
    units = TRUE, round = TRUE),
  febr.repo = febr_repo)
# Identificar campos numéricos que foram reconhecidos pelo R como texto.
# Isso pode acontecer quando da presença de algum caractere especial.
# Nestes, é preciso garantir que o separador decimal é o ponto.
which_cols <- sapply(vars, function(x) grepl(x, colnames(camada)))
which_cols <- which(rowSums(which_cols) > 0)
camada[, which_cols] <-
  lapply(camada[, which_cols], gsub, pattern = ",", replacement = ".", fixed = TRUE)
camada[, which_cols] <- lapply(camada[, which_cols], as.integer)
# Coalescer dados de argila total
camada[, "argila_sodio"] <- NA_real_
clay_cols <- paste0("argila_", c("sodio_pipeta", "sodio_densimetro", "sodio_xxx", "xxx_xxx"))
has_clay <- which(camada[, clay_cols] > 0, arr.ind = TRUE)
has_clay_duplicated <- duplicated(has_clay[, "row"])
if (any(has_clay_duplicated)) {
  stop(paste("there are", sum(has_clay_duplicated), "layers with duplicated CLAY measurements\n"))
} else {
  camada[has_clay[, "row"], "argila_sodio"] <- camada[, clay_cols][has_clay]
}
# Coalescer dados de silte
camada[, "silte_0002mm0050mm"] <- NA_real_
silt_cols <- paste0("silte_", c("0002mm0050mm_calc", "xxx_xxx"))
has_silt <- which(camada[, silt_cols] > 0, arr.ind = TRUE)
has_silt_duplicated <- duplicated(has_silt[, "row"])
if (any(has_silt_duplicated)) {
  stop(paste("there are", sum(has_silt_duplicated), "layers with duplicated SILT measurements\n"))
} else {
  camada[has_silt[, "row"], "silte_0002mm0050mm"] <- camada[, silt_cols][has_silt]
}
# Coalescer dados de areia
camada[, "areia_0050mm2000mm"] <- NA_real_
sand_cols <- paste0("areia_", c("0050mm2000mm_peneira", "xxx_xxx"))
has_sand <- which(camada[, sand_cols] > 0, arr.ind = TRUE)
has_sand_duplicated <- duplicated(has_sand[, "row"])
if (any(has_sand_duplicated)) {
  stop(paste("there are", sum(has_sand_duplicated), "layers with duplicated SAND measurements\n"))
} else {
  camada[has_sand[, "row"], "areia_0050mm2000mm"] <- camada[, sand_cols][has_sand]
}


# Corrigir distribuição do tamanho de partí
psd <- c("argila_sodio", "silte_0002mm0050mm", "areia_0050mm2000mm")
idx <- which(rowSums(camada[, psd]) > 1010)
rowSums(camada[idx, psd])
camada[idx, c("dataset_id", "evento_id_febr", "camada_id_febr", psd)]

plot(camada[, c("argila_sodio", "silte_0002mm0050mm")], cex = 0.5, pch = 20)
abline(a = 1000, b = -1)


par(mfrow = c(1, 2))
plot(camada[idx, c("argila_sodio", "silte_0002mm0050mm")], cex = 0.1)
text(camada[idx, c("argila_sodio", "silte_0002mm0050mm")], labels = camada[idx, "dataset_id"])
plot(camada[idx, c("argila_sodio", "silte_0002mm0050mm")], cex = 0.1)
text(camada[idx, c("argila_sodio", "silte_0002mm0050mm")], labels = camada[idx, "evento_id_febr"])

# camada <- febr::layer(data.set = "ctb0005", febr.repo = febr_repo, variable = vars)
# dim(camada)
# head(camada)
# colnames(camada)
# idx <- which(camada[["silte_xxx_xxx"]] > 0)
# unique(camada[["dataset_id"]][idx])

# class(camada[["argila_xxx_xxx"]])



class(camada[, "argila_sodio_xxx"])

for (i in has_clay_xxx) {
  has_clay_water <- all(is.na(camada[i, water_cols]))
  if (has_clay_water) {
    camada[i, "argila_sodio_xxx"] <- as.numeric(camada[i, "argila_xxx_xxx"])
  }
}
hist(as.numeric(camada[["argila_sodio_xxx"]]))
camada[["argila_sodio_xxx"]]
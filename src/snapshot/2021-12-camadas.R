# title: Instância de dezembro de 2021
# subtitle: Repositório de Dados do Solo Brasileiro
# author: Alessandro Samuel-Rosa
#
# Instalar última versão do pacote febr diretamente o GitHub
if (!require(remotes)) {
  install.packages(pkgs = "remotes")
}
remotes::install_github(repo = "laboratorio-de-pedometria/febr-package")
febr_repo <- "~/ownCloud/febr-repo/publico"
#
## 2021-12-camadas.txt ##############################################################################
# Campos exportados da tabela 'camada':
camada_cols <- c(
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
  # data.set = "ctb0804",
  variable = vars,
  # variable = "argila",
  stack = TRUE,
  # harmonization = list(harmonize = TRUE, level = 2),
  harmonization = list(harmonize = FALSE),
  standardization = list(
    plus.sign = "add",
    plus.depth = 20,
    lessthan.sign = "remove",
    # lessthan.frac = 0.5,
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
clay_cols <- c("argila_sodio_pipeta", "argila_sodio_densimetro", "argila_sodio_xxx", "argila_xxx_xxx")
has_clay <- which(camada[, clay_cols] > 0, arr.ind = TRUE)
has_clay_duplicated <- duplicated(has_clay[, "row"])
if (any(has_clay_duplicated)) {
  stop(paste0("there are ", sum(has_clay_duplicated), " layers with duplicated CLAY measurements\n"))
} else {
  camada[has_clay[, "row"], "argila_sodio"] <- camada[, clay_cols][has_clay]
}
# Coalescer dados de silte
camada[, "silte_0002mm0050mm"] <- NA_real_
silt_cols <- c("silte_0002mm0050mm_calc", "silte_xxx_xxx")
has_silt <- which(camada[, silt_cols] > 0, arr.ind = TRUE)
has_silt_duplicated <- duplicated(has_silt[, "row"])
if (any(has_silt_duplicated)) {
  stop(paste0("there are ", sum(has_silt_duplicated), " layers with duplicated SILT measurements\n"))
} else {
  camada[has_silt[, "row"], "silte_0002mm0050mm"] <- camada[, silt_cols][has_silt]
}




idx <- which(rowSums(camada[, c("argila_sodio", "silte_0002mm0050mm")]) == 1000)
camada[idx, c("dataset_id", "evento_id_febr", "camada_id_febr", "silte_0002mm0050mm", "argila_sodio")]

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
# title: Instantâneo de Junho de 2022 - camadas de dados
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
# Carregar funções auxiliares
source("febr-output/instantaneo/src/help.r")
## 2022-06-camadas.txt #############################################################################
instantaneo <- "2022-06"
# Campos exportados da tabela 'camada':
camada_cols <- c(
  "dados_id",
  "evento_id_febr",
  "camada_id_febr",
  "argila_sodio",
  "silte_0002mm0050mm",
  "areia_0050mm2000mm",
  "carbono"
)
# Descarregar dados utilizando o nível 3 de harmonização
vars <- c("argila_", "silte_", "areia_", "carbono_")
  # "carbono_", "argila_", "silte_")
  # , "areia_", "areiagrossa2_", "areiafina2_", ,
  # "terrafina_", "cascalho_", "calhau_",
  # "densidade_",
  # "ctc_", "ph_", "ce_",
  # "camada_nome")
camada <- febr::layer(
  data.set = "all",
  # data.set = "ctb0050",
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
camada[, which_cols] <- lapply(camada[, which_cols], as.numeric)

# Coalescer dados de ARGILA TOTAL
# A ordem das colunas determina a prioridade dos dados.
clay_cols <- paste0("argila_", c("sodio_pipeta", "sodio_densimetro", "sodio_xxx", "xxx_xxx"))
camada[, clay_cols] <- coalesce(camada[, clay_cols], target = "argila_sodio_xxx")

# Coalescer dados de SILTE TOTAL
# A ordem das colunas determina a prioridade dos dados.
silt_cols <- paste0("silte_", c("0002mm0050mm_calc", "xxx_xxx"))
camada[, silt_cols] <- coalesce(camada[, silt_cols], target = "silte_xxx_xxx")

# Coalescer dados de AREIA TOTAL
# A ordem das colunas determina a prioridade dos dados.
sand_cols <- paste0("areia_", c("0050mm2000mm_peneira", "xxx_xxx"))
camada[, sand_cols] <- coalesce(camada[, sand_cols], target = "areia_xxx_xxx")

# Coalescer dados de argila total
# camada[, "argila_sodio"] <- NA_real_
# clay_cols <- paste0("argila_", c("sodio_pipeta", "sodio_densimetro", "sodio_xxx", "xxx_xxx"))
# has_clay <- which(camada[, clay_cols] > 0, arr.ind = TRUE)
# has_clay_duplicated <- duplicated(has_clay[, "row"])
# if (any(has_clay_duplicated)) {
#   stop(paste("there are", sum(has_clay_duplicated), "layers with duplicated CLAY measurements\n"))
# } else {
#   camada[has_clay[, "row"], "argila_sodio"] <- camada[, clay_cols][has_clay]
# }

# Coalescer dados de silte
# camada[, "silte_0002mm0050mm"] <- NA_real_
# silt_cols <- paste0("silte_", c("0002mm0050mm_calc", "xxx_xxx"))
# has_silt <- which(camada[, silt_cols] > 0, arr.ind = TRUE)
# has_silt_duplicated <- duplicated(has_silt[, "row"])
# if (any(has_silt_duplicated)) {
#   stop(paste("there are", sum(has_silt_duplicated), "layers with duplicated SILT measurements\n"))
# } else {
#   camada[has_silt[, "row"], "silte_0002mm0050mm"] <- camada[, silt_cols][has_silt]
# }

# Coalescer dados de areia
# camada[, "areia_0050mm2000mm"] <- NA_real_
# sand_cols <- paste0("areia_", c("0050mm2000mm_peneira", "xxx_xxx"))
# has_sand <- which(camada[, sand_cols] > 0, arr.ind = TRUE)
# has_sand_duplicated <- duplicated(has_sand[, "row"])
# if (any(has_sand_duplicated)) {
#   stop(paste("there are", sum(has_sand_duplicated), "layers with duplicated SAND measurements\n"))
# } else {
#   camada[has_sand[, "row"], "areia_0050mm2000mm"] <- camada[, sand_cols][has_sand]
# }

# psd <- c("argila_sodio", "silte_0002mm0050mm", "areia_0050mm2000mm")

# Verificar consistência da distribuição do tamanho de partículas
# A soma das frações deve ser igual a 1000 g/kg. Informações sobre as camadas em que a soma das
# frações é diferente de 1000 g/kg são escritas no arquivo <bug/2022-06-psd-total.txt> para
# posterior verificação manual na planilha de dados.
psd <- c("argila_sodio_xxx", "silte_xxx_xxx", "areia_xxx_xxx")
camada[["psd_total"]] <- rowSums(camada[, psd])
idx_not_1000 <- which(camada[["psd_total"]] != 1000)
bug_psd_total <-
  camada[idx_not_1000, c("dataset_id", "evento_id_febr", "camada_id_febr", psd, "psd_total")]
writeBug(object = bug_psd_total, file.name = paste0(instantaneo, "-psd-total.txt"))

# Corrigir distribuição do tamanho de partículas, distribuindo o erro linearmente entre as três
# frações de tamanho. Quando isso não for suficiente, atribuir o erro ao silte.
camada[idx_not_1000, psd] <- round(
  camada[idx_not_1000, psd] * 1000 / camada[idx_not_1000, "psd_total"])
bug_psd_total <- rowSums(camada[idx_not_1000, psd])
camada[idx_not_1000, "silte_0002mm0050mm"] <-
  camada[idx_not_1000, "silte_0002mm0050mm"] + (1000 - bug_psd_total)

# Coalescer dados de CARBONO
camada[, "carbono"] <- NA_real_
carbon_cols <- paste0("carbono_",
  c("forno_1min950_cgdct", "cromo_30min150_mohr", "cromo_5min150_mohr", "cromo_xxx_mohr",
  "cromo_xxx_xxx", "forno_xxx_xxx", "xxx_xxx_xxx"))
has_carbon <- which(camada[, carbon_cols] > 0, arr.ind = TRUE)
has_carbon_duplicated <- duplicated(has_carbon[, "row"])
if (any(has_carbon_duplicated)) {
  stop(paste("there are", sum(has_carbon_duplicated), "layers with duplicated CARBON measurements\n"))
} else {
  camada[has_carbon[, "row"], "carbono"] <- camada[, carbon_cols][has_carbon]
}
cmd <- camada






# Atualizar campos dataset_id -> dados_id
which_cols <- match("dataset_id", colnames(camada))
colnames(camada)[which_cols] <- "dados_id"
# Escrever tabela em disco
writeTable(object = camada[, camada_cols], file.name = paste0(instantaneo, "-camadas.txt"))



# camada <- febr::layer(data.set = "ctb0005", febr.repo = febr_repo, variable = vars)
# dim(camada)
# head(camada)
# colnames(camada)
# idx <- which(camada[["silte_xxx_xxx"]] > 0)
# unique(camada[["dataset_id"]][idx])

# class(camada[["argila_xxx_xxx"]])

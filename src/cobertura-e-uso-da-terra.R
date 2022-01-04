observacao <- febr::observation(
  data.set = "all",
  variable = c("terra_", "fito_", "coord_descricao"),
  stack = TRUE,
  febr.repo = "~/ownCloud/febr-repo/publico")
observacao[, "terra_usoatual"] <- paste0(observacao[, "terra_usoatual"], " ",
  observacao[, "terra_manejo"], " ", observacao[, "terra_cultura"])

observacao[, "terra_usoatual"] <- ifelse(
  observacao[, "terra_usoatual"] == "NA NA NA", NA_character_, observacao[, "terra_usoatual"])
observacao[, "terra_usoatual"] <- gsub("  ", "", observacao[, "terra_usoatual"])
observacao[, "terra_usoatual"] <- gsub(" NA NA", "", observacao[, "terra_usoatual"])
observacao[, "terra_usoatual"] <- gsub("NA NA ", "", observacao[, "terra_usoatual"])
observacao[, "terra_usoatual"] <- gsub("^NA ", "", observacao[, "terra_usoatual"])
observacao[, "terra_usoatual"] <- gsub(" NA ", " ", observacao[, "terra_usoatual"])
observacao[, "terra_usoatual"] <- gsub(" NA$", "", observacao[, "terra_usoatual"])
observacao[, "terra_usoatual"] <- gsub(" $", "", observacao[, "terra_usoatual"])
observacao[, "terra_usoatual"] <- gsub(".", "", observacao[, "terra_usoatual"], fixed = TRUE)

isna_terra <- is.na(observacao[, "terra_usoatual"])
isna_estado <- is.na(observacao[, "estado_sigla"])
isna <- (isna_terra + isna_estado) >= 1
terra_dados <- observacao[!isna, c("dataset_id", "observacao_id", "estado_sigla", "coord_descricao", "fito_primaria", "terra_usoatual")]
dim(terra_dados)
head(terra_dados)

n_amostras <- nrow(terra_dados)
set.seed(2021)
idx_treinamento <- sample(1:n_amostras, round(n_amostras / 2))
terra_dados[idx_treinamento, "treinamento"] <- TRUE
terra_dados[-idx_treinamento, "treinamento"] <- FALSE
mix_samples <- sample(1:n_amostras)
terra_dados <- terra_dados[mix_samples, ]
write.table(
  terra_dados[terra_dados[, "treinamento"] == TRUE, c("terra_usoatual")],
  "tmp/terra-dados-treinamento.csv", row.names = FALSE)
sample_order <- order(terra_dados[terra_dados[, "treinamento"] == FALSE, "terra_usoatual"])
write.table(
  terra_dados[terra_dados[, "treinamento"] == FALSE, ][sample_order, ],
  "tmp/terra-dados-validacao.csv", row.names = FALSE)

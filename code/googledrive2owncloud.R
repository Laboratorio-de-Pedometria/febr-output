# Transição do Google Drive para ownCloud
# Autor: Alessandro Samuel-Rosa
# Data: 2020-04-11

# Identificar endereço local do repositório e conjuntos de dados
repo <- path.expand('~/ownCloud/febr-repo')
dir <- list.dirs(repo)[-1]
ctb <- basename(dir)

febr_template <-
  openxlsx::loadWorkbook(
    file = 'https://docs.google.com/spreadsheets/d/1rXIiT1zSYhFegSdAvE0yJX16q-bvXVNpYIYdd5YgjhI/export?format=xlsx')
  
# identificacao ###############################################################################################

# A tabela 'dataset' precisa ser renomeada como 'identificacao'
for (i in 1:length(dir)) {
  old <- paste(dir[i], '/', ctb[i], '-', 'dataset', '.xlsx', sep = '')
  new <- paste(dir[i], '/', ctb[i], '-', 'identificacao', '.xlsx', sep = '')
  file.rename(from = old, to = new)
}

# A estrutura de armazenamento de dados da tabela 'identificacao' foi alterado
# Aqui essa estrutura é atualizada
for (i in 1:length(dir)) {
  
  print(ctb[i])
  
  # Carregar dados
  xlsxFile <- list.files(path = dir[i], pattern = 'identificacao.xlsx', full.names = TRUE)
  file <- openxlsx::read.xlsx(xlsxFile = xlsxFile)
  
  # organizacao_local
  endereco <- 
    c("organizacao_rua_nome", 
      "organizacao_rua_numero",
      "organizacao_municipio_id",
      "organizacao_pais_id",
      "organizacao_codigo_postal")
  file[file$campo == endereco[1], "valor"] <- 
    paste(file[match(endereco, file$campo), "valor"], sep = '', collapse = ', ')
  file <- file[!(file$campo %in% endereco[-1]), ]
  file[file$campo == endereco[1], "campo"] <- "organizacao_local"
  
  # autor_nome_email
  file[file$campo == 'autor_nome', "campo"] <- "autor_nome_email"
  file[file$campo == 'autor_nome_email', "valor"] <-
    paste(apply(cbind(
      strsplit(file[file$campo == 'autor_nome_email', "valor"], split = '; ')[[1]], 
      ' (',
      strsplit(file[file$campo == 'autor_email', "valor"], split = '; ')[[1]],
      ')'
    ), 1, paste, collapse = ''), collapse = '; ')
  file <- file[file$campo != 'autor_email', ]
  
  # dataset_referencia
  file[startsWith(file$campo, 'dataset_referencia_1'), 'campo'] <- 'dataset_referencia'
  file[startsWith(file$campo, 'dataset_referencia'), 'valor'] <- 
    paste(file[startsWith(file$campo, 'dataset_referencia'), 'valor'], collapse = '; ')
  file <- file[!startsWith(file$campo, 'dataset_referencia_'), ]
  
  # Outras colunas a seres descartadas
  drop <- 
    c("dataset_versao", "publicacao_data", "contribuidor_nome", "contribuidor_email", 
      "contribuidor_organizacao", "categoria_vcge")
  file <- file[!(file$campo %in% drop), ]
  
  # salvar arquivo xlsx
  xlsxFile <- paste(dir[i], '/new-', ctb[i], '-identificacao.xlsx', sep = '')
  openxlsx::write.xlsx(x = file, file = xlsxFile, overwrite = TRUE) 
}

# metadado ####################################################################################################

# Inúmeros conjuntos de dados não possuem metadados, ou então um arquivo alternativo denominado 
# ctb0000-metadado.csv. Quando presente, esse arquivo é removido
for (i in 1:length(dir)) {
  files_found <- list.files(path = paste(dir[i]))
  if ("ctb0000-metadado.csv" %in% files_found) {
    file.remove(paste(dir[i], "/ctb0000-metadado.csv", sep = ''))
  }
  if (!paste(ctb[i], '-metadado.xlsx', sep = '') %in% files_found) {
    openxlsx::write.xlsx(
      x = openxlsx::read.xlsx(xlsxFile = febr_template, sheet = 'metadado'),
      file = paste(dir[i], "/", ctb[i], "-metadado.xlsx", sep = ''))
  }
}

# os nomes das colunas de 'observacao' e 'camada' coincidem com aqueles de 'metadado'?
subs <- read.csv('data/googledrive2owncloud-gsub-metadado.csv', stringsAsFactors = FALSE)
for (i in 1:length(dir)) {
  print(ctb[i])
  meta <- paste(dir[i], "/", ctb[i], "-metadado.xlsx", sep = '')
  meta <- openxlsx::read.xlsx(xlsxFile = meta)
  if (any(meta$campo_id %in% subs$pattern)) {
    idx <- which(meta$campo_id %in% subs$pattern)
    meta$campo_id[idx] <- subs[na.omit(match(meta$campo_id[idx], subs$pattern)), "replacement"]
    openxlsx::write.xlsx(x = meta, file = paste(dir[i], "/", ctb[i], "-metadado.xlsx", sep = ''))
  }
}

# todas as variáveis de 'observacao' e 'camada' estão em 'metadado'?
# se não estão, então adiciona-se as linhas, com campo_id, campo_nome, e campo_unidade
for (i in 1:length(dir)) {
  print(ctb[i])
  meta <- paste(dir[i], "/", ctb[i], "-metadado.xlsx", sep = '')
  meta <- openxlsx::read.xlsx(xlsxFile = meta)
  meta$tabela_id <- gsub('observação', 'observacao', meta$tabela_id)
  meta$tabela_id <- gsub('observacao ', 'observacao', meta$tabela_id)
  meta$tabela_id <- gsub('camada ', 'camada', meta$tabela_id)
  obs <- paste(dir[i], "/", ctb[i], "-observacao.xlsx", sep = '')
  obs <- openxlsx::read.xlsx(xlsxFile = obs, rows = 1:3)
  obs_in <- colnames(obs) %in% meta$campo_id
  cam <- paste(dir[i], "/", ctb[i], "-camada.xlsx", sep = '')
  cam <- openxlsx::read.xlsx(xlsxFile = cam, rows = 1:3)
  cam_in <- colnames(cam) %in% meta$campo_id
  if (!all(obs_in)) {
    tmp_o <- data.frame(matrix(rep(NA, sum(!obs_in) * ncol(meta)), ncol = ncol(meta)))
    colnames(tmp_o) <- colnames(meta)
    tmp_o$tabela_id <- 'observacao'
    tmp_o$campo_id <- colnames(obs[!obs_in])
    tmp_o$campo_nome <- as.character(obs[1, !obs_in])
    tmp_o$campo_unidade <- as.character(obs[2, !obs_in])
  } else {
    tmp_o <- data.frame(matrix(rep(NA, ncol(meta)), ncol = ncol(meta)))
    colnames(tmp_o) <- colnames(meta)
  }
  if (!all(cam_in)) {
    tmp_c <- data.frame(matrix(rep(NA, sum(!cam_in) * ncol(meta)), ncol = ncol(meta)))
    colnames(tmp_c) <- colnames(meta)
    tmp_c$tabela_id <- 'camada'
    tmp_c$campo_id <- colnames(cam[!cam_in])
    tmp_c$campo_nome <- as.character(cam[1, !cam_in])
    tmp_c$campo_unidade <- as.character(cam[2, !cam_in])
  } else {
    tmp_c <- data.frame(matrix(rep(NA, ncol(meta)), ncol = ncol(meta)))
    colnames(tmp_c) <- colnames(meta)
  }
  meta <- rbind(
    meta[meta$tabela_id == 'observacao', ], tmp_o, 
    meta[meta$tabela_id == 'camada', ], tmp_c)
  meta <- meta[meta$campo_id != '...', ]
  meta <- meta[!is.na(meta$tabela_id), ]
  
  if (length(which(!(c(colnames(cam), colnames(obs)) %in% meta$campo_id))) == 0) {
    openxlsx::write.xlsx(
      x = meta, file = paste(dir[i], "/", ctb[i], "-metadado.xlsx", sep = ''), overwrite = TRUE)
  } else {
    stop(which(!(c(colnames(cam), colnames(obs)) %in% meta$campo_id)))
  }
}

# historico ###################################################################################################

# Inúmeros conjuntos de dados não possuem historico, ou então um arquivo alternativo denominado 
# ctb0000-historico.csv. Quando presente, esse arquivo é removido. Quando ausente, um arquivo é criado.
for (i in 1:length(dir)) {
  files_found <- list.files(path = paste(dir[i]))
  if ("ctb0000-historico.csv" %in% files_found) {
    file.remove(paste(dir[i], "/ctb0000-historico.csv", sep = ''))
  }
  if (!paste(ctb[i], '-historico.xlsx', sep = '') %in% files_found) {
    openxlsx::write.xlsx(
      x = openxlsx::read.xlsx(xlsxFile = febr_template, sheet = 'historico'),
      file = paste(dir[i], "/", ctb[i], "-historico.xlsx", sep = ''))
  }
}

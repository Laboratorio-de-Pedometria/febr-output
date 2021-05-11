# Transição do Google Drive para ownCloud
# Autor: Alessandro Samuel-Rosa
# Data: 2020-04-11

# Estrutura de diretórios #####################################################################################
# Identificar endereço local do repositório
# Preparar estrutura local de diretórios
ownCloud <- path.expand('~/ownCloud')
repo <- path.expand('~/ownCloud/febr-repo')
publico <- paste(repo, '/', 'publico', sep = '')
if (!dir.exists(publico)) {
  cmd <- paste('mkdir', publico)
  system(cmd)
}
processamento <- paste(repo, '/', 'processamento', sep = '')
if (!dir.exists(processamento)) {
  cmd <- paste('mkdir', processamento)
  system(cmd)
}

# Dados do GoogleDrive ########################################################################################
# remover diretórios e arquivos existentes
cmd <- paste('rm -rf ', list.dirs(processamento)[-1], sep = '')
lapply(cmd, system)

# descompactar ZIP descarregado do Google Drive, (re)movendo diretórios e arquivos
cmd <- paste("unzip -o '", processamento, "/febr-repo.zip' -d '", processamento, "/'", sep = '')
system(cmd)
cmd <- paste('mv -v ', processamento, '/febr-repo/* ', processamento, '/',  sep = '')
system(cmd)
cmd <- paste('rm -rf ', processamento, '/febr-repo', sep = '')
system(cmd)

# Objetos, funções e dados auxiliares #########################################################################
# Identificar diretórios e conjuntos de dados existentes
dir <- list.dirs(processamento)[-1]
ctb <- basename(dir)

# Criar diretórios públicos caso não existam
dir_publico <- sub('processamento', 'publico', dir)
parallel::mclapply(dir_publico, function (x) {
  if (!file.exists(x)) {
    cmd <- paste('mkdir', dir_publico)
    system(cmd)
  }
})

# Descarregar planilha modelo
gs_key <- '1rXIiT1zSYhFegSdAvE0yJX16q-bvXVNpYIYdd5YgjhI'
febr_template <-
  openxlsx::loadWorkbook(
    file = paste('https://docs.google.com/spreadsheets/d/', gs_key, '/export?format=xlsx', sep = ''))
for (i in 1:length(febr_template$comments)) {
  for (j in 1:length(febr_template$comments[[i]])) {
    febr_template$comments[[i]][[j]]$style <- 
      "<rPr><sz val=\"11\"/><color rgb=\"FF000000\"/><name val=\"Inconsolata\"/></rPr>"
  }
}
openxlsx::saveWorkbook(febr_template, file = 'data/febr-modelo-planilha-geral.xlsx', overwrite = TRUE)

# Funções auxiliares
verbose <- 
  function() {
    print(paste(i, ":", ctb[i]))
  }
getProcessFile <- 
  function (ctb, table, 
            ownCloud = path.expand('~/ownCloud'), 
            repo = path.expand('~/ownCloud/febr-repo'),
            processamento = paste(repo, '/', 'processamento', sep = '')) {
    
    paste(processamento, '/', ctb, '/', ctb, '-', table, '.xlsx', sep = '')
  }
getBaseFile <- 
  function (ctb, table, 
            ownCloud = path.expand('~/ownCloud'), 
            repo = path.expand('~/ownCloud/febr-repo'),
            processamento = paste(repo, '/', 'processamento', sep = '')) {
    
    paste(processamento, '/', ctb, '/base/', ctb, '-', table, '.xlsx', sep = '')
  }
# Geral #######################################################################################################
# Criar tabelas faltantes
# Deletar arquivos desnecessários
# Renomear tabelas existentes

# identificacao
# A tabela 'dataset' precisa ser renomeada como 'identificacao'
for (i in 1:length(ctb)) {
  # i <- 1
  verbose()
  old <- getProcessFile(ctb = ctb[i], table = 'dataset')
  new <- paste(processamento, '/', ctb[i], '/', ctb[i], '-', 'identificacao', '.xlsx', sep = '')
  file.rename(from = old, to = new)
}

# versionamento
# Inúmeros conjuntos de dados não possuem histórico, ou então um arquivo alternativo denominado 
# ctb0000-historico.csv. Quando presente, esse arquivo é removido, criando-se um arquivo XLSX baseado no modelo
# do FEBR em sua substituição. Como apenas dados descarregados do SISB apresentam esse problema, a tabela é 
# preenchida com dados.
# A tabela 'historico' precisa ser renomeada como 'versionamento'
for (i in 1:length(ctb)) {
  # i <- 42
  # i <- 1
  verbose()
  files_found <- list.files(path = paste(dir[i]))
  if ("ctb0000-historico.csv" %in% files_found) {
    file.remove(paste(dir[i], "/ctb0000-historico.csv", sep = ''))
    
  } else if (paste(ctb[i], "-historico.xlsx", sep = '') %in% files_found) {
    old <- getProcessFile(ctb = ctb[i], table = 'historico')
    new <- paste(processamento, '/', ctb[i], '/', ctb[i], '-', 'versionamento', '.xlsx', sep = '')
    file.rename(from = old, to = new)
  } else if (!paste(ctb[i], '-historico.xlsx', sep = '') %in% files_found) {
    versao_modelo <- openxlsx::read.xlsx(xlsxFile = febr_template, sheet = 'versionamento')
    versionamento <- 
      rbind(
        c(
          '0.0.0',
          '2017-04-26',
          'Alessandro Samuel-Rosa (alessandrorosa@utfpr.edu.br)',
          'Entrada do conjunto de dados descarregados do Sistema de Informação de Solos Brasileiros'
          ),
        c(
          '1.0.0',
          '2017-06-28',
          'Alessandro Samuel-Rosa (alessandrorosa@utfpr.edu.br)',
          'Criação de tabelas para atender à estrutura do repositório'
          ),
        c(
          '2.0.0',
          '2017-09-07',
          'Alessandro Samuel-Rosa (alessandrorosa@utfpr.edu.br)',
          'Adição/remoção de colunas e linhas para atender à estrutura do repositório'
          ))
    versionamento <- as.data.frame(versionamento, stringsAsFactors = FALSE)
    colnames(versionamento) <- colnames(versao_modelo)
    versionamento$versao_data <- as.Date(versionamento$versao_data, format = "%Y-%m-%d")
    openxlsx::write.xlsx(
      x = versionamento, keepNA = TRUE, file = paste(dir[i], "/", ctb[i], "-versionamento.xlsx", sep = ''))
  }
}

# metadado
# Inúmeros conjuntos de dados não possuem metadados, ou então um arquivo alternativo denominado 
# ctb0000-metadado.csv. Quando presente, esse arquivo é removido, criando-se um arquivo XLSX baseado no modelo
# do FEBR em sua substituição.
for (i in 1:length(ctb)) {
  # i <- 1
  verbose()
  files_found <- list.files(path = paste(processamento, '/', ctb[i], sep = ''))
  if ("ctb0000-metadado.csv" %in% files_found) {
    file.remove(paste(processamento, '/', ctb[i], "/ctb0000-metadado.csv", sep = ''))
  }
  if (!paste(ctb[i], '-metadado.xlsx', sep = '') %in% files_found) {
    meta <- openxlsx::read.xlsx(xlsxFile = febr_template, sheet = 'metadado')[1:5, ]
    meta <- 
      openxlsx::write.xlsx(
        x = meta, file = paste(processamento, '/', ctb[i], "/", ctb[i], "-metadado.xlsx", sep = ''))
  }
}

# organizar dados originais em diretório 'base'
base_dir <- paste(dir, '/base', sep = '')
if (!all(file.exists(base_dir))) {
  cmd <- paste('mkdir', base_dir)
  lapply(cmd, system)
  for (i in 1:length(dir)) {
    cmd <- paste('mv -v ', dir[i], '/* ', base_dir[i], '/', sep = '')
    system(cmd)
  }
}

# identificacao ###############################################################################################
# A estrutura de armazenamento de dados da tabela 'identificacao' precisa ser atualizada
# Campos são renomeados
# Campos são removidos
# Campos são acrescentados
campos <- openxlsx::read.xlsx(febr_template, 'identificacao')$campo
for (i in 1:length(ctb)) {
  # i <- 1
  verbose()
  
  # Carregar dados
  in_file <- getBaseFile(ctb[i], 'identificacao')
  identificacao <- openxlsx::read.xlsx(xlsxFile = in_file, detectDates = TRUE, na.strings = c("NA", '','-'))
  
  # substituir dataset_ por dados_
  identificacao$campo[startsWith(x = identificacao$campo, 'dataset_')] <- 
    sub('dataset_', 'dados_', identificacao$campo[startsWith(x = identificacao$campo, 'dataset_')])
  
  # autor_nome_email
  identificacao[identificacao$campo == 'autor_nome', "campo"] <- "autor_nome_email"
  autor <- strsplit(identificacao[identificacao$campo == 'autor_nome_email', "valor"], split = '; ')[[1]]
  email <- strsplit(identificacao[identificacao$campo == 'autor_email', "valor"], split = '; ')[[1]]
  if (length(autor) > length(email)) {
    email <- c(email, rep(NA_character_, length(autor) - length(email)))
  }
  identificacao[identificacao$campo == 'autor_nome_email', "valor"] <-
    paste(autor, " (", email, ")", sep = '', collapse = "; ")
  
  # organizacao_local
  endereco <- 
    c("organizacao_rua_nome", 
      "organizacao_rua_numero",
      "organizacao_municipio_id",
      "organizacao_pais_id",
      "organizacao_codigo_postal")
  identificacao[identificacao$campo == endereco[1], "valor"] <- 
    paste(na.exclude(identificacao[match(endereco, identificacao$campo), "valor"]), sep = '', collapse = ', ')
  identificacao[identificacao$campo == endereco[1], "campo"] <- "organizacao_local"
  
  # dataset_referencia
  identificacao[startsWith(identificacao$campo, 'dados_referencia_1'), 'campo'] <- 'dados_referencia'
  identificacao[startsWith(identificacao$campo, 'dados_referencia'), 'valor'] <- 
    paste(identificacao[startsWith(identificacao$campo, 'dados_referencia'), 'valor'], collapse = '; ')
  identificacao <- identificacao[!startsWith(identificacao$campo, 'dados_referencia_'), ]
  
  # publicacao_data
  identificacao[identificacao$campo == 'publicacao_data', "campo"] <- "criacao_data"
  
  # Outras linhas a seres descartadas
  drop <- c(
    "dados_versao", 
    "autor_email",
    "contribuidor_nome", 
    "contribuidor_email", 
    "contribuidor_organizacao", 
    "categoria_vcge",
    endereco[-1]
  )
  identificacao <- identificacao[!(identificacao$campo %in% drop), ]
  
  # linhas a serem adicionadas
  identificacao <-
    rbind(
      identificacao,
      c('dados_completude', NA_character_),
      c('dados_consistencia', 'consistência estrutural'))
  
  # Organizar linhas
  identificacao <- identificacao[match(identificacao$campo, campos), ]
  
  # salvar dados processados
  out_file <- sub('xlsx', 'csv', in_file)
  out_file <- sub('/base', '', out_file)
  write.csv(identificacao, file = out_file, row.names = FALSE)
}
rm(autor, campos, cmd, drop, email, endereco, files_found, gs_key, i, identificacao, in_file, j, meta, new, 
   old, out_file, versionamento)

# versionamento ###############################################################################################
# A estrutura de armazenamento de dados da tabela 'versionamento' de alguns conjuntos de dados precisa ser
# atualizada.
# Redefinir nomes das colunas
# Corrigir dados
# Corrigir datas e versão
versao_modelo <- openxlsx::read.xlsx(febr_template, 'versionamento')
# for (i in 201:240) {
for (i in 1:length(ctb)) {
  # i <- 41 # cols1
  # i <- 42
  # i <- 227
  # i <- 199
  # i <- 1
  verbose()
  in_file <- getBaseFile(ctb[i], 'versionamento')
  versionamento <- openxlsx::read.xlsx(in_file, na.strings = c("NA", '', 'n/a'), detectDates = TRUE)
  
  # nomes de colunas existentes em diferentes conjuntos de dados
  cols1 <- c("INICIADO.EM", "RESPONSÁVEL", "TAREFA", "VISTO.EM", "OBSERVAÇÕES", "CONCLUÍDO.EM")
  cols2 <- strsplit(c('dataset_versao publicacao_data contribuidor_nome atividade_descricao'), ' ')[[1]]
  cols3 <- strsplit(c('dataset_versao publicacao_data contribuidor_nome tarefa_descricao'), ' ')[[1]]
  if(all(colnames(versionamento) %in% cols2) | all(colnames(versionamento) %in% cols3)) {
    colnames(versionamento) <- colnames(versao_modelo)
  }
  if (all(colnames(versionamento) %in% cols1)) {
    versionamento <- rbind(versao_modelo, rep(NA_character_, ncol(versao_modelo)), stringsAsFactors = FALSE)
    colnames(versionamento) <- colnames(versao_modelo)
  }
  
  # versao_data
  if (all(is.numeric(versionamento$versao_data))) {
    versionamento$versao_data <- as.Date(versionamento$versao_data, "1900-01-01")
  }
  if (is.character(versionamento$versao_data)) {
    versionamento$versao_data <- 
      sapply(versionamento$versao_data, function (x) {
        if (is.na(suppressWarnings(as.numeric(x)))) {
          if (is.na(as.Date(x, "%d/%m/%Y"))) {
            as.Date(x, "%d-%m-%Y")
          } else {
            as.Date(x, "%d/%m/%Y")
          }
        } else {
          as.Date(as.numeric(x), "1900-01-01")
        }
      })
  }
  if (!inherits(versionamento$versao_data, 'Date')) {
    versionamento$versao_data <- as.Date(versionamento$versao_data, origin = "1970-01-01")
  }
  
  # dados_versao
  # a versão de um conjunto de dados é um dado com cinco caracteres
  if (any(nchar(versionamento$dados_versao) != 5, na.rm = TRUE)) {
    versionamento$dados_versao[nchar(versionamento$dados_versao) == 3] <- 
      paste(versionamento$dados_versao[nchar(versionamento$dados_versao) == 3], '.0', sep = '')
    if (any(nchar(versionamento$dados_versao) > 5) & ctb[i] != 'ctb0005') {
      versionamento$dados_versao[nchar(versionamento$dados_versao) > 5] <- NA_character_
    }
  }
  
  # a primeira versão de todos os conjuntos de dados passa a ser 0.0.0
  if (as.integer(sub('ctb', '', ctb[i])) > 100 & nrow(versionamento) < 3 | 
      versionamento$dados_versao[1] == '2.0.0' | versionamento$dados_versao[1] == '2.1.0') {
      if (nrow(versionamento) == 1) {
        versionamento <- versionamento[-1, ]
        # if (versionamento$dados_versao == '0.0.0' | is.na(versionamento$dados_versao)) {
        #   versionamento$dados_versao <- '2.0.0'
        #   versionamento$versao_data <- as.Date('2017-09-07')
        # }
      }
    versionamento <- rbind(
      c(
        '0.0.0',
        '2017-04-26',
        'Alessandro Samuel-Rosa (alessandrorosa@utfpr.edu.br)',
        'Entrada do conjunto de dados descarregados do Sistema de Informação de Solos Brasileiros'
        ),
      c(
        '1.0.0',
        '2017-06-28',
        'Alessandro Samuel-Rosa (alessandrorosa@utfpr.edu.br)',
        'Criação de tabelas para atender à estrutura do repositório'
        ),
      c(
        '2.0.0',
        '2017-09-07',
        'Alessandro Samuel-Rosa (alessandrorosa@utfpr.edu.br)',
        'Adição/remoção de colunas e linhas para atender à estrutura do repositório'
        ),
      versionamento,
      stringsAsFactors = FALSE
    )
    colnames(versionamento) <- colnames(versao_modelo)
    if (versionamento$dados_versao[4] %in% c('2', '2.0', '2.0.0')) {
      versionamento <- versionamento[-4, ]
    }
  }

  # versao_data
  if (nrow(versionamento) > 0 & versionamento$versao_data[1] < as.Date('2017-01-01')) {
    versionamento$versao_data[1] <- as.Date('2017-04-26')
  }

  # nova versão
  dados_versao <- 
    as.integer(strsplit(versionamento$dados_versao[nrow(versionamento)], split = '.', fixed = TRUE)[[1]])
  dados_versao[1] <- dados_versao[1] + 1
  dados_versao[2:3] <- 0
  dados_versao <- paste(dados_versao, collapse = '.')
  versionamento <- rbind(
    versionamento,
    c(dados_versao, 
      format(Sys.time(), "%Y-%m-%d"),
      'Alessandro Samuel-Rosa (alessandrorosa@utfpr.edu.br)',
      'Transferência do GoogleDrive para servidor da UTFPR; Adota novos padrões de armazenamento dos dados')
    )
  
  # editor_nome_email
  versionamento$editor_nome_email <- as.character(versionamento$editor_nome_email)
  versionamento$editor_nome_email[is.na(versionamento$editor_nome_email)] <-
    'Alessandro Samuel-Rosa (alessandrorosa@utfpr.edu.br)'
  versionamento$editor_nome_email[versionamento$editor_nome_email == 'Alessandro Samuel-Rosa'] <-
    'Alessandro Samuel-Rosa (alessandrorosa@utfpr.edu.br)'
  
  # salvar dados processamentoados
  out_file <- sub('xlsx', 'csv', in_file)
  out_file <- sub('/base', '', out_file)
  write.csv(versionamento, file = out_file, row.names = FALSE)
}
rm(cols1, cols2, cols3, dados_versao, i, in_file, versionamento)

# metadado ####################################################################################################
# 1. realizar alterações anteriomente feitas usando a API do Google Sheets nomes das colunas de 'observacao'
# e 'camada'
# 2. todas as variáveis de 'observacao' e 'camada' estão em 'metadado'? se não estão, então adiciona-se as 
# linhas, inclusive campo_id, campo_nome, e campo_unidade
# 3. a estrutura da tabela 'metadado' mudou no que diz respeito ao armazenamento das informações dos
# laboratórios. Aqui atualizamos. Os campos existenes agora são lab_nome e lab_endereco. Alguns conjuntos de 
# dados também possuem um campo chamado 'metodo_id' que eliminamos aqui
# 4. precisamos verificar se os campos campo_unidade e campo_tipo estão todos de acordo com os padrões
# isso inclui substituir o valor 'sem unidade' por um valor padrão
subs <- read.csv('data/googledrive2owncloud-gsub-metadado.csv', stringsAsFactors = FALSE)
glossario <- openxlsx::read.xlsx(xlsxFile = febr_template, sheet = 'glossario')
for (i in 1:length(ctb)) {
  # i <- 17
  verbose()
  in_file <- getBaseFile(ctb[i], 'metadado')
  metadado <- openxlsx::read.xlsx(xlsxFile = in_file, na.strings = c('NA', '', 'n/a'))
  
  # 1. realizar alterações anteriomente feitas usando a API do Google Sheets nomes das colunas de 'observacao'
  # e 'camada'
  if (any(metadado$campo_id %in% subs$pattern)) {
    idx <- which(metadado$campo_id %in% subs$pattern)
    metadado$campo_id[idx] <- subs[na.omit(match(metadado$campo_id[idx], subs$pattern)), "replacement"]
  }
  
  # 2. todas as variáveis de 'observacao' e 'camada' estão em 'metadado'? se não estão, então adiciona-se as 
  # linhas, inclusive campo_id, campo_nome, e campo_unidade
  metadado$tabela_id <- sub('observação', 'observacao', metadado$tabela_id)
  metadado$tabela_id <- sub('observacao ', 'observacao', metadado$tabela_id)
  metadado$tabela_id <- sub('camada ', 'camada', metadado$tabela_id)
  
  # carregar tabelas 'observacao' e 'camada'
  obs_file <- getBaseFile(ctb[i], 'observacao')
  obs <- openxlsx::read.xlsx(xlsxFile = obs_file, rows = 1:3, check.names = TRUE)
  cam_file <- getBaseFile(ctb[i], 'camada')
  cam <- openxlsx::read.xlsx(xlsxFile = cam_file, rows = 1:3, check.names = TRUE)
  
  obs_in <- colnames(obs) %in% metadado[metadado$tabela_id == 'observacao', 'campo_id']
  cam_in <- colnames(cam) %in% metadado[metadado$tabela_id == 'camada', 'campo_id']
  
  # processamentoar dados da 'observacao'
  if (!all(obs_in)) {
    tmp_o <- data.frame(matrix(rep(NA, sum(!obs_in) * ncol(metadado)), ncol = ncol(metadado)))
    colnames(tmp_o) <- colnames(metadado)
    tmp_o$tabela_id <- 'observacao'
    tmp_o$campo_id <- colnames(obs[!obs_in])
    tmp_o$campo_nome <- as.character(obs[1, !obs_in])
    tmp_o$campo_unidade <- as.character(obs[2, !obs_in])
    metadado <- rbind(
      metadado[metadado$tabela_id == 'observacao', ], tmp_o, metadado[metadado$tabela_id == 'camada', ])
    
    # remover linhas em 'metadado' que não estão em 'observacao'
    in_obs <- metadado[metadado$tabela_id == 'observacao', 'campo_id'] %in% colnames(obs)
    metadado <- rbind(
      metadado[metadado$tabela_id == 'observacao', ][in_obs, ], metadado[metadado$tabela_id == 'camada', ])
  }
  
  # processamentoar dados da 'camada'
  if (!all(cam_in)) {
    tmp_c <- data.frame(matrix(rep(NA, sum(!cam_in) * ncol(metadado)), ncol = ncol(metadado)))
    colnames(tmp_c) <- colnames(metadado)
    tmp_c$tabela_id <- 'camada'
    tmp_c$campo_id <- colnames(cam[!cam_in])
    tmp_c$campo_nome <- as.character(cam[1, !cam_in])
    tmp_c$campo_unidade <- as.character(cam[2, !cam_in])
    metadado <- rbind(
      metadado[metadado$tabela_id == 'observacao', ], metadado[metadado$tabela_id == 'camada', ], tmp_c)
    
    # remover linhas em 'metadado' que não estão em 'camada'
    in_cam <- metadado[metadado$tabela_id == 'camada', 'campo_id'] %in% colnames(cam)
    metadado <- rbind(
      metadado[metadado$tabela_id == 'observacao', ], metadado[metadado$tabela_id == 'camada', ][in_cam, ])
  }
  metadado <- metadado[metadado$campo_id != '...', ]
  metadado <- metadado[!is.na(metadado$tabela_id), ]
  
  # agora que todas as colunas estão contidas
  # colocar linhas de 'metadado' na mesma ordem das colunas de 'camada' e 'observacao'
  idx <- match(colnames(obs), metadado[metadado$tabela_id == 'observacao', 'campo_id'])
  metadado[metadado$tabela_id == 'observacao', ] <- metadado[metadado$tabela_id == 'observacao', ][idx, ]
  idx <- match(colnames(cam), metadado[metadado$tabela_id == 'camada',     'campo_id'])
  metadado[metadado$tabela_id == 'camada',     ] <- metadado[metadado$tabela_id == 'camada',     ][idx, ]
  
  # 3. a estrutura da tabela 'metadado' mudou no que diz respeito ao armazenamento das informações dos
  # laboratórios. Aqui atualizamos. Os campos existenes agora são lab_nome e lab_endereco. Alguns conjuntos de 
  # dados também possuem um campo chamado 'metodo_id' que eliminamos aqui
  if (!('lab_endereco' %in% colnames(metadado))) {
    cols <- strsplit(c('lab_rua_nome lab_rua_numero lab_municipio_id lab_pais_id lab_codigo_postal'), ' ')[[1]]
    metadado$lab_pais_id <- apply(
      metadado[cols], 1, function (x) {
        paste(na.exclude(x), collapse = ', ')
      })
    colnames(metadado)[colnames(metadado) == 'lab_pais_id'] <- 'lab_endereco'
    metadado <- metadado[, !(colnames(metadado) %in% c(cols, 'metodo_id'))]
  }
  
  # 4. precisamos verificar se os campos campo_unidade e campo_tipo estão todos de acordo com os padrões
  # # isso inclui substituir o valor 'sem unidade' por um valor padrão
  # metadado$campo_unidade <- gsub('sem unidade', '-', metadado$campo_unidade)
  if(!all(metadado$campo_unidade %in% glossario$campo_unidade)) {
    idx <- which(metadado$campo_unidade %in% glossario$campo_unidade)
    metadado$campo_unidade[!idx] <- NA_character_
  }
  metadado$campo_tipo <- tolower(metadado$campo_tipo)
  if(!all(tolower(metadado$campo_tipo) %in% glossario$campo_tipo)) {
    idx <- which(metadado$campo_tipo %in% glossario$campo_tipo)
    metadado$campo_tipo[!idx] <- NA_character_
  }
  
  # salvar dados
  out_file <- sub('xlsx', 'csv', in_file)
  out_file <- sub('/base', '', out_file)
  write.csv(metadado, file = out_file, row.names = FALSE)
}
rm(cam, cam_file, cam_in, cols, glossario, i, idx, in_cam, in_file, in_obs, metadado, obs, obs_file, obs_in,
   out_file, subs, tmp_c, tmp_o)

# observacao ##################################################################################################
# 1. remover o cabeçalho da tabela
# 2. processamentoar dados de alguns campos. também ordenamos as linhas em função de observacao_id
# 3. alguns campos foram identificados incorretamente como texto em vez de número. É preciso substituir
# o separador decimal e transformar para numérico.
ref <- 'https://docs.google.com/spreadsheets/d/1Dalqi5JbW4fg9oNkXw5TykZTA39pR5GezapVeV0lJZI/gviz/tq?tqx=out:csv'
ref <- read.csv(ref, header = TRUE, stringsAsFactors = FALSE)
# for (i in 1:50) {
for (i in 1:length(ctb)) {
  # i <- 40
  # i <- 1
  # i <- 228
  verbose()
  
  in_file <- getBaseFile(ctb[i], "observacao")
  
  # 1. remover o cabeçalho da tabela
  obs_head <- openxlsx::read.xlsx(in_file, detectDates = TRUE, rows = 1)
  observacao <- openxlsx::read.xlsx(
    in_file, detectDates = TRUE,
    startRow = 3, colNames = TRUE, na.strings = c('NA', '-', '', 'unknown', 'na'), 
    skipEmptyRows = FALSE, skipEmptyCols = FALSE)
  if (ncol(obs_head) > ncol(observacao)) {
    col_add <- ncol(obs_head) - ncol(observacao)
    col_add <- as.data.frame(matrix(rep(NA, nrow(observacao) * col_add), ncol = col_add))
    observacao <- cbind(observacao, col_add)
  }
  cols <- colnames(obs_head)
  colnames(observacao) <- cols
  
  # 2. processamentoar dados de alguns campos. também ordenamos as linhas em função de observacao_id
  # sisb_id
  if ('sisb_id' %in% cols) {
    if (is.numeric(observacao$sisb_id)) {
      observacao$sisb_id[!is.na(observacao$sisb_id)] <- 
        paste('SISB:', round(observacao$sisb_id)[!is.na(observacao$sisb_id)], sep = '')
    } else if (is.character(observacao)) {
      observacao$sisb_id[!is.na(observacao$sisb_id)] <- 
        paste('SISB:', observacao$sisb_id[!is.na(observacao$sisb_id)], sep = '')
    }
  }
  
  # observacao_data
  if (!all(is.na(observacao$observacao_data))) {
    idx <- grepl('.0', observacao$observacao_data, fixed = TRUE)
    if (any(idx)) {
      observacao$observacao_data[idx] <- 
        as.integer(gsub('.0', '', observacao$observacao_data[idx], fixed = TRUE))
    }
    idx <- is.numeric(observacao$observacao_data)
    if (any(idx)) {
      observacao$observacao_data[idx] <-
        format(as.Date(observacao$observacao_data[idx], origin = "1904-01-01"), "%Y-%m-%d")
    }
    idx <- is.character(observacao$observacao_data)
    if (any(idx)) {
      observacao$observacao_data[idx] <-
        gsub('/', '-', observacao$observacao_data[idx], fixed = TRUE)
    }
    idx <- grepl('\\d{2}-\\d{2}-\\d{4}', observacao$observacao_data)
    if (any(idx)) {
      observacao$observacao_data[idx] <- 
        format(as.Date(observacao$observacao_data[idx], "%d-%m-%Y"), "%Y-%m-%d")
    }
  }
  
  # observacao_id
  idx <- grepl('\\.0$', observacao$observacao_id)
  if (any(idx)) {
    observacao$observacao_id[idx] <- 
      as.character(as.integer(gsub('.0', '', observacao$observacao_id[idx], fixed = TRUE)))
  }
  
  # coord_sistema
  idx <- observacao$coord_sistema == 'SAD69'
  if (any(idx, na.rm = TRUE)) {
    observacao$coord_sistema[idx] <- 'EPSG:4618'
  }
  idx <- observacao$coord_sistema == 'SIRGAS'
  if (any(idx, na.rm = TRUE)) {
    observacao$coord_sistema[idx] <- 'EPSG:4674'
  }
  
  # coord_x	coord_y
  idx <- is.numeric(observacao$coord_x)
  if(idx) {
    observacao$coord_x <- round(observacao$coord_x, 8)
  }
  idx <- is.numeric(observacao$coord_y)
  if(idx) {
    observacao$coord_y <- round(observacao$coord_y, 8)
  }
  
  # ordenar linhas
  observacao <- observacao[order(observacao$observacao_id), ]
  
  # 3. alguns campos foram identificados incorretamente como texto em vez de número. É preciso substituir
  # o separador decimal e transformar para numérico.
  idx <- match(colnames(observacao), ref$campo_id)
  idx <- na.exclude(ref[idx, c("campo_id", "campo_tipo")])
  idx <- idx[idx$campo_tipo == 'real', ]
  is_text <- sapply(observacao[, idx$campo_id], is.character)
  if (any(is_text)) {
    has_comma <- colSums(sapply(observacao[, idx$campo_id], grepl, pattern = ',')) > 0
    has_lower <- colSums(sapply(observacao[, idx$campo_id], grepl, pattern = '<')) > 0
    is_text <- is_text[!has_lower]
    has_comma <- has_comma[!has_lower]
    if (any(has_comma)) {
      cols <- names(which(is_text & has_comma))
      idx <- idx[idx$campo_id %in% cols, ]
      if (inherits(observacao[, idx$campo_id], 'character')) {
        observacao[, idx$campo_id] <- sub(pattern = ',', replacement = '.', observacao[, idx$campo_id])
        not_na <- !is.na(suppressWarnings(as.numeric(observacao[, idx$campo_id])))
        observacao[not_na, idx$campo_id] <- as.numeric(observacao[not_na, idx$campo_id])
      } else {
        observacao[, idx$campo_id] <- 
          parallel::mclapply(observacao[, idx$campo_id], sub, pattern = ',', replacement = '.')
        observacao[, idx$campo_id] <- parallel::mclapply(observacao[, idx$campo_id], as.numeric)
      }
    }
  }
  # salvar dados
  out_file <- sub('xlsx', 'csv', in_file)
  out_file <- sub('/base', '', out_file)
  write.csv(observacao, file = out_file, row.names = FALSE)
}
rm(col_add, cols, has_comma, has_lower, i, idx, in_file, is_text, not_na, obs_head, observacao, out_file, ref)

# camada ######################################################################################################
# 1. remover o cabeçalho da tabela
# 2. processamentoar dados de alguns campos. observacao_id, profund_sup	profund_inf, camada_id. também ordena
# as linhas em função de observacao_id e profundidade
# 3. alguns campos foram identificados incorretamente como texto em vez de número
ref <- 'https://docs.google.com/spreadsheets/d/1Dalqi5JbW4fg9oNkXw5TykZTA39pR5GezapVeV0lJZI/gviz/tq?tqx=out:csv'
ref <- read.csv(ref, header = TRUE, stringsAsFactors = FALSE)
for (i in 1:length(ctb)) {
  
  # i <- 124
  
  verbose()
  in_file <- getBaseFile(ctb[i], "camada")
  
  # 1. remover o cabeçalho da tabela
  cam_head <- openxlsx::read.xlsx(in_file, rows = 1)
  camada <- openxlsx::read.xlsx(
    in_file, startRow = 3, colNames = TRUE, na.strings = c('NA', '-', '', 'unknown'), 
    skipEmptyRows = FALSE, skipEmptyCols = FALSE)
  
  if (ncol(cam_head) > ncol(camada)) {
    col_add <- ncol(cam_head) - ncol(camada)
    col_add <- as.data.frame(matrix(rep(NA, nrow(camada) * col_add), ncol = col_add))
    camada <- cbind(camada, col_add)
  }
  cols <- colnames(cam_head)
  colnames(camada) <- cols
  
  # 2. processamentoar dados de alguns campos. observacao_id, profund_sup	profund_inf, camada_id. também ordena
  # as linhas em função de observacao_id e profundidade
  
  # observacao_id
  idx <- grepl('\\.0$', camada$observacao_id)
  if (any(idx)) {
    camada$observacao_id[idx] <- 
      as.character(as.integer(gsub('.0', '', camada$observacao_id[idx], fixed = TRUE)))
  }
  
  # em profund_sup	profund_inf, se for texto, substituir ponto por vírgula como separador decimal
  idx <- grepl('\\.0$', camada$profund_sup)
  if (any(idx)) {
    camada$profund_sup[idx] <- gsub('.0', '', camada$profund_sup[idx], fixed = TRUE)
  }
  idx <- grepl('\\.0$', camada$profund_inf)
  if (any(idx)) {
    camada$profund_inf[idx] <- gsub('.0', '', camada$profund_inf[idx], fixed = TRUE)
  }
  
  # ordenar linhas
  camada <- camada[order(camada$observacao_id, camada$profund_sup, camada$profund_inf), ]
  
  # camada_id
  has_no_id <- is.na(camada$camada_id)
  if(any(has_no_id)) {
    tmp <- split(camada, as.factor(camada$observacao_id))
    tmp <- parallel::mclapply(tmp, function (x) {
      if (nrow(x) == 1) {
        x$camada_id <- 1
      } else {
        x$camada_id <- 1:nrow(x)
      }
      x
    })
    camada <- do.call(rbind, tmp)
  }
  
  # 3. alguns campos foram identificados incorretamente como texto em vez de número
  idx <- match(colnames(camada), ref$campo_id)
  idx <- na.exclude(ref[idx, c("campo_id", "campo_tipo")])
  idx <- idx[idx$campo_tipo == 'real', ]
  
  is_text <- sapply(camada[, idx$campo_id], is.character)
  if (any(is_text)) {
    
    has_comma <- colSums(sapply(camada[, idx$campo_id], grepl, pattern = ',')) > 0
    if (any(has_comma)) {
      cols <- names(which(is_text & has_comma))
      idx_comma <- idx[idx$campo_id %in% cols, ]
      if (inherits(camada[, idx_comma$campo_id], 'character')) {
        camada[, idx_comma$campo_id] <- sub(pattern = ',', replacement = '.', camada[, idx_comma$campo_id])
        not_na <- !is.na(suppressWarnings(as.numeric(camada[, idx_comma$campo_id])))
        camada[not_na, idx_comma$campo_id] <- as.numeric(camada[not_na, idx_comma$campo_id])
      } else {
        camada[, idx_comma$campo_id] <-
          parallel::mclapply(camada[, idx_comma$campo_id], sub, pattern = ',', replacement = '.')
        camada[, idx_comma$campo_id] <- parallel::mclapply(camada[, idx_comma$campo_id], as.numeric)
      }
    }
    
    has_lower <- colSums(sapply(camada[, idx$campo_id], grepl, pattern = '<')) > 0
    if (any(has_lower)) {
      cols <- names(which(is_text & has_lower))
      idx_lower <- idx[idx$campo_id %in% cols, ]
      if (inherits(camada[, idx_lower$campo_id], 'character')) {
        camada[, idx_lower$campo_id] <- 
          sub(pattern = '.', replacement = ',', camada[, idx_lower$campo_id], fixed = TRUE)
        camada[, idx_lower$campo_id] <- sub(pattern = '< ', replacement = '<', camada[, idx_lower$campo_id])
        not_na <- !is.na(suppressWarnings(as.numeric(camada[, idx_lower$campo_id])))
        camada[not_na, idx_lower$campo_id] <- as.numeric(camada[not_na, idx_lower$campo_id])
      } else {
        camada[, idx_lower$campo_id] <- 
          parallel::mclapply(camada[, idx_lower$campo_id], sub, pattern = '.', replacement = ',', fixed = TRUE)
        camada[, idx_lower$campo_id] <- 
          parallel::mclapply(camada[, idx_lower$campo_id], sub, pattern = '< ', replacement = '<')
        camada[, idx_lower$campo_id] <- parallel::mclapply(camada[, idx_lower$campo_id], as.numeric)
      }
    }
  }
  
  # salvar dados
  out_file <- sub('xlsx', 'csv', in_file)
  out_file <- sub('/base', '', out_file)
  write.csv(camada, file = out_file, row.names = FALSE)
}
rm(cam_head, camada, col_add, cols, has_comma, has_lower, has_no_id, i, idx, idx_comma, idx_lower, in_file,
   is_text, not_na, out_file, tmp)

# xlsx e csv ##################################################################################################
tab <- c('identificacao', 'versionamento', 'metadado', 'observacao', 'camada')
for (i in 1:length(ctb)) {
  # i <- 1
  verbose()
  
  # preparar workbook
  wb <- openxlsx::loadWorkbook(file = 'data/febr-modelo-planilha-geral.xlsx')
  openxlsx::deleteData(wb = wb, sheet = 'metadado', cols = 1:6, rows = 2:26, gridExpand = TRUE)

  # escrever dados nas abas
  for (j in 1:length(tab)) {
    print(tab[j])
    x <- read.csv(paste(dir[i], '/', ctb[i], '-', tab[j], '.csv', sep = ''))
    
    for (k in 1:ncol(x)) {
      openxlsx::writeData(wb = wb, sheet = tab[j], x = x[, k], startCol = k, startRow = 2, keepNA = TRUE)
    }
    write.table(
      x = x,
      file = paste(publico, '/', ctb[i], "/", ctb[i], '-', tab[j], ".csv", sep = ''), 
      dec = ',', sep = ';', row.names = FALSE)
  }

  # publicar dados
  file_out <- paste(dir[i], '/', Sys.Date(), '-', ctb[i], ".xlsx", sep = '')
  openxlsx::saveWorkbook(wb = wb, file = file_out, overwrite = TRUE)
  dir_out <- sub('processamento/', 'publico/', file_out)
  cmd <- paste('cp', file_out, dir_out)
  system(cmd)
  to <- sub(paste(Sys.Date(), '-', sep = ''), '', dir_out)
  file.rename(dir_out, to)
  
  # remover arquivos CSV
  rm_files <- list.files(dir[i], pattern = 'csv', full.names = TRUE)
  file.remove(rm_files)
}

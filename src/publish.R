# title: Repositório Brasileiro Livre para Dados Abertos do Solo
# subtitle: Publicação de conjuntos de dados
# autor: Alessandro Samuel-Rosa
# summary:
#   * Publicar conjunto de dados processado no dia de hoje, criando um arquivo de texto TXT para
#     cada uma das planilhas existentes, bem como uma cópia do arquivo XLSX.
#   * Os arquivos gerados são copiados/salvos no respectivo diretório do conjunto de dados no
#     diretório 'public'.
#   * Para conjuntos de dados sem consistência documental, ou seja, em que faltam todos ou alguns
#     dos campos obrigatórios das tabelas observacao e camada, faz-se a adição dos mesmos a fim de
#     permitir o descarregamento dos arquivos via pacote febr sem erro.
# main #############################################################################################
rm(list = ls())

# caminhos dos diretórios de dados
processamento <- path.expand('~/ownCloud/febr-repo/processamento')
publico <- path.expand('~/ownCloud/febr-repo/publico')

# conjunto de dados a ser publicado
# ctb <- list.dirs(processamento, recursive = FALSE, full.names = FALSE)
ctb <- "ctb0020"

for (i in seq_along(ctb)) {
  # i <- 1
  # identificar workbook com dados a serem publicados
  # requisito: arquivo XLSX com a data de hoje
  # xlsxFile <- list.files(path = paste0(processamento, "/", ctb[i]), pattern = "xlsx", full.names = TRUE)
  # xlsxFile <- xlsxFile[length(xlsxFile)]
  # xlsxFile <- paste0(processamento, "/", ctb, '/', Sys.Date(), '-', ctb, '.xlsx')
  xlsxFile <- list.files(
    path = paste0(processamento, "/", ctb[i]), 
    pattern = paste0(Sys.Date()), 
    full.names = TRUE)
  
  # Carregar workbook
  workbook <- openxlsx::loadWorkbook(file = xlsxFile)
  
  # Processar tabela 'identificacao'
  # Campos de identificação são obtidos da planilha padrão armazenada no Google Drive
  # Campos faltantes são inseridos. Em seguida os campos são reordenados conforme o padrão
  # (esse processo também é realizado em make-index.R)
  id_campo <- 
    paste0('https://docs.google.com/spreadsheets/d/', '1rXIiT1zSYhFegSdAvE0yJX16q-bvXVNpYIYdd5YgjhI', 
           '/export?format=csv&gid=', '1085102806')
  id_campo <- read.csv(id_campo, header = TRUE, stringsAsFactors = FALSE)
  identificacao <- openxlsx::read.xlsx(workbook, sheet = 'identificacao')
  rownames(identificacao) <- identificacao$campo
  identificacao["autor_nome_email", "campo"] <- "dados_autor"
  identificacao["dados_referencia", "campo"] <- "dados_publicacao"
  identificacao <- merge(x = identificacao, y = id_campo['campo'], all.y = TRUE)
  identificacao <- identificacao[match(id_campo$campo, identificacao$campo), ]
  rownames(identificacao) <- identificacao$campo
  if (as.integer(sub("ctb", "", ctb[i])) > 100) {
    identificacao["dados_licenca", "valor"] <- "CC BY-NC 4.0 Atribuição-NãoComercial"
  }
  file <- paste0(publico, "/", ctb[i], '/', ctb[i], '-identificacao.txt', sep = '')
  if(!dir.exists(dirname(file))) {
    dir.create(dirname(file))
  }
  write.table(x = identificacao, file = file, sep = '\t', dec = ',', row.names = FALSE)
  
  # processar tabela 'versionamento'
  file <- paste0(publico, '/', ctb[i], '/', ctb[i], '-versionamento.txt')
  versionamento <- openxlsx::read.xlsx(workbook, sheet = 'versionamento')
  write.table(x = versionamento, file = file, sep = '\t', dec = ',', row.names = FALSE)
  
  # Verificar se conjunto de dados está sob embargo
  embargado <- grepl("embargado", xlsxFile)
  if (embargado) {
    tab <- c('metadado')
  } else {
    tab <- c('metadado', 'observacao', 'camada')
  }
  # processar tabelas 'metadado', 'observacao' e 'camada'
  # requisito: consistência estrutural
  consistencia <- identificacao[identificacao$campo == 'dados_consistencia', 'valor']
  if (consistencia == 'consistência estrutural') {
    for (j in 1:length(tab)) {
      x <- openxlsx::read.xlsx(workbook, sheet = tab[j])
      x <- x[colnames(x) != '']
      file <- paste0(publico, '/', ctb[i], '/', ctb[i], '-', tab[j], '.txt')
      write.table(x = x, file = file, sep = '\t', dec = ',', row.names = FALSE)
    }
  }
  
  if (!embargado) {
    # copiar workbook para diretório público
    cmd <- paste('cp', xlsxFile, paste0(publico, '/', ctb[i], '/', ctb[i], '.xlsx'))
    system(cmd)
  }

  # Atualizar catálogo de conjuntos de dados
  source("code/make-index.R")
}

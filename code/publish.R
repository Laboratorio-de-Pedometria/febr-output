# title: Repositório Brasileiro Livre para Dados Abertos do Solo
# subtitle: Publicação de conjuntos de dados
# autor: Alessandro Samuel-Rosa
# summary:
#   * Publicar conjunto de dados processado no dia de hoje, criando um arquivo de texto TXT para cada uma das
#     planilhas existentes, bem como uma cópia do arquivo XLSX.
#   * Os arquivos gerados são copiados/salvos no respectivo diretório do conjunto de dados no diretório 
#     'public'.
#   * Para conjuntos de dados sem consistência documental, ou seja, em que faltam todos ou alguns dos campos
#     obrigatórios das tabelas observacao e camada, faz-se a adição dos mesmos a fim de permitir o 
#     descarregamento dos arquivos via pacote febr sem erro.
# main ########################################################################################################
rm(list = ls())

# conjunto de dados a ser publicado
ctb <- "ctb0025"

# caminhos dos diretórios de dados
processamento <- path.expand('~/ownCloud/febr-repo/processamento/')
publico <- path.expand('~/ownCloud/febr-repo/publico/')

# identificar workbook com dados a serem publicados
# requisito: arquivo XLSX com a data de hoje
xlsxFile <- paste(processamento, ctb, '/', Sys.Date(), '-', ctb, '.xlsx', sep = '')

# Processar tabela 'identificacao'
# Campos de identificação são obtidos da planilha padrão armazenada no Google Drive
# Campos faltantes são inseridos. Em seguida os campos são reordenados conforme o padrão
# (esse processo também é realizado em make-index.R)
id_campo <- 
  paste0('https://docs.google.com/spreadsheets/d/', '1rXIiT1zSYhFegSdAvE0yJX16q-bvXVNpYIYdd5YgjhI', 
         '/export?format=csv&gid=', '1085102806')
id_campo <- read.csv(id_campo, header = TRUE, stringsAsFactors = FALSE)
identificacao <- openxlsx::read.xlsx(xlsxFile, sheet = 'identificacao')
identificacao$campo[identificacao$campo == 'autor_nome_email'] <- 'dados_autor'
identificacao$campo[identificacao$campo == 'dados_referencia'] <- 'dados_publicacao'
identificacao <- merge(identificacao, id_campo['campo'], all.y = TRUE)
identificacao <- identificacao[match(id_campo$campo, identificacao$campo), ]
file <- paste0(publico, ctb, '/', ctb, '-identificacao.txt', sep = '')
if(!dir.exists(dirname(file))) {
  dir.create(dirname(file))
}
write.table(x = identificacao, file = file, sep = '\t', dec = ',', row.names = FALSE)

# processar tabela 'versionamento'
file <- paste0(publico, ctb, '/', ctb, '-versionamento.txt')
write.table(
  x = openxlsx::read.xlsx(xlsxFile, sheet = 'versionamento'), 
  file = file, sep = '\t', dec = ',', row.names = FALSE)

# processar tabelas 'metadado', 'observacao' e 'camada'
# requisito: consistência estrutural
consistencia <- identificacao[identificacao$campo == 'dados_consistencia', 'valor']
if (consistencia == 'consistência estrutural') {
  tab <- c('metadado', 'observacao', 'camada')
  for (i in 1:length(tab)) {
    # i <- 1
    x <- openxlsx::read.xlsx(xlsxFile, sheet = tab[i])
    x <- x[colnames(x) != '']
    file <- paste0(publico, ctb, '/', ctb, '-', tab[i], '.txt')
    write.table(x = x, file = file, sep = '\t', dec = ',', row.names = FALSE)
  }
}

# copiar workbook para diretório público
cmd <- paste('cp', xlsxFile, paste0(publico, ctb, '/', ctb, '.xlsx'))
system(cmd)

# title: Repositório Brasileiro Livre para Dados Abertos do Solo
# subtitle: Publicação de conjuntos de dados
# autor: Alessandro Samuel-Rosa

# TODO ----
# 1. Adicionar campo dados_idioma
# 2. Substituir campo autor_nome_email por dados_autor
# 3. Substituir campo dados_referencia por dados_publicacao

# variables ----
ctb <- ""

# main ########################################################################################################

# caminhos dos diretórios de dados
processamento <- path.expand('~/ownCloud/febr-repo/processamento/')
publico <- path.expand('~/ownCloud/febr-repo/publico/')

# identificar workbook com dados a serem publicados
# requisito: arquivo XLSX com a data de hoje
xlsxFile <- paste(processamento, ctb, '/', Sys.Date(), '-', ctb, '.xlsx', sep = '')

# processar tabela identificacao
identificacao <- openxlsx::read.xlsx(xlsxFile, sheet = 'identificacao')
consistencia <- identificacao[identificacao$campo == 'dados_consistencia', 'valor']
file <- paste(publico, ctb, '/', ctb, '-identificacao.csv', sep = '')
if(!dir.exists(dirname(file))) {
  dir.create(dirname(file))
}
write.table(x = identificacao, file = file, sep = ';', dec = ',', row.names = FALSE)

# processar tabela versionamento
file <- paste(publico, ctb, '/', ctb, '-versionamento.csv', sep = '')
write.table(
  x = openxlsx::read.xlsx(xlsxFile, sheet = 'versionamento'), 
  file = file, sep = ';', dec = ',', row.names = FALSE)

# processar tabelas metadado, observacao e camada
# requisito: consistência estrutural
if (consistencia == 'consistência estrutural') {
  tab <- c('metadado', 'observacao', 'camada')
  for (i in 1:length(tab)) {
    x <- openxlsx::read.xlsx(xlsxFile, sheet = tab[i])
    x <- x[colnames(x) != '']
    file <- paste(publico, ctb, '/', ctb, '-', tab[i], '.csv', sep = '')
    write.table(x = x, file = file, sep = ';', dec = ',', row.names = FALSE)
  }
}

# copiar workbook para diretório público
cmd <- paste('cp', xlsxFile, paste(publico, ctb, '/', ctb, '.xlsx', sep = ''))
system(cmd)

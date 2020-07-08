# title: Repositório Brasileiro Livre para Dados Abertos do Solo
# subtitle: Índice de conjuntos de dados
# autor: Alessandro Samuel Rosa
# summary: Criar índice público dos conjunto de dados publicados no repositório utilizando, para isso, os dados
#   de identificação dos conjuntos de dados. No índice, exportado no formato XLSX, cada conjunto de dados ocupa
#   uma linha.

# main ########################################################################################################
rm(list = ls())

# caminho do diretório de dado públicos
publico <- path.expand('~/ownCloud/febr-repo/publico')

# carregar dados
identificacao <- 
  list.files(path = publico, pattern = "identificacao.txt$", recursive = TRUE, full.names = TRUE)
identificacao <-
  parallel::mclapply(identificacao, function (x) {
    read.table(x, sep = '\t', dec = ',', header = TRUE, stringsAsFactors = FALSE)
  })

# processar dados de identificação
# campos de identificação são obtidos da planilha padrão armazenada no Google Drive
# campos faltantes são inseridos; em seguida os campos são reordenados conforme o padrão
# (esse processo também é realizado em publish.R)
id_campo <- 
  paste0('https://docs.google.com/spreadsheets/d/', '1rXIiT1zSYhFegSdAvE0yJX16q-bvXVNpYIYdd5YgjhI', 
         '/export?format=csv&gid=', '1085102806')
padrao <- read.csv(url, header = TRUE, stringsAsFactors = FALSE)
identificacao <- 
  parallel::mclapply(
    identificacao, function (id) {
      id$campo[id$campo == 'autor_nome_email'] <- 'dados_autor'
      id$campo[id$campo == 'dados_referencia'] <- 'dados_publicacao'
      out <- merge(id, padrao['campo'], all.y = TRUE)
      out <- out[match(padrao$campo, out$campo), ]
      return (out['valor'])
  })
identificacao <- data.frame(t(do.call(cbind, identificacao)), row.names = NULL, stringsAsFactors = FALSE)
colnames(identificacao) <- padrao$campo
identificacao$dados_acesso <- 
  paste("https://cloud.utfpr.edu.br/index.php/s/Df6dhfzYJ1DDeso?path=%2F", identificacao$dados_id, sep = "")

# salvar planilha eletrônica no formato XLSX
hs <- openxlsx::createStyle(textDecoration = "BOLD", fgFill = "lightgray")
openxlsx::write.xlsx(
  x = identificacao,
  file = paste(publico, '/febr-indice.xlsx', sep = ''),
  sheetName = 'febr-indice', 
  rowNames = FALSE, 
  headerStyle = hs, 
  firstRow = TRUE,
  firstCol = TRUE,
  colWidths = "auto")

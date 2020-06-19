# título: Criar índice de conjuntos de dados a partir dos dados de identificação
# autor: Alessandro Samuel Rosa
# data: 2020-06-19

# Carregar dados do repositório local
publico <- path.expand('~/ownCloud/febr-repo/publico/')
identificacao <- list.files(path = publico, pattern = "identificacao.csv$", recursive = TRUE, full.names = TRUE)
identificacao <- parallel::mclapply(
  identificacao, read.table, sep = ';', dec = ',', header = TRUE, row.names = 1, stringsAsFactors = FALSE)

# Processar dados de identificação
col_names <- rownames(identificacao[[1]])
identificacao <- parallel::mclapply(
  identificacao, function (x) {
    x[rownames(x) != "fonte_financeira", ] # nem todos os conjuntos de dados informam a fonte financeira
  }
)
identificacao <- data.frame(t(do.call(cbind, identificacao)), row.names = NULL)
colnames(identificacao) <- col_names
identificacao$dados_acesso <- 
  paste("https://cloud.utfpr.edu.br/index.php/s/Df6dhfzYJ1DDeso?path=%2F", identificacao$dados_id, sep = "")

# Salvar planilha eletrônica no formato XLSX
hs <- openxlsx::createStyle(textDecoration = "BOLD", fgFill = "lightgray")
openxlsx::write.xlsx(
  x = identificacao, file = paste(publico, '/febr-indice.xlsx', sep = ''),
  sheetName = 'febr-indice', rowNames = FALSE, headerStyle = hs, firstRow = TRUE,
  firstCol = TRUE, colWidths = "auto")

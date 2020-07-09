# title: Repositório Brasileiro Livre para Dados Abertos do Solo
# subtitle: Atualizar estrutura do modelo de dados
# autor: Alessandro Samuel-Rosa
# summary: Atualizar a estrutura da planilha de dados de todos os conjuntos de dados usando a planilha modelo,
#   armazenada no Google Drive, como referência

# main ########################################################################################################
rm(list = ls())

# descarregar planilha modelo armazenada no Google Drive
modelo <- '1rXIiT1zSYhFegSdAvE0yJX16q-bvXVNpYIYdd5YgjhI'
modelo <- paste0('https://docs.google.com/spreadsheets/d/', modelo, '/export?format=xlsx')
modelo <- openxlsx::loadWorkbook(file = modelo)
for (i in 1:length(modelo$comments[[2]])) {
  modelo$comments[[2]][[i]]$style <- 
    "<rPr><sz val=\"11\"/><color rgb=\"FF000000\"/><name val=\"Inconsolata\"/></rPr>"
}
modelo_id <- openxlsx::read.xlsx(xlsxFile = modelo, sheet = 'identificacao')
modelo_dic <- openxlsx::read.xlsx(xlsxFile = modelo, sheet = 'dicionario')

# identificar os workbooks dos conjuntos de dados
processamento <- path.expand('~/ownCloud/febr-repo/processamento')
dirs <- list.dirs(path = processamento, full.names = TRUE, recursive = FALSE)
files <- parallel::mclapply(dirs, function (x) list.files(x, pattern = '.xlsx$', full.names = TRUE))
files <- unlist(files)

# processamento
# 1. carregar workbook
# 2. ler planilha de identificação
# 3. alterar campos existentes
# 4. fundir com campos do modelo
# 5. escrever dados no workbook
# 6. salvar workbook
sty_campo <- openxlsx::createStyle(
  fontName = 'Inconsolata', fontSize = 11, fontColour = '#000000', bgFill = '#CCCCFF', fgFill = '#CCCCCC',
  halign = 'left', valign = 'center', numFmt = 'TEXT', textDecoration = 'bold')
sty_valor <- openxlsx::createStyle(
  fontName = 'Inconsolata', fontSize = 11, fontColour = '#000000', halign = 'left', valign = 'center', 
  numFmt = 'TEXT', wrapText = TRUE)
sty_meta <- openxlsx::createStyle(
  fontName = 'Inconsolata', fontSize = 11, fontColour = '#000000', halign = 'left', valign = 'center', 
  numFmt = 'TEXT', wrapText = TRUE)
parallel::mclapply(files, function (x) {
  
  # carregar workbook e atualizar nomes das tabelas
  wb <- openxlsx::loadWorkbook(x)
  names(wb) <- names(modelo)
  
  id <- openxlsx::read.xlsx(wb, sheet = 'identificacao')
  id$campo[id$campo == 'autor_nome_email'] <- 'dados_autor'
  id$campo[id$campo == 'dados_referencia'] <- 'dados_publicacao'
  id <- merge(id, modelo_id['campo'], all.y = TRUE)
  idx <- match(modelo_id$campo, id$campo)
  id <- id[idx, ]
  rownames(id) <- id$campo
  if (id['dados_licenca', 'valor'] %in% c('CC BY 4.0', 'CC BY-NC 4.0')) {
    id['dados_licenca', 'valor'] <- ifelse(
      test = id['dados_licenca', 'valor'] == 'CC BY 4.0',
      yes = 'CC BY 4.0 Atribuição', 
      no = 'CC BY-NC 4.0 Atribuição-NãoComercial'
    )
  }
  
  openxlsx::writeData(wb = wb, sheet = 'identificacao', x = id, startCol = 1, startRow = 1, keepNA = TRUE)
  openxlsx::addStyle(wb = wb, sheet = 'identificacao', style = sty_campo, rows = 1:(nrow(id)+1), cols = 1)
  openxlsx::addStyle(wb = wb, sheet = 'identificacao', style = sty_valor, rows = 2:(nrow(id)+1), cols = 2)
  
  # atualizar notas explicativas
  wb$comments[2] <- modelo$comments[2] # identificacao
  
  # atualizar validação de dados
  wb$worksheets[[2]]$dataValidations <- modelo$worksheets[[2]]$dataValidations # identificacao
  wb$worksheets[[4]]$dataValidations <- modelo$worksheets[[4]]$dataValidations # metadado
  
  # atualizar dicionário
  openxlsx::writeData(wb = wb, sheet = 'dicionario', x = modelo_dic, startCol = 1, startRow = 1, keepNA = FALSE)
  
  # atualizar metadado
  openxlsx::addStyle(wb = wb, sheet = 'metadado', style = sty_meta, rows = 2:200, cols = 1:20, gridExpand = TRUE)
  
  # salvar workbook
  openxlsx::saveWorkbook(wb = wb, file = x, overwrite = TRUE)
})

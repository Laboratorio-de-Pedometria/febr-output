# title: Repositório Brasileiro Livre para Dados Abertos do Solo
# subtitle: Descrição do diretório de arquivos de um conjunto de dados
# author: Alessandro Samuel-Rosa
# date: 2020-07-03

# Identificar diretórios públicos
publico <- path.expand('~/ownCloud/febr-repo/publico')  
dirs <- list.dirs(path = publico)[-1]

# Copiar arquivo LEIAME.md para cada diretório
cmd <- paste0('cp code/LEIAME.docx ', dirs)
lapply(cmd, system)

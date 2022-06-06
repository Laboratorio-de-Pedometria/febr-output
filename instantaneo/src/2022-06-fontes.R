# title: Instantâneo de Junho de 2022 - fontes de dados
# subtitle: Repositório de Dados do Solo Brasileiro (FEBR)
# author: Alessandro Samuel-Rosa
#
# Documentos importantes:
# - FEBR Dicionário de Dados v2: https://goo.gl/hi77sB
#
# Instalar última versão do pacote febr diretamente o GitHub
if (!require(remotes)) {
  install.packages(pkgs = "remotes")
}
remotes::install_github(repo = "laboratorio-de-pedometria/febr-package")
library(febr)
febr_repo <- "~/ownCloud/febr-repo/publico"
#
# Carregar funções auxiliares
source("febr-output/instantaneo/src/help.r")
#
## 2022-06-fontes.txt ##############################################################################
instantaneo <- "2022-06"
#
# Campos exportados da tabela 'identificacao':
# * dados_id: Código identificador único dos dados no FEBR. O código é atribuído aos dados pelos
#   gestores do repositório.
# * dados_licenca: Licença de uso e distribuição dos dados por terceiros
ide <- febr::identification(data.set = "all", febr.repo = febr_repo, verbose = FALSE)
ide <- lapply(ide, function(x) {
  y <- as.list(x[["valor"]])
  names(y) <- x[["campo"]]
  data.frame(y)
})
ide <- do.call(rbind, ide)
ide_cols <- c("dados_id", "dados_licenca")
ide <- ide[, ide_cols]
# Campos exportados da tabela 'versionamento':
# * dados_versao: Versão do conjunto de dados usando o sistema semântico MAIOR.MENOR.CORREÇÃO.
ver <- febr::readFEBR(data.set = "all", data.table = "versionamento", verbose = FALSE)
latest_version <- character(length(ver))
for (i in seq_along(ver)) {
  which_row <- dim(ver[[i]])[1]
  latest_version[i] <- ver[[i]][which_row, "dados_versao"][[1]]
}
# Escrever tabela em disco
ide <- cbind(ide, dados_versao = latest_version)
writeTable(object = ide, file.name = paste0(instantaneo, "-fontes.txt"))

# title: Instantâneo de Junho de 2022 - fontes
# subtitle: Repositório de Dados do Solo Brasileiro
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
## 2022-06-fontes.txt ##############################################################################
snapshot <- "2022-06"
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
# ide[["dados_licenca"]] <- gsub(" 4.0 Atribuição-NãoComercial-CompartilhaIgual", "",
#   ide[["dados_licenca"]])
# ide[["dados_licenca"]] <- gsub(" 4.0 Atribuição-NãoComercial", "", ide[["dados_licenca"]])
# ide[["dados_licenca"]] <- gsub(" 4.0 Atribuição", "", ide[["dados_licenca"]])
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
file <- paste0("febr-output/tmp/", snapshot, "-fontes.txt")
write.table(ide, file = file, sep = ";", dec = ",", row.names = FALSE)

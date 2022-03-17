# title: Instância de dezembro de 2021
# subtitle: Repositório de Dados do Solo Brasileiro
# author: Alessandro Samuel-Rosa
# 
# Instalar última versão do pacote febr diretamente o GitHub
if (!require(remotes)) {
  install.packages(pkgs = "remotes")
}
remotes::install_github(repo = "laboratorio-de-pedometria/febr-package")
library(febr)
febr_repo <- "~/ownCloud/febr-repo/publico"
# 
## 2021-12-fontes.txt ##############################################################################
# Campos exportados da tabela 'identificacao':
# * dados_id: código de identificação dos dados no FEBR
# * dados_licenca: licença de uso e distribuição dos dados definida pelos autores ou organização
#   responsável
ide <- febr::identification(data.set = "all", febr.repo = febr_repo, verbose = FALSE)
ide <- lapply(ide, function(x){
  y <- as.list(x[["valor"]])
  names(y) <- x[["campo"]]
  data.frame(y)
})
ide <- do.call(rbind, ide)
ide[["dados_licenca"]] <- gsub(" 4.0 Atribuição-NãoComercial-CompartilhaIgual", "",
  ide[["dados_licenca"]])
ide[["dados_licenca"]] <- gsub(" 4.0 Atribuição-NãoComercial", "", ide[["dados_licenca"]])
ide[["dados_licenca"]] <- gsub(" 4.0 Atribuição", "", ide[["dados_licenca"]])
ide_cols <- c("dados_id", "dados_licenca")
ide <- ide[, ide_cols]
# Campos exportados da tabela 'versionamento':
# * dados_versao: número da última versão dos dados no FEBR
ver <- febr::readFEBR(data.set = "all", data.table = "versionamento", verbose = FALSE)
latest_version <- character(length(ver))
for (i in seq_along(ver)) {
  which_row <- dim(ver[[i]])[1]  
  latest_version[i] <- ver[[i]][which_row, "dados_versao"][[1]]
}
# Escrever table em disco
ide <- cbind(ide, dados_versao = latest_version)
write.table(ide, "febr-output/tmp/2021-12-fontes.txt", sep = ";", dec = ",", row.names = FALSE)

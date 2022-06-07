# Escrever tabela em disco no formato TXT
writeTable <-
  function(object, file.name) {
    dir <- "febr-output/instantaneo/res/"
    file <- paste0(dir, file.name)
    write.table(x = object, file = file, sep = ";", dec = ",", row.names = FALSE)
  }

writeBug <-
  function(object, file.name) {
    dir <- "febr-output/instantaneo/bug/"
    file <- paste0(dir, file.name)
    write.table(x = object, file = file, sep = "\t", dec = ",", row.names = FALSE)
  }

coalesce <-
  function(object, target) {
    has_data <- which(!is.na(object), arr.ind = TRUE)
    has_data_duplicated <- duplicated(has_data[, "row"])
    idx_keep <- which(!has_data_duplicated)
    has_data <- has_data[idx_keep, ]
    object[has_data[, "row"], target] <- object[has_data]
    return(object)
  }
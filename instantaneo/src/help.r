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
    write.table(x = object, file = file, sep = ";", dec = ",", row.names = FALSE)
  }

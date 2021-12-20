# Formatar data de observação ----
# * obj: vector of event dates
standardize <-
  function(obj, what = "date", time.format = "%d-%m-%Y") {
    switch(
      what,
      date = {
        # 1. Identificar quantos são os formatos de data existentes: comprimento do string
        # 2. Processar cada formato de data individualmente
        # 2.1 Identificar o separador de dia, mês e ano usado
        # 2.2 Criar objeto para armazenar o formato de entrada
        # 2.3
        date_length <- nchar(obj)
        obj_split <- split(obj, date_length)
        n_formats <- length(obj_split)
        for (i in seq_along(n_formats)) {
          # Identificar formatação da data: separador (/ ou -)
          date_sep <- ifelse(all(grepl("/", obj[[i]])), "/", "-")
          current_format <- paste0("%d", date_sep, "%m", date_sep, "%Y")
          # Verificar se falta data para alguma observação
          time_miss <- grepl("xx", obj[[i]])
            if (any(time_miss)) {
              ## Falta dia
              miss_day <- grepl(paste0("^xx", date_sep), obj[[i]])
              if (any(miss_day)) {
                obj[[i]][miss_day] <- gsub(
                  pattern = paste0("^xx", date_sep),
                  replacement = paste0(format(Sys.Date(), "%d"), date_sep),
                  x = obj[[i]][miss_day])
              }
              # Falta mês
              miss_month <- grepl(paste0(date_sep, "xx", date_sep), obj[[i]])
              if (any(miss_month)) {
                obj[[i]][miss_month] <- gsub(
                  pattern = paste0(date_sep, "xx", date_sep),
                  replacement = paste0(date_sep, format(Sys.Date(), "%m"), date_sep),
                  x = obj[[i]][miss_month])
              }
            }
            # Formatar data
            obj[[i]] <- as.Date(x = obj[[i]], format = current_format)
            obj[[i]] <- as.Date(x = obj[[i]], format = time.format)
          }
          obj <- unlist(obj)
        }
      )
    # Retornar objeto
    return(obj)
  }
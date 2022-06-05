# title: Instância de dezembro de 2021
# subtitle: Repositório de Dados do Solo Brasileiro
# author: Alessandro Samuel-Rosa
#
# Instalar última versão do pacote febr diretamente o GitHub
if (!require(remotes)) {
  install.packages(pkgs = "remotes")
}
remotes::install_github(repo = "laboratorio-de-pedometria/febr-package")
febr_repo <- "~/ownCloud/febr-repo/publico"
#
## 2021-12-camadas.txt ##############################################################################
# Campos exportados da tabela 'camada':
camada_cols <- c(
  "evento_id_febr",
  "camada_id_febr"
)
# Descarregar dados utilizando o nível 3 de harmonização
vars <- c("areia")
  # "carbono_", "argila_", "silte_")
  # , "areia_", "areiagrossa2_", "areiafina2_", ,
  # "terrafina_", "cascalho_", "calhau_",
  # "densidade_",
  # "ctc_", "ph_", "ce_",
  # "camada_nome")
camada <- febr::layer(
  data.set = "all",
  # data.set = "ctb0047",
  variable = vars,
  stack = TRUE,
  # harmonization = list(harmonize = TRUE, level = 2),
  harmonization = list(harmonize = FALSE),
  standardization = list(
    plus.sign = "add",
    plus.depth = 20,
    lessthan.sign = "remove",
    # lessthan.frac = 0.5,
    repetition = "combine",
    combine.fun = "mean",
    transition = "smooth",
    smoothing.fun = "mean",
    units = TRUE, round = TRUE),
  febr.repo = febr_repo)
# camada <- febr::layer(data.set = "ctb0005", febr.repo = febr_repo, variable = vars)
dim(camada)
head(camada)
colnames(camada)
idx <- which(camada[["silte_xxx_xxx_xxx"]] > 0)
unique(camada[["dataset_id"]][idx])

# 
camada %<>% 
  dplyr::mutate(
    carbono = dplyr::coalesce(carbono_cromo, carbono_xxx, carbono_forno),
    argila = dplyr::coalesce(argila_naoh, argila_xxx),
    silte = dplyr::coalesce(silte_naoh, silte_xxx),
    areia = dplyr::coalesce(areia_naoh, areia_xxx),
    areiagrossa = dplyr::coalesce(areiagrossa2_naoh, areiagrossa2_xxx),
    areiafina = dplyr::coalesce(areiafina2_naoh, areiafina2_xxx),
    ctc = dplyr::coalesce(ctc_soma, ctc_xxx),
    dsi = dplyr::coalesce(dsi_cilindro, dsi_xxx),
    ph = dplyr::coalesce(ph_h2o),
    terrafina = dplyr::coalesce(terrafina_peneira, terrafina_xxx),
    ce = dplyr::coalesce(ce_pastasat),
    cascalho = dplyr::coalesce(cascalho_peneira, cascalho_olho, cascalho_xxx),
    calhau = dplyr::coalesce(calhau_peneira, calhau_xxx)
  ) %T>% 
  print()

# Após a fusão das colunas com dados da mesma variável, faz-se ajustes nos dados das frações granulométricas. 
# Primeiro, na falta de uma das duas frações granulométricas finas, `argila` ou `silte`, calcula-se seu valor 
# como sendo a diferença entre 1000 g/kg e a soma das duas outras frações granulométricas. Na falta dos dados do
# conteúdo de areia total -- `areia`, primeiro se usa (quando disponível) a soma dos dados do conteúdo de areia
# grossa -- `areiagrossa` -- e areia fina `areiafina`. Caso não estejam disponíveis, então se usa a estratégia 
# anterior. A seguir, verifica-se se a soma dos valores inteiros das três frações é igual a 1000 g/kg. Caso não 
# seja -- a diferença geralmente é de 1 g/kg para menos ou para mais --, então altera-se os dados do conteúdo de
# silte total de maneira que a soma seja igual a 1000 g/kg. Finalmente, faz-se o processamento dos dados das
# frações granulométricas grossas calhau -- `calhau` --, cascalho -- `cascalho` -- e terra fina -- `terrafina`.
# na ausência da última, utiliza-se as demais para sua estimativa. Caso todas as frações estejam faltando, então
# assume-se que o conteúdo da fração terra fina seja igual a 1000 g/kg. Isso porque é comum os trabalhos omitirem
# o conteúdo de terra fina quando a mesma corresponde a totalidade do solo.

correct_depth <-
  function (profund.sup, profund.inf) {
    res <- as.matrix(data.frame(profund.sup, profund.inf))
    if (any(profund.inf < profund.sup, na.rm = TRUE)) {
      id <- which(profund.inf < profund.sup)
      plus_depth <- max(res[id, 1])
      res[id,  1:2] <- abs(res[id,  1:2] - plus_depth)
      res[-id, 1:2] <- abs(res[-id, 1:2] + plus_depth)
    }
    as.data.frame(res)
  }
camada %<>%
  dplyr::mutate(
    argila_in = !is.na(argila),
    silte_in = !is.na(silte),
    areia_in = !is.na(areia),
    dtp_in = argila_in + silte_in + areia_in,
    argila = ifelse(dtp_in == 2 & !argila_in, 1000 - areia - silte, argila),
    silte = ifelse(dtp_in == 2 & !silte_in, 1000 - argila - areia, silte),
    areia = ifelse(!areia_in, areiagrossa + areiafina, areia),
    areia_in = !is.na(areia),
    areia = ifelse(dtp_in == 2 & !areia_in, 1000 - argila - silte, areia),
    dtp = areia + argila + silte,
    argila = round((argila / dtp) * 1000),
    silte = round((silte / dtp) * 1000),
    areia = round((areia / dtp) * 1000),
    dtp = areia + argila + silte,
    silte = ifelse(dtp != 1000, 1000 - areia - argila, silte),
    terrafina_in = !is.na(terrafina),
    cascalho_in = !is.na(cascalho),
    calhau_in = !is.na(calhau),
    terrafina = ifelse(!terrafina_in & cascalho_in & calhau_in, 1000 - cascalho - calhau, terrafina),
    terrafina = ifelse(!terrafina_in & cascalho_in & !calhau_in, 1000 - cascalho, terrafina),
    terrafina = ifelse(!terrafina_in & !cascalho_in & calhau_in, 1000 - calhau, terrafina),
    terrafina = ifelse(!terrafina_in & !cascalho_in & !calhau_in, 1000, terrafina) %>% round(),
    ph = ifelse(ph > 14, NA_real_, ph),
    carbono = round(carbono, 2),
    dsi = round(dsi, 2),
    profund_sup = ifelse(is.na(profund_inf), NA_real_, abs(profund_sup)),
    profund_inf = ifelse(is.na(profund_sup), NA_real_, abs(profund_inf))
  ) %>% 
  dplyr::group_by(dataset_id, observacao_id) %>%
  dplyr::mutate(
    sup = correct_depth(profund.sup = profund_sup, profund.inf = profund_inf)$profund.sup,
    inf = correct_depth(profund.sup = profund_sup, profund.inf = profund_inf)$profund.inf,
    profund_sup = sup,
    profund_inf = inf
  ) %>% 
  dplyr::ungroup()

# A última etapa de processamento lida com os dados da profundidade do solo, especificamente, com as camadas
# compostas por material orgânico. Em geral, o registro dessas camadas é realizado fixando o limite inferior como
# sendo igual a zero. Isso gera uma sequência invertida de valores de profundidade. Por exemplo, uma camada com 
# profundidade de 3--0 cm representa uma camada orgânica de três centímetros de espessura acima das camadas de
# material mineral do solo. Em alguns casos a profundidade superior pode ser negativa, por exemplo, -3--0 cm. O
# processamento desses dados consiste em ajustar a profundidade superior do solo a profundidade superior da 
# camada de material orgânico do solo, definida assim como sendo igual a zero centímetros de profundidade.

camada %<>% 
  dplyr::select(
    dataset_id, observacao_id, camada_id, amostra_id, camada_nome, profund_sup, profund_inf,
    terrafina, argila, silte, areia, carbono, ctc, ph, dsi, ce) %T>% 
  print()

### Análise exploratória

# Verificar a distribuição empírica dos dados, procurando por insconsistências nos dados.

png("../res/fig/febr-camada.png", width = 480 * 2, height = 480 * 2, res = 72 * 2)
par(oma = c(0, 0, 1, 0), las = 1)
camada %>% 
  dplyr::mutate(fragmentos = 1000 - terrafina) %>% 
  dplyr::rename(profund = profund_inf) %>%
  dplyr::select(
    'profund\n(cm)' = profund,
    'carbono\n(g/kg)' = carbono,
    'argila\n(g/kg)' = argila,
    'areia\n(g/kg)' = areia,
    'silte\n(g/kg)' = silte,
    'terrafina\n(g/kg)' = terrafina,
    'dsi\n(kg/dm³)' = dsi,
    'ctc\n(cmolc/kg)' = ctc,
    'ph\n(-)' = ph,
    'ce\n(mS/cm)' = ce) %>%
  # dplyr::select(
  #   'Depth\n(cm)' = profund, 
  #   'Carbon\n(g/kg)' = carbono,
  #   'Clay\n(g/kg)' = argila, 
  #   'Sand\n(g/kg)' = areia, 
  #   'Silt\n(g/kg)' = silte, 
  #   'Coarse\n(g/kg)' = fragmentos, 
  #   'BD\n(kg/dm³)' = dsi, 
  #   'CEC\n(cmolc/kg)' = ctc,
  #   'pH\n(-)' = ph,
  #   'EC\n(mS/cm)' = ce) %>%
  plot(
    cex = 0.5, col = "firebrick1", 
    main = "Distribuição empírica dos dados",
    # main = "Empirical data distribuion",
    sub = glue::glue("{Sys.Date()}-febr-camada"))
dev.off()

png("../res/fig/febr-camada-200cm.png", width = 480 * 2, height = 480 * 2, res = 72 * 2)
par(oma = c(0, 0, 1, 0), las = 1)
camada %>% 
  dplyr::mutate(fragmentos = 1000 - terrafina) %>% 
  dplyr::rename(profund = profund_inf) %>%
  dplyr::filter(profund <= 200) %>% 
  dplyr::select(
    'profund\n(cm)' = profund,
    'carbono\n(g/kg)' = carbono,
    'argila\n(g/kg)' = argila,
    'areia\n(g/kg)' = areia,
    'silte\n(g/kg)' = silte,
    'terrafina\n(g/kg)' = terrafina,
    'dsi\n(kg/dm³)' = dsi,
    'ctc\n(cmolc/kg)' = ctc,
    'ph\n(-)' = ph,
    'ce\n(mS/cm)' = ce) %>%
  # dplyr::select(
  #   'Depth\n(cm)' = profund, 
  #   'Carbon\n(g/kg)' = carbono,
  #   'Clay\n(g/kg)' = argila, 
  #   'Sand\n(g/kg)' = areia, 
  #   'Silt\n(g/kg)' = silte, 
  #   'Coarse\n(g/kg)' = fragmentos, 
  #   'BD\n(kg/dm³)' = dsi, 
  #   'CEC\n(cmolc/kg)' = ctc,
  #   'pH\n(-)' = ph,
  #   'EC\n(mS/cm)' = ce) %>%
  plot(
    cex = 0.5, col = "firebrick1",
    main = "Distribuição empírica dos dados (< 200 cm)",
    # main = "Empirical data distribuion (< 200 cm)",
    sub = glue::glue("{Sys.Date()}-febr-camada"))
dev.off()

### Salvar dados

# Salvar os dados no formato TXT.

write.table(camada, file = glue::glue("../data/febr-camada.txt"), sep = ";", dec = ",", row.names = FALSE)

## Tabelas _dataset_ + _observacao_ + _camada_

febr <- 
  merge(dataset, observacao, by = "dataset_id") %>% 
  merge(camada, by = c("dataset_id", "observacao_id"))

### Salvar dados

# Salvar os dados no formato TXT.

write.table(febr, file = "../data/febr-superconjunto.txt", sep = ";", dec = ",", row.names = FALSE)

# Packages #########################################################################################
# renv::init()
install.packages(c("languageserver", "openxlsx", "remotes", "rmarkdown"))
remotes::install_github("samuel-rosa/febr-package")
renv::snapshot()
renv::status()

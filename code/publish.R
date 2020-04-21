
x <- openxlsx::read.xlsx(
  xlsxFile = '/home/alessandrorosa/ownCloud/febr-repo/processamento/ctb0001/2020-04-21-ctb0001.xlsx', 
  sheet = 'camada')
write.table(
  x = x,
  file = '/home/alessandrorosa/ownCloud/febr-repo/processamento/ctb0001/ctb0001-camada.csv', 
  sep = ';', dec = ',', row.names = FALSE)


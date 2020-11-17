
#########################################################################################################
####################### Carregando os microdados da Pesquisa Nacional de Saúde ##########################
#########################################################################################################
#####   ESCRITO POR:      Cleiton Rocha - www.linkedin.com/in/cleitonoerocha/
#####   EMAIL:  cleitonrocha@sei.ba.gov.br // cleitonotavio058@gmail.com 
#####   LICENÇA:          GPLv3
#########################################################################################################
#########################################################################################################

library(tidyverse)
library(SAScii)

# pasta de trabalho
setwd("C:\\Users\\pc\\Desktop\\PNS")

############### Dataset ############## 

# Link do FTP
file.path <- "ftp://ftp.ibge.gov.br/PNS/2019/Microdados/Dados/PNS_2019_20201029.zip"

# Fazendo download do arquivo
download.file(url = file.path, destfile = "PNS_2019_20201029.zip" , mode = "wb")

# Descompactando arquivo
files_PNS <- unzip(zipfile = "PNS_2019_20201029.zip")


############### Documentação ##############  

file.doc <-  "ftp://ftp.ibge.gov.br/PNS/2019/Microdados/Documentacao/Dicionario_e_input_20201021.zip"

# Fazendo download do arquivo
download.file(url = file.doc, destfile = "Dicionario_e_input_20201021.zip" , mode = "wb")

# Descompactando arquivo
files_doc_PNS <- unzip(zipfile = "Dicionario_e_input_20201021.zip")


############## Carregando e Salvando ############## 

# lendo os dados
pns2019 <- read.SAScii("PNS_2019.txt","input_PNS_2019.sas" )

# salvando em csv
write.csv2(pns2019, "pns2019.csv", row.names = F)












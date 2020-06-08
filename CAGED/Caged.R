
#########################################################################################################
########################### Carregar dados do Caged, via FTP do MTPS ####################################
#########################################################################################################
#####   ESCRITO POR:      Cleiton Rocha - www.linkedin.com/in/cleitonoerocha/
#####   EMAIL:  cleitonrocha@sei.ba.gov.br // cleitonotavio058@gmail.com 
#####   LICENÇA:          GPLv3
#########################################################################################################
#########################################################################################################

# Pacotes
library(devtools)
library(dplyr)

devtools::install_github("jimhester/archive")
library(archive)

# Pasta de trabalho
setwd("C:\\Users\\cleitonrocha\\Desktop\\Scripts\\CAGED")

# criando arquivos temporários
tf <- tempfile() ; td <- tempdir()

# Link do FTP
file.path <- "ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED/Estabelecimentos/Mar%E7o/acumulado202003.7z"

# Fazendo download e descompactando
download.file( file.path , tf , mode = "wb" )

# Carregando dataset geral
Caged_202003 <- read.csv2(archive_read(tf), encoding = "UTF-8")

# dataset com dados pra Bahia
Bahia_CAGED <- Caged_202003 %>% filter(uf == 29)

# dataset com dados para Salvador/BA
Salvador_CAGED <- Caged_202003 %>% filter(município == 292740)


# Salvando arquivos no formato CSV
write.csv2(Salvador_CAGED, "Salvador_CAGED.csv", row.names = F) # Salvador
write.csv2(Bahia_CAGED,"Bahia_CAGED.csv", row.names = F) # Bahia
write.csv2(Caged_202003, "CAGED_acumulado_2020_03.csv", row.names = F) # Geral





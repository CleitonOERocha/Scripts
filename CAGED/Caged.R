
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
file.path <- "ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED/Estabelecimentos/Mar%E7o/202003.7z"

# Fazendo download e descompactando
download.file( file.path , tf , mode = "wb" )

# Carregando dataset
Caged_202003 <- read.csv2(archive_read(tf), encoding = "UTF-8")

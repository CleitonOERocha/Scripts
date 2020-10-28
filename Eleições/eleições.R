
library(tidyverse)
library(data.table)

# pasta de trabalho
setwd("C:\\Users\\cleitonrocha\\Desktop\\Eleições")

# datasets
#eleitores <- fread("perfil_eleitorado_ATUAL.csv")

eleitores <- fread("perfil_eleitorado_2020.csv")

cod_mun_tse <- read.csv2("https://raw.githubusercontent.com/betafcc/Municipios-Brasileiros-TSE/master/municipios_brasileiros_tse.csv", sep=",", encoding = "UTF-8")

cod_mun_tse <- cod_mun_tse %>% select(codigo_tse, codigo_ibge)

# fazendo join para obter código do IBGE para o municipio
eleitores <- left_join(eleitores, cod_mun_tse, by=c("CD_MUNICIPIO" = "codigo_tse"))


###################################################################################################
####################################### Informações da Bahia ######################################
###################################################################################################

# filtrando UF
eleitores_estado <- eleitores %>% filter(SG_UF == "BA")

# removendo dataset geral
rm(eleitores)

# sexo dos eleitores
sexo_estado <- eleitores_estado %>% group_by(DS_GENERO) %>%
  summarise(total=sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percent = (total/sum(total))*100)

# estado civil dos eleitores
estado_civil_estado <- eleitores_estado %>% group_by(DS_ESTADO_CIVIL) %>%
  summarise(total=sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percent = (total/sum(total))*100)


# faixa etária dos eleitores
fxa_etaria_estado <- eleitores_estado %>% group_by(DS_FAIXA_ETARIA) %>%
  summarise(total=sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percent = (total/sum(total))*100)

# escolaridade dos eleitores
escolaridade_estado <- eleitores_estado %>% group_by(DS_GRAU_ESCOLARIDADE) %>%
  summarise(total=sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percent = (total/sum(total))*100)


# percentual de deficientes entre os eleitores aptos
percent_defs_estado <- (sum(eleitores_estado$QT_ELEITORES_DEFICIENCIA)/sum(eleitores_estado$QT_ELEITORES_PERFIL)*100)

# Total de eleitores por Zona Eleitoral
zonas_eleitorais_estado <- eleitores_estado %>% group_by(NR_ZONA) %>%
  summarise(total=sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percent = (total/sum(total))*100)


###################################################################################################
##################################### Informações por municipio ###################################
###################################################################################################

# filtrando municipio
eleitores_mun <- eleitores_estado %>% filter(codigo_ibge == 2927408)

# sexo dos eleitores
sexo_mun <- eleitores_mun %>% group_by(DS_GENERO) %>%
  summarise(total=sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percent = (total/sum(total))*100)

# estado civil dos eleitores
estado_civil_mun <- eleitores_mun %>% group_by(DS_ESTADO_CIVIL) %>%
  summarise(total=sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percent = (total/sum(total))*100)


# faixa etária dos eleitores
fxa_etaria_mun <- eleitores_mun %>% group_by(DS_FAIXA_ETARIA) %>%
  summarise(total=sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percent = (total/sum(total))*100)

# escolaridade dos eleitores
escolaridade_mun <- eleitores_mun %>% group_by(DS_GRAU_ESCOLARIDADE) %>%
  summarise(total=sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percent = (total/sum(total))*100)


# percentual de deficientes entre os eleitores aptos
percent_defs_mun <- (sum(eleitores_mun$QT_ELEITORES_DEFICIENCIA)/sum(eleitores_mun$QT_ELEITORES_PERFIL)*100)

# Total de eleitores por Zona Eleitoral
zonas_eleitorais_mun <- eleitores_mun %>% group_by(NR_ZONA) %>%
  summarise(total=sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percent = (total/sum(total))*100)


# Salvando dataset apenas com Bahia
write.csv2(eleitores_estado,"eleitores_estado.csv", row.names = F)










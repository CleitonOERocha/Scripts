

library(dplyr)
library(tidyr)
library(purrr)
library(tidyverse)
library(xml2)
library(stringr)
library(rvest)

# altere o caminho para onde os arquivos serão salvos #
setwd("C:\\Users\\cleitonrocha\\Desktop\\Cidacs\\ETL")

########################################################
#### DECRETOS POR ESTADO
########################################################

Acre <- read.csv2("https://leismunicipais.com.br/coronavirus?estado=AC", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Alagoas <- read.csv2("https://leismunicipais.com.br/coronavirus?estado=AL", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Amapá <- read.csv2("https://leismunicipais.com.br/coronavirus?estado=AP", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Amazonas <- read.csv2("https://leismunicipais.com.br/coronavirus?estado=AM", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Bahia<- read.csv2("https://leismunicipais.com.br/coronavirus?estado=BA", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Ceará <-read.csv2("https://leismunicipais.com.br/coronavirus?estado=CE", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Distrito_Federal<- read.csv2("https://leismunicipais.com.br/coronavirus?estado=DF", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Espírito_Santo <- read.csv2("https://leismunicipais.com.br/coronavirus?estado=ES", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Goiás <- read.csv2("https://leismunicipais.com.br/coronavirus?estado=GO", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Maranhão <- read.csv2("https://leismunicipais.com.br/coronavirus?estado=MA", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Mato_Grosso <- read.csv2("https://leismunicipais.com.br/coronavirus?estado=MT", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Mato_Grosso_do_Sul <-read.csv2("https://leismunicipais.com.br/coronavirus?estado=MS", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Minas_Gerais <- read.csv2("https://leismunicipais.com.br/coronavirus?estado=MG", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Pará <- read.csv2("https://leismunicipais.com.br/coronavirus?estado=PA", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Paraíba  <- read.csv2("https://leismunicipais.com.br/coronavirus?estado=PB", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Paraná  <- read.csv2("https://leismunicipais.com.br/coronavirus?estado=PR", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Pernambuco <- read.csv2("https://leismunicipais.com.br/coronavirus?estado=PE", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Piauí <- read.csv2("https://leismunicipais.com.br/coronavirus?estado=PI", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Rio_de_Janeiro <- read.csv2("https://leismunicipais.com.br/coronavirus?estado=RJ", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Rio_Grande_do_Norte <- read.csv2("https://leismunicipais.com.br/coronavirus?estado=RN", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Rio_Grande_do_Sul  <- read.csv2("https://leismunicipais.com.br/coronavirus?estado=RS", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Rondônia <- read.csv2("https://leismunicipais.com.br/coronavirus?estado=RO", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Roraima <- read.csv2("https://leismunicipais.com.br/coronavirus?estado=RR", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Santa_Catarina  <- read.csv2("https://leismunicipais.com.br/coronavirus?estado=SC", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
São_Paulo <- read.csv2("https://leismunicipais.com.br/coronavirus?estado=SP", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Sergipe <- read.csv2("https://leismunicipais.com.br/coronavirus?estado=SE", encoding =  'UTF-8', sep=",", stringsAsFactors = F)
Tocantins <- read.csv2("https://leismunicipais.com.br/coronavirus?estado=TO", encoding =  'UTF-8', sep=",", stringsAsFactors = F)

# criando coluna 'Estado'
Acre <-Acre %>% mutate(Estado = "Acre")
Alagoas <-Alagoas %>% mutate(Estado = "Alagoas")
Amapá <- Amapá %>% mutate(Estado = "Amapá")
Amazonas <- Amazonas %>% mutate(Estado = "Amazonas")
Bahia <- Bahia %>% mutate(Estado = "Bahia")
Ceará <- Ceará %>% mutate(Estado = "Ceará")
Distrito_Federal <- Distrito_Federal %>% mutate(Estado = "Distrito Federal")
Espírito_Santo <- Espírito_Santo %>% mutate(Estado = "Espírito Santo")
Goiás <- Goiás %>% mutate(Estado = "Goiás")
Maranhão <- Maranhão %>% mutate(Estado = "Maranhão")
Mato_Grosso <- Mato_Grosso %>% mutate(Estado = "Mato Grosso")
Mato_Grosso_do_Sul <- Mato_Grosso_do_Sul %>% mutate(Estado = "Mato Grosso do Sul")
Minas_Gerais <- Minas_Gerais %>% mutate(Estado = "Minas Gerais")
Pará <- Pará %>% mutate(Estado = "Pará")
Paraíba <- Paraíba %>% mutate(Estado = "Paraíba")
Paraná <- Paraná %>% mutate(Estado = "Paraná")
Pernambuco <- Pernambuco %>% mutate(Estado = "Pernambuco")
Piauí <- Piauí %>% mutate(Estado = "Piauí")
Rio_de_Janeiro <- Rio_de_Janeiro %>% mutate(Estado = "Rio de Janeiro")
Rio_Grande_do_Norte <- Rio_Grande_do_Norte %>% mutate(Estado = "Rio Grande do Norte")
Rio_Grande_do_Sul <- Rio_Grande_do_Sul %>% mutate(Estado = "Rio Grande do Sul")
Rondônia <- Rondônia %>% mutate(Estado = "Rondônia")
Roraima <- Roraima %>% mutate(Estado = "Roraima")
Santa_Catarina <- Santa_Catarina %>% mutate(Estado = "Santa Catarina")
São_Paulo <- São_Paulo %>% mutate(Estado = "São Paulo")
Sergipe <- Sergipe %>% mutate(Estado = "Sergipe")
Tocantins <- Tocantins %>% mutate(Estado = "Tocantins")


####################################################
######## Dataset com todos os decretos, por Estado
####################################################

DECRETOS_ESTADOS <- list(Acre,Alagoas,Amapá,Amazonas,Bahia,Ceará,Distrito_Federal,
                Espírito_Santo,Goiás,Maranhão,	Mato_Grosso,Mato_Grosso_do_Sul,
                Minas_Gerais,Pará,Paraíba,Paraná,	Pernambuco,	Piauí,
                Rio_de_Janeiro,Rio_Grande_do_Norte,Rio_Grande_do_Sul,
                Rondônia,Roraima,Santa_Catarina,São_Paulo,Sergipe,
                Tocantins) %>%
                  reduce(full_join, by = c("X.U.FEFF.Epigrafe","Localidade","Ementa","Url","Estado")) %>%
                    rename(Decretos = "X.U.FEFF.Epigrafe")

### Criando coluna 'Tipo' com informação se o decreto é Estadual ou Municipal

DECRETOS_ESTADOS <- DECRETOS_ESTADOS %>% mutate(Tipo="")

DECRETOS_ESTADOS$Tipo <- ifelse(DECRETOS_ESTADOS$Localidade %in% c("Acre","Alagoas","Amapá","Amazonas",
                                "Bahia","Ceará","Distrito Federal",
                                "Espírito Santo","Goiás","Maranhão",
                                "Mato Grosso","Mato Grosso do Sul",
                                "Minas Gerais","Pará","Paraíba","Paraná",
                                "Pernambuco","Piauí","Rio de Janeiro",
                                "Rio Grande do Norte","Rio Grande do Sul",
                                "Rondônia","Roraima","Santa Catarina",
                                "São Paulo","Sergipe","Tocantins"), "Estadual", "Municipal")


#############################################################
###### Decretos Federais - Ajustando Javascript para HTML 
#############################################################

writeLines("var url = 'http://www.planalto.gov.br/ccivil_03/Portaria/quadro_portaria.htm';
var page = new WebPage();
var fs = require('fs');

page.open(url, function (status) {
        just_wait();
});

function just_wait() {
    setTimeout(function() {
               fs.write('dec_federal.html', page.content, 'w');
            phantom.exit();
    }, 2500);
}
", con = "scrape.js")

js_scrape <- function(url = "http://www.planalto.gov.br/ccivil_03/Portaria/quadro_portaria.htm", 
                      js_path = "scrape.js", 
                      phantompath = "phantomjs.exe"){
  
  lines <- readLines(js_path)
  lines[1] <- paste0("var url ='", url ,"';")
  writeLines(lines, js_path)
  
  command = paste(phantompath, js_path, sep = " ")
  system(command)
  
}

js_scrape()

###################################################################
######### Carregando o HTML convertido
##################################################################

# lendo o html
html2 <- read_html("dec_federal.html", encoding="UTF-8")

# criando formato de tabela com dados do html
html_tabela <- html2 %>% html_nodes("table") %>% html_table(fill = T)

fed_covid <- html_tabela[[3]]

# Ajustando nome das colunas
fed_covid <- fed_covid %>% rename(Decretos = "X1") %>% rename(Ementa = "X2")

# Ajustando dados no dataset (Excesso de espaçamento, etc)
fed_covid$Decretos <- gsub("[\t\n]", "", fed_covid$Decretos)
fed_covid$Ementa <- gsub("[\t\n]", "", fed_covid$Ementa)

fed_covid$Decretos <- gsub("\\s+"," ", fed_covid$Decretos)
fed_covid$Ementa <- gsub("\\s+"," ", fed_covid$Ementa)

# Criando coluna 'Tipo' e 'Localidade'
fed_covid <- fed_covid %>% mutate(Tipo="Federal") %>% mutate(Localidade="Brasil")

##################################################################
######## Dataset unindo decretos Federais, Estaduais e Municipais
##################################################################

Decretos_Covid_Brasil <- full_join(DECRETOS_ESTADOS, fed_covid, by=c("Decretos","Localidade","Ementa","Tipo"))

#### Criando coluna 'Tipo do Documento'

Decretos_Covid_Brasil$Tipo_Documento <- word(Decretos_Covid_Brasil$Decretos)

### Retirando '/' depois do nome do município 
Decretos_Covid_Brasil$Localidade <- gsub("\\/.*","",Decretos_Covid_Brasil$Localidade)

########################################################################
######### Carregando dataset com códigos dos municípios
#######################################################################

codigos_igbe <- read.csv2("https://raw.githubusercontent.com/CleitonOERocha/Scripts/master/Decretos%20Covid/codigo_municipios_IBGE.csv",
                          encoding = "ISO-8859-1", stringsAsFactors = F)

### Merge entre códigos e dataset
Decretos_Covid_Brasil <- left_join(Decretos_Covid_Brasil, codigos_igbe, by = c("Localidade","Estado"))

### Criando condições para registrar apenas Municípios, e não Estados 
  Decretos_Covid_Brasil$Codigo_Municipio <- ifelse(Decretos_Covid_Brasil$Tipo == "Municipal",
                                                   Decretos_Covid_Brasil$Codigo_Municipio, NA)
  
  Decretos_Covid_Brasil$Nome_Mesorregião <- ifelse(Decretos_Covid_Brasil$Tipo == "Municipal",
                                                   Decretos_Covid_Brasil$Nome_Mesorregião, NA)
  
  Decretos_Covid_Brasil$Cod_UF <- ifelse(Decretos_Covid_Brasil$Tipo == "Municipal",
                                         Decretos_Covid_Brasil$Cod_UF, NA)

############################################################
############# Dataset com todos os decretos em Salvador
############################################################

DECRETOS_SALVADOR <- Decretos_Covid_Brasil %>% filter(Localidade == "Salvador")

############################################################################################
######## Dataset com todos os decretos na Bahia - Apenas Estaduais e Estaduais/Municipais
############################################################################################

DECRETOS_BAHIA_ESTADUAL <- Decretos_Covid_Brasil %>% filter(Localidade == "Bahia") %>% 
                           select(Localidade, Decretos, Ementa, Tipo, Url, Tipo_Documento) # Decretos Estaduais

DECRETOS_BAHIA_TODOS <- Decretos_Covid_Brasil %>% filter(Estado == "Bahia") # Decretos do Estado e Municipios

#############################################
######## Dataset com decretos Federais
#############################################

DECRETOS_FEDERAIS <- Decretos_Covid_Brasil %>% filter(Localidade == "Brasil") %>%
                     select(Localidade, Decretos, Ementa, Tipo, Tipo_Documento)

###########################################################
########## Salvando em CSV
###########################################################

write.csv2(DECRETOS_SALVADOR,"Decretos_em_Salvador.csv", row.names = F) # SSA
write.csv2(DECRETOS_BAHIA_ESTADUAL,"DecretosEstaduais_na_Bahia.csv", row.names = F) # BA Estadual
write.csv2(DECRETOS_BAHIA_TODOS,"Decretos_na_Bahia.csv", row.names = F) # Ba Estaduais e Municipais
write.csv2(Decretos_Covid_Brasil,"Decretos_Covid_Brasil.csv", row.names = F) # Todos os decretos no Brasil
write.csv2(DECRETOS_FEDERAIS,"Decretos_Federais.csv", row.names = F) # Federais 

# removendo datasets não utilizados #
rm(html2, html_tabela, fed_covid)

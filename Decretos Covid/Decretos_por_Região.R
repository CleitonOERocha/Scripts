#########################################################################################################
################# Quadro Geral com total de Decretos por Regiões e Gov. Federal #########################
#########################################################################################################
### obs: O script serve como suporte para o ETL, trazendo um panorâma geral para o total em cada Estado #
#########################################################################################################
#########################################################################################################
#####   ESCRITO POR:      Cleiton Rocha - www.linkedin.com/in/cleitonoerocha/                           #
#####   EMAIL:  cleitonrocha@sei.ba.gov.br // cleitonotavio058@gmail.com                                #
#####   LICENÇA:          GPLv3                                                                         #
#########################################################################################################
#########################################################################################################

library(dplyr)

# altere o caminho para onde os arquivos serão salvos #
setwd("C:\\Users\\cleitonrocha\\Desktop\\Cidacs\\ETL")

# Decretos no Brasil
dec_br <- read.csv2("Decretos_Covid_Brasil.csv")

### Norte
Norte <- dec_br %>%
  filter(Estado %in% c("Pará","Amazonas", "Roraima","Acre","Rondônia","Tocantins","Amapá")) %>% 
    filter(Tipo == "Estadual") %>%
       mutate(cont = 1) %>%
         group_by(Estado, Tipo_Documento) %>%
           summarise(total_cont = sum(cont)) %>%
              mutate(percent = total_cont/sum(total_cont)*100)

### Sudeste
Sudeste <- dec_br %>%
  filter(Estado %in% c("São Paulo", "Rio de Janeiro", "Minas Gerais", "Espírito Santo")) %>% 
    filter(Tipo == "Estadual") %>%
       mutate(cont = 1) %>%
          group_by(Estado, Tipo_Documento) %>%
             summarise(total_cont = sum(cont)) %>%
               mutate(percent = total_cont/sum(total_cont)*100)

### Centro-Oeste
Centro_Oeste <- dec_br %>%
  filter(Estado %in% c("Goiás","Distrito Federal", "Mato Grosso", "Mato Grosso do Sul")) %>% 
    filter(Tipo == "Estadual") %>%
       mutate(cont = 1) %>%
         group_by(Estado, Tipo_Documento) %>%
           summarise(total_cont = sum(cont)) %>%
             mutate(percent = total_cont/sum(total_cont)*100)


### Sul
Sul <- dec_br %>%
  filter(Estado %in% c("Paraná", "Santa Catarina", "Rio Grande do Sul")) %>% 
    filter(Tipo == "Estadual") %>%
      mutate(cont = 1) %>%
        group_by(Estado, Tipo_Documento) %>%
          summarise(total_cont = sum(cont)) %>%
            mutate(percent = total_cont/sum(total_cont)*100)

### Nordeste
Nordeste <- dec_br %>%
  filter(Estado %in% c("Bahia", "Alagoas", "Sergipe", "Maranhão",
                       "Pernambuco", "Rio Grande do Norte", "Ceará", "Paraíba", "Piauí")) %>% 
  filter(Tipo == "Estadual") %>%
    mutate(cont = 1) %>%
      group_by(Estado, Tipo_Documento) %>%
        summarise(total_cont = sum(cont)) %>%
          mutate(percent = total_cont/sum(total_cont)*100)

### Decretos federais
Federal <- dec_br %>%
  filter(Tipo == "Federal") %>%
    mutate(cont = 1) %>%
      group_by(Tipo_Documento) %>%
        summarise(total_cont = sum(cont)) %>%
          mutate(percent = total_cont/sum(total_cont)*100)


### Salvando ###
write.csv2(Sul,"Sul_dec.csv", row.names = F)
write.csv2(Centro_Oeste,"Centro_Oeste_dec.csv", row.names = F)
write.csv2(Norte,"Norte_dec.csv", row.names = F)
write.csv2(Sudeste,"Sudeste_dec.csv", row.names = F)
write.csv2(Nordeste,"Nordeste_dec.csv", row.names = F)
write.csv2(Federal,"Federal_dec.csv", row.names = F)






#########################################################################################################
########################## Calculando a Taxa de Informalidade - PNAD ####################################
#########################################################################################################
#####   ESCRITO POR:      Cleiton Rocha - www.linkedin.com/in/cleitonoerocha/
#####   EMAIL:  cleitonrocha@sei.ba.gov.br // cleitonotavio058@gmail.com 
#####   LICENÇA:          GPLv3
#########################################################################################################
#########################################################################################################

library(dplyr)
library(srvyr) 
library(readr)
library(ggplot2)
library(tidyr)
library(Cairo)
library(scales)
library(ggrepel)

#setwd("C:\\Users\\pc\\Deskt1J2WMuE8JJeN5WsA1v8DxhXBezXy1STMy3")
setwd("C:\\Users\\pc\\Deskt1J2WMuE8JJeN5WsA1v8DxhXBezXy1STMy320")
#setwd("C:\\Users\\pc\\Deskt1J2WMuE8JJeN5WsA1v8DxhXBezXy1STMy320")



### Carregando dataset ###
#pnad_covid <- read_csv("PNAD_COVID_062020.csv", col_types = cols(.default = "d"))
pnad_covid <- read_csv("PNAD_COVID_082020.csv", col_types = cols(.default = "d"))
#pnad_covid <- read_csv("PNAD_COVID_092020.csv", col_types = cols(.default = "d"))


### ligando Pesos ###
pnad_com_pesos <- pnad_covid %>% as_survey_design(ids = UPA, strata = Estrato, weights = V1032, nest = TRUE) #%>%
#filter(CAPITAL == "29")


####### Criando colunas
pnad_com_pesos <- pnad_com_pesos %>% mutate(pop_ocupada = ifelse(C001 == 1,"Trabalha", "Não trabalha"),
                          nao_contribuinte_CP = ifelse(C007 == 7 & C014 == 2, "Conta própria e Não Contribuinte",
                                                       "Demais"),
                          Trab_privado_sem_cart = ifelse(C007 == 4 & C007B == 3,
                                                         "Trablhador Setor Privado sem carteira", "Demais"),
                          Empregadores_nao_contri = ifelse(C007 == 6 & C014 == 2,
                                                           "Empregador que não é contribuinte", "Demais"),
                          Trab_dom_sem_cart = ifelse(C007 == 1 & C007B == 3,
                                                     "Trabalhador doméstico sem carteira","Demais"),
                          Trab_fam_aux = ifelse(C007 == 8, "Trabalhador familiar auxiliar", "Demais")) 




####### Dados sobre informalidade

# populaçao ocupada
pop_ocupada_pnad <- pnad_com_pesos %>%
                      group_by(pop_ocupada) %>%
                         summarise(total = survey_total())

# Trabalhador Conta própria e Não Contribuinte
nao_contribuinte_CP_pnad <- pnad_com_pesos %>%
                              group_by(nao_contribuinte_CP) %>%
                                summarise(total = survey_total())

# Trablhador Setor Privado sem carteira
Trab_privado_sem_cart_pnad <- pnad_com_pesos %>%
                                group_by(Trab_privado_sem_cart) %>%
                                  summarise(total = survey_total())

# Empregador que não é contribuinte
Empregadores_nao_contri_pnad <- pnad_com_pesos %>%
                                  group_by(Empregadores_nao_contri) %>%
                                     summarise(total = survey_total())

# Trabalhador doméstico sem carteira
Trab_dom_sem_cart_pnad <- pnad_com_pesos %>% 
                            group_by(Trab_dom_sem_cart) %>%
                               summarise(total = survey_total())

# Trabalhador familiar auxiliar
Trab_fam_aux_pnad <- pnad_com_pesos %>%
                       group_by(Trab_fam_aux) %>%
                          summarise(total = survey_total())


# criando dataframe sobre informalidade
informalidade <- data.frame(Situação = c(Empregadores_nao_contri_pnad$Empregadores_nao_contri,
                                         nao_contribuinte_CP_pnad$nao_contribuinte_CP,
                                         pop_ocupada_pnad$pop_ocupada,
                                         Trab_fam_aux_pnad$Trab_fam_aux,
                                         Trab_dom_sem_cart_pnad$Trab_dom_sem_cart,
                                         Trab_privado_sem_cart_pnad$Trab_privado_sem_cart),
                            Valor = c(Empregadores_nao_contri_pnad$total,
                                      nao_contribuinte_CP_pnad$total,
                                      pop_ocupada_pnad$total,
                                      Trab_fam_aux_pnad$total,
                                      Trab_dom_sem_cart_pnad$total,
                                      Trab_privado_sem_cart_pnad$total))

# Removendo valores NA's
informalidade <- informalidade %>%
  filter(!is.na(Situação)) %>% filter(!Situação %in%  c("Demais","Não trabalha"))


# Transformando dataset em formato wider
informalidade_wider <- informalidade %>% spread(Situação, Valor)

# calculando taxa de informalidade
txa_informalidade = (informalidade_wider$`Conta própria e Não Contribuinte` +
                    informalidade_wider$`Empregador que contribuinte` +
                    informalidade_wider$`Trabalhador doméstico sem carteira` +
                    informalidade_wider$`Trabalhador familiar auxiliar` +
                    informalidade_wider$`Trablhador Setor Privado sem carteira`)/ informalidade_wider$Trabalha



# arredondando valor
txa_informalidade <- round(txa_informalidade,3)*100








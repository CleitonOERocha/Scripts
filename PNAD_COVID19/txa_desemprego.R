
library(dplyr)
library(srvyr) 
library(readr)


setwd("C:\Users\pc\Desktop\PNAD\PNAD_COVID19\PNAD_COVID_082020")

### Carregando dataset ###
pnad_covid <- read_csv("PNAD_COVID_082020.csv", col_types = cols(.default = "d"))

### ligando Pesos ###
pnad_com_pesos <- pnad_covid %>% as_survey_design(ids = UPA, strata = Estrato, weights = V1032, nest = TRUE) 

########## Pessoas que estão procurando emprego ------------------------------

#### Na semana passada ___ tomou alguma providência efetiva para conseguir trabalho? R = 1 (Sim) #### 
procurou_emprego <- pnad_com_pesos %>% group_by(C015) %>% summarise(total = survey_total())
procurou_emprego <- procurou_emprego %>% filter(C015 == 1) %>% select(total)

########## Pessoas que estão empregadas --------------------------------------

#### Obtendo o número de pessoas empregadas. R = 1 (Sim) ####  
empregados <- pnad_com_pesos %>% group_by(C001) %>% summarise(total = survey_total())
empregados <- empregados %>% filter(C001 == 1) %>% select(total)

########## Taxa de desemprego --------------------------------------

#### calculando a taxa de desemprego (%) ####
txa_desemprego <- round(procurou_emprego$total/sum(procurou_emprego$total, empregados$total),4) * 100

# resultado: 14,33% 










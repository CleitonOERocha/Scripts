library(dplyr)
library(srvyr) 
library(readr)
library(ggplot2)
library(tidyr)
library(Cairo)
library(scales)
library(ggrepel)

setwd("C:\\Users\\pc\\Desktop\\PNAD\\PNAD_COVID19\\PNAD_COVID_082020")
### Carregando dataset ###
pnad_covid <- read_csv("PNAD_COVID_082020.csv", col_types = cols(.default = "d"))

### ligando Pesos e filtrando Salvador ###
pnad_com_pesos <- pnad_covid %>% as_survey_design(ids = UPA, strata = Estrato, weights = V1032, nest = TRUE)# %>%
#   filter(CAPITAL == "29")

### Criando colunas com Variáveis ###
pnad_com_pesos <- pnad_com_pesos %>% mutate(one = 1,
                                            Estado = case_when(UF == 11 ~ "Rondônia" , 
                                                               UF == 12 ~ "Acre" , 
                                                               UF == 13 ~ "Amazonas" , 
                                                               UF == 14 ~ "Roraima" , 
                                                               UF == 15 ~ "Pará" , 
                                                               UF == 16 ~ "Amapá" , 
                                                               UF == 17 ~ "Tocantins" , 
                                                               UF == 21 ~ "Maranhão" , 
                                                               UF == 22 ~ "Piauí" , 
                                                               UF == 23 ~ "Ceará" , 
                                                               UF == 24 ~ "Rio Grande do Norte" , 
                                                               UF == 25 ~ "Paraíba" , 
                                                               UF == 26 ~ "Pernambuco" , 
                                                               UF == 27 ~ "Alagoas" , 
                                                               UF == 28 ~ "Sergipe" , 
                                                               UF == 29 ~ "Bahia" , 
                                                               UF == 31 ~ "Minas Gerais" , 
                                                               UF == 32 ~ "Espírito Santo" , 
                                                               UF == 33 ~ "Rio de Janeiro" , 
                                                               UF == 35 ~ "São Paulo" , 
                                                               UF == 41 ~ "Paraná" , 
                                                               UF == 42 ~ "Santa Catarina" , 
                                                               UF == 43 ~ "Rio Grande do Sul" , 
                                                               UF == 50 ~ "Mato Grosso do Sul" , 
                                                               UF == 51 ~ "Mato Grosso" , 
                                                               UF == 52 ~ "Goiás" , 
                                                               UF == 53 ~ "Distrito Federal"),
                                            Regiao = case_when(UF == 11 ~ "Norte",
                                                               UF == 12 ~ "Norte" , 
                                                               UF == 13 ~ "Norte" , 
                                                               UF == 14 ~ "Norte" , 
                                                               UF == 15 ~ "Norte" , 
                                                               UF == 16 ~ "Norte" , 
                                                               UF == 17 ~ "Norte" , 
                                                               UF == 21 ~ "Nordeste" , 
                                                               UF == 22 ~ "Nordeste" , 
                                                               UF == 23 ~ "Nordeste" , 
                                                               UF == 24 ~ "Nordeste" , 
                                                               UF == 25 ~ "Nordeste" , 
                                                               UF == 26 ~ "Nordeste" , 
                                                               UF == 27 ~ "Nordeste" , 
                                                               UF == 28 ~ "Nordeste" , 
                                                               UF == 29 ~ "Nordeste" ,
                                                               UF == 31 ~ "Sudeste" , 
                                                               UF == 32 ~ "Sudeste" , 
                                                               UF == 33 ~ "Sudeste" , 
                                                               UF == 35 ~ "Sudeste" ,
                                                               UF == 41 ~ "Sul" , 
                                                               UF == 42 ~ "Sul" , 
                                                               UF == 43 ~ "Sul" ,
                                                               UF == 50 ~ "Centro-Oeste" , 
                                                               UF == 51 ~ "Centro-Oeste" , 
                                                               UF == 52 ~ "Centro-Oeste" , 
                                                               UF == 53 ~ "Centro-Oeste"),
                                            Sexo = ifelse(A003 == 1, "Homem", "Mulher"), 
                                            Idade = case_when(
                                              A002 %in% 15:24 ~ "15-24",
                                              A002 %in% 25:34 ~ "25-34", 
                                              A002 %in% 35:49 ~ "35-49", 
                                              A002 %in% 50:64 ~ "50-64", 
                                              A002 > 64 ~ "65+"),
                                            Cor = case_when(
                                              A004 == 1 ~ "Branca", 
                                              A004 == 2 ~ "Preta", 
                                              A004 == 4 ~ "Parda"),
                                            Escolaridade = factor(case_when( 
                                              A005 %in% 1:2 ~ "Sem Instrução ou Fundamental Incompleto", 
                                              A005 %in% 3:4 ~ "Fundamental completo ou Médio Incompleto", 
                                              A005 %in% 5:6 ~ "Médio completo ou Superior Incompleto", 
                                              A005 == 7 ~ "Superior completo", 
                                              A005 == 8 ~ "Pós-graduação"), 
                                              levels = c( "Sem Instrução ou Fundamental Incompleto",
                                                          "Fundamental completo ou Médio Incompleto", 
                                                          "Médio completo ou Superior Incompleto",
                                                          "Superior completo",
                                                          "Pós-graduação")), 
                                            Tipo_emprego = factor(case_when(
                                              C007 == 1 ~ "Trabalhador doméstico (empregado doméstico, cuidados, babá)",
                                              C007 == 2 ~ "Militar",
                                              C007 == 3 ~ "Policial ou Bombeiro",
                                              C007 == 4 ~ "Setor privado",
                                              C007 == 5 ~ "Setor público",
                                              C007 == 6 ~ "Empregador",
                                              C007 == 7 ~ "Autônomo (Conta própria)"),
                                              levels = c( "Trabalhador doméstico (empregado doméstico, cuidados, babá)",
                                                          "Militar", 
                                                          "Policial ou Bombeiro",
                                                          "Setor privado",
                                                          "Setor público",
                                                          "Empregador",
                                                          "Autônomo (Conta própria)")), 
                                            Faixa_salario = factor(case_when(
                                              C01012 <= 1044 ~ "Menos de um salário mínimo",
                                              C01012 %in% c(1045:2090) ~ "Entre 1 e 2",
                                              C01012 %in% c(2091:3135) ~ "Entre 2 e 3",
                                              C01012 %in% c(3136:4180) ~ "Entre 3 e 4",
                                              C01012 %in% c(4181:5225) ~ "Entre 4 e 5",
                                              C01012 >= 5226 ~ "Mais de 5"),
                                              levels = c("Menos de um salário mínimo",
                                                         "Entre 1 e 2",
                                                         "Entre 2 e 3",
                                                         "Entre 3 e 4",
                                                         "Entre 4 e 5",
                                                         "Mais de 5")), 
                                            domicilio_situacao = factor(case_when(
                                              F001 == 1 ~ "Próprio - já pago",
                                              F001 == 2 ~ "Próprio - ainda pagando" ,                                  
                                              F001 == 3 ~ "Alugado",
                                              F001 %in% 4:6 ~ "Cedido (Por empregador, Familiar ou outro)"),
                                              levels = c("Próprio - já pago",
                                                         "Próprio - ainda pagando",
                                                         "Alugado", 
                                                         "Cedido (Por empregador, Familiar ou outro)")),
                                            home_office = ifelse(C013 == 1, "Home Office", "Presencial"),
                                            auxilio_emergencial = ifelse(D0051 == 1, "Auxílio", "Sem auxílio")
                                            
)

########## Home office - Por sexo e cor ##################
# Criando dataset para conferir pessoas em Home Office

funcao_home_sexo_cor <- function(estado) {
  
  home_sexo_cor <- pnad_com_pesos %>%
    filter(Estado == format(estado)) %>% 
    group_by(Sexo, Cor) %>%
    summarise(
      home_office = survey_total(C013 == 1, na.rm = TRUE),
      mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
    mutate(trab_home_office = (home_office/mao_de_obra)*100) 
  
  # removendo colunas NA'S
  home_sexo_cor$`__SRVYR_WITHIN_GRP_ID__.x` <- NULL
  home_sexo_cor$`__SRVYR_WITHIN_GRP_ID__.y` <- NULL
  
  # usando o drop_na
  home_sexo_cor <- drop_na(home_sexo_cor)
  
  
  home_sexo_cor <- home_sexo_cor %>%
    group_by(Sexo) %>%
    mutate(ValuePer=(home_office/sum(home_office))) %>%
    ungroup()
  

########## Home office - Por sexo e cor ##################
  # gráfico
  home_sexo_cor_agrup <- ggplot(home_sexo_cor, aes(fill = Cor, y = ValuePer, x = Sexo)) +
    geom_bar(position = "dodge", stat = "identity") +
    geom_text(aes(label=sprintf("%1.2f%%",ValuePer*100)),size = 3, position =position_dodge(width=0.9),
              vjust=-0.5, color = 'black',fontface='bold') +
    theme_classic() +
    theme(axis.title.x = element_text(colour = "black"),
          axis.title.y = element_text(colour = "black"),
          axis.text.y = element_text(face="bold", color="#000000", 
                                     size=10),
          axis.line = element_line(colour = "black", 
                                   size = 1, linetype = "solid"),
          axis.text=element_text(size=6, face="bold"),
          axis.text.x = element_text(face="bold", color="#000000", size=10),
          plot.title = element_text(colour = "black", size = 17, hjust=0.5),
          legend.position = "bottom", legend.background = element_rect(fill="ghostwhite", size=0.7, linetype="blank")) +
    labs(x = "Sexo",y = "Percentual (%)", fill = "Cor/Raça: ", caption = "Fonte: Microdados da PNAD Covid-19 - IBGE. Agosto de 2020.",
         title = paste0("Pessoas em home office, por cor/raça e sexo - ",estado)) +
    scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7")) +
    scale_y_continuous(labels = percent_format(), limits=c(0,1)) 
  
  ggsave(plot = home_sexo_cor_agrup, "home_sexo_cor_agrup.png",
         width = 10, height = 7, dpi = 120, units = "in",type = "cairo")
  
  write.csv2(home_sexo_cor,"home_sexo_cor.csv", row.names = F)
  

}



################# Home office - Por Cor e Escolaridade #####################

funcao_home_edu_cor <- function(estado) {
  
home_edu_cor <- pnad_com_pesos %>%
  filter(Estado == format(estado)) %>% 
  group_by(Escolaridade, Cor) %>%
  summarise(
    home_office = survey_total(C013 == 1, na.rm = TRUE),
    mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
  mutate(trab_home_office = (home_office/mao_de_obra)*100)

# removendo colunas NA'S
home_edu_cor$`__SRVYR_WITHIN_GRP_ID__.x` <- NULL
home_edu_cor$`__SRVYR_WITHIN_GRP_ID__.y` <- NULL

# usando o drop_na
home_edu_cor <- drop_na(home_edu_cor)

home_edu_cor <- home_edu_cor %>%
  group_by(Cor) %>%
  mutate(ValuePer=(home_office/sum(home_office))) %>%
  ungroup()

# gráfico
home_edu_cor_agrup <- ggplot(home_edu_cor, aes(fill = Escolaridade, y = ValuePer, x = Cor)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",ValuePer*100)),size = 3, position =position_dodge(width=0.9),
            vjust=-0.5, color = 'black',fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold", color="#000000", size=10),
        plot.title = element_text(colour = "black", size = 17, hjust=0.5),
        legend.position = "bottom",
        legend.background = element_rect(fill="ghostwhite", size=0.7, linetype="blank"),
        legend.key.height=unit(0.5, "cm")) +
  labs(x = "Cor/Raça",y = "Percentual (%)", fill = "Escolaridade: ", caption = "Fonte: Microdados da PNAD Covid-19 - IBGE. Agosto de 2020.",
       title = paste0("Pessoas em home office, por cor/raça e escolaridade - ",estado)) +
  scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7","#fdcb6e")) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1)) +
  guides(fill = guide_legend(ncol=3,nrow=2))

# Salvando em PNG
ggsave(plot = home_edu_cor_agrup, "home_edu_cor_agrup.png",
       width = 14, height = 7, dpi = 150, units = "in",type = "cairo")

write.csv2(home_edu_cor,"home_edu_cor.csv", row.names = F)


}


################# Home office - Por Sexo e Idade #####################

funcao_home_sexo_idade <- function(estado) {

home_sexo_idade <- pnad_com_pesos %>%
  filter(Estado == format(estado)) %>% 
  group_by(Sexo, Idade) %>%
  summarise(
    home_office = survey_total(C013 == 1, na.rm = TRUE),
    mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
  mutate(trab_home_office = (home_office/mao_de_obra)*100) 

# removendo colunas NA'S
home_sexo_idade$`__SRVYR_WITHIN_GRP_ID__.x` <- NULL
home_sexo_idade$`__SRVYR_WITHIN_GRP_ID__.y` <- NULL

# usando o drop_na
home_sexo_idade <- drop_na(home_sexo_idade)

home_sexo_idade <- home_sexo_idade %>%
  group_by(Sexo) %>%
  mutate(ValuePer=(home_office/sum(home_office))) %>%
  ungroup()


# gráfico
home_sexo_idade_agrup <- ggplot(home_sexo_idade, aes(fill = Idade, y = ValuePer, x = Sexo)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",ValuePer*100)),size = 3, position =position_dodge(width=0.9),
            vjust=-0.5, color = 'black',fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold", color="#000000", size=10),
        plot.title = element_text(colour = "black", size = 17, hjust=0.5),
        legend.position = "bottom", legend.background = element_rect(fill="ghostwhite", size=0.7, linetype="blank")) +
  labs(x = "Sexo", fill = "Faixa Etária: ",y="Percentual (%)", caption = "Fonte: Microdados da PNAD Covid-19 - IBGE. Agosto de 2020.",
       title = paste0("Pessoas em home office, por sexo e faixa etária - ",estado)) +
  scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7","#fdcb6e")) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1)) 


ggsave(plot = home_sexo_idade_agrup, "home_sexo_idade_agrup.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")  

write.csv2(home_sexo_idade,"home_sexo_idade.csv", row.names = F)

}



########################## Home office - Por trabalho ################

funcao_home_emprego <- function(estado) {

home_emprego <- pnad_com_pesos %>%
  filter(Estado == format(estado)) %>% 
  group_by(Tipo_emprego) %>%
  summarise(
    home_office = survey_total(C013 == 1, na.rm = TRUE),
    mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
  mutate(trab_home_office = (home_office/mao_de_obra)*100)

# removendo colunas NA'S
home_emprego$`__SRVYR_WITHIN_GRP_ID__.x` <- NULL
home_emprego$`__SRVYR_WITHIN_GRP_ID__.y` <- NULL

# usando o drop_na
home_emprego <- drop_na(home_emprego)

home_emprego <- home_emprego %>%
  mutate(ValuePer=(home_office/sum(home_office))) %>%
  ungroup()

# ordenando eixo X
legenda_trabalhos <- c("Trabalhador doméstico\n (empregado doméstico,\n cuidados, babá)",
                       "Militar", 
                       "Policial ou\n Bombeiro",
                       "Setor privado",
                       "Setor público",
                       "Empregador",
                       "Autônomo\n (Conta própria)")


# Gráfico
home_emprego_agrup <- ggplot(home_emprego, aes(fill = Tipo_emprego, y = ValuePer, x = Tipo_emprego)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",ValuePer*100)),size = 3, position =position_dodge(width=0.9),
            vjust=-0.5, color = 'black',fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold", color="#000000", size=8),
        plot.title = element_text(colour = "black", size = 17, hjust=0.5),
        legend.position = "none") +
  labs(x = "Tipo de Ocupação",y="Percentual (%)", caption = "Fonte: Microdados da PNAD Covid-19 - IBGE. Agosto de 2020.",
       title = paste0("Pessoas em home office, por tipo de ocupação - ",estado)) +
  scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7","#fdcb6e","#636e72", "#55efc4")) +
  scale_x_discrete(labels = legenda_trabalhos) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1)) 

ggsave(plot = home_emprego_agrup, "home_emprego_agrup.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo") 

write.csv2(home_emprego,"home_emprego.csv", row.names = F)


} 

################## Home office - Por faixa salarial e cor #####################   

funcao_home_renda <- function(estado) {

home_renda <- pnad_com_pesos %>%
  filter(Estado == format(estado)) %>% 
  group_by(Faixa_salario, Cor) %>%
  summarise(
    home_office = survey_total(C013 == 1, na.rm = TRUE),
    mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
  mutate(trab_home_office = (home_office/mao_de_obra)*100) 

# removendo colunas NA'S
home_renda$`__SRVYR_WITHIN_GRP_ID__.x` <- NULL
home_renda$`__SRVYR_WITHIN_GRP_ID__.y` <- NULL

# usando o drop_na
home_renda <- drop_na(home_renda)

home_renda <- home_renda %>%
  group_by(Cor) %>%
  mutate(ValuePer=(home_office/sum(home_office))) %>%
  ungroup()

# gráfico
home_renda_agrup <- ggplot(home_renda, aes(fill = Faixa_salario, y = ValuePer, x = Cor)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",ValuePer*100)),size = 2.5, position =position_dodge(width=0.9),
            vjust=-0.5, color = 'black',fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold", color="#000000", size=10),
        plot.title = element_text(colour = "black", size = 17, hjust=0.5),
        legend.position = "bottom", legend.background = element_rect(fill="ghostwhite", size=0.7, linetype="blank")) +
  labs(x = "Cor/Raça",y="Percentual (%)", fill = "Faixa Salarial:\n(Salários mínimos) ", caption = "Fonte: Microdados da PNAD Covid-19 - IBGE. Agosto de 2020.",
       title = paste0("Pessoas em home office, por cor/raça e faixa salarial - ",estado)) +
  scale_fill_manual(values = c("#fad390","#e55039","#4a69bd","#60a3bc","#78e08f","#079992")) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1)) 


ggsave(plot = home_renda_agrup, "home_renda_agrup.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")


write.csv2(home_renda,"home_renda.csv", row.names = F)

}



##################### Auxilio - Faixa Salarial ####################

funcao_auxilio_renda <- function(estado) {

auxilio_renda <- pnad_com_pesos %>%
  filter(Estado == format(estado)) %>% 
  group_by(Faixa_salario) %>%
  summarise(
    auxilio = survey_total(D0051 == 1, na.rm = TRUE),
    total = survey_total(one, na.rm = TRUE)) %>%
  mutate(pessoas_auxilio = (auxilio/total)*100)

# removendo colunas NA'S
auxilio_renda$`__SRVYR_WITHIN_GRP_ID__.x` <- NULL
auxilio_renda$`__SRVYR_WITHIN_GRP_ID__.y` <- NULL

# usando o drop_na
auxilio_renda <- drop_na(auxilio_renda)

auxilio_renda <- auxilio_renda %>%
  mutate(ValuePer=(auxilio/sum(auxilio))) %>%
  ungroup()


# gráfico
auxilio_renda_agrup <- ggplot(auxilio_renda, aes(fill = Faixa_salario, y = ValuePer, x = Faixa_salario)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",ValuePer*100)),size = 3, position =position_dodge(width=0.9),
            hjust=-0.1, color = 'black',fontface='bold') +
  theme_classic() +
  coord_flip() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold", color="#000000", size=10),
        plot.title = element_text(colour = "black", size = 17, hjust=0.5),
        legend.position = "none") +
  labs(x = "Faixa Salarial", y="Percentual (%)", caption = "Fonte: Microdados da PNAD Covid-19 - IBGE. Agosto de 2020.",
       title = paste0("Pessoas que receberam auxílio emergencial, por renda - ",estado)) +
  scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7","#fdcb6e","#636e72")) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1)) 

ggsave(plot = auxilio_renda_agrup, "auxilio_renda_agrup.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")

write.csv2(auxilio_renda,"auxilio_renda.csv", row.names = F)


}



########################### Auxilio - Sexo e Cor #########################

funcao_auxilio_cor_sexo <- function(estado) {
  
auxilio_cor_sexo <- pnad_com_pesos %>%
  filter(Estado == format(estado)) %>% 
  group_by(Cor, Sexo) %>%
  summarise(
    auxilio = survey_total(D0051 == 1, na.rm = TRUE),
    total = survey_total(one, na.rm = TRUE)) %>%
  mutate(pessoas_auxilio = (auxilio/total)*100)

# removendo colunas NA'S
auxilio_cor_sexo$`__SRVYR_WITHIN_GRP_ID__.x` <- NULL
auxilio_cor_sexo$`__SRVYR_WITHIN_GRP_ID__.y` <- NULL

# usando o drop_na
auxilio_cor_sexo <- drop_na(auxilio_cor_sexo)

auxilio_cor_sexo <- auxilio_cor_sexo %>%
  group_by(Sexo) %>%
  mutate(ValuePer=(auxilio/sum(auxilio))) %>%
  ungroup()


# gráfico
auxilio_cor_sexo_agrup <- ggplot(auxilio_cor_sexo, aes(fill = Cor, y = ValuePer, x = Sexo)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",ValuePer*100)),size = 3, position =position_dodge(width=0.9),
            vjust=-0.5, color = 'black',fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold", color="#000000", size=10),
        plot.title = element_text(colour = "black", size = 17, hjust=0.5),
        legend.position = "bottom", legend.background = element_rect(fill="ghostwhite", size=0.7, linetype="blank")) +
  labs(fill = "Cor: ", x = "Sexo",y="Percentual (%)",caption = "Fonte: Microdados da PNAD Covid-19 - IBGE. Agosto de 2020.",
       title = paste0("Pessoas que receberam auxílio emergencial, por cor/raça e sexo - ",estado)) +
  scale_fill_manual(values = c("#00b894","#ff7675","#0984e3")) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1)) 

ggsave(plot = auxilio_cor_sexo_agrup, "auxilio_cor_sexo_agrup.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo") 

write.csv2(auxilio_cor_sexo,"auxilio_cor_sexo.csv", row.names = F)

}

funcao_auxilio_cor_sexo("Distrito Federal")
funcao_auxilio_renda("Distrito Federal")
funcao_home_edu_cor("Distrito Federal")
funcao_home_emprego("Distrito Federal")
funcao_home_renda("Distrito Federal")
funcao_home_sexo_cor("Distrito Federal")
funcao_home_sexo_idade("Distrito Federal")

# Tamanho de cada população 
# sum(auxilio_renda$auxilio)
# sum(auxilio_domicilio$auxilio)  
# sum(home_sexo_cor$home_office)
# sum(home_emprego$home_office)
# sum(home_renda$home_office)  
# sum(auxilio_cor_sexo$auxilio)  




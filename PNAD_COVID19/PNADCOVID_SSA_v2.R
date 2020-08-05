
library(dplyr)
library(srvyr) 
library(readr)
library(ggplot2)
library(tidyr)
library(Cairo)

setwd("C:\\Users\\pc\\Desktop\\PNAD\\PNAD_COVID19")

### Carregando dataset ###
pnad_covid <- read_csv("PNAD_COVID_052020.csv", col_types = cols(.default = "d"))

### ligando Pesos e filtrando Salvador ###
pnad_com_pesos <- pnad_covid %>% as_survey_design(ids = UPA, strata = Estrato, weights = V1032, nest = TRUE) %>%
  filter(CAPITAL == "29")

### Criando colunas com Variáveis ###
pnad_com_pesos <- pnad_com_pesos %>% mutate(one = 1,
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
home_sexo_cor <- pnad_com_pesos %>%
  group_by(Sexo, Cor) %>%
  summarise(
    home_office = survey_total(C013 == 1, na.rm = TRUE),
    mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
  mutate(trab_home_office = (home_office/mao_de_obra)*100) %>%
  drop_na()


# gráfico
home_sexo_cor_ssa <- ggplot(home_sexo_cor, aes(fill = Cor, y = trab_home_office, x = Sexo)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",trab_home_office)),size = 3, position =position_dodge(width=0.9),
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
  labs(x = "Sexo", fill = "Cor/Raça: ", caption = "Fonte: Microdados da PNAD-COVID19 - IBGE",
       title = "Pessoas em home office, por cor/raça e sexo - Salvador/BA") +
  scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7")) +
  scale_y_discrete(limits=factor(0:100), breaks = c(0,10,20,30,40,50,60,70,80,90,100), name = "Percentual (%)")

ggsave(plot = home_sexo_cor_ssa, "home_sexo_cor_ssa.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")

################# Home office - Por Cor e Escolaridade #####################

home_edu_cor <- pnad_com_pesos %>%
  group_by(Escolaridade, Cor) %>%
  summarise(
    home_office = survey_total(C013 == 1, na.rm = TRUE),
    mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
  mutate(trab_home_office = (home_office/mao_de_obra)*100) %>%
  drop_na()


# gráfico
home_edu_cor_ssa <- ggplot(home_edu_cor, aes(fill = Escolaridade, y = trab_home_office, x = Cor)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",trab_home_office)),size = 3, position =position_dodge(width=0.9),
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
  labs(x = "Cor/Raça", fill = "Escolaridade: ", caption = "Fonte: Microdados da PNAD-COVID19 - IBGE",
       title = "Pessoas em home office, por cor/raça e escolaridade - Salvador/BA ") +
  scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7","#fdcb6e")) +
  scale_y_discrete(limits=factor(0:100), breaks = c(0,10,20,30,40,50,60,70,80,90,100), name = "Percentual (%)")

ggsave(plot = home_edu_cor_ssa, "home_edu_cor_ssa.png",
       width = 14, height = 7, dpi = 150, units = "in",type = "cairo")



################# Home office - Por Sexo e Idade #####################

home_sexo_idade <- pnad_com_pesos %>%
  group_by(Sexo, Idade) %>%
  summarise(
    home_office = survey_total(C013 == 1, na.rm = TRUE),
    mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
  mutate(trab_home_office = (home_office/mao_de_obra)*100) %>%
  drop_na()


# gráfico
home_sexo_idade_ssa <- ggplot(home_sexo_idade, aes(fill = Idade, y = trab_home_office, x = Sexo)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",trab_home_office)),size = 3, position =position_dodge(width=0.9),
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
  labs(x = "Sexo", fill = "Faixa Etária: ", caption = "Fonte: Microdados da PNAD-COVID19 - IBGE",
       title = "Pessoas em home office, por sexo e faixa etária - Salvador/BA") +
  scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7","#fdcb6e")) +
  scale_y_discrete(limits=factor(0:100), breaks = c(0,10,20,30,40,50,60,70,80,90,100), name = "Percentual (%)")


ggsave(plot = home_sexo_idade_ssa, "home_sexo_idade_ssa.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")

########################## Home office - Por trabalho ################

home_emprego <- pnad_com_pesos %>%
  group_by(Tipo_emprego) %>%
  summarise(
    home_office = survey_total(C013 == 1, na.rm = TRUE),
    mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
  mutate(trab_home_office = (home_office/mao_de_obra)*100) %>%
  drop_na()


# ordenando eixo X
legenda_trabalhos <- c("Trabalhador doméstico\n (empregado doméstico,\n cuidados, babá)",
"Militar", 
"Policial ou\n Bombeiro",
"Setor privado",
"Setor público",
"Empregador",
"Autônomo\n (Conta própria)")


# Gráfico
home_emprego_ssa <- ggplot(home_emprego, aes(fill = Tipo_emprego, y = trab_home_office, x = Tipo_emprego)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",trab_home_office)),size = 3, position =position_dodge(width=0.9),
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
  labs(x = "Tipo de Ocupação", caption = "Fonte: Microdados da PNAD-COVID19 - IBGE",
       title = "Pessoas em home office, por tipo de ocupação - Salvador/BA") +
  scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7","#fdcb6e","#636e72", "#55efc4")) +
  scale_x_discrete(labels = legenda_trabalhos) +
  scale_y_discrete(limits=factor(0:100), breaks = c(0,10,20,30,40,50,60,70,80,90,100), name = "Percentual (%)")


ggsave(plot = home_emprego_ssa, "home_emprego_ssa.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")

################## Home office - Por faixa salarial e cor #####################   

home_renda <- pnad_com_pesos %>%
  group_by(Faixa_salario, Cor) %>%
  summarise(
    home_office = survey_total(C013 == 1, na.rm = TRUE),
    mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
  mutate(trab_home_office = (home_office / mao_de_obra) * 100) %>%
  drop_na()


# gráfico
home_renda_ssa <- ggplot(home_renda, aes(fill = Faixa_salario, y = trab_home_office, x = Cor)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",trab_home_office)),size = 2.5, position =position_dodge(width=0.9),
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
  labs(x = "Cor/Raça", fill = "Faixa Salarial:\n(Salários mínimos) ", caption = "Fonte: Microdados da PNAD-COVID19 - IBGE",
       title = "Pessoas em home office, por cor/raça e faixa salarial - Salvador/BA ") +
  scale_fill_manual(values = c("#fad390","#e55039","#4a69bd","#60a3bc","#78e08f","#079992")) +
  scale_y_discrete(limits=factor(0:100), breaks = c(0,10,20,30,40,50,60,70,80,90,100), name = "Percentual (%)")


ggsave(plot = home_renda_ssa, "home_renda_ssa.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")

##################### Auxilio - Faixa Salarial ####################

auxilio_renda <- pnad_com_pesos %>%
  group_by(Faixa_salario) %>%
  summarise(
    auxilio = survey_total(D0051 == 1, na.rm = TRUE),
    total = survey_total(one, na.rm = TRUE)) %>%
  mutate(pessoas_auxilio = (auxilio/total)*100) %>%
  drop_na()


# gráfico
auxilio_renda_ssa <- ggplot(auxilio_renda, aes(fill = Faixa_salario, y = pessoas_auxilio, x = Faixa_salario)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",pessoas_auxilio)),size = 3, position =position_dodge(width=0.9),
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
  labs(x = "Faixa Salarial", caption = "Fonte: Microdados da PNAD-COVID19 - IBGE",
       title = "Pessoas que receberam auxílio emergencial, por renda - Salvador/BA") +
  scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7","#fdcb6e","#636e72")) +
  scale_y_discrete(limits=factor(0:100), breaks = c(0,10,20,30,40,50,60,70,80,90,100), name = "Percentual (%)")

ggsave(plot = auxilio_renda_ssa, "auxilio_renda_ssa.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")

################## Auxilio - Por tipo do domicilio #####################   

auxilio_domicilio <- pnad_com_pesos %>%
  group_by(domicilio_situacao) %>%
  summarise(
    auxilio = survey_total(D0051 == 1, na.rm = TRUE),
    total = survey_total(one, na.rm = TRUE)) %>%
  mutate(pessoas_auxilio  = (auxilio/total)*100) %>%
  drop_na()

# ordenando eixo X
legenda_domicilio <- c("Próprio (já pago)",
                       "Próprio (ainda pagando)",
                       "Alugado", 
                       "Cedido (Por empregador,\n Familiar ou outro)")

# gráfico
auxilio_domicilio_ssa <- ggplot(auxilio_domicilio, aes(fill = domicilio_situacao, y = pessoas_auxilio, x = domicilio_situacao)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",pessoas_auxilio)),size = 3, position =position_dodge(width=0.9),
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
        legend.position = "none") +
  labs(x = "Tipo de domicílio", y ="Percentual (%)",caption = "Fonte: Microdados da PNAD-COVID19 - IBGE",
       title = "Situação do domicílio daqueles que receberam o auxílio emergencial -\n Salvador/BA") +
  scale_fill_manual(values = c("#fad390","#e55039","#4a69bd","#60a3bc","#78e08f","#079992")) +
  scale_x_discrete(labels = legenda_domicilio) +
  scale_y_discrete(limits=factor(0:100), breaks = c(0,10,20,30,40,50,60,70,80,90,100), name = "Percentual (%)")

ggsave(plot = auxilio_domicilio_ssa, "auxilio_domicilio_ssa.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")


########################### Auxilio - Sexo e Cor #########################
auxilio_cor_sexo <- pnad_com_pesos %>%
  group_by(Cor, Sexo) %>%
  summarise(
    auxilio = survey_total(D0051 == 1, na.rm = TRUE),
    total = survey_total(one, na.rm = TRUE)) %>%
  mutate(pessoas_auxilio = (auxilio/total)*100) %>%
  drop_na()


# gráfico
auxilio_cor_sexo_ssa <- ggplot(auxilio_cor_sexo, aes(fill = Cor, y = pessoas_auxilio, x = Sexo)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",pessoas_auxilio)),size = 3, position =position_dodge(width=0.9),
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
  labs(fill = "Cor: ", x = "Sexo", caption = "Fonte: Microdados da PNAD-COVID19 - IBGE",
       title = "Pessoas que receberam auxílio emergencial, por cor/raça e sexo -\n Salvador/BA") +
  scale_fill_manual(values = c("#00b894","#ff7675","#0984e3")) +
  scale_y_discrete(limits=factor(0:100), breaks = c(0,10,20,30,40,50,60,70,80,90,100), name = "Percentual (%)")

ggsave(plot = auxilio_cor_sexo_ssa, "auxilio_cor_sexo_ssa.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")


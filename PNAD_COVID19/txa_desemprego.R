
library(dplyr)
library(srvyr) 
library(readr)
library(tidyverse)
library(ggrepel)

setwd("C:\\Users\\pc\\Desktop\\PNAD\\PNAD_COVID19")

### Carregando dataset ###
pnad_covid <- read_csv("PNAD_COVID_112020.csv", col_types = cols(.default = "d"))

### ligando Pesos ###
pnad_com_pesos <- pnad_covid %>% as_survey_design(ids = UPA, strata = Estrato, weights = V1032, nest = TRUE) 

### Criando colunas com Variáveis ###
pnad_com_pesos <- pnad_com_pesos %>% mutate(Escolaridade = factor(case_when( 
  A005 == 1 ~ "Sem instrução", 
  A005 == 2 ~ "Fundamental incompleto", 
  A005 == 3 ~ "Fundamental completo", 
  A005 == 4 ~ "Médio incompleto", 
  A005 == 5 ~ "Médio completo", 
  A005 == 6 ~ "Superior incompleto", 
  A005 == 7 ~ "Superior completo", 
  A005 == 8 ~ "Pós-graduação"), 
  levels = c( "Sem instrução",
              "Fundamental incompleto",
              "Fundamental completo",
              "Médio incompleto",
              "Médio completo",
              "Superior incompleto", 
              "Superior completo", 
              "Pós-graduação")), 
Tipo_emprego = factor(case_when(
  C007 == 1 ~ "Trabalhador doméstico (empregado doméstico, cuidados, babá)",
  C007 == 2 ~ "Militar",
  C007 == 3 ~ "Policial ou Bombeiro",
  C007 == 4 ~ "Setor privado",
  C007 == 5 ~ "Setor público",
  C007 == 6 ~ "Empregador",
  C007 == 7 ~ "Autônomo (Conta própria)",
  C007 %in% 8:9 ~ "Trabalhador familiar não remunerado ou fazia apenas afazeres domésticos/produção para próprio consumo"),
  levels = c( "Trabalhador doméstico (empregado doméstico, cuidados, babá)",
              "Militar", 
              "Policial ou Bombeiro",
              "Setor privado",
              "Setor público",
              "Empregador",
              "Autônomo (Conta própria)"))) 

########## Pessoas que estão procurando emprego ------------------------------

#### Na semana passada ___ tomou alguma providência efetiva para conseguir trabalho? R = 1 (Sim) #### 
procurou_emprego <- pnad_com_pesos %>% group_by(C015) %>% summarise(total = survey_total())
procurou_emprego <- procurou_emprego %>% filter(C015 == 1) %>% select(total)
procurou_emprego
########## Pessoas que estão empregadas --------------------------------------

#### Obtendo o número de pessoas empregadas. R = 1 (Sim) ####  
empregados <- pnad_com_pesos %>% group_by(C001) %>% summarise(total = survey_total())
empregados <- empregados %>% filter(C001 == 1) %>% select(total)
empregados
########## Taxa de desemprego --------------------------------------

#### calculando a taxa de desemprego (%) ####
txa_desemprego <- round(procurou_emprego$total/sum(procurou_emprego$total, empregados$total),5) * 100
txa_desemprego

# resultado: 14,595% 

################################################################################
########## Gráficos sobre desemprego e escolaridade/trabalho ---------------
################################################################################

# gerando dados agrupados
escolaridade_desempregados <- pnad_com_pesos %>%
  filter(C001 == 2) %>% # Na semana passada, por pelo menos uma hora, trabalhou ou fez algum bico? R = 2 (Não)
  filter(C015 == 1) %>% # Na semana passada ___ tomou alguma providência efetiva para conseguir trabalho? R = 1 (Sim)
  group_by(Escolaridade) %>%
  summarise(total = survey_total())

# coluna com percentual
escolaridade_desempregados <- escolaridade_desempregados %>% mutate(percentual = total/sum(total))

######################### Gráfico de barras --------------------

# ordenando eixo X
legenda_escolaridade <- c("Sem\n instrução",
                          "Fundamental\n incompleto",
                          "Fundamental\n completo",
                          "Médio\n incompleto",
                          "Médio\n completo",
                          "Superior\n incompleto",
                          "Superior\n completo",
                          "Pós-graduação")

# gráfico de barras
educ_desemp <- ggplot(escolaridade_desempregados, aes(fill = Escolaridade, y = percentual, x = Escolaridade)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",percentual*100)),size = 3, position =position_dodge(width=0.9),
            vjust=-0.5, color = 'black',fontface='bold') +
  theme_minimal() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=8),
        axis.text.x = element_text(face="bold", color="#000000", size=8),
        plot.title = element_text(colour = "black", size = 14),
        legend.position = "none") +
  labs(x = "",y = "%", fill = "Escolaridade: ",
       caption = "Elaborado por: Cleiton Rocha (cleitonotavio058@gmail.com)\nFonte: Microdados da PNAD Covid-19 - IBGE. Novembro, 2020.",
       title = "Escolaridade dos indivíduos que estão desempregados") +
  scale_fill_manual(values = c("#2d3a5c","#5a4973","#8b5681","#b96584","#e07a7e",
                               "#fa9874","#ffbd6e","#ffe675")) +
  scale_y_continuous(labels = percent_format(), limits=c(0,0.4)) +
  scale_x_discrete(labels = legenda_escolaridade) 
  
# Salvando em PNG
ggsave(plot = educ_desemp, "educ_desemp.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")

######################### Gráfico de pizza --------------------
# criando coluna com a posicao da legenda
escolaridade_desempregados <- escolaridade_desempregados %>% arrange(desc(Escolaridade)) %>%
  mutate(yposicao_legenda = cumsum(percentual)- 0.5*percentual)


# gráfico de pizza
pizza_desemp <- ggplot(escolaridade_desempregados,aes(x= "", y= percentual, fill=Escolaridade)) +
  geom_bar(width=1, stat = "identity") +
  coord_polar("y", start=0) +
  geom_label_repel(aes(y=yposicao_legenda, label = sprintf("%1.1f%%",percentual*100)), size = 4,
                   color = 'black',fontface='bold',show_guide = F) +
  labs(x = "",y = "", fill = "Escolaridade: ", caption = "Fonte: Microdados da PNAD Covid-19 - IBGE. Novembro, 2020.",
       title = "Escolaridade dos indivíduos que estão desempregados") +
  theme_minimal() +
  theme(legend.position="bottom",
        axis.text = element_blank(),
        legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank"),
        legend.text = element_text(size=9),
        legend.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(colour = "black", size = 12, hjust=0.5, face="bold")) +
  scale_fill_manual(values = c("#2d3a5c","#5a4973","#8b5681","#b96584","#e07a7e",
                               "#fa9874","#ffbd6e","#ffe675")) +
  #scale_fill_manual(values = c("#2d3a5c","#7f537f","#ce6f82","#fea172","#ffe675")) +
  guides(fill = guide_legend(ncol=3,nrow=3))


# Salvando em PNG
ggsave(plot = pizza_desemp, "pizza_desemp.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")





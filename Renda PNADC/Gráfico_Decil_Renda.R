

library(PNADcIBGE)
library(srvyr)
library(survey)
library(tidyverse)
library(Cairo)


# Pasta de Trabalho #
setwd("C:\\Users\\pc\\Desktop\\PNAD\\PNAD2019")

# OBS: Façam o download dos microdados no site do IBGE. Depois de descompactar, vai gerar dois arquivos txt's: Os microdados e o input.

### Carregando dataset ###
dados_pnadc <- read_pnadc("PNADC_012019_20190729.txt", "Input_PNADC_trimestral.txt")

### ligando Pesos ###
pnad_com_pesos <- dados_pnadc %>% as_survey_design(ids = UPA, strata = Estrato, weights = V1028, nest = TRUE)

### Criando colunas com Variáveis ###
pnad_com_pesos <- pnad_com_pesos %>% mutate(UF_nomes = case_when(
                                              UF == 11 ~ "Rondônia",
                                              UF == 12 ~ "Acre",
                                              UF == 13 ~ "Amazonas",
                                              UF == 14 ~ "Roraima",
                                              UF == 15 ~ "Pará",
                                              UF == 16 ~ "Amapá",
                                              UF == 17 ~ "Tocantins",
                                              UF == 21 ~ "Maranhão",
                                              UF == 22 ~ "Piauí",
                                              UF == 23 ~ "Ceará",
                                              UF == 24 ~ "Rio Grande do Norte",
                                              UF == 25 ~ "Paraíba",
                                              UF == 26 ~ "Pernambuco",
                                              UF == 27 ~ "Alagoas",
                                              UF == 28 ~ "Sergipe",
                                              UF == 29 ~ "Bahia",
                                              UF == 31 ~ "Minas Gerais",
                                              UF == 32 ~ "Espírito Santo",
                                              UF == 33 ~ "Rio de Janeiro",
                                              UF == 35 ~ "São Paulo",
                                              UF == 41 ~ "Paraná",
                                              UF == 42 ~ "Santa Catarina",
                                              UF == 43 ~ "Rio Grande do Sul",
                                              UF == 50 ~ "Mato Grosso do Sul",
                                              UF == 51 ~ "Mato Grosso",
                                              UF == 52 ~ "Goiás",
                                              UF == 53 ~ "Distrito Federal")) 
                                            
                                            
                                            
################### Renda por UF #########################
                                            
# Renda 
rend_percentil <- svyby(~VD4020, ~UF_nomes, pnad_com_pesos,
                        svyquantile, quantiles = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci=TRUE, na.rm=T)

# reestruturando dataset
Renda_quantile_UF <- rend_percentil %>% select("0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","UF_nomes") %>%
  gather(key="quantil",value = "renda", - "UF_nomes")


# ordenando eixo X
legenda_quantis <- c("10% mais pobres", "20% mais pobres","30% mais pobres",
                     "40% mais pobres","50% mais pobres/\nricos", "40% mais ricos",
                     "30% mais ricos","20% mais ricos","10% mais ricos")

# retirando números decimais
Renda_quantile_UF$renda <- round(Renda_quantile_UF$renda,0)

# paleta
paleta <- c("#6b856d","#0b2345","#78465f","#dd9587")

# gráfico
decil_por_uf <- ggplot(Renda_quantile_UF, aes(y = quantil, x = reorder(UF_nomes,+renda))) +   
                   geom_tile(aes(fill = renda)) + 
                   geom_text(aes(label = format(round(renda, 0),big.mark = ".", decimal.mark=",")),
                                 colour="white", size = 2) +
                   coord_flip() +
                   theme_classic() +
                   theme(axis.text.y = element_text(color="#000000", size=6),
                         axis.text.x = element_text(color="#000000", size=6),
                         axis.line = element_line(colour = "white", 
                                 size = 0, linetype = "solid"),
                         plot.title = element_text(face="bold", colour = "black", size = 10, hjust=0.0),
                        legend.position = "right", legend.background = element_rect(fill="ghostwhite", size=0.7, linetype="blank")) +
                   labs(x="",y="", color="", title = "Rendimento médio mensal por quantil de renda, Brasil, por UF, 2019.",
                        caption = "Elaborado por Cleiton Rocha. (cleitonotavio058@gmail.com)\n Fonte: PNADC 2019. IBGE.", fill="Renda: ")+
                   scale_y_discrete(labels = legenda_quantis) +
                   scale_fill_gradientn(colours = paleta)


# Salvando em PNG
ggsave(plot = decil_por_uf, "decil_por_uf8.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")



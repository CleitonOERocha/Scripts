

library(tidyverse)
library(RColorBrewer)
library(data.table)

# pasta de trabalho
setwd("C:\\Users\\cleitonrocha\\Desktop\\Eleições")

eleitores_estado <- fread("eleitores_estado.csv")

#####################################################################################################
######################################## Gráfico de gênero ##########################################
#####################################################################################################


# sexo do eleitorado
sexo_estado <- eleitores_estado %>% group_by(DS_GENERO) %>%
  summarise(total=sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percent = (total/sum(total))*100)


# criando coluna com a posicao da legenda
sexo_estado <- sexo_estado %>% arrange(desc(DS_GENERO)) %>%
  mutate(yposicao_legenda = cumsum(percent)- 0.5*percent)


# alterando o nome das variaveis 
sexo_estado <- sexo_estado %>% mutate(DS_GENERO = recode(DS_GENERO, "MASCULINO" = "Masculino",
                                                         "FEMININO" = "Feminino","NÃO INFORMADO"="Não informado"))



# alterando o nome das variaveis 
sexo_estado$DS_GENERO <- factor(sexo_estado$DS_GENERO, levels=c("Masculino",
                                                                "Feminino",
                                                                "Não informado"))
# Grafico
ggplot(sexo_estado,aes(x="", y=percent, fill=DS_GENERO)) +
  geom_bar(width=1, stat = "identity") +
  coord_polar("y", start=0) +
  geom_text(aes(y=yposicao_legenda, label = sprintf("%1.2f%%",percent)), size = 4,
            color = 'black',fontface='bold') +
  scale_fill_manual(values = c("#1b6db5","#b51b8f","#28df99"))+
  labs(x="",y="", title="Gênero dos eleitores - Bahia, 2020.",
       caption = "Fonte: Repositório de dados eleitorais. TSE.",fill="") +
  theme_minimal() +
  theme(legend.position="bottom", legend.title = element_text(colour="Black", size=12, face="bold"),
        legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face="bold", color="#000000", 
                                   size=9))


#####################################################################################################
################################### Gráfico de Estado civil #########################################
#####################################################################################################

# estado civil dos eleitores
estado_civil_estado <- eleitores_estado %>% group_by(DS_ESTADO_CIVIL) %>%
  summarise(total=sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percent = (total/sum(total))*100)



# Ajustando o eixo x
estado_civil_estado <- estado_civil_estado %>% mutate(DS_ESTADO_CIVIL=recode(DS_ESTADO_CIVIL, 
                                                                       "SEPARADO JUDICIALMENTE"="Separado Jud.","DIVORCIADO"="Divorciado",
                                                                       "VIÚVO" = "Viúvo", "CASADO" = "Casado", "SOLTEIRO" = "Solteiro",
                                                                       "NÃO INFORMADO" = "Não informado"))

# grafico
ggplot(estado_civil_estado, aes(x=reorder(DS_ESTADO_CIVIL, +percent), y=percent)) + 
  geom_bar(stat="identity",fill="#389393") +
  geom_text(aes(label=sprintf("%1.2f%%",percent)),size = 4, position =position_dodge(width=0.9),
            vjust=-0.25, color = 'black',fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold",color="#000000", size=10),
        legend.text = element_text(size=9, face="bold"),
        legend.title = element_text(size = 9, face = "bold"),
        plot.title = element_text(colour = "black", size = 14, hjust=0.5, face="bold"), 
        legend.position = "bottom", legend.background = element_rect(fill="ghostwhite", size=0.7, 
                                                                     linetype="blank")) +
  labs(y="Percentual (%)", x="Estado Civil", title = "Estado civil do eleitorado - Bahia. 2020.") +
  guides(fill=F) +
  scale_y_continuous(breaks = c(0,20,40,60,80,100), labels = c("0","20","40","60","80","100"))


#####################################################################################################
################################### Gráfico de Faixa etária #########################################
#####################################################################################################

# faixa etária dos eleitores
fxa_etaria_estado <- eleitores_estado %>% group_by(DS_FAIXA_ETARIA) %>%
  summarise(total=sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percent = (total/sum(total))*100)

fxa_etaria_estado$DS_FAIXA_ETARIA <- str_squish(fxa_etaria_estado$DS_FAIXA_ETARIA)


# Ajustando o eixo x
fxa_etaria_estado <- fxa_etaria_estado %>% 
  mutate(faixa_etaria=dplyr::recode(DS_FAIXA_ETARIA, "100 anos ou mais" = ">= 75",
                                    "16 anos" = "<= 18",
                                    "17 anos" = "<= 18",
                                    "18 anos" = "<= 18",
                                    "19 anos" = "19 a 24",
                                    "20 anos" = "19 a 24",
                                    "21 a 24 anos" = "19 a 24",
                                    "25 a 29 anos" = "25 a 39",
                                    "30 a 34 anos" = "25 a 39",
                                    "35 a 39 anos" = "25 a 39",
                                    "40 a 44 anos" = "40 a 54",
                                    "45 a 49 anos" = "40 a 54",
                                    "50 a 54 anos" = "40 a 54",
                                    "55 a 59 anos" = "55 a 64",
                                    "60 a 64 anos" = "55 a 64",
                                    "65 a 69 anos" = "65 a 74",
                                    "70 a 74 anos" = "65 a 74",
                                    "75 a 79 anos" = ">= 75",
                                    "80 a 84 anos" = ">= 75",
                                    "85 a 89 anos" = ">= 75",
                                    "90 a 94 anos" = ">= 75",
                                    "95 a 99 anos" = ">= 75",
                                    "Inválido " = "Inválido "))


fxa_etaria_estado <- fxa_etaria_estado %>% 
                        group_by(faixa_etaria) %>%
                          summarise(total_fxa = sum(total)) %>%
                             mutate(percentual = (total_fxa/sum(total_fxa)*100))


# alterando o nome das variaveis 
fxa_etaria_estado$faixa_etaria <- factor(fxa_etaria_estado$faixa_etaria, levels=c("<= 18", "19 a 24",
                                                                "25 a 39", "40 a 54",
                                                                "55 a 64", "65 a 74",">= 75", "Inválido"))



fxa_etaria_estado <- fxa_etaria_estado[-8,]



# grafico
ggplot(fxa_etaria_estado, aes(x=faixa_etaria, y=percentual)) + 
  geom_bar(stat="identity",fill="#726a95") +
  geom_text(aes(label=sprintf("%1.2f%%",percentual)),size = 4, position =position_dodge(width=0.9),
            vjust=-0.25, color = 'black',fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold",color="#000000", size=10),
        legend.text = element_text(size=9, face="bold"),
        legend.title = element_text(size = 9, face = "bold"),
        plot.title = element_text(colour = "black", size = 14, hjust=0.5, face="bold"), 
        legend.position = "bottom", legend.background = element_rect(fill="ghostwhite", size=0.7, 
                                                                     linetype="blank")) +
  labs(y="Percentual (%)", x="Faixa etária", title = "Faixa etária do eleitorado - Bahia. 2020.") +
  guides(fill=F) +
  scale_y_continuous(breaks = c(0,20,40,60,80,100), labels = c("0","20","40","60","80","100"))




#####################################################################################################
################################### Gráfico de Escolaridade #########################################
#####################################################################################################


# escolaridade dos eleitores
escolaridade_estado <- eleitores_estado %>% group_by(DS_GRAU_ESCOLARIDADE) %>%
  summarise(total=sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percent = (total/sum(total))*100)

# Ajustando o eixo x
escolaridade_estado <- escolaridade_estado %>% 
  mutate(DS_GRAU_ESCOLARIDADE=recode(DS_GRAU_ESCOLARIDADE, "ANALFABETO" = "Analfabeto",
                                     "ENSINO FUNDAMENTAL COMPLETO" = "Ensino fundamental completo",
                                     "ENSINO FUNDAMENTAL INCOMPLETO" = "Ensino fundamental incompleto",
                                     "ENSINO MÉDIO COMPLETO" = "Ensino médio completo",
                                     "ENSINO MÉDIO INCOMPLETO" = "Ensino médio incompleto",
                                     "LÊ E ESCREVE" = "Lê e escreve",
                                     "NÃO INFORMADO" = "Não informado",
                                     "SUPERIOR COMPLETO" = "Superior completo",
                                     "SUPERIOR INCOMPLETO" = "Superior incompleto"))


escolaridade_estado$DS_GRAU_ESCOLARIDADE <- factor(escolaridade_estado$DS_GRAU_ESCOLARIDADE, levels=c("Não informado",
                                                                "Analfabeto",
                                                                "Lê e escreve",
                                                                "Ensino fundamental incompleto",
                                                                "Ensino fundamental completo",
                                                                "Ensino médio incompleto",
                                                                "Ensino médio completo",
                                                                "Superior incompleto",
                                                                "Superior completo"))


# ordenando eixo X
legenda_escolaridade <- c("Não informado",
                       "Analfabeto",
                       "Lê e escreve",
                       "Ensino fundamental\n incompleto",
                       "Ensino fundamental\n completo",
                       "Ensino médio\n incompleto",
                       "Ensino médio\n completo",
                       "Superior\n incompleto",
                       "Superior\n completo")

# grafico
ggplot(escolaridade_estado, aes(x=DS_GRAU_ESCOLARIDADE, y=percent)) + 
  geom_bar(stat="identity",fill="#f5a25d") +
  geom_text(aes(label=sprintf("%1.2f%%",percent)),size = 4, position =position_dodge(width=0.9),
            vjust=-0.25, color = 'black',fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold",color="#000000", size=8),
        legend.text = element_text(size=9, face="bold"),
        legend.title = element_text(size = 9, face = "bold"),
        plot.title = element_text(colour = "black", size = 14, hjust=0.5, face="bold"), 
        legend.position = "bottom", legend.background = element_rect(fill="ghostwhite", size=0.7, 
                                                                     linetype="blank")) +
  labs(y="Percentual (%)", x="Escolaridade", title = "Escolaridade do eleitorado - Bahia. 2020.") +
  guides(fill=F) +
  scale_y_continuous(breaks = c(0,20,40,60,80,100), labels = c("0","20","40","60","80","100")) +
  scale_x_discrete(labels = legenda_escolaridade)



###########################################################################################################
######################################### Tipo de deficiência #############################################
###########################################################################################################

defs_ba <- fread("perfil_eleitor_deficiencia_2020_BA.csv")

tipo_def <- defs_ba %>% group_by(DS_TIPO_DEFICIENCIA) %>% summarise(total=sum(n())) %>% mutate(percent=(total/sum(total))*100)


# Ajustando o eixo x
tipo_def <- tipo_def %>% 
  mutate(DS_TIPO_DEFICIENCIA=recode(DS_TIPO_DEFICIENCIA, "DEFICIÊNCIA AUDITIVA" = "Auditiva",
                                    "DEFICIÊNCIA DE LOCOMOÇÃO" = "Locomoção", "DEFICIÊNCIA VISUAL" = "Visual",
                                    "DIFICULDADE PARA O EXERCÍCIO DO VOTO" = "Dificuldade para o exercício do voto",
                                    "OUTROS" = "Outros"))




tipo_def$DS_TIPO_DEFICIENCIA <- factor(tipo_def$DS_TIPO_DEFICIENCIA, levels=c("Outros", "Dificuldade para o exercício do voto",
                                                                              "Auditiva","Visual","Locomoção"))


# ordenando eixo X
legenda_defs <- c("Outros","Dificuldade para o\n exercício do voto","Auditiva","Visual","Locomoção")




# grafico
ggplot(tipo_def, aes(x=DS_TIPO_DEFICIENCIA, y=percent)) + 
  geom_bar(stat="identity",fill="#ff414d") +
  coord_flip() +
  geom_text(aes(label=sprintf("%1.2f%%",percent)),size = 3, position =position_dodge(width=0.9),
            hjust=-0.25, color = 'black',fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold",color="#000000", size=8),
        legend.text = element_text(size=9, face="bold"),
        legend.title = element_text(size = 9, face = "bold"),
        plot.title = element_text(colour = "black", size = 14, hjust=0.5, face="bold"), 
        legend.position = "bottom", legend.background = element_rect(fill="ghostwhite", size=0.7, 
                                                                     linetype="blank")) +
  labs(y="Percentual (%)", x="Deficiência", title = "Tipo de deficiência. Bahia. 2020") +
  guides(fill=F) +
  scale_y_continuous(breaks = c(0,20,40,60,80,100), labels = c("0","20","40","60","80","100")) +
  scale_x_discrete(labels = legenda_defs)









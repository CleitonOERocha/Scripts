
########## Home office - Por sexo e cor ##################
############ EXEMPLO COM BARRA 100% EMPILHADA ############
# Criando dataset para conferir pessoas em Home Office
home_sexo_cor <- pnad_com_pesos %>%
  group_by(Sexo, Cor) %>%
  summarise(
    home_office = survey_total(C013 == 1, na.rm = TRUE),
    mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
  mutate(trab_home_office = (home_office/mao_de_obra)*100) %>%
  drop_na()


home_sexo_cor <- home_sexo_cor %>%
                 group_by(Sexo) %>%
                 mutate(ValuePer=(home_office/sum(home_office))) %>%
                 ungroup()
  

  # gráfico
home_sexo_cor_ssa_100 <- ggplot(home_sexo_cor, aes(fill = Cor, y = ValuePer, x = Sexo)) +
    geom_bar(position = "fill", stat = "identity") +
    geom_text(aes(label = percent(ValuePer)),
              position = position_stack(vjust = .5),color = 'black',fontface='bold') +
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
    labs(x = "Sexo",y = "Percentual (%)", fill = "Cor/Raça: ", caption = "Fonte: Microdados da PNAD-COVID19 - IBGE",
         title = "Pessoas em home office, por cor/raça e sexo - Salvador/BA") +
    scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7")) +
    scale_y_continuous(labels = scales::percent)

  ggsave(plot = home_sexo_cor_ssa_100, "home_sexo_cor_ssa_100.png",
         width = 10, height = 7, dpi = 120, units = "in",type = "cairo")
  
########## Home office - Por sexo e cor ##################
  # gráfico
  home_sexo_cor_ssa_agrup <- ggplot(home_sexo_cor, aes(fill = Cor, y = ValuePer, x = Sexo)) +
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
    labs(x = "Sexo",y = "Percentual (%)", fill = "Cor/Raça: ", caption = "Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
         title = "Pessoas em home office, por cor/raça e sexo - Salvador/BA") +
    scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7")) +
    scale_y_continuous(labels = percent_format(), limits=c(0,1)) 
  
  ggsave(plot = home_sexo_cor_ssa_agrup, "home_sexo_cor_ssa_agrup.png",
         width = 10, height = 7, dpi = 120, units = "in",type = "cairo")
  
    
################# Home office - Por Cor e Escolaridade #####################

  home_edu_cor <- pnad_com_pesos %>%
    group_by(Escolaridade, Cor) %>%
      summarise(
           home_office = survey_total(C013 == 1, na.rm = TRUE),
           mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
         mutate(trab_home_office = (home_office/mao_de_obra)*100) %>%
           drop_na() %>% 
              group_by(Cor) %>% 
                mutate(ValuePer = (home_office/sum(home_office))) %>% 
                   ungroup()
  
  #home_edu_cor <- home_edu_cor %>%
  # group_by(Cor) %>%
  #  mutate(ValuePer=(home_office/sum(home_office))) %>%
  #  ungroup()
  
  
  # gráfico
  home_edu_cor_ssa_agup <- ggplot(home_edu_cor, aes(fill = Escolaridade, y = ValuePer, x = Cor)) +
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
    labs(x = "Cor/Raça",y = "Percentual (%)", fill = "Escolaridade: ", caption = "Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
         title = "Pessoas em home office, por cor/raça e escolaridade - Salvador/BA ") +
    scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7","#fdcb6e")) +
    scale_y_continuous(labels = percent_format(), limits=c(0,1)) +
    guides(fill = guide_legend(ncol=3,nrow=2))
  
  ggsave(plot = home_edu_cor_ssa_agup, "home_edu_cor_ssa_agup.png",
         width = 14, height = 7, dpi = 150, units = "in",type = "cairo")  
  
  
################# Home office - Por Sexo e Idade #####################
  
  home_sexo_idade <- pnad_com_pesos %>%
    group_by(Sexo, Idade) %>%
    summarise(
      home_office = survey_total(C013 == 1, na.rm = TRUE),
      mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
    mutate(trab_home_office = (home_office/mao_de_obra)*100) %>%
    drop_na() %>% 
    group_by(Sexo) %>% 
    mutate(ValuePer = (home_office/sum(home_office))) %>% 
    ungroup()
  
 
  # gráfico
  home_sexo_idade_ssa_agrup <- ggplot(home_sexo_idade, aes(fill = Idade, y = ValuePer, x = Sexo)) +
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
    labs(x = "Sexo", fill = "Faixa Etária: ",y="Percentual (%)", caption = "Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
         title = "Pessoas em home office, por sexo e faixa etária - Salvador/BA") +
    scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7","#fdcb6e")) +
    scale_y_continuous(labels = percent_format(), limits=c(0,1)) 
    
  
  ggsave(plot = home_sexo_idade_ssa_agrup, "home_sexo_idade_ssa_agrup.png",
         width = 10, height = 5, dpi = 120, units = "in",type = "cairo")  
  
  
########################## Home office - Por trabalho ################
  
  home_emprego <- pnad_com_pesos %>%
    group_by(Tipo_emprego) %>%
    summarise(
      home_office = survey_total(C013 == 1, na.rm = TRUE),
      mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
    mutate(trab_home_office = (home_office/mao_de_obra)*100) %>%
    drop_na() %>% 
    mutate(ValuePer = (home_office/sum(home_office)))
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
  home_emprego_ssa_agrup <- ggplot(home_emprego, aes(fill = Tipo_emprego, y = ValuePer, x = Tipo_emprego)) +
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
    labs(x = "Tipo de Ocupação",y="Percentual (%)", caption = "Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
         title = "Pessoas em home office, por tipo de ocupação - Salvador/BA") +
    scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7","#fdcb6e","#636e72", "#55efc4")) +
    scale_x_discrete(labels = legenda_trabalhos) +
    scale_y_continuous(labels = percent_format(), limits=c(0,1)) 
  
  
  ggsave(plot = home_emprego_ssa_agrup, "home_emprego_ssa_agrup.png",
         width = 10, height = 5, dpi = 120, units = "in",type = "cairo") 
  
################## Home office - Por faixa salarial e cor #####################   
  
  home_renda <- pnad_com_pesos %>%
    group_by(Faixa_salario, Cor) %>%
    summarise(
      home_office = survey_total(C013 == 1, na.rm = TRUE),
      mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
    mutate(trab_home_office = (home_office/mao_de_obra)*100) %>%
    drop_na() %>% 
    group_by(Cor) %>% 
    mutate(ValuePer = (home_office/sum(home_office))) %>% 
    ungroup()
  
  # gráfico
  home_renda_ssa_agrup <- ggplot(home_renda, aes(fill = Faixa_salario, y = ValuePer, x = Cor)) +
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
    labs(x = "Cor/Raça",y="Percentual (%)", fill = "Faixa Salarial:\n(Salários mínimos) ", caption = "Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
         title = "Pessoas em home office, por cor/raça e faixa salarial - Salvador/BA ") +
    scale_fill_manual(values = c("#fad390","#e55039","#4a69bd","#60a3bc","#78e08f","#079992")) +
    scale_y_continuous(labels = percent_format(), limits=c(0,1)) 
  
  
  ggsave(plot = home_renda_ssa_agrup, "home_renda_ssa_agrup.png",
         width = 10, height = 5, dpi = 120, units = "in",type = "cairo")
  
  
##################### Auxilio - Faixa Salarial ####################
  
  auxilio_renda <- pnad_com_pesos %>%
    group_by(Faixa_salario) %>%
    summarise(
      auxilio = survey_total(D0051 == 1, na.rm = TRUE),
      total = survey_total(one, na.rm = TRUE)) %>%
    mutate(pessoas_auxilio = (auxilio/total)*100) %>%
    drop_na() %>% 
    mutate(ValuePer = (auxilio/sum(auxilio))) %>% 
    ungroup()
  
  
  # gráfico
  auxilio_renda_ssa_agrup <- ggplot(auxilio_renda, aes(fill = Faixa_salario, y = ValuePer, x = Faixa_salario)) +
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
    labs(x = "Faixa Salarial", y="Percentual (%)", caption = "Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
         title = "Pessoas que receberam auxílio emergencial, por renda - Salvador/BA") +
    scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7","#fdcb6e","#636e72")) +
    scale_y_continuous(labels = percent_format(), limits=c(0,1)) 
  
  ggsave(plot = auxilio_renda_ssa_agrup, "auxilio_renda_ssa_agrup.png",
         width = 10, height = 5, dpi = 120, units = "in",type = "cairo")
  
  
  ################## Auxilio - Por tipo do domicilio #####################   
  
  auxilio_domicilio <- pnad_com_pesos %>%
    group_by(domicilio_situacao) %>%
    summarise(
      auxilio = survey_total(D0051 == 1, na.rm = TRUE),
      total = survey_total(one, na.rm = TRUE)) %>%
    mutate(pessoas_auxilio  = (auxilio/total)*100) %>%
    drop_na() %>% 
    mutate(ValuePer = (auxilio/sum(auxilio))) %>% 
    ungroup()
  
  # ordenando eixo X
  legenda_domicilio <- c("Próprio (já pago)",
                         "Próprio (ainda pagando)",
                         "Alugado", 
                         "Cedido (Por empregador,\n Familiar ou outro)")
  
  # gráfico
  auxilio_domicilio_ssa_agrup <- ggplot(auxilio_domicilio, aes(fill = domicilio_situacao, y = ValuePer, x = domicilio_situacao)) +
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
          legend.position = "none") +
    labs(x = "Tipo de domicílio", y ="Percentual (%)",caption = "Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
         title = "Situação do domicílio daqueles que receberam o auxílio emergencial -\n Salvador/BA") +
    scale_fill_manual(values = c("#fad390","#e55039","#4a69bd","#60a3bc","#78e08f","#079992")) +
    scale_x_discrete(labels = legenda_domicilio) +
    scale_y_continuous(labels = percent_format(), limits=c(0,1)) 
  
  ggsave(plot = auxilio_domicilio_ssa_agrup, "auxilio_domicilio_ssa_agrup.png",
         width = 10, height = 5, dpi = 120, units = "in",type = "cairo") 
  
  
########################### Auxilio - Sexo e Cor #########################
  auxilio_cor_sexo <- pnad_com_pesos %>%
    group_by(Cor, Sexo) %>%
    summarise(
      auxilio = survey_total(D0051 == 1, na.rm = TRUE),
      total = survey_total(one, na.rm = TRUE)) %>%
    mutate(pessoas_auxilio = (auxilio/total)*100) %>%
    drop_na() %>% 
    group_by(Sexo) %>% 
    mutate(ValuePer = (auxilio/sum(auxilio))) %>% 
    ungroup()
  
  
  # gráfico
  auxilio_cor_sexo_ssa_agrup <- ggplot(auxilio_cor_sexo, aes(fill = Cor, y = ValuePer, x = Sexo)) +
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
    labs(fill = "Cor: ", x = "Sexo",y="Percentual (%)",caption = "Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
         title = "Pessoas que receberam auxílio emergencial, por cor/raça e sexo -\n Salvador/BA") +
    scale_fill_manual(values = c("#00b894","#ff7675","#0984e3")) +
    scale_y_continuous(labels = percent_format(), limits=c(0,1)) 
  
  ggsave(plot = auxilio_cor_sexo_ssa_agrup, "auxilio_cor_sexo_ssa_agrup.png",
         width = 10, height = 5, dpi = 120, units = "in",type = "cairo") 
 
  
  # Tamanho de cada população 
  sum(auxilio_renda$auxilio)
  sum(auxilio_domicilio$auxilio)  
  sum(home_sexo_cor$home_office)
  sum(home_emprego$home_office)
  sum(home_renda$home_office)  
  sum(auxilio_cor_sexo$auxilio)  

  
  
    
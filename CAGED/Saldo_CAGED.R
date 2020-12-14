#########################################################################################################
############################# Calculando Saldo de empregos - CAGED ######################################
#########################################################################################################
#####   ESCRITO POR:      Cleiton Rocha - www.linkedin.com/in/cleitonoerocha/
#####   EMAIL:  cleitonrocha@sei.ba.gov.br // cleitonotavio058@gmail.com 
#####   LICENÇA:          GPLv3
#########################################################################################################
#########################################################################################################

library(readxl)
library(tidyverse)
library(scales)

# pasta de trabalho
setwd("C:\\Users\\pc\\Desktop\\Scripts\\CAGED")

# caminho para download, descompactação e carregamento dos dados do Novo CAGED
url = 'http://pdet.mte.gov.br/images/Novo_CAGED/Ago2020/3-tabelas.xlsx'

download.file(url, destfile='caged.xlsx', mode='wb')

data = read_excel('caged.xlsx', sheet = 'Tabela 5.1',
                  range="B5:F13") %>%
  mutate(`Mês` = parse_date(`Mês`, format='%B/%Y', locale=locale('pt')))

# condição para registrar negativos e positivos com cores diferentes
data <-data %>% mutate(Color = ifelse(Saldos < 0, "negativo","green"))

# gráfico
caged_graph <- ggplot(data, aes(x=Mês, y=Saldos, fill=Color)) +
  labs(y="Saldo", x="", fill="",
       title = "Saldo de admissões no Novo CAGED. Janeiro a Agosto. 2020.",
       caption = "Fonte: Novo CAGED. Ministério da Economia - Secretaria de Trabalho.") +
  geom_bar(position = "dodge", stat = "identity") +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = format(Saldos,big.mark = "."),
                vjust = ifelse(Saldos >= 0, 0, 1)),size = 2.5,
            color = 'black',fontface='bold') +
  theme_classic() +
  theme(plot.title = element_text(face="bold", size = 12),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.title.y = element_text(size=10,  colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=8),
        axis.text.x = element_text(face="bold", color="#000000", 
                                   size=9),
        legend.position = "none") +
  scale_fill_manual(values = c("#9dc7d3","#831b2c")) +
  scale_y_continuous(breaks = c(-900000,-600000,-300000,-100000,0,100000,200000,300000),
                     labels = c("-900.000","-600.000","-300.000","-100.000","0","100.000",
                                "200.000","300.000")) +
  scale_x_date(date_labels = "%b", breaks = "1 month")

# salvando
ggsave(plot = caged_graph, "caged_graph.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")
  
  
  
  

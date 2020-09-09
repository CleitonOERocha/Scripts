
library(viridis)
library(dplyr)
library(ggplot2)
library(Cairo)
library(tidyr)
library(ggrepel)
library(RColorBrewer)

# Pasta de trabalho
setwd("C:\\Users\\pc\\Desktop\\Cidacs\\ETL")

# Dados
Decretos_Covid_Brasil <- read.csv2("Decretos_Covid_Brasil.csv")

Decretos_Covid_Brasil <- Decretos_Covid_Brasil %>% mutate(one=1)


######################## Gráficos - Decretos por UF ###############################

# Agrupando
total_estados <- Decretos_Covid_Brasil %>% 
                                    group_by(Estado) %>% 
                                       summarise(total=sum(one)) %>% 
                                         drop_na()

# Convertendo em factor
total_estados$Estado <- as.factor(total_estados$Estado)

# gráfico
graph_uf <- ggplot(total_estados, aes(fill = Estado, y = total, x =Estado)) +
  geom_bar(position = "dodge", stat = "identity") +
   geom_text(aes(label=total),size = 2.5, position =position_dodge(width=0.9),
             hjust=-0.1, color = 'black',fontface='bold') +
  theme_classic() +
  coord_flip() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=8),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold", color="#000000", size=10),
        plot.title = element_text(colour = "black", size = 17, hjust=0.5),
        legend.position = "none") +
  labs(x = "", y="", caption = "Fonte: https://leismunicipais.com.br/\nDados coletados em 08/09/2020",
       title = "Total de decretos (estaduais + municipais), por UF") +
  scale_x_discrete(limits = rev(levels(total_estados$Estado))) +
  scale_fill_viridis(discrete=TRUE,option = "B")

# Salvando em PNG
ggsave(plot = graph_uf, "graph_uf.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")


######################## Gráficos - Decretos por mês ################################

# Recortando apenas o mês do decreto
Decretos_Covid_Brasil$mes_dec <- format(as.Date(Decretos_Covid_Brasil$Decretos_date), "%m")


# Nome do mês
Decretos_Covid_Brasil$mes_dec <- factor(Decretos_Covid_Brasil$mes_dec, levels = c("01","02","03","04",
                                                                                  "05","06","07","08","09"),
                                        labels = c("Janeiro","Fevereiro","Março","Abril","Maio","Junho","Julho",
                                                   "Agosto","Setembro"))

# Agrupando
total_mes <- Decretos_Covid_Brasil %>% 
  group_by(mes_dec) %>% 
  summarise(total=sum(one)) %>% 
  drop_na()


# gráfico
graph_mes <- ggplot(total_mes, aes(fill = mes_dec, y = total, x =mes_dec)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=total),size = 2.5, position =position_dodge(width=0.9),
             vjust=-0.5, color = 'black',fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=8),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold", color="#000000", size=10),
        plot.title = element_text(colour = "black", size = 17, hjust=0.5),
        legend.position = "none") +
  labs(x = "", y="", caption = "Fonte: https://leismunicipais.com.br/\nDados coletados em 08/09/2020",
       title = "Total de decretos no Brasil, por mês") +
  scale_fill_viridis(discrete=TRUE,option = "B") +
  scale_y_discrete(limits=factor(0:6000), breaks = c(0,1000,2000,3000,4000,5000,6000), name = "")


# Salvando em PNG
ggsave(plot = graph_mes, "graph_mes.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")

 
######################## Tipo de ato normativo ###################

# Agrupando 
total_doc <- Decretos_Covid_Brasil %>% 
  group_by(Tipo_Documento) %>% 
  summarise(total=sum(one)) %>% 
  drop_na()

# Classificando decretos menores que 100 em 'Outros'
total_doc$Tipo_Documento <- ifelse(total_doc$total < 100,"Outros",
                                   total_doc$Tipo_Documento)

# reagrupando e calculando porcentagem 
total_doc <- total_doc %>% 
  group_by(Tipo_Documento) %>% 
  summarise(total=sum(total)) %>% 
  drop_na() %>% 
  mutate(percent = total/sum(total)*100)

# criando coluna com a posicao da legenda
total_doc <- total_doc %>% arrange(desc(Tipo_Documento)) %>%
  mutate(yposicao_legenda = cumsum(percent)- 0.5*percent)

# gráfico
ato_graph <- ggplot(total_doc,aes(x= "", y= percent, fill=Tipo_Documento)) +
  geom_bar(width=1, stat = "identity") +
  coord_polar("y", start=0) +
   geom_label_repel(aes(y=yposicao_legenda, label = sprintf("%1.1f%%",percent)), size = 4,
                         color = 'black',fontface='bold') +
  labs(x="",y="",title = "Tipo de ato normativo", fill = "Ato normativo: ",
       caption = "Fonte: https://leismunicipais.com.br/\nDados coletados em 08/09/2020") +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank"),
        axis.text.x = element_text(face="bold",color="#000000", size=12),
        legend.text = element_text(size=10, face="bold"),
        legend.title = element_text(size = 8, face = "bold"),
        plot.title = element_text(colour = "black", size = 12, hjust=0.5, face="bold")) +
  scale_fill_manual(values = c("#581845","#900C3F","#C70039","#FF5733","#FFC300")) 
  

# Salvando em PNG
ggsave(plot = ato_graph, "ato_graph.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")

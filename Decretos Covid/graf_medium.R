library(viridis)

Decretos_Covid_Brasil <- Decretos_Covid_Brasil %>% mutate(one=1)

tota_estados <- Decretos_Covid_Brasil %>% filter(Tipo == "Estadual") %>%
                                    group_by(Estado) %>% 
                                       summarise(total=sum(one)) 


# gráfico
ggplot(tota_estados, aes(y = total, x =Estado)) +
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
       title = "Total de decretos estaduais") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_viridis(discrete=TRUE,option = "B")





library(dplyr)
library(Cairo)
library(lubridate)
library(ggthemes)
library(gridExtra)
library(ggplot2)

# dataset
casos_br <- read.csv2("https://raw.githubusercontent.com//wcota//covid19br//master//cases-brazil-cities-time.csv",
                       sep=",", encoding = "UTF-8")

casos_br$date <- ymd(casos_br$date)

##### datasets

bsb <- casos_br %>% filter(city == "Brasília/DF") %>% group_by(date) %>% summarise(total_casos = sum(totalCases))

pa <- casos_br %>% filter(state == "PA")  %>% group_by(date) %>% summarise(total_casos = sum(totalCases))

sp <- casos_br %>% filter(state == "SP") %>% group_by(date) %>% summarise(total_casos = sum(totalCases))

rj <- casos_br %>% filter(state == "RJ") %>% group_by(date) %>% summarise(total_casos = sum(totalCases))

# estados do NE
MA <- casos_br %>% filter(state == "MA") %>% group_by(date) %>% summarise(total_casos = sum(totalCases))
PE <- casos_br %>% filter(state == "PE") %>% group_by(date) %>% summarise(total_casos = sum(totalCases))
SE <- casos_br %>% filter(state == "SE") %>% group_by(date) %>% summarise(total_casos = sum(totalCases))
CE <- casos_br %>% filter(state == "CE") %>% group_by(date) %>% summarise(total_casos = sum(totalCases))
PB <- casos_br %>% filter(state == "PB") %>% group_by(date) %>% summarise(total_casos = sum(totalCases))
RN <- casos_br %>% filter(state == "RN") %>% group_by(date) %>% summarise(total_casos = sum(totalCases))
ba <- casos_br %>% filter(state == "BA") %>% group_by(date) %>% summarise(total_casos = sum(totalCases))
AL <- casos_br %>% filter(state == "AL") %>% group_by(date) %>% summarise(total_casos = sum(totalCases))
PI <- casos_br %>% filter(state == "PI") %>% group_by(date) %>% summarise(total_casos = sum(totalCases))



#### Gráficos 
  
# evolução de casos em BSB
bsb_graph <- ggplot(bsb, aes(x=as.POSIXct(date)))+
  geom_ribbon(aes(ymin=0, ymax=total_casos), fill="#800000", alpha=0.2)+
  geom_line(aes(y=total_casos), color = "#800000", size =1)+
  labs(x="", y="Número de casos", title = "Distrito Federal")+
  theme_classic()+
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-03-11")), size=1.3, alpha=0.7)+
  annotate(geom="label", x=as.POSIXct("2020-03-23"), y=200000,
           label="Decreto Nº 40.509 de\n 11 de março de 2020",
           fill= "#f0f0f0", size = 3)+
  geom_point(aes(y=total_casos),size=1.5,color ="#800000")+
  theme(legend.position = "none", axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000"),
        axis.text.x = element_text(face="bold", color="#000000")) +
  scale_y_continuous(breaks=c(10000,50000,100000,150000,200000,230000,260000), limits = c(0, 260000),
                     labels =c("10.000","50.000","100.000","150.000","200.000","230.000","260.000"))

# evolução de casos no Rio
rj_graph <- ggplot(rj, aes(x=as.POSIXct(date)))+
  geom_ribbon(aes(ymin=0, ymax=total_casos), fill="#FF8C00", alpha=0.4)+
  geom_line(aes(y=total_casos), color = "#FF8C00", size =1)+
  labs(x="", y="Número de casos", title = "Rio de Janeiro")+
  theme_classic()+
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-03-08")), size=1.3, alpha=0.7)+
  annotate(geom="label", x=as.POSIXct("2020-03-18"), y=200000,
           label="Decreto Estadual\nNº 47.008 de\n 8 de março de 2020",
           fill= "#f0f0f0", size = 3)+
  geom_point(aes(y=total_casos),size=1.5,color ="#FF8C00")+
  theme(legend.position = "none", axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000"),
        axis.text.x = element_text(face="bold", color="#000000")) +
  scale_y_continuous(breaks=c(10000,50000,100000,150000,200000,230000,260000), limits = c(0, 260000),
                    labels =c("10.000","50.000","100.000","150.000","200.000","230.000","260.000"))


# evolução de casos em SP
sp_graph <- ggplot(sp, aes(x=as.POSIXct(date)))+
  geom_ribbon(aes(ymin=0, ymax=total_casos), fill="#3CB371", alpha=0.2)+
  geom_line(aes(y=total_casos), color = "#3CB371", size =1)+
  labs(x="", y="Número de casos", title = "São Paulo")+
  theme_classic()+
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-03-13")), size=1.3, alpha=0.7)+
  annotate(geom="label", x=as.POSIXct("2020-03-16"), y=200000,
           label="Decreto Estadual\n Nº 64.862 de\n 13 de março de 2020",
           fill= "#f0f0f0",size = 3)+
  geom_point(aes(y=total_casos),size=1.5,color ="#3CB371")+
  theme(legend.position = "none", axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000"),
        axis.text.x = element_text(face="bold", color="#000000")) +
  scale_y_continuous(breaks=c(10000,50000,100000,150000,200000,230000,260000),
                    labels =c("10.000","50.000","100.000","150.000","200.000","230.000","260.000"))

# evolução de casos no Pará
pa_graph <- ggplot(pa, aes(x=as.POSIXct(date)))+
  geom_ribbon(aes(ymin=0, ymax=total_casos), fill="#8B008B", alpha=0.2)+
  geom_line(aes(y=total_casos), color = "#8B008B", size =1)+
  labs(x="", y="Número de casos", title = "Pará")+
  theme_classic()+
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-03-16")), size=1.3, alpha=0.7)+
  annotate(geom="label", x=as.POSIXct("2020-03-28"), y=200000,
           label="Decreto Estadual\n Nº 609 de\n 16 de março de 2020",
           fill= "#f0f0f0", size = 3)+
  geom_point(aes(y=total_casos),size=1.5,color ="#8B008B")+
  theme(legend.position = "none", axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000"),
        axis.text.x = element_text(face="bold", color="#000000")) +
  scale_y_continuous(breaks=c(10000,50000,100000,150000,200000,230000,260000), limits = c(0, 260000),
                     labels =c("10.000","50.000","100.000","150.000","200.000","230.000","260.000"))

# unindo gráficos
grid.arrange(bsb_graph, pa_graph, rj_graph, sp_graph, ncol=2, nrow=2)


##### Graficos NE


# evolução de casos na Bahia
ba_graph <- ggplot(ba, aes(x=as.POSIXct(date)))+
  geom_ribbon(aes(ymin=0, ymax=total_casos), fill="#e74c3c", alpha=0.2)+
  geom_line(aes(y=total_casos), color = "#e74c3c", size =1)+
  labs(x="", y="Número de casos", title = "Bahia")+
  theme_classic()+
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-03-16")), size=1.3, alpha=0.7)+
  annotate(geom="label", x=as.POSIXct("2020-03-28"), y=75000,
           label="Decreto Estadual \nnº 19.529",
           fill= "#f0f0f0", size = 3)+
  geom_point(aes(y=total_casos),size=1.5,color ="#e74c3c")+
  theme(legend.position = "none", axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black", size = 8),
        axis.text.y = element_text(face="bold", color="#000000"),
        axis.text.x = element_text(face="bold", color="#000000")) +
  scale_y_continuous(breaks=c(10000,30000,50000,70000,90000, 110000),limits = c(0, 110000),
                     labels =c("10.000","30.000","50.000","70.000","90.000","110.000"))


# evolução de casos Maranhão
ma_graph <- ggplot(MA, aes(x=as.POSIXct(date)))+
  geom_ribbon(aes(ymin=0, ymax=total_casos), fill="#ecdb54", alpha=0.2)+
  geom_line(aes(y=total_casos), color = "#ecdb54", size =1)+
  labs(x="", y="Número de casos", title = "Maranhão")+
  theme_classic()+
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-03-19")), size=1.3, alpha=0.7)+
  annotate(geom="label", x=as.POSIXct("2020-04-06"), y=75000,
           label="Decreto Estadual\n Nº 35.672",
           fill= "#f0f0f0", size = 3)+
  geom_point(aes(y=total_casos),size=1.5,color ="#ecdb54")+
  theme(legend.position = "none", axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black", size = 8),
        axis.text.y = element_text(face="bold", color="#000000"),
        axis.text.x = element_text(face="bold", color="#000000")) +
  scale_y_continuous(breaks=c(10000,30000,50000,70000,90000, 110000),limits = c(0, 110000),
                     labels =c("10.000","30.000","50.000","70.000","90.000","110.000"))

# evolução de casos Pernambuco
pe_graph <- ggplot(PE, aes(x=as.POSIXct(date)))+
  geom_ribbon(aes(ymin=0, ymax=total_casos), fill="#6ca0dc", alpha=0.2)+
  geom_line(aes(y=total_casos), color = "#6ca0dc", size =1)+
  labs(x="", y="Número de casos", title = "Pernambuco")+
  theme_classic()+
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-03-14")), size=1.3, alpha=0.7)+
  annotate(geom="label", x=as.POSIXct("2020-03-30"), y=75000,
           label="Decreto Estadual\nNº 48.809",
           fill= "#f0f0f0", size = 3)+
  geom_point(aes(y=total_casos),size=1.5,color ="#6ca0dc")+
  theme(legend.position = "none", axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black", size = 8),
        axis.text.y = element_text(face="bold", color="#000000"),
        axis.text.x = element_text(face="bold", color="#000000")) +
  scale_y_continuous(breaks=c(10000,30000,50000,70000,90000, 110000),limits = c(0, 110000),
                     labels =c("10.000","30.000","50.000","70.000","90.000","110.000"))

# evolução de casos Piauí
pi_graph <- ggplot(PI, aes(x=as.POSIXct(date)))+
  geom_ribbon(aes(ymin=0, ymax=total_casos), fill="#944743", alpha=0.2)+
  geom_line(aes(y=total_casos), color = "#944743", size =1)+
  labs(x="", y="Número de casos", title = "Piauí")+
  theme_classic()+
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-03-16")), size=1.3, alpha=0.7)+
  annotate(geom="label", x=as.POSIXct("2020-04-08"), y=75000,
           label="Decreto Estadual\n Nº 18.884",
           fill= "#f0f0f0", size = 3)+
  geom_point(aes(y=total_casos),size=1.5,color ="#944743")+
  theme(legend.position = "none", axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black", size = 8),
        axis.text.y = element_text(face="bold", color="#000000"),
        axis.text.x = element_text(face="bold", color="#000000")) +
  scale_y_continuous(breaks=c(10000,30000,50000,70000,90000, 110000),limits = c(0, 110000),
                     labels =c("10.000","30.000","50.000","70.000","90.000","110.000"))

# evolução de casos Ceará
ce_graph <- ggplot(CE, aes(x=as.POSIXct(date)))+
  geom_ribbon(aes(ymin=0, ymax=total_casos), fill="#dbb2d1", alpha=0.2)+
  geom_line(aes(y=total_casos), color = "#dbb2d1", size =1)+
  labs(x="", y="Número de casos", title = "Ceará")+
  theme_classic()+
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-03-13")), size=1.3, alpha=0.7)+
  annotate(geom="label", x=as.POSIXct("2020-04-02"), y=75000,
           label="Decreto Estadual\nNº 33.509",
           fill= "#f0f0f0", size = 3)+
  geom_point(aes(y=total_casos),size=1.5,color ="#dbb2d1")+
  theme(legend.position = "none", axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black", size = 8),
        axis.text.y = element_text(face="bold", color="#000000"),
        axis.text.x = element_text(face="bold", color="#000000")) +
  scale_y_continuous(breaks=c(10000,30000,50000,70000,90000, 110000),limits = c(0, 110000),
                     labels =c("10.000","30.000","50.000","70.000","90.000","110.000"))

# evolução de casos RN
rn_graph <- ggplot(RN, aes(x=as.POSIXct(date)))+
  geom_ribbon(aes(ymin=0, ymax=total_casos), fill="#00a68c", alpha=0.2)+
  geom_line(aes(y=total_casos), color = "#00a68c", size =1)+
  labs(x="", y="Número de casos", title = "Rio Grande do Norte")+
  theme_classic()+
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-03-13")), size=1.3, alpha=0.7)+
  annotate(geom="label", x=as.POSIXct("2020-03-29"), y=75000,
           label="Decreto Estadual\n Nº 29.512",
           fill= "#f0f0f0", size = 3)+
  geom_point(aes(y=total_casos),size=1.5,color ="#00a68c")+
  theme(legend.position = "none", axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black", size = 8),
        axis.text.y = element_text(face="bold", color="#000000"),
        axis.text.x = element_text(face="bold", color="#000000")) +
  scale_y_continuous(breaks=c(10000,30000,50000,70000,90000, 110000),limits = c(0, 110000),
                     labels =c("10.000","30.000","50.000","70.000","90.000","110.000"))

# evolução de casos Sergipe
se_graph <- ggplot(SE, aes(x=as.POSIXct(date)))+
  geom_ribbon(aes(ymin=0, ymax=total_casos), fill="#645394", alpha=0.2)+
  geom_line(aes(y=total_casos), color = "#645394", size =1)+
  labs(x="", y="Número de casos", title = "Sergipe")+
  theme_classic()+
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-03-16")), size=1.3, alpha=0.7)+
  annotate(geom="label", x=as.POSIXct("2020-03-30"), y=75000,
           label="Decreto Estadual\n Nº 40.567",
           fill= "#f0f0f0", size = 3)+
  geom_point(aes(y=total_casos),size=1.5,color ="#645394")+
  theme(legend.position = "none", axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black", size = 8),
        axis.text.y = element_text(face="bold", color="#000000"),
        axis.text.x = element_text(face="bold", color="#000000")) +
  scale_y_continuous(breaks=c(10000,30000,50000,70000,90000, 110000),limits = c(0, 110000),
                     labels =c("10.000","30.000","50.000","70.000","90.000","110.000"))

# evolução de casos Alagoas
al_graph <- ggplot(AL, aes(x=as.POSIXct(date)))+
  geom_ribbon(aes(ymin=0, ymax=total_casos), fill="#6c4f3d", alpha=0.2)+
  geom_line(aes(y=total_casos), color = "#6c4f3d", size =1)+
  labs(x="", y="Número de casos", title = "Alagoas")+
  theme_classic()+
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-03-12")), size=1.3, alpha=0.7)+
  annotate(geom="label", x=as.POSIXct("2020-03-28"), y=75000,
           label="Decreto Estadual\n Nº 69.463",
           fill= "#f0f0f0", size = 3)+
  geom_point(aes(y=total_casos),size=1.5,color ="#6c4f3d")+
  theme(legend.position = "none", axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black", size = 8),
        axis.text.y = element_text(face="bold", color="#000000"),
        axis.text.x = element_text(face="bold", color="#000000")) +
  scale_y_continuous(breaks=c(10000,30000,50000,70000,90000, 110000),limits = c(0, 110000),
                     labels =c("10.000","30.000","50.000","70.000","90.000","110.000"))



# evolução de casos Paraiba
pb_graph <- ggplot(PB, aes(x=as.POSIXct(date)))+
  geom_ribbon(aes(ymin=0, ymax=total_casos), fill="#b4b7ba", alpha=0.2)+
  geom_line(aes(y=total_casos), color = "#b4b7ba", size =1)+
  labs(x="", y="Número de casos", title = "Paraíba")+
  theme_classic()+
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-03-11")), size=1.3, alpha=0.7)+
  annotate(geom="label", x=as.POSIXct("2020-04-02"), y=75000,
           label="Decreto Estadual\nNº 40.122",
           fill= "#f0f0f0", size = 3)+
  geom_point(aes(y=total_casos),size=1.5,color ="#b4b7ba")+
  theme(legend.position = "none", axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black", size = 8),
        axis.text.y = element_text(face="bold", color="#000000"),
        axis.text.x = element_text(face="bold", color="#000000")) +
  scale_y_continuous(breaks=c(10000,30000,50000,70000,90000, 110000),limits = c(0, 110000),
                     labels =c("10.000","30.000","50.000","70.000","90.000","110.000"))


# unindo gráficos
grid.arrange(al_graph, ba_graph, ce_graph,
             ma_graph, pb_graph, pe_graph,
             pi_graph, rn_graph,se_graph, ncol=3, nrow=3)

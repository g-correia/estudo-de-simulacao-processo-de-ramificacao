
# ativação da fonte
windowsFonts("Times New Roman" = windowsFont("Times New Roman"))

# função para potencia de base 10:
potencia_10 <- function(x) {
  parse(text=gsub("e\\+", " %*% 10^", scales::scientific_format()(x)))
}

data_Mg1=tibble(n=0:20,media1_infec_geracao) # mu = 1.5
teorico_Mg1 = tibble(x=0:20, y=1.5^x)
g1g= 
  ggplot() + 
  geom_line(data=data_Mg1, aes(x=n, y=media1_infec_geracao),size=1,color="darkorange") +
  geom_point(data=teorico_Mg1, aes(x=x, y=y),fill="dodgerblue4",color="dodgerblue4")+
  ylab("Média do número de infectados") +
  labs(x="<br> <br> (a) <i>C</i> ~ Geom( <i>p =</i> 0,4)",
       parse = T)+
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size = 16),
        axis.title.x = element_markdown())
g1g

data_Mg2=tibble(n=0:20,media2_infec_geracao) # mu = ~ 0.54
teorico_Mg2 = tibble(x=0:20, y=0.538462^x)
g2g = 
  ggplot() + 
  geom_line(data=data_Mg2, aes(x=n, y=media2_infec_geracao,colour = "Média amostral"),size=1)+
  geom_point(data=teorico_Mg2, aes(x=x, y=y, colour = "Média teórica"))+
  ylab("") +
  labs(x="Geração <br> <br> (b) <i>C</i> ~ Geom( <i>p =</i> 0,65)",
       parse = T)+
  theme_light() +
  theme(legend.position = c(0.7, 0.9),
        text = element_text(family = "Times New Roman", size = 16),
        legend.title = element_blank(),
        legend.background = element_rect(colour = "black"),
        axis.title.x = element_markdown())+
  scale_colour_manual(name="",
                      values=c(`Média amostral`="darkorange", `Média teórica`="dodgerblue4"), 
                      guide = guide_legend(override.aes = list(shape = c(NA, 16)) ) ) 
g2g


data_Mg3=tibble(n=0:20,media3_infec_geracao) # mu = 1
teorico_Mg3 = tibble(x=0:20, y=1^x)
g3g_1 = 
  ggplot() + 
  geom_line(data=data_Mg3, aes(x=n, y=media3_infec_geracao),size=1,color="darkorange")+
  geom_line(data=teorico_Mg3, aes(x=x, y=y),color="dodgerblue4")+
  scale_y_continuous(limits = c(0.75,1.25), breaks=c(0.5,0.75,0.9,1,1.1,1.25,1.5)) +
  ylab("") +
  xlab("")+
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size = 16))
g3g_1


data_Mg3_2=tibble(n=0:100,media3_infec_geracao_2) # mu = 1, n=100
teorico_Mg3_2 = tibble(x=0:100, y=1^x)
g3g_2 = 
  ggplot() +
  geom_line(data=data_Mg3_2, aes(x=n, y=media3_infec_geracao_2),size=1,color = "darkorange")+
  geom_line(data=teorico_Mg3_2, aes(x=x, y=y), color = "dodgerblue4")+
  scale_y_continuous(limits = c(0.75,1.25), breaks=c(0.5,0.75,0.9,1,1.1,1.25,1.5)) +
  ylab("") +
  labs(x="<br> <br> (c) <i>C</i> ~ Geom( <i>p =</i> 0,5)",
       parse = T)+
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size = 16),
        axis.title.x = element_markdown())
g3g_2

g3g=grid.arrange(g3g_1,g3g_2,heights=c(1.8,2))


result_img_1 = grid.arrange(g1g,g2g,g3g,nrow=1)
# tamanho exportação pdf: 12 x 4.8

######## Gráficos variância: mu = (1-p)/p, var = (1-p)/p^2
# media 1.5
n=20
p=0.4
mu=(1-p)/p
var=(1-p)/(p^2)

data_var1 = tibble(n=0:n,var1_infec_geracao)
teorico_var1 = tibble(x=0:n, y=(var)*(mu^(x-1))*(mu^x-1)/(mu-1))
options(scipen=0)

g1b_var = 
  ggplot() + 
  geom_line(data=data_var1, aes(x=n, y=var1_infec_geracao),size=1,color = "darkorange")+
  geom_point(data=teorico_var1, aes(x=x, y=y), color = "dodgerblue4") +
  scale_y_continuous(label = potencia_10) +
  ylab("Variância do número de infectados") +
  labs(x="<br> <br> (a) <i>C</i> ~ Geom(<i> p =</i> 0,4)",
       parse = T)+
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size = 16),
        axis.title.x = element_markdown())

g1b_var

# media 0.54, n = 20
n=20
p=0.65
mu=(1-p)/p
var=(1-p)/p^2
data2_var = tibble(n=0:n,var2_infec_geracao)
teorico_var2 = tibble(x=0:n, y=(var)*(mu^(x-1))*(mu^x-1)/(mu-1))
options(scipen=999)
g2b_var = ggplot() + 
  geom_line(data=data2_var, aes(x=n, y=var2_infec_geracao,colour = "Variância amostral"),size=1)+
  geom_point(data=teorico_var2, aes(x=x, y=y, colour = "Variância teórica")) +
  scale_y_continuous() +
  ylab("") +
  labs(x="Geração <br> <br> (b) <i>C</i> ~ Geom( <i>p =</i> 0,65)",
       parse = T)+
  theme_light() +
  theme(legend.position = c(0.7, 0.9),
        text = element_text(family = "Times New Roman", size = 16),
        legend.title = element_blank(),
        legend.background = element_rect(colour = "black"),
        axis.title.x = element_markdown())+
  scale_colour_manual(name="",
                      values=c(`Variância amostral`="darkorange", `Variância teórica`="dodgerblue4"), 
                      guide = guide_legend(override.aes = list(shape = c(NA, 16)) ) )  
g2b_var

# media 1, n = 20
n=20
p=0.5
mu=(1-p)/p
var=(1-p)/p^2
data3_var = tibble(n=0:n,var3_infec_geracao)
teorico_var3 = tibble(x=0:n, y=x*var)
options(scipen=999)
g3b_var_1 = 
  ggplot() + 
  geom_line(data=data3_var, aes(x=n, y=var3_infec_geracao),size=1,color = "darkorange") +
  geom_line(data=teorico_var3, aes(x=x, y=y), color = "dodgerblue4") +
  ylab("") +
  xlab("")+
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size = 16))
g3b_var_1

# media 1, n = 100
data3_var_2 = tibble(n=0:(length(var3_infec_geracao_2)-1),var3_infec_geracao_2)
teorico_var3_2 = tibble(x=0:(length(var3_infec_geracao_2)-1), y=x*var)
options(scipen=999)

g3b_var_2 = 
  ggplot() + 
  geom_line(data=data3_var_2, aes(x=n, y=var3_infec_geracao_2),size=1,color = "darkorange")+
  geom_line(data=teorico_var3_2, aes(x=x, y=y), colour = "dodgerblue4") +
  ylab("") +
  labs(x=" <br> <br> (c) <i>C</i> ~ Geom(<i> p =</i> 0,5)",
       parse = T)+
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size=16),
        axis.title.x = element_markdown())   
g3b_var_2

g3b_var = grid.arrange(g3b_var_1, g3b_var_2, nrow = 2, heights=c(1.8,2))
result_img_3 = grid.arrange(g1b_var, g2b_var, g3b_var, nrow=1)


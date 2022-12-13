# ativação da fonte
windowsFonts("Times New Roman" = windowsFont("Times New Roman"))

# função para potência de base 10:
potencia_10 <- function(x) {
  parse(text=gsub("e\\+", " %*% 10^", scales::scientific_format()(x)))
}

data_Mq1=tibble(n=0:20,media_infec_geracao) # mu = 1.5
media_teorica_Mq1 = tibble(x=0:20, y=1.5^x)
g1p = 
  ggplot() +
  geom_line(data=data_Mq1, aes(x=n, y=media_infec_geracao),size=1,color="darkorange") +
  geom_point(data=media_teorica_Mq1, aes(x=x, y=y),fill="dodgerblue4",color="dodgerblue4")+
  ylab("Média do número de infectados") +
  labs(x="<br> <br> (a) <i>C</i> ~ Poisson(<i>&lambda; =</i> 1,5)"
       ,
       parse = T)+
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size = 16),
        axis.title.x = element_markdown())
g1p

data_Mq2=tibble(n=0:20,media2_infec_geracao) # mu = 0,3
teorico_Mq2 = tibble(x=0:20, y=0.3^x)
g2p = ggplot() + 
  geom_line(data=data_Mq2, aes(x=n, y=media2_infec_geracao,colour = "Média amostral"),size=1)+
  geom_point(data=teorico_Mq2, aes(x=x, y=y, colour = "Média teórica"))+
  ylab("") +
  labs(x="Geração <br> <br> (b) <i>C</i> ~ Poisson(<i>&lambda; =</i> 0,3)",
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
g2p



data_Mq3=tibble(n=0:20, media3_infec_geracao) # mu = 1, n = 20
media_teorica_Mq3 = tibble(x=0:20, y=1^x)

g3p_1 = 
  ggplot() + 
  geom_line(data=data_Mq3, aes(x=n, y=media3_infec_geracao),size=1,color = "darkorange")+
  geom_line(data=media_teorica_Mq3, aes(x=x, y=y), color = "dodgerblue4")+
  scale_y_continuous(limits = c(0.75,1.25), breaks=c(0.5,0.75,0.9,1,1.1,1.25,1.5)) +
  ylab("") +
  xlab("")+
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size = 16))
g3p_1

data_Mq3_2=tibble(n=0:100,media3_infec_geracao_2) # mu = 1, n = 100
teorico_Mq3_2 = tibble(x=0:100, y=1^x)
g3p_2 = 
  ggplot() + 
  geom_line(data=data_Mq3_2, aes(x=n, y=media3_infec_geracao_2),size=1,color = "darkorange")+
  geom_line(data=teorico_Mq3_2, aes(x=x, y=y), color = "dodgerblue4")+
  scale_y_continuous(limits = c(0.75,1.25), breaks=c(0.5,0.75,0.9,1,1.1,1.25,1.5)) +
  ylab("") +
  labs(x="<br> <br> (c) <i>C</i> ~ Poisson(<i>&lambda; =</i> 1)",
       parse = T)+
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size = 16),
        axis.title.x = element_markdown())

g3p=grid.arrange(g3p_1,g3p_2,nrow=2,heights=c(1.8,2))


result_img_1=grid.arrange(g1p,g2p,g3p, nrow=1)
# Tamanho exportacao pdf: 12 x 4.8

# Gráficos variância: mu = var = lambda
# mu = 1.5
lambda = 1.5  
data_var = tibble(n=0:n,var1_infec_geracao)
teorico_var1 = tibble(x=0:n, y=(lambda)*(lambda^(x-1))*(lambda^x-1)/(lambda-1))
options(scipen=999)
g1p_var = 
  ggplot() +
  geom_line(data=data_var, aes(x=n, y=var1_infec_geracao),size=1,color = "darkorange")+
  geom_point(data=teorico_var1, aes(x=x, y=y), color = "dodgerblue4") +
  scale_y_continuous(label = potencia_10) +
  ylab("Variância do número de infectados") +
  labs(x="<br> <br> (a) <i>C</i> ~ Poisson(<i>&lambda; =</i> 1,5)",
       parse = T)+
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size = 16),
        axis.title.x = element_markdown())
g1p_var

# mu = 0.3
lambda = 0.3
data2_var = tibble(n=0:n,var2_infec_geracao)
teorico_var2 = tibble(x=0:n, y=(lambda)*(lambda^(x-1))*(lambda^x-1)/(lambda-1))
options(scipen=999)
g2p_var = 
  ggplot() + 
  geom_line(data=data2_var, aes(x=n, y=var2_infec_geracao,colour = "Variância amostral"),size=1)+
  geom_point(data=teorico_var2, aes(x=x, y=y, colour = "Variância teórica")) +
  scale_y_continuous() +
  ylab("") +
  labs(x="Geração <br> <br> (b) <i>C</i> ~ Poisson(<i>&lambda; =</i> 0,3)",
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
g2p_var

# mu = 1, n = 20
lambda = 1
data3_var = tibble(n=0:n,var3_infec_geracao)
teorico_var3 = tibble(x=0:n, y=x*lambda)
options(scipen=999)
g3p_var_1 = 
  ggplot() + 
  geom_line(data=data3_var, aes(x=n, y=var3_infec_geracao),size=1,color = "darkorange") +
  geom_line(data=teorico_var3, aes(x=x, y=y), color = "dodgerblue4") +
  ylab("") +
  xlab("")+
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size = 16))
g3p_var_1

# lambda=1, n = 100
data3_var_2 = tibble(n=0:(length(var3_infec_geracao_2)-1),var3_infec_geracao_2)
teorico_var3_2 = tibble(x=0:(length(var3_infec_geracao_2)-1), y=x*lambda)
options(scipen=999)
g3p_var_2 = 
  ggplot() + 
  geom_line(data=data3_var_2, aes(x=n, y=var3_infec_geracao_2),size=1,color = "darkorange")+
  geom_line(data=teorico_var3_2, aes(x=x, y=y), color = "dodgerblue4") +
  ylab("") +
  labs(x=" <br> <br> (c) <i>C</i> ~ Poisson(<i>&lambda; =</i> 1)",
       parse = T)+
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size=16),
        axis.title.x = element_markdown())
g3p_var_2

g3p_var = grid.arrange(g3p_var_1,g3p_var_2,nrow=2,heights=c(1.8,2))

result_img_2 = grid.arrange(g1p_var, g2p_var, g3p_var,nrow=1)



# ativa??o da fonte
windowsFonts("Times New Roman" = windowsFont("Times New Roman"))

# funcao para potencia de base 10:
potencia_10 <- function(x) {
  parse(text=gsub("e\\+", " %*% 10^", scales::scientific_format()(x)))
}

data_Mp1=tibble(n=0:20,media_infec_geracao) # mu = 1.5
media_teorica_Mp1 = tibble(x=0:20, y=1.5^x)
g1b = 
  ggplot() + 
  geom_line(data=data_Mp1, aes(x=n, y=media_infec_geracao),size=1,color="darkorange") +
  geom_point(data=media_teorica_Mp1, aes(x=x, y=y),fill="dodgerblue4",color="dodgerblue4")+
  ylab("M?dia do n?mero de infectados") +
  labs(x="<br> <br> (a) <i>C</i> ~ Binomial(<i> m =</i> 10; <i>p =</i> 0,15)",
       parse = T)+
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size = 16),
        axis.title.x = element_markdown())
g1b

data_Mp2=tibble(n=0:20,media2_infec_geracao) # m?dia 0.3
teorico_Mp2 = tibble(x=0:20, y=0.3^x)
g2b = 
  ggplot() + 
  geom_line(data=data_Mp2, aes(x=n, y=media2_infec_geracao,colour = "M?dia amostral"),size=1)+
  geom_point(data=teorico_Mp2, aes(x=x, y=y, colour = "M?dia te?rica"))+
  ylab("") +
  labs(x="Gera??o <br> <br> (b) <i>C</i> ~ Binomial( <i>m = </i>10; <i>p =</i> 0,03)",
       parse = T)+
  theme_light() +
  theme(legend.position = c(0.7, 0.9),
        text = element_text(family = "Times New Roman", size = 16),
        legend.title = element_blank(),
        legend.background = element_rect(colour = "black"),
        axis.title.x = element_markdown())+
  scale_colour_manual(name="",
                      values=c(`M?dia amostral`="darkorange", `M?dia te?rica`="dodgerblue4"), 
                      guide = guide_legend(override.aes = list(shape = c(NA, 16)) ) ) 
g2b

data_Mp3=tibble(n=0:20, media3_infec_geracao) # mu = 1, n=20
media_teorica_Mp3 = tibble(x=0:20, y=1^x)
g3b_1 = 
  ggplot() + 
  geom_line(data=data_Mp3, aes(x=n, y=media3_infec_geracao),size=1,color="darkorange")+
  geom_line(data=media_teorica_Mp3, aes(x=x, y=y),color="dodgerblue4")+
  scale_y_continuous(limits = c(0.75,1.25), breaks=c(0.5,0.75,0.9,1,1.1,1.25,1.5)) +
  ylab("") +
  xlab("")+
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size = 16)) 
g3b_1

data_Mp3_2=tibble(n=0:100,media3_infec_geracao_2) # mu = 1, n = 100
teorico_Mp3_2 = tibble(x=0:100, y=1^x)
g3b_2 = 
  ggplot() + 
  geom_line(data=data_Mp3_2, aes(x=n, y=media3_infec_geracao_2),size=1,color = "darkorange")+
  geom_line(data=teorico_Mp3_2, aes(x=x, y=y), color = "dodgerblue4")+
  scale_y_continuous(limits = c(0.75,1.25), breaks=c(0.5,0.75,0.9,1,1.1,1.25,1.5)) +
  ylab("") +
  labs(x="<br> <br> (c) <i>C</i> ~ Binomial(<i> m =</i> 10; <i>p = </i>0,1)",
       parse = T)+
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size = 16),
        axis.title.x = element_markdown()) 
g3b_2

g3b=grid.arrange(g3b_1,g3b_2,heights=c(1.8,2))


result_img_1 = grid.arrange(g1b,g2b,g3b,nrow=1)
# Tamanho exportacao pdf: 12 x 4.8


######## Gr?ficos vari?ncia: mu = p*n, var = n*p*(1-p)

# media 1.5
n=20
size=10
p=0.15
mu=size*p
var=size*p*(1-p)

data_var1 = tibble(n=0:n,var_infec_geracao)
teorico_var1 = tibble(x=0:n, y=(var)*(mu^(x-1))*(mu^x-1)/(mu-1))
options(scipen=0)

g1b_var = 
  ggplot() + 
  geom_line(data=data_var1, aes(x=n, y=var_infec_geracao),size=1,color = "darkorange")+
  geom_point(data=teorico_var1, aes(x=x, y=y), color = "dodgerblue4") +
  scale_y_continuous(
    limits = c(0,20000000),
    label = potencia_10
  ) +
  ylab("Vari?ncia do n?mero de infectados") +
  labs(x="<br> <br> (a) <i>C</i> ~ Binomial(<i> m = </i>10; <i>p = </i>0,15)",
       parse = T)+
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size = 16),
        axis.title.x = element_markdown())

g1b_var

# media 0.3
n=20
size=10
p=0.03
mu=size*p
var=size*p*(1-p)

data2_var = tibble(n=0:n,var2_infec_geracao)
teorico_var2 = tibble(x=0:n, y=(var)*(mu^(x-1))*(mu^x-1)/(mu-1))

options(scipen=999)
g2b_var = ggplot() + 
  geom_line(data=data2_var, aes(x=n, y=var2_infec_geracao,colour = "Vari?ncia amostral"),size=1)+
  geom_point(data=teorico_var2, aes(x=x, y=y, colour = "Vari?ncia te?rica")) +
  scale_y_continuous(
    limits = c(0,0.3)#,
    #label = potencia_10
  ) +
  ylab("") +
  labs(x="Gera??o <br> <br> (b) <i>C</i> ~ Binomial( <i>m = </i>10; <i>p = </i>0,03)",
       parse = T)+
  theme_light() +
  theme(legend.position = c(0.7, 0.9),
        text = element_text(family = "Times New Roman", size = 16),
        legend.title = element_blank(),
        legend.background = element_rect(colour = "black"),
        axis.title.x = element_markdown())+
  scale_colour_manual(name="",
                      values=c(`Vari?ncia amostral`="darkorange", `Vari?ncia te?rica`="dodgerblue4"), 
                      guide = guide_legend(override.aes = list(shape = c(NA, 16)) ) )  
g2b_var

# media 1, n = 20
size=10
p=0.1
mu=size*p
var=size*p*(1-p)

data3_var_1 = tibble(n=0:n,var3_infec_geracao)
teorico_var3_1 = tibble(x=0:n, y=x*var)

options(scipen=999)
g3b_var_1 = 
  ggplot() + 
  geom_line(data=data3_var_1, aes(x=n, y=var3_infec_geracao),size=1,color = "darkorange") +
  geom_line(data=teorico_var3_1, aes(x=x, y=y), color = "dodgerblue4") +
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
  labs(x=" <br> <br> (c) <i>C</i> ~ Binomial( <i>m = </i>10; <i>p = </i>0,1)",
       parse = T)+
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size=16),
        axis.title.x = element_markdown())   
g3b_var_2

g3b_var = grid.arrange(g3b_var_1, g3b_var_2, nrow = 2, heights=c(1.8,2))
result_img_3 = grid.arrange(g1b_var, g2b_var, g3b_var, nrow=1)

# Mp1: poisson(lambda=1.5), n = 20
# x = G(x) <=> x = e^(lambda(x-1))
# x = e^(1,5x-1,5)
# exp(1.5*(x-1))-x = 0 <- raíz

gx = function(x){
  exp(1.5*(x-1))-x
} 

x=seq(0,1,0.1)
y=gx(x)
plot(x,y)
abline(h=0)
abline(v=c(0.3,0.5))

gx(0.3)
gx(0.5) # sinais diferentes

# gx eh continua, logo, de acordo com o Metodo da Bissecao,
# possui raiz no intervalo [0.3;0.5].
# Pela analise grafica, a menor raiz esta contida neste 
# intervalo, esta sera a prob. de extincao.

# Aplicando o método da bisseção para encontrar a raíz: a = 0.3; b = 0.5
RaizBissecao = function(a, b, g, e){
  if(g(a)*g(b) >= 0){
    stop("ERRO")
  }
  repeat{
    m = (a+b)/2
    if(g(m) == 0 | abs(b-m) < e){
      return(m)
    }
    if(g(a)*g(m) < 0){
      b = m
    } else {
      a = m
    }
  }
}

RaizBissecao(0.3,0.5,gx,0.000001)
x=0.41718 # PROB DE EXT

p_ext_Mq1 = NULL
for (i in 1:nrow(Mq1)) {
  p_ext_Mq1 = c(p_ext_Mq1, sum(Mq1[i,] == 0)/N)
}
t1p=tibble(x=0:20,y=p_ext_Mq1)
pext1= 
  ggplot() + 
  geom_hline(aes(colour="Probabilidade de extinção teórica",yintercept = x),size=1)+
  geom_point(data=t1p, aes(x=x, y=y,
                           colour = "Estimativa da probabilidade de \n extinção na n-ésima geração"),size=1)+
  scale_y_continuous(limits = c(0,1), breaks = c(0.00,0.25,0.42,0.50,0.75,1.00))+
  ylab("Probabilidade de extinção na n-ésima geração") +
  labs(x="<br> <br> (a) <i>C</i> ~ Poisson(<i>&lambda; =</i> 1,5)",
       parse = T)+
  theme_light() +
  theme(legend.position = c(0.5, 0.87),
        text = element_text(family = "Times New Roman", size = 16),
        legend.title = element_blank(),
        legend.background = element_rect(colour = "black"),
        axis.title.x = element_markdown())+
  scale_colour_manual(name="",
                      values=c(`Estimativa da probabilidade de \n extinção na n-ésima geração`="dodgerblue4", 
                               `Probabilidade de extinção teórica`="darkorange")
                      ,guide = guide_legend(override.aes = list(linetype = c(0, 1),
                                                                shape = c(16, NA)) ) 
  ) 

pext1

# lambda = 0,3
p_ext_Mq2 = NULL
for (i in 1:nrow(Mq2)) {
  p_ext_Mq2 = c(p_ext_Mq2, sum(Mq2[i,] == 0)/N)
}
t2p=tibble(x=0:20,y=p_ext_Mq2)
pext2= 
  ggplot() + 
  geom_hline(color="darkorange",yintercept = 1,size=1)+
  geom_point(data=t2p, aes(x=x, y=y),size=1,color = "dodgerblue4")+
  scale_y_continuous(limits = c(0,1))+
  ylab("") +
  labs(x="Geração <br> <br> (b) <i>C</i> ~ Poisson(<i>&lambda; =</i> 0,3)",
       parse = T)+
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size = 16),
        axis.title.x = element_markdown())
pext2

# lambda = 1, n = 100
p_ext_Mq3 = NULL
for (i in 1:nrow(Mq3_2)) {
  p_ext_Mq3 = c(p_ext_Mq3, sum(Mq3_2[i,] == 0)/N)
}
t3p=tibble(x=0:100,y=p_ext_Mq3)
pext3= 
  ggplot() + 
  geom_hline(color="darkorange",yintercept = 1,size=1)+
  geom_point(data=t3p, aes(x=x, y=y),size=1,color = "dodgerblue4")+
  scale_y_continuous(limits = c(0,1))+
  ylab("") +
  labs(x="<br> <br> (c) <i>C</i> ~ Poisson(<i>&lambda; =</i> 1)",
       parse = T)+
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size = 16),
        axis.title.x = element_markdown())
pext3

grid.arrange(pext1,pext2,pext3,nrow=1)
# dimensoes para exportação pdf 12 x 5.1
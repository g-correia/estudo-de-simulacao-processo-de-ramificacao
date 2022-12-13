# Mp1: binomial(n=10, p=0.15), media 1,5 (no de gerações = 20)
# x = G(x) <=> x = (1-p+ps)^n
# x = (1 - 0.15 + 0.15x)^10
# (0.85 + 0.15x)^10 - x = 0 <- raíz
gx = function(x){
  (0.85 + 0.15*x)^10 - x
} 

x=seq(0,1,0.1)
y=gx(x)
plot(x,y)
abline(h=0)
abline(v=c(0.3,0.4))

gx(0.3)
gx(0.4) # sinais diferentes

# gx eh continua, logo, pelo Metodo da Bissecao,
# possui raiz no intervalo [0.3;0.4].
# Pela analise grafica, a menor raiz esta contida neste 
# intervalo, esta sera a prob. de extincao.

# Aplicando o método da bisseção para encontrar a raíz: 
a = 0.3
b = 0.4
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

RaizBissecao(a,b,gx,0.000001)
x=0.3715111 # PROB DE EXT

p_ext_Mp1 = NULL
for (i in 1:nrow(Mp1)) {
  p_ext_Mp1 = c(p_ext_Mp1, sum(Mp1[i,] == 0)/N)
}
t1b=tibble(x=0:20,y=p_ext_Mp1)
pext1= 
  ggplot() + 
  geom_hline(aes(colour="Probabilidade de extinção teórica",yintercept = x),size=1)+
  geom_point(data=t1b, aes(x=x, y=y,
                           colour = "Estimativa da probabilidade de \n extinção na n-ésima geração"),size=1)+
  scale_y_continuous(limits = c(0,1), breaks = c(0.00,0.25,0.37,0.50,0.75,1.00))+
  ylab("Probabilidade de extinção na n-ésima geração") +
  labs(x="<br> <br> (a) <i>C</i> ~ Binomial( <i>m =</i> 10; <i>p =</i> 0,15)",
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

# média = 0,3
p_ext_Mp2 = NULL
for (i in 1:nrow(Mp2)) {
  p_ext_Mp2 = c(p_ext_Mp2, sum(Mp2[i,] == 0)/N)
}
t2b=tibble(x=0:20,y=p_ext_Mp2)
pext2= 
  ggplot() + 
  geom_hline(color="darkorange",yintercept = 1,size=1)+
  geom_point(data=t2b, aes(x=x, y=y),size=1,color = "dodgerblue4")+
  scale_y_continuous(limits = c(0,1))+
  ylab("") +
  labs(x="Geração <br> <br> (b) <i>C</i> ~ Binomial( <i>m =</i> 10; <i>p =</i> 0,03)",
       parse = T)+
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size = 16),
        axis.title.x = element_markdown())
pext2

# lambda = 1, n=100
p_ext_Mp3 = NULL
for (i in 1:nrow(Mp3_2)) {
  p_ext_Mp3 = c(p_ext_Mp3, sum(Mp3_2[i,] == 0)/N)
}
t3b=tibble(x=0:100,y=p_ext_Mp3)
pext3= 
  ggplot() + 
  geom_hline(color="darkorange",yintercept = 1,size=1)+
  geom_point(data=t3b, aes(x=x, y=y),size=1,color = "dodgerblue4")+
  scale_y_continuous(limits = c(0,1))+
  ylab("") +
  labs(x="<br> <br> (c) <i>C</i> ~ Binomial(<i>m =</i> 10; <i>p =</i> 0,1)",
       parse = T)+
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size = 16),
        axis.title.x = element_markdown())
pext3

grid.arrange(pext1,pext2,pext3,nrow=1)
# dimensoes para exportacao pdf 12 x 5.1


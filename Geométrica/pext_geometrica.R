
# Probabilidade de extinção:
# Mg1: geométrica(p=0,4), mu = 1,5. 

n = 20
p_ext_Mg1 = NULL
for (i in 1:nrow(Mg1)) {
  p_ext_Mg1 = c(p_ext_Mg1, sum(Mg1[i,] == 0)/N)
}

t1g=tibble(x=0:20,y=p_ext_Mg1)
pext1= 
  ggplot() + 
  geom_hline(aes(colour="Probabilidade de extinção teórica",yintercept = 1.5^(-1)),size=1)+
  geom_point(data=t1g, aes(x=x, y=y,
                           colour = "Estimativa da probabilidade de \n extinção na n-ésima geração"),size=1)+
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.5,0.67,0.75,1.0))+
  ylab("Probabilidade de extinção na n-ésima geração") +
  labs(x="<br> <br> (a) <i>C</i> ~ Geom( <i>p =</i> 0,4)",
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


# Mg2: geométrica(p=0,65), media 0,54. 
n = 20
p_ext_Mg2 = NULL
for (i in 1:nrow(Mg2)) {
  p_ext_Mg2 = c(p_ext_Mg2, sum(Mg2[i,] == 0)/N)
}
t2g=tibble(x=0:n,y=p_ext_Mg2)
pext2 = 
  ggplot() + 
  geom_hline(color="darkorange",yintercept = 1,size=1)+
  geom_point(data=t2g, aes(x=x, y=y),size=1,color = "dodgerblue4")+
  ylab("") +
  labs(x="Geração <br> <br> (b) <i>C</i> ~ Geom( <i>p =</i> 0,65)",
       parse = T)+
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size = 16),
        axis.title.x = element_markdown())
pext2

# Mg3_2: geométrica(p=0,5), media 1, n = 100. 
n = 100
p_ext_Mg3_2 = NULL
for (i in 1:nrow(Mg3_2)) {
  p_ext_Mg3_2 = c(p_ext_Mg3_2, sum(Mg3_2[i,] == 0)/N)
}
t3g=tibble(x=0:n,y=p_ext_Mg3_2)
pext3 = 
  ggplot() +
  geom_point(data=t3g, aes(x=x, y=y),size=1,color = "dodgerblue4")+
  geom_hline(color="darkorange",size=1,yintercept = 1)+
  ylab("") +
  labs(x="<br> <br> (c) <i>C</i> ~ Geom( <i>p =</i> 0,5)",
       parse = T)+
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size = 16),
        axis.title.x = element_markdown())
pext3

grid.arrange(pext1,pext2,pext3,nrow=1)
# dimensoes para exportacao pdf 12 x 5.1

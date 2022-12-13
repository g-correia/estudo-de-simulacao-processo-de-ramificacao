# Argumentos da funcao f1:
N <- 10000
n <- 20

######### (a) Simulacao 1: mu = 1.5
# parametro p da dist. geometrica:
p = 0.4

# Matriz nxN que armazenara N processos de ramificacao ate a n-esima geracao.
Mg1 = matrix(NA,nrow=(n+1),ncol=N)
set.seed(1513)
for(i in 1:N){
  Mg1[,i] <- f1(n = n, fun = "geom", p = p)
}

# media do numero de infectados a cada geracao:
(media1_infec_geracao = rowMeans(Mg1) )

######### (b) Simulacao 2: mu ~ 0.54
# parametro p da dist. geometrica:
p=0.65

Mg2 = matrix(NA,nrow=(n+1),ncol=N)
set.seed(1513)
for(i in 1:N){
  Mg2[,i] <- f1(n = n, fun = "geom", p = p)
}
(media2_infec_geracao = rowMeans(Mg2))

######### (c) Simulacao 3: mu = 1
# parametro p da dist. geometrica:
p=0.5

Mg3 = matrix(NA,nrow=(n+1),ncol=N)
set.seed(1513)
for(i in 1:N){
  Mg3[,i] <- f1(n = n, fun = "geom", p = p)
}
(media3_infec_geracao = rowMeans(Mg3))

# para n maior: 
n=100
Mg3_2 = matrix(NA,nrow=(n+1),ncol=N)
set.seed(1513)
for(i in 1:N){
  Mg3_2[,i] <- f1(n = n, fun = "geom", p = p)
}
(media3_infec_geracao_2 = rowMeans(Mg3_2))

# Variancias
var1_infec_geracao = rowVars(Mg1)
var2_infec_geracao = rowVars(Mg2)
var3_infec_geracao = rowVars(Mg3)
var3_infec_geracao_2 = rowVars(Mg3_2)






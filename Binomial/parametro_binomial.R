# Argumentos da funcao f1:
N <- 10000
n <- 20 

######### (a) Simulacao 1: mu = 1.5
# parametros n e p da dist. binomial:
size = 10 
p = 0.15

# Matriz nxN que armazenara N processos de ramificacao ate a n-esima geracao.
Mp1 = matrix(NA,nrow=(n+1),ncol=N)
set.seed(15)
for(i in 1:N){
  Mp1[,i] <- f1(n = n, fun = "binom", size=size, p=p)
}

# media do numero de infectados a cada geracao:
(media_infec_geracao = rowMeans(Mp1))

####### (b) Simulacao 2: mu = 0.3
size=10
p=0.03
Mp2 = matrix(NA,nrow=(n+1),ncol=N)
set.seed(15)
for(i in 1:N){
  Mp2[,i] <- f1(n = n, fun = "binom", size=size, p=p)
}

# media do numero de infectados a cada geracao:
(media2_infec_geracao = rowMeans(Mp2))

####### (c) Simula??o 3: mu = 1
size=10
p=0.1
Mp3 = matrix(NA,nrow=(n+1),ncol=N)
set.seed(15)
for(i in 1:N){
  Mp3[,i] <- f1(n = n, fun = "binom", size=size, p=p)
}

# m?dia do n?mero de infectados a cada gera??o:
(media3_infec_geracao = rowMeans(Mp3))

####### (c) Simula??o 3.2: mesmos parametros, aumenta n
n=100
Mp3_2 = matrix(NA,nrow=(n+1),ncol=N)
set.seed(15)
for(i in 1:N){
  Mp3_2[,i] <- f1(n = n, fun = "binom", size=size, p=p)
}

# m?dia do n?mero de infectados a cada gera??o:
(media3_infec_geracao_2 = rowMeans(Mp3_2))

n=20 # retomando n

####################
#### Vari?ncia: ####
####################

# Te?rico
# para mu = 1:
# var = n*(sigma^2)

# para mu != 1:
# var = (sigma^2)*(mu^(n-1))*(mu^n-1)/(mu-1)

(var_infec_geracao = rowVars(Mp1))
(var2_infec_geracao = rowVars(Mp2))
(var3_infec_geracao = rowVars(Mp3))
(var3_infec_geracao_2 = rowVars(Mp3_2))


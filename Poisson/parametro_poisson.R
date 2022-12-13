# Argumentos da funcao f1:
N <- 10000
n <- 20

######### (a) Simulacao 1: mu = 1.5
# parametro lambda da dist. Poisson:
lambda=1.5
set.seed(151)
# Matriz nxN que armazenara N processos de ramificacao ate a n-esima geracao.
Mq1 = matrix(NA,nrow=(n+1),ncol=N)
for(i in 1:N){
  Mq1[,i] <- f1(n = n, fun = "pois", lambda = lambda)
}

# media do numero de infectados a cada geracao:
(media_infec_geracao = rowMeans(Mq1)) 

######## (b) Simulacao 2: mu = 0.3
# parametro lambda da dist. Poisson:
lambda=0.3
set.seed(151)
Mq2 = matrix(NA,nrow=(n+1),ncol=N)
for(i in 1:N){
  Mq2[,i] <- f1(n = n, fun = "pois", lambda = lambda)
}

# media do numero de infectados a cada geracao:
(media2_infec_geracao = rowMeans(Mq2)) 

######## (c) Simulacao 3: mu = 1
# parametro lambda da dist. Poisson:
lambda=1
set.seed(151)
Mq3 = matrix(NA,nrow=(n+1),ncol=N)
for(i in 1:N){
  Mq3[,i] <- f1(n = n, fun = "pois", lambda = lambda)
}

# media do numero de infectados a cada geracao:
(media3_infec_geracao = rowMeans(Mq3))

# para n maior
n=100
set.seed(151)
Mq3_2 = matrix(NA,nrow=(n+1),ncol=N)
for(i in 1:N){
  Mq3_2[,i] <- f1(n = n, fun = "pois", lambda = lambda)
}

n=20 # retomando n

# média do número de infectados a cada geração:
(media3_infec_geracao_2 = rowMeans(Mq3_2))


####################
#### Variância: ####
####################

# Teórico
# para mu = 1:
# var = n*(sigma^2)

# para mu != 1:
# var = (sigma^2)*(mu^(n-1))*(mu^n-1)/(mu-1)
# Sendo sigma^2 = variância de C e mu = esperança de C

(var1_infec_geracao = rowVars(Mq1))
(var2_infec_geracao = rowVars(Mq2))
(var3_infec_geracao = rowVars(Mq3))
(var3_infec_geracao_2 = rowVars(Mq3_2))

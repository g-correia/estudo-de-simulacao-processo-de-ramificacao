                          ####################
                          #### Funcao f1: #### 
                          ####################

#### Argumentos:

### n - o numero de geracoes pretendido no processo de ramificacao;
### fun - a distribuicao de C: "geom" para geometrica, "binom" para binomial, "pois" para poisson;
### size - (opcional) parametro m da distribuicao binomial;
### p - (opcional) parametro p das distribuicoes binomial ou geometrica;
### lambda - (opcional) parametro lambda da distribuicao poisson.
                          
### Retorna: um vetor de tamanho n com o numero de infectados na geração Z_i,
# i de 1 a n.

# Objetivo: observar o comportamento do numero de infectados em um processo
# de ramificacao ao longo das gerações.

f1 <- function(n,fun,size=0,p=0,lambda=0){
  sorteia <- function(n,fun,size=0,p=0,lambda=0){
    if((fun!="geom") & (fun!="binom") & (fun!="pois")){
      stop("verificar entrada fun: 'geom', 'binom' ou 'pois'")
    }
    if(fun=="geom"){
      rgeom(n,p)
    } else if(fun=="binom"){
      rbinom(n,size,p)
    } else{
      rpois(n,lambda)
    }
  }
  Z <- 1
  for(i in 2:(n+1)){
    Z[i] <- sum(sorteia(n = Z[i-1], fun=fun, size=size, p=p, lambda=lambda))
  }
  return(Z)  
}







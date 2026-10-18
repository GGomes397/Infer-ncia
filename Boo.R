data(swiss)
Fert <- swiss$Fertility
tam<- length(Fert)
n <- 1000
resultados <- data.frame(
  mediana = numeric(0),
  q1 = numeric(0),
  q3 = numeric(0)
 
)
reamostra <- function(amos,t1,t2,resultados){
 
  q_1 <- numeric(t2)
  q_3 <- numeric(t2)
  md <- numeric(t2)

    
  for(i in 1:t2){
    amostra <- sample(x=amos,size = t1,replace = T)
    md[i]<- quantile(amostra,0.5)
    q_1[i] <- quantile(amostra,0.25)
    q_3[i]<- quantile(amostra,0.75)
    
  }

  n_r <- data.frame(
    mediana = md,
    q1 = q_1,
    q3 = q_3
    
  )
  return(rbind(resultados, n_r))
                               
                               
}
resultados <- reamostra(Fert,tam,n,resultados)

#ic
gm <- 0.95#dando a confiança
alpha <- 1-gm#nivel de signicância
ic_md <- c(quantile(resultados$mediana,alpha/2),quantile(resultados$mediana,1-alpha/2))
ic_q1 <- c(quantile(resultados$q1,alpha/2),quantile(resultados$q1,1-alpha/2))
ic_q3 <- c(quantile(resultados$q3,alpha/2),quantile(resultados$q3,1-alpha/2))

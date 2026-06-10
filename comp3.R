mu <- 70
sigma <- 5
n <- 3
set.seed(123)
amostra <- rnorm(n,mean = mu,sd=sigma)
#media amostral 
xbarra <- mean(amostra)
var_amostral <-sum(((amostra-xbarra)**2))/(n-1)
#contruindo o ic 
#a
z <- qnorm(0.975)
ic_a <- c(xbarra-z*sigma / sqrt(n),
          xbarra+z*sigma / sqrt(n))
#b
t <- qt(0.975, df = n - 1)
s <- sd(amostra)
ic_b <- c(xbarra-t*s / sqrt(n),
          xbarra+t*s/ sqrt(n))
cat("a) IC 95% (sigma desconhecido):", ic_b, "\n")
contem_a <- logical(100)  
contem_b <- logical(100)
amplitude_a <- numeric(100)
amplitude_b <- numeric(100)

set.seed(123)
function
for (i in 1:100) {
  
  amostra <- rnorm(n, mean = mu, sd = sigma)
  xbarra <- mean(amostra)
  
  # a) IC com sigma conhecido
  z <- qnorm(0.975)
  ic_a_inf <- xbarra - z * sigma / sqrt(n)
  ic_a_sup <- xbarra + z * sigma / sqrt(n)
  contem_a[i] <- (ic_a_inf <= mu & mu <= ic_a_sup)
  amplitude_a[i]<- ic_a_sup-ic_a_inf
  
  # b) IC com sigma desconhecido (usando t)
  t <- qt(0.975, df = n - 1)
  s <- sd(amostra)
  
  ic_b_inf <- xbarra - t * s / sqrt(n)
  ic_b_sup <- xbarra + t * s / sqrt(n)
  contem_b[i] <- (ic_b_inf <= mu & mu <= ic_b_sup)
  amplitude_b[i]<- ic_b_sup-ic_b_inf
}

prop_conhecido <- sum(contem_a) / 100
prop_desconhecido <- sum(contem_b) / 100

lm_a <- mean(amplitude_a)
lm_b <- mean(amplitude_b)

cobertura_a <- mean(contem_a)
cobertura_b <- mean(contem_b)

vies_a <- cobertura_a - 0.95
vies_b <- cobertura_b  - 0.95

cat("a) Sigma conhecido:\n")
cat("   Proporção de ICs que contêm μ:", prop_conhecido * 100, "%\n")
cat("   Largura média dos ICs:", round(lm_a, 4), "\n")
cat("   viés:", vies_a,"\n")
cat("b) Sigma desconhecido:\n")
cat("   Proporção de ICs que contêm μ:", prop_desconhecido * 100, "%\n")
cat("   Largura média dos ICs:", round(lm_b, 4), "\n")
cat("   viés:", vies_b)
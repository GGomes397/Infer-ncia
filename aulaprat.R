set.seed(123)

# Parâmetros
mu <- 0
sigma2 <- 1
sigma <- sqrt(sigma2)

# tamanhos amostrais
n_vals <- c(5, 10, 20, 50, 100,200,300,400,500)

# número de simulações
B <- 10000

# matriz para guardar resultados
resultados <- data.frame(
  n = integer(),
  vies_MV = numeric(),
  var_MV = numeric(),
  eqm_MV = numeric(),
  vies_S2 = numeric(),
  var_S2 = numeric(),
  eqm_S2 = numeric()
)

for (n in n_vals) {
  
  est_mv <- numeric(B)
  est_s2 <- numeric(B)
  
  for (b in 1:B) {
    x <- rnorm(n, mean = mu, sd = sigma)
    
    # estimador de máxima verossimilhança
    est_mv[b] <- mean((x - mean(x))^2)
    
    # estimador com denominador n-1
    est_s2[b] <- var(x)
  }
  
  # medidas empíricas
  vies_mv <- mean(est_mv) - sigma2
  var_mv  <- var(est_mv)
  eqm_mv  <- mean((est_mv - sigma2)^2)
  
  vies_s2 <- mean(est_s2) - sigma2
  var_s2  <- var(est_s2)
  eqm_s2  <- mean((est_s2 - sigma2)^2)
  
  resultados <- rbind(resultados,
                      data.frame(
                        n = n,
                        vies_MV = vies_mv,
                        var_MV = var_mv,
                        eqm_MV = eqm_mv,
                        vies_S2 = vies_s2,
                        var_S2 = var_s2,
                        eqm_S2 = eqm_s2
                      ))
}


resultados$vies_MV_teorico <- -1 / resultados$n
resultados$var_MV_teorica <- 2 * (resultados$n - 1) / resultados$n^2
resultados$eqm_MV_teorico <- (2 * resultados$n - 1) / resultados$n^2

resultados$vies_S2_teorico <- 0
resultados$var_S2_teorica <- 2 / (resultados$n - 1)
resultados$eqm_S2_teorico <- 2 / (resultados$n - 1)

print(resultados)

##############################################
plot(resultados[,2],resultados[,8])
plot(resultados[,3],resultados[,9])
plot(resultados[,6],resultados[,12])
#################################################


plot(resultados$n, resultados$vies_MV, type = "b", pch = 19,
     ylim = range(c(resultados$vies_MV, resultados$vies_S2)),
     xlab = "n", ylab = "Viés",
     main = "Comparação do viés")

lines(resultados$n, resultados$vies_S2, type = "b", pch = 17)

abline(h = 0, lty = 2)

legend("bottomright",
       legend = c(expression(hat(sigma)[MV]^2), expression(S^2)),
       pch = c(19, 17), lty = 1)

#####################################################


plot(resultados$n, resultados$var_MV, type = "b", pch = 19,
     ylim = range(c(resultados$var_MV, resultados$var_S2)),
     xlab = "n", ylab = "Variância",
     main = "Comparação da variância")

lines(resultados$n, resultados$var_S2, type = "b", pch = 17)

legend("topright",
       legend = c(expression(hat(sigma)[MV]^2), expression(S^2)),
       pch = c(19, 17), lty = 1)

plot(resultados$n[1:5], resultados$var_MV[1:5], type = "b", pch = 19,
     ylim = range(c(resultados$var_MV[1:5], resultados$var_S2[1:5])),
     xlab = "n", ylab = "Variância",
     main = "Comparação da variância")

lines(resultados$n, resultados$var_S2, type = "b", pch = 17)

legend("topright",
       legend = c(expression(hat(sigma)[MV]^2), expression(S^2)),
       pch = c(19, 17), lty = 1)

################################################################
plot(resultados$n, resultados$eqm_MV, type = "b", pch = 19,
     ylim = range(c(resultados$eqm_MV, resultados$eqm_S2)),
     xlab = "n", ylab = "EQM",
     main = expression("Comparação do EQM de " * hat(sigma)[MV]^2 * " e " * S^2))

lines(resultados$n, resultados$eqm_S2, type = "b", pch = 17)

legend("topright",
       legend = c(expression(hat(sigma)[MV]^2), expression(S^2)),
       pch = c(19, 17), lty = 1)


plot(resultados$n[1:5], resultados$eqm_MV[1:5], type = "b", pch = 19,
     ylim = range(c(resultados$eqm_MV[1:5], resultados$eqm_S2[1:5])),
     xlab = "n", ylab = "EQM",
     main = expression("Comparação do EQM de " * hat(sigma)[MV]^2 * " e " * S^2))

lines(resultados$n, resultados$eqm_S2, type = "b", pch = 17)

legend("topright",
       legend = c(expression(hat(sigma)[MV]^2), expression(S^2)),
       pch = c(19, 17), lty = 1)
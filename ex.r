# Parâmetros
amostra <- c(14.4, 12.9, 7, 13.7, 13.5)
n <- length(amostra)
media <- mean(amostra)
s <- sd(amostra)

alpha <- 0.02
grau_liberdade <- n - 1
mi0 <- 12
miA <- seq(-20, 50, by = 0.1)

# Bilateral
t_critico <- qt(1 - alpha/2, grau_liberdade)
powersBi <- vector()
for(mi in miA){
  delta <- (mi - mi0) / (s / sqrt(n))
  power <- (1-pt(t_critico, grau_liberdade, delta)) + pt(-t_critico, grau_liberdade, delta)
  powersBi <- c(powersBi, power)
}

# Unilateral
t_critico <- qt(alpha, grau_liberdade, lower.tail = FALSE)
powersUn <- vector()
for(mi in miA){
  delta <- (mi - mi0) / (s / sqrt(n))
  powersUn <- c(powersUn, 1 - pt(t_critico, grau_liberdade, delta))
}

# Curva de Poder
plot(miA, powersBi, col="blue", type="l", xlab = expression(mu), ylab = "Poder", main = "Curvas de Poder")
lines(miA, powersUn, col="green")
legend("bottomright", legend = c("Bilateral", "Unilateral"), col = c("blue", "green"), lty = 1)
# Parte 1
n <- 20
pop_mean <- 0
pop_sd <- 5
gamma <- 0.95
alpha <- 1-gamma
z <- -qnorm(alpha/2)
count <- 0

for(i in 1:100){
    amostra <- rnorm(n, pop_mean, pop_sd)
    intervalo <- c(mean(amostra) - z*(sd(amostra)/sqrt(n)), mean(amostra) + z*(sd(amostra)/sqrt(n)))

    #cat("Amostra:", amostra, "\n")
    #cat("Média amostral:", mean(amostra), "\n")
    #cat("Desvio padrão amostral:", sd(amostra), "\n")
    cat("Intervalo de confiança", i, ":", intervalo, "\n")

    if(pop_mean >= intervalo[1] && pop_mean <= intervalo[2]){
        count <- count + 1
    }
}
cat("\n")

cat(count, "intervalos de confiança (95%) de 100 capturam o verdadeiro valor da média populacional (", pop_mean, ")\n\n")
freq <- count/100
if(freq < gamma){
    cat("A frequência de intervalos que capturam a média populacional é menor que o nível de confiança esperado", freq, "<", gamma, "\n")
} else if(freq > gamma){
    cat("A frequência de intervalos que capturam a média populacional é maior que o nível de confiança esperado", freq, ">", gamma, "\n")
} else{
    cat("A frequência de intervalos que capturam a média populacional é igual ao nível de confiança esperado", freq, "=", gamma, "\n")
}

# Parte 2
n <- 30
lambda <- 3.25
gamma <- 0.95
alpha <- 1-gamma
z <- -qnorm(alpha/2)
count <- 0

for(i in 1:100){
    amostra <- rpois(n, lambda)
    intervalo <- c(mean(amostra) - z*(sqrt(lambda/n)), mean(amostra) + z*(sqrt(lambda/n)))

    #cat("Amostra:", amostra, "\n")
    #cat("Média amostral:", mean(amostra), "\n")
    #cat("Desvio padrão amostral:", sd(amostra), "\n")
    cat("Intervalo de confiança", i, ":", intervalo, "\n")

    if(lambda >= intervalo[1] && lambda <= intervalo[2]){
        count <- count + 1
    }
}
cat("\n")

cat(count, "intervalos de confiança (95%) de 100 capturam o verdadeiro valor de lambda (", lambda, ")\n\n")
freq <- count/100
if(freq < gamma){
    cat("A frequência de intervalos que capturam o verdadeiro valor de lambda é menor que o nível de confiança esperado", freq, "<", gamma, "\n")
} else if(freq > gamma){
    cat("A frequência de intervalos que capturam o verdadeiro valor de lambda é maior que o nível de confiança esperado", freq, ">", gamma, "\n")
} else{
    cat("A frequência de intervalos que capturam o verdadeiro valor de lambda é igual ao nível de confiança esperado", freq, "=", gamma, "\n")
}
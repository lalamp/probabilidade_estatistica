# Exercício 1
dt <- read.table("http://dcm.ffclrp.usp.br/~rrosales/aulas/orto.txt", header=TRUE)
fisioN <- subset(dt, Sequelas == 'N', Fisioterapia)
fisioS <- subset(dt, Sequelas == 'S', Fisioterapia)

barplot(table(fisioN), xlab="Meses de Fisio", ylab="Nº de Pacientes")
barplot(table(fisioS), xlab="Meses de Fisio", ylab="Nº de Pacientes")

cirurgia <- dt$Cirugia
fisioA <- subset(dt, Cirugia == 'A', Fisioterapia)
fisioM <- subset(dt, Cirugia == 'M', Fisioterapia)
fisioB <- subset(dt, Cirugia == 'B', Fisioterapia)

barplot(table(fisioA), xlab="Meses de Fisio", ylab="Nº de Pacientes")
barplot(table(fisioM), xlab="Meses de Fisio", ylab="Nº de Pacientes")
barplot(table(fisioB), xlab="Meses de Fisio", ylab="Nº de Pacientes")

# Exercício 2
frequencias <- c(14, 28, 27, 11, 4)
notas <- c(1, 3, 5, 7, 9)
dados <- rep(notas, frequencias)

hist(dados, breaks=seq(0, 10, by=2), xlab="Notas", ylab="Frequencia", main="Histograma das Notas")

boxplot(notas, ylab="Notas")

# Exercício 3
n1 <- c(8, 25, 28, 12, 9)
peso1 <- c(35, 45, 55, 65, 75)
dt1 <- rep(peso1, n1)

hist(dt1, breaks=seq(0, 100, by=10), xlab="Pesos", ylab="Ni", main="Histograma Região A")
boxplot(dt1, ylab="Peso", main="Boxplot Região A")

n2 <- c(10, 34, 109, 111, 55)
peso2 <- c(55, 65, 75, 85, 95)
dt2 <- rep(peso2, n2)

hist(dt2, breaks=seq(0, 120, by=10), xlab="Pesos", ylab="Ni", main="Histograma Região B")
boxplot(dt2, ylab="Peso", main="Boxplot Região B")

# Exercício 4
dt <- read.table("http://dcm.ffclrp.usp.br/~rrosales/aulas/cancer.txt", header=TRUE)

idade_falso_positivo <- subset(dt, Grupo == 4, Idade)
idade_falso_negativo <- subset(dt, Grupo == 1, Idade)

cat("\n Tabela de Frequência das Idades")
table(idade_falso_positivo)
cat("\n")
barplot(table(idade_falso_positivo), main="Falso Positivo", xlab="Idades", ylab="Frequências", las=1)

cat("\n Tabela de Frequência das Idades")
table(idade_falso_negativo)
cat("\n")
barplot(table(idade_falso_negativo), main="Falso Negativo", xlab="Idades", ylab="Frequências", las=1)

dados <- subset(dt, Grupo == 1 | Grupo == 4)
boxplot(Idade ~ Grupo, data=dados, las=1)
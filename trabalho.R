########################################
# Trabalho Final - INF-0612          
# Nome(s): ??tila de Moura Tavano Moretto
#          S??rgio Henrique Martini Marinello
########################################

# Download do arquivo csv que cont??m os dados
con <- url("https://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv")

# Criando um vetor para indexar os nomes das columas
names <- c("Horario", "Temperatura", "Vento", "Umidade", "Sensacao")

# Ir?? ler a tabela e preencher os campos faltando
cepagri <- read.table(con , header = FALSE , fill = TRUE, sep = ";", col.names = names, 
                      colClasses = c("character","character","numeric","numeric","numeric"),
                      stringsAsFactors=FALSE)
# 210336 amostras no conjunto de dados original

# Converte a coluna 2 para numeric, onde tiver a string "ERRO", ira'transformar pra NA
cepagri [ , 2] <- as.numeric(cepagri [ ,2])

# Remove qualquer NA na tabela
for(i in 1:5) {
  cepagri <- cepagri [!is.na(cepagri[ , i]), ]
}
# 208183 amostras sobraram - 2153 amostras removidas

# Converte a string com horarios para POSIXlt
cepagri$Horario <- strptime (cepagri$Horario, "%d/%m/%Y-%H:%M")

# Escolhe apneas as amostras entre o periodo desejado: 01/01/2015-31/12/2017
periodo <- cepagri$Horario >= "2015-01-01" & cepagri$Horario < "2018-01-01"
cepagri <- cepagri[periodo, ]
# 154257 amostras dentro do periodo desejado de 3 anos
# 3 anos * 365 dias + 1 dia de ano bissexto 2016 * 24 horas * 6 coletas por hora = 157824 amostras

# Remove os outliers - 137 valores de Sensacao = 99.90, total de amostra = 154120
cepagri <- cepagri[cepagri$Sensacao != 99.90,]

summary(cepagri)

# Função que dado um vetor e um inteiro k verifica se cada posição do vetor é igual às k posições
# anteriores ou posteriores
consecutive <- function (vector , k = 1) {
  n <- length ( vector )
  result <- logical (n)
  for (i in (1+k):n)
    if (all( vector [(i-k):(i-1)] == vector [i]))
      result [i] <- TRUE
  for (i in 1:(n-k))
    if (all( vector [(i+1):(i+k)] == vector [i]))
      result [i] <- TRUE
  return ( result )
}




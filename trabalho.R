########################################
# Trabalho Final - INF-0612          
# Nome(s): Átila de Moura Tavano Moretto
#          Sérgio Henrique Martini Marinello
########################################

# Download do arquivo csv que contém os dados
con <- url("https://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv")

# Criando um vetor para indexar os nomes das columas
names <- c("Horario", "Temperatura", "Vento", "Umidade", "Sensacao")

#cepagri <- read.csv(con , header = FALSE , sep = ";", col.names = names )

# Irá ler a tabela e preencher os campos faltando
cepagri <- read.table(con , header = FALSE , fill = TRUE, sep = ";", col.names = names, stringsAsFactors=FALSE)
#26673 tem erros
#26491 só tem data

pDataExcluded <- nrow(cepagri)
cepagriOriginal <- cepagri

# Eliminar as colunas com horário invalido
cepagri <- cepagri [!is.na(cepagri[ , 1]), ]

# Remove qualquer NA na tabela
for(i in 2:5) {
  cepagri [ , i] <- as.numeric(cepagri [ ,i])
  cepagri <- cepagri [!is.na(cepagri[ , i]), ]
}


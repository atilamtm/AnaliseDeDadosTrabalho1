########################################
# Trabalho Final - INF-0612          
# Nome(s): ??tila de Moura Tavano Moretto
#          S??rgio Henrique Martini Marinello
########################################

# Download do arquivo csv que cont??m os dados
con <- url("https://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv")

# Criando um vetor para indexar os nomes das columas
names <- c("Horario", "Temperatura", "Vento", "Umidade", "Sensacao")

#cepagri <- read.csv(con , header = FALSE , sep = ";", col.names = names )

# Ir?? ler a tabela e preencher os campos faltando
cepagri <- read.table(con , header = FALSE , fill = TRUE, sep = ";", col.names = names, 
                      colClasses = c("character","character","numeric","numeric","numeric"),
                      stringsAsFactors=FALSE)
#26673 tem erros
#26491 s?? tem data

pDataExcluded <- nrow(cepagri)
cepagriOriginal <- cepagri

# Converte a coluna 2 para numeric, onde tiver a string "ERRO", ira'transformar pra NA
cepagri [ , 2] <- as.numeric(cepagri [ ,2])

# Remove qualquer NA na tabela
for(i in 1:5) {
  cepagri <- cepagri [!is.na(cepagri[ , i]), ]
}

# Converte a string com horarios para POSIXlt
cepagri$Horario <- strptime (cepagri$Horario, "%d/%m/%Y-%H:%M")

# Escolhe apneas as amostras entre o periodo desejado: 01/01/2015-31/12/2017
periodo <- cepagri$Horario >= "2015-01-01" & cepagri$Horario < "2018-01-01"
cepagri <- cepagri[periodo, ]

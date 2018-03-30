########################################
# Trabalho Final - INF-0612          
# Nome(s): Átila de Moura Tavano Moretto
#          Sérgio Henrique Martini Marinello
########################################

library(ggplot2)

# Download do arquivo csv que cont??m os dados
con <- url("https://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv")

# Criando um vetor para indexar os nomes das columas
names <- c("Horario", "Temperatura", "Vento", "Umidade", "Sensacao")

# Ir?? ler a tabela e preencher os campos faltando
cepagri <- read.table(con , header = FALSE , fill = TRUE, sep = ";", col.names = names, 
                      #colClasses = c("character","character","numeric","numeric","numeric"),
                      stringsAsFactors=FALSE)
# 210475 amostras no conjunto de dados original

# Converte a coluna 2 para numeric, onde tiver a string "ERRO", ira'transformar pra NA
cepagri [ , 2] <- as.numeric(cepagri [ ,2])

# Remove qualquer NA na tabela
for(i in 1:5) {
  cepagri <- cepagri [!is.na(cepagri[ , i]), ]
}
# 208320 amostras sobraram - 2155 amostras removidas

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

# Numero de dados relacionadas a 1h, 2h, 4h, 6h, 12h, 24h, 36h e 48h
nTimes = c(6, 12, 24, 36, 72, 144, 216, 288);
vRepeatedValues = rep(x=0, times=length(nTimes))
vFailDate = list();
for(i in 1:length(nTimes)) {
  repeated = 
    consecutive(cepagri$Temperatura, nTimes[i]) & 
    consecutive(cepagri$Vento, nTimes[i]) & 
    consecutive(cepagri$Umidade, nTimes[i]) & 
    consecutive(cepagri$Sensacao, nTimes[i])
  
  vRepeatedValues[i] = sum (repeated)
  vFailDate <- c(vFailDate, repeated)
}

dfRepeticoes <-data.frame(vRepeatedValues=vRepeatedValues, nHoras=nTimes/6, porcentagem=(vRepeatedValues/length(cepagri$Horario)) * 100)
names(dfRepeticoes) <- c("Número de repetições consecutivas", "Periodo de tempo (Horas)", "Porcetagem")

dfRepeticoes
# Não houveram falhas que perduraram 48hs


#confirmar se ha horarios duplicados
# Remover as amostras que se repetem em 24 horas 
########## REVER ESTE PERIODO ############
cepagri <- cepagri[!(consecutive(cepagri$Temperatura, 144) &
                       consecutive(cepagri$Vento, 144) &
                       consecutive(cepagri$Umidade, 144) &
                       consecutive(cepagri$Sensacao, 144)),]


#########   Analise 2 - estacoes do ano ############

# Calcula a média das medições realizadas ao longo dos 3 anos, agrupadas por cada dia de cada mês
mediaPorDia <- aggregate(cepagri[,2:5],list(format(cepagri$Horario, "%m-%d")), mean)
# Nomeia cada coluna do data frame mediaPorDia
colnames(mediaPorDia) <- c("Data", "Temperatura", "Vento", "Umidade", "Sensacao")

# Adiciona uma coluna no data frame que indica a qual estação aquele dia pertence
mediaPorDia$Estacao <- ""
# Os dias de Verão são aqueles compreendidos entre 21 de dezembro e 20 de março
mediaPorDia[mediaPorDia$Data >= "12-21" | mediaPorDia$Data < "03-21",]$Estacao <- "Verao"
# Os dias de Outono são aqueles compreendidos entre 21 de março e 20 de junho
mediaPorDia[mediaPorDia$Data >= "03-21" & mediaPorDia$Data < "06-21",]$Estacao <- "Outono"
# Os dias de Inverno são aqueles compreendidos entre 21 de Junho e 20 de Setembro
mediaPorDia[mediaPorDia$Data >= "06-21" & mediaPorDia$Data < "09-21",]$Estacao <- "Inverno"
# Os dias de Primavera são aqueles compreendidos entre 21 de Setembro e 20 de Dezembro
mediaPorDia[mediaPorDia$Data >= "09-21" & mediaPorDia$Data < "12-21",]$Estacao <- "Primavera"

# Adiciona uma coluna no data frame mediaPorDia que indica qual o ordinal de cada dia
# dentro da estação ao qual ele pertence. Inicializa a coluna com valores "0"
mediaPorDia$diaEstacao <- 0

# A partir de agora vamos calcular os ordinais de cada dia de cada estação e popular
# no data frame mediaPorDia.
# O Verão precisa ter uma tratamento especial pois ele começa no final do ano anterior
# e termina no começo do ano atual.
# veraoInicial indica o ordinal do primeiro dia da estação Verão a partir do inicio do ano
# A primeira atribuição a veraoInicial conta quantos dias de verão existem em dezembro
veraoInicial <- length(mediaPorDia[mediaPorDia$Estacao ==  "Verao" & mediaPorDia$Data > "12-01",]$Data)
# Esta segunda atribuição adiciona 1 para refletir o ordinal do 
# primeiro dia de verão em Janeiro
veraoInicial <- veraoInicial + 1
# veraoFinal indica o ordinal do ultimo dia da estação verão
veraoFinal <- length(mediaPorDia[mediaPorDia$Estacao ==  "Verao",]$Data)

# O vetor "ordinal" contem o ordinal do primeiro dia de cada estação a começar a contar
# do primeiro dia de janeiro
ordinal <- c("Verao" = veraoInicial, "Outono" = 1, "Inverno" = 1, "Primavera" = 1)

# Aqui populamos os ordinais de cada dia em sua respectiva estação do ano 
# no data frame "mediaPorDia"
for (i in mediaPorDia$Data) {
  # estacao_i guarda a estação ao qual o dia atual pertence
  estacao_i <- mediaPorDia[mediaPorDia$Data == i,]$Estacao
  
  # linha_i é um vetor de booleanos que será utilizado para indexar o data frame
  # "mediaPorDia" e acessar a linha correspondente à iteração atual no "for"
  linha_i <- mediaPorDia$Data == i
  
  # Atribuição do ordinal do dia em sua respectiva estação
  mediaPorDia[linha_i,]$diaEstacao <- ordinal[[estacao_i]]
  # Soma 1 ao ordinal da estação utilizada nesta iteração
  ordinal[[estacao_i]] <- ordinal[[estacao_i]] + 1
  
  # Ao terminar de passar pelos dias de Verão do inicio do ano, chegamos ao 
  # final dos ordinais que representam o Verão. Devemos reiniciar a variável
  # dos ordinais do Verão para representar os primeiros ordinais da estação,
  # que estão localizados no fim do ano.
  if (estacao_i == "Verao" & ordinal[[estacao_i]] > veraoFinal) {
    ordinal[[estacao_i]] <- 1
  }
}

# No momento, as estações são tratadas como texto e, quando necessário, o R irá
# ordená-las pelos seus caracteres. Para melhor visualição dos gráficos
# vamos ordenar as estações de acordo com sua posição no ano
# A ordem será: Verão, Outono, Inverno, Primavera
estacaoFactor <- as.factor(mediaPorDia$Estacao)
mediaPorDia$Estacao <- factor(estacaoFactor, levels(estacaoFactor)[c(4,2,1,3)])

# gTemperatura representa o grafico das médias diárias de temperatura em cada
# estação, levando em consideração os 3 anos com registro.
# Alem disso, traçamos uma curva de ajuste com intervalo de confiança para
# termos uma referência da tendendencia dos valores desenhados ao longo do período

# Cria o grafico gTemperatura indicando que os dados deverão ser agrupados por Estacao
# e que cada estação deverá ter uma cor distinta, além disso, o eixo X deverá contem os
# valores dos ordinais de cada dia dentro de sua estação.
gTemperatura <- ggplot(mediaPorDia, aes(x=diaEstacao, group = Estacao, colour = Estacao)) 
# Adiciona uma linha conectando todos os pontos de temperatura média
gTemperatura <- gTemperatura + geom_line(aes(y = Temperatura))
# Adiciona uma curva de ajuste com intervalo de confiança para os valores de temperatura média
gTemperatura <- gTemperatura + geom_smooth(aes(y = Temperatura))
# Pede a separação do gráfico em imagens distintas, sendo uma para cada estação, alem disso
# indica que as imagens devem estar concentradas em uma única linha.
gTemperatura <- gTemperatura + facet_wrap(~ Estacao, nrow=1)
# Exibe o gráfico
gTemperatura


# gUmidade representa o gráfico das médias diárias de umidade em cada
# estação, levando em consideração os 3 anos com registro.
# Alem disso, traçamos uma curva de ajuste com intervalo de confiança para
# termos uma referência da tendendencia dos valores desenhados ao longo do periodo

# Cria o gráfico gUmidade indicando que os dados deverão ser agrupados por Estacao
# e que cada estação deverá ter uma cor distinta, além disso, o eixo X deverá contem os
# valores dos ordinais de cada dia dentro de sua estação.
gUmidade <- ggplot(mediaPorDia, aes(x=diaEstacao, group = Estacao, colour = Estacao)) 
# Adiciona uma linha conectando todos os pontos de umidade média
gUmidade <- gUmidade + geom_line(aes(y = Umidade))
# Adiciona uma curva de ajuste com intervalo de confiança para os valores de umidade média
gUmidade <- gUmidade + geom_smooth(aes(y = Umidade))
# Pede a separação do gráfico em imagens distintas, sendo uma para cada estação, alem disso
# indica que as imagens devem estar concentradas em uma unica linha.
gUmidade <- gUmidade + facet_wrap(~ Estacao, nrow=1)
# Exibe o gráfico
gUmidade


# gVento representa o gráfico das médias diárias de velocidade do vento em cada
# estação, levando em consideração os 3 anos com registro.
# Alem disso, traçamos uma curva de ajuste com intervalo de confiança para
# termos uma referência da tendendencia dos valores desenhados ao longo do periodo

# Cria o gráfico gVento indicando que os dados deverão ser agrupados por Estacao
# e que cada estação deverá ter uma cor distinta, além disso, o eixo X deverá contem os
# valores dos ordinais de cada dia dentro de sua estação.
gVento <- ggplot(mediaPorDia, aes(x=diaEstacao, group = Estacao, colour = Estacao)) 
# Adiciona uma linha conectando todos os pontos de velocidade média do vento
gVento <- gVento + geom_line(aes(y = Vento))
# Adiciona uma curva de ajuste com intervalo de confiança para os valores de velocidade média do vento
gVento <- gVento + geom_smooth(aes(y = Vento))
# Pede a separação do gráfico em imagens distintas, sendo uma para cada estação, alem disso
# indica que as imagens devem estar concentradas em uma unica linha.
gVento <- gVento + facet_wrap(~ Estacao, nrow=1)
# Exibe o gráfico
gVento


# gSensacao representa o gráfico das médias diárias de sensação térmica em cada
# estação, levando em consideração os 3 anos com registro.
# Alem disso, traçamos uma curva de ajuste com intervalo de confiança para
# termos uma referência da tendendencia dos valores desenhados ao longo do periodo.

# Cria o gráfico gSensacao indicando que os dados deverão ser agrupados por Estacao
# e que cada estação deverá ter uma cor distinta, além disso, o eixo X deverá contem os
# valores dos ordinais de cada dia dentro de sua estação.
gSensacao <- ggplot(mediaPorDia, aes(x=diaEstacao, group = Estacao, colour = Estacao)) 
# Adiciona uma linha conectando todos os pontos de sensação térmica média
gSensacao <- gSensacao + geom_line(aes(y = Sensacao))
# Adiciona uma curva de ajuste com intervalo de confiança para os valores de sensação térmica média
gSensacao <- gSensacao + geom_smooth(aes(y = Sensacao))
# Pede a separação do gráfico em imagens distintas, sendo uma para cada estação, alem disso
# indica que as imagens devem estar concentradas em uma unica linha.
gSensacao <- gSensacao + facet_wrap(~ Estacao, nrow=1)
# Exibe o gráfico
gSensacao


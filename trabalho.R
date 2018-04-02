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
                      colClasses = c("character","character","numeric","numeric","numeric"),
                      stringsAsFactors=FALSE)
# 210736 amostras no conjunto de dados original em 30/03/2018 16:26


#########   Analise 1 - Distribuição de erros nos dados ############

# Cria o dataframe que ira guardar o número total de amostras inicias, NAs, outiliers e dados repetidos
dfAnaliseErros <-data.frame(dataType = c("Inicial",  "Com valor NA", "Com valor outliers", "Com um travamento do sensor"), 
                            number = rep(x=NA, times=4))

# Converte a string com horarios para POSIXlt
cepagri$Horario <- strptime (cepagri$Horario, "%d/%m/%Y-%H:%M")

# Escolhe apenas as amostras entre o periodo desejado: 01/01/2015-31/12/2017
periodo <- cepagri$Horario >= "2015-01-01" & cepagri$Horario < "2018-01-01"
cepagri <- cepagri[periodo, ]

# Adiciona o número de dados do perído, inclusive, com erros
dfAnaliseErros$number[1] = length(cepagri$Horario)

# Converte a coluna 2 para numeric, onde tiver a string "ERRO", ira'transformar pra NA
cepagri [ , 2] <- as.numeric(cepagri [ ,2])

# Remove qualquer NA na tabela
for(i in 1:5) {
  cepagri <- cepagri [!is.na(cepagri[ , i]), ]
}
# 154257 amostras sobraram - 1801 amostras removidas do periodo

# Atualiza os valores do número de erros relacionados a NAs
dfAnaliseErros$number[2] = dfAnaliseErros$number[1] - length(cepagri$Horario)

# 154257 amostras dentro do periodo desejado de 3 anos
# 3 anos * 365 dias + 1 dia de ano bissexto 2016 * 24 horas * 6 coletas por hora = 157824 amostras

# Summary irá evidenciar erros de outiliers, principalmente nos valores máximo e mínimo
summary(cepagri)

# Remove os outliers - 137 valores de Sensacao = 99.90, total de amostra = 154120
cepagri <- cepagri[cepagri$Sensacao != 99.90,]

# Agora a temperatura máxima está com factível (37.40 Celsius)
summary(cepagri)

# Atualiza os valores do número de erros relacionados aos outiliers
dfAnaliseErros$number[3] = dfAnaliseErros$number[1] - dfAnaliseErros$number[2] - length(cepagri$Horario)

# Função vista em sala de aula, que dado um vetor e um inteiro k verifica se cada posição do vetor é 
# igual às k posições anteriores ou posteriores
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

# Numero de repetições relacionadas a 10min, 20 min, 1h, 2h, 4h, 6h, 12h, 24h, 36h e 48h
nTimes = c(1, 2, 6, 12, 24, 36, 72, 144, 216, 288);
vRepeatedValues = rep(x=0, times=length(nTimes))
for(i in 1:length(nTimes)) {
  repeated = 
    consecutive(cepagri$Temperatura, nTimes[i]) & 
    consecutive(cepagri$Vento, nTimes[i]) & 
    consecutive(cepagri$Umidade, nTimes[i]) & 
    consecutive(cepagri$Sensacao, nTimes[i])
  
  vRepeatedValues[i] = sum (repeated)
}

dfRepeticoes <-data.frame(vRepeatedValues=vRepeatedValues, nHoras=round(nTimes/6,2), porcentagem=round((vRepeatedValues/length(cepagri$Horario)) * 100,2))
names(dfRepeticoes) <- c("Número de repetições consecutivas", "Periodo de tempo (Horas)", "Porcetagem nos dados")


#Analise 1
dfRepeticoes
# Não houveram falhas que perduraram 48hs
# Devida a natureza do problema é altamente improvavel que os 4 variáveis físicas fiquem constantes 
# por mais 1h. Devido ao seu pouco impacto no conjuntos de dados pode-se remover esses dados, 
# o que seria equivalente a 1.31% das amostras válidas

cepagri <- cepagri[!(consecutive(cepagri$Temperatura, 6) &
                       consecutive(cepagri$Vento, 6) &
                       consecutive(cepagri$Umidade, 6) &
                       consecutive(cepagri$Sensacao, 6)),]



# Atualiza os valores do número de erros relacionados aos outiliers
dfAnaliseErros$number[4] = dfAnaliseErros$number[1] - dfAnaliseErros$number[2] - dfAnaliseErros$number[3] - length(cepagri$Horario)


# Gráfico que mostra quantitivamente os tipos de erros removidos comparando com a quantidade inicial de dados
pErros <- ggplot (dfAnaliseErros , aes(x = dataType , y = number))
pErros <- pErros + geom_bar(stat = "identity", fill = "#66FFFB") + xlab("Tipo") + ylab("Quantidade de dados")
pErros <- pErros + ggtitle("Comparação dos tipos de erros e os dados iniciais")
pErros




#########   Analise 2 - previsão do tempo simplista ############

# Esta função irá determinar os valores de Temperatura, Umidade, 
# Velocidade do Vento e Sensação Térmica mínimos e máximos para cada dia
# a partir do valores dessas grandezas de um número arbitrário de dias
# anteriores. dfMin e dfMax devem ter o mesmo tamanho
# Parametros: dfMin: Um dataframe contendo as colunas
#                    Data - no formato "YYYY-mm-dd"
#                    Temperatura, Umidade, Vento, Sensação Térmica
#                      Contendo os valores mínimos registrados para
#                      estas grandezas do respectivo dia.
#             dfMax: Um dataframe contendo as colunas
#                    Data - no formato "YYYY-mm-dd"
#                    Temperatura, Umidade, Vento, Sensação Térmica
#                      Contendo os valores máximos registrados para
#                      estas grandezas do respectivo dia.
#             ndias: número de dias anteriores que se deseja considerar
#                    ao buscar as grandezas mínimas e máximas prévias a cada dia
# Retorno: Um dataframe contendo as colunas
#          Data - no formato "YYYY-mm-dd" 
#          minNTemp, maxNTemp, minNUnid, maxNUmid, minNVent, maxNVent,
#          minNSens, maxNSens que indicam, para cada Data, os valores
#          minimos e máximos para as grandezas encontrados nos "ndias"
#          anteriores à cada Data.
calculaIntervaloMinMax <- function(dfMax, dfMin, ndias) {
  # dfaux será o data frame retornado pela função. Todos os valores são 
  # inicializados com NA, e aquelas datas que possuem dados suficientes
  # serão populadas com os valores corretos.
  dfaux <- data.frame(Data = dfMax$Data, 
                      minNTemp = rep(NA,length(dfMin$Data)),
                      maxNTemp = rep(NA,length(dfMax$Data)), 
                      minNUmid = rep(NA,length(dfMin$Data)),
                      maxNUmid = rep(NA,length(dfMax$Data)), 
                      minNVent = rep(NA,length(dfMin$Data)),
                      maxNVent = rep(NA,length(dfMax$Data)), 
                      minNSens = rep(NA,length(dfMin$Data)),
                      maxNSens = rep(NA,length(dfMax$Data)), 
                      stringsAsFactors = FALSE)
  
  # Este laço percorre todas as posições dos data frames de entrada
  # e calcula os valores das grandezas máximas e mínimas para cada dia.
  # Começa em "2" pois o primeiro dia nunca terá nenhum dia anterior a ele.
  for (i in (2):length(dfMax$Data)) {
    # Calcula o "ndias" anterior ao registro atual sendo analisado,
    # utiliza o max() com o segundo parametro sendo a data do primeiro registro
    # para não tentar olhar registros inexistentes.
    dataNDiaAnterior <- max(as.Date(dfMax[i,]$Data) - ndias, as.Date(dfMax[1,]$Data))
    # Calcula a data imediatamente anterior ao registro atual.
    ontem <- as.Date(dfMax[i,]$Data) - 1
    
    # Cria dois data frames auxiliares, contendo as mesmas colunas dos data
    # frames dfMin e dfMax de entrada, porém apenas as linhas correspondentes 
    # às datas compreendidas entre "dataNDiaAnterior" e "ontem"
    nUltimosDiasMax <- dfMax[(dfMax$Data >= dataNDiaAnterior & dfMax$Data <= ontem),]
    nUltimosDiasMin <- dfMin[(dfMin$Data >= dataNDiaAnterior & dfMin$Data <= ontem),]
    
    # Garante que há registro de pelo menos 1 dia no intervalo procurado
    if (length(nUltimosDiasMax$Temperatura) > 0) {
      # Popula os campos da Data atual no data frame de saída
      dfaux[i,]$minNTemp <- min(nUltimosDiasMin$Temperatura)
      dfaux[i,]$maxNTemp <- max(nUltimosDiasMax$Temperatura)
      dfaux[i,]$minNUmid <- min(nUltimosDiasMin$Umidade)
      dfaux[i,]$maxNUmid <- max(nUltimosDiasMax$Umidade)
      dfaux[i,]$minNVent <- min(nUltimosDiasMin$Vento)
      dfaux[i,]$maxNVent <- max(nUltimosDiasMax$Vento)
      dfaux[i,]$minNSens <- min(nUltimosDiasMin$Sensacao)
      dfaux[i,]$maxNSens <- max(nUltimosDiasMax$Sensacao)
    }
  }
  return(dfaux)
}

# Esta função irá determinar se os registros de temperatura, umidade, 
# velocidade do vento e sensação térmica de cada dia estão dentro de 
# um determinado intervalo.
# Parametros: dfMin: Um dataframe contendo as colunas
#                    Data - no formato "YYYY-mm-dd"
#                    Temperatura, Umidade, Vento, Sensação Térmica
#                      Contendo os valores mínimos registrados para
#                      estas grandezas do respectivo dia.
#             dfMax: Um dataframe contendo as colunas
#                    Data - no formato "YYYY-mm-dd"
#                    Temperatura, Umidade, Vento, Sensação Térmica
#                      Contendo os valores máximos registrados para
#                      estas grandezas do respectivo dia.
#                     Temperatura, Umidade, Vento, Sensação Térmica com os valores
#                     máximos de cada grandeza
#             previsao: data frame contendo as os dados para serem avaliados contra
#                       os valores de dfMin e dfMax
# Retorno: Um data frame com as seguintes colunas:
#          Data - no format "YYYY-mm-dd"
#          Na - que indica que não valores de mínimo e máximo para aquele dia
#          tempAcerto, tempErro, umidAcerto, umidErro, ventAcerto, ventErro, sensAcerto
#          sensErro - que indicam se os valores para mínimo e máximo dessas grandezas
#          na data observada (dfMin e dfMax) estão dentro dos mínimos e máximos previstos 
#          em "previsao"
verificaPrevisao <- function(dfMin,dfMax, previsao) {
  # data frame que será retornado
  result <- data.frame(Data = dfMin$Data, 
                       Na = rep(FALSE,length(dfMin$Data)),
                       tempAcerto = rep(FALSE,length(dfMin$Data)),
                       tempErro = rep(FALSE,length(dfMin$Data)),
                       umidAcerto = rep(FALSE,length(dfMin$Data)),
                       umidErro = rep(FALSE,length(dfMin$Data)),
                       ventAcerto = rep(FALSE,length(dfMin$Data)),
                       ventErro = rep(FALSE,length(dfMin$Data)),
                       sensAcerto = rep(FALSE,length(dfMin$Data)),
                       sensErro = rep(FALSE,length(dfMin$Data)))
  # Laço que percorre os data frames de entrada comparando os valores observados
  # em cada dia (dfMin e dfMax) com as previsões realizadas em "previsao"
  for(i in 1:length(dfMin$Data)) {
    # Nos data frames deste trabalho, sempre que o valor de uma medição está
    # com NA, todos os valores das medições estão com NA.
    if (is.na(previsao[i,]$maxNTemp)) {
      result[i,]$Na <- TRUE
    } else {
      # Verifica se cada valor observado (dfMin e dfMax) está dentro dos limites
      # do indicado pela previsao (previsao)
      if (dfMax[i,]$Temperatura <= previsao[i,]$maxNTemp & 
          dfMin[i,]$Temperatura >= previsao[i,]$minNTemp) {
        result[i,]$tempAcerto <- TRUE
      } else {
        result[i,]$tempErro <- TRUE
      }
      if (dfMax[i,]$Umidade <= previsao[i,]$maxNUmid & 
          dfMin[i,]$Umidade >= previsao[i,]$minNUmid) {
        result[i,]$umidAcerto <- TRUE
      } else {
        result[i,]$umidErro <- TRUE
      }
      if (dfMax[i,]$Vento <= previsao[i,]$maxNVent & 
          dfMin[i,]$Vento >= previsao[i,]$minNVent) {
        result[i,]$ventAcerto <- TRUE
      } else {
        result[i,]$ventErro <- TRUE
      }
      if (dfMax[i,]$Sensacao <= previsao[i,]$maxNSens & 
          dfMin[i,]$Sensacao >= previsao[i,]$minNSens) {
        result[i,]$sensAcerto <- TRUE
      } else {
        result[i,]$sensErro <- TRUE
      }
    }
  }
  return(result)
}

# Calcular as medições máxima e mínima diárias
maxPorDia <- aggregate(cepagri[,2:5],list(format(cepagri$Horario, "%Y-%m-%d")), max)
minPorDia <- aggregate(cepagri[,2:5],list(format(cepagri$Horario, "%Y-%m-%d")), min)
# Nomear as colunas dos data frames que contem as medições mínimas e máximas diárias
colnames(maxPorDia) <- c("Data", "Temperatura", "Vento", "Umidade", "Sensacao")
colnames(minPorDia) <- c("Data", "Temperatura", "Vento", "Umidade", "Sensacao")

# Usa a função calculaIntervaloMinMax para determinar, para cada dia, qual as medições
# mínima e máxima observadas considerando diversos números de dias anteriores à cada 
# medição: 1 dia, 3 dias, 5 dias, 7 dias, 15 dias, 30 dias.
df1 <- calculaIntervaloMinMax(dfMax = maxPorDia, dfMin = minPorDia, ndias = 1)
df3 <- calculaIntervaloMinMax(dfMax = maxPorDia, dfMin = minPorDia, ndias = 3)
df5 <- calculaIntervaloMinMax(dfMax = maxPorDia, dfMin = minPorDia, ndias = 5)
df7 <- calculaIntervaloMinMax(dfMax = maxPorDia, dfMin = minPorDia, ndias = 7)
df15 <- calculaIntervaloMinMax(dfMax = maxPorDia, dfMin = minPorDia, ndias = 15)
df30 <- calculaIntervaloMinMax(dfMax = maxPorDia, dfMin = minPorDia, ndias = 30)

# Usa a função verificaPrevisao para determinar, em cada dia, se as medições realizadas
# ficam dentro de um intervalo determinado (calculado em df1..df30)
resultado1 <- verificaPrevisao(dfMin = minPorDia, dfMax = maxPorDia, previsao = df1)
resultado3 <- verificaPrevisao(dfMin = minPorDia, dfMax = maxPorDia, previsao = df3)
resultado5 <- verificaPrevisao(dfMin = minPorDia, dfMax = maxPorDia, previsao = df5)
resultado7 <- verificaPrevisao(dfMin = minPorDia, dfMax = maxPorDia, previsao = df7)
resultado15 <- verificaPrevisao(dfMin = minPorDia, dfMax = maxPorDia, previsao = df15)
resultado30 <- verificaPrevisao(dfMin = minPorDia, dfMax = maxPorDia, previsao = df30)

# Vetor com o total de acertos para cada tipo de medida, em cada tipo de previsão.
# Acertos da medida do dia atual utilizando os máximos e mínimos de 1 dia anterior,
# 3 dias anteriores, 5 dias anteriores, 7 dias anteriores, 15 dias anteriores, 30 
# dias anteriores. Além disso tambem contém os acertos de previsões para os mesmos períodos
# levando em conta todas as medidas simultaneamente, e também levando em conta um acerto
# em qualquer uma das medidas
Acertos <- c(sum(resultado1$tempAcerto), 
             sum(resultado3$tempAcerto), 
             sum(resultado5$tempAcerto), 
             sum(resultado7$tempAcerto), 
             sum(resultado15$tempAcerto), 
             sum(resultado30$tempAcerto),
             sum(resultado1$umidAcerto), 
             sum(resultado3$umidAcerto), 
             sum(resultado5$umidAcerto), 
             sum(resultado7$umidAcerto), 
             sum(resultado15$umidAcerto), 
             sum(resultado30$umidAcerto),
             sum(resultado1$tempAcerto & resultado1$umidAcerto & resultado1$ventAcerto & resultado1$sensAcerto),
             sum(resultado3$tempAcerto & resultado3$umidAcerto & resultado3$ventAcerto & resultado3$sensAcerto),
             sum(resultado5$tempAcerto & resultado5$umidAcerto & resultado5$ventAcerto & resultado5$sensAcerto),
             sum(resultado7$tempAcerto & resultado7$umidAcerto & resultado7$ventAcerto & resultado7$sensAcerto),
             sum(resultado15$tempAcerto & resultado15$umidAcerto & resultado15$ventAcerto & resultado15$sensAcerto),
             sum(resultado30$tempAcerto & resultado30$umidAcerto & resultado30$ventAcerto & resultado30$sensAcerto),
             sum(resultado1$tempAcerto | resultado1$umidAcerto | resultado1$ventAcerto | resultado1$sensAcerto),
             sum(resultado3$tempAcerto | resultado3$umidAcerto | resultado3$ventAcerto | resultado3$sensAcerto),
             sum(resultado5$tempAcerto | resultado5$umidAcerto | resultado5$ventAcerto | resultado5$sensAcerto),
             sum(resultado7$tempAcerto | resultado7$umidAcerto | resultado7$ventAcerto | resultado7$sensAcerto),
             sum(resultado15$tempAcerto | resultado15$umidAcerto | resultado15$ventAcerto | resultado15$sensAcerto),
             sum(resultado30$tempAcerto | resultado30$umidAcerto | resultado30$ventAcerto | resultado30$sensAcerto))

# Vetor com o total de erros para cada tipo de medida, em cada tipo de previsão.
# Erros da medida do dia atual utilizando os máximos e mínimos de 1 dia anterior,
# 3 dias anteriores, 5 dias anteriores, 7 dias anteriores, 15 dias anteriores, 30 
# dias anteriores. Além disso tambem contém os erros de previsões para os mesmos períodos
# levando em conta qualquer uma das medidas, e também levando em conta erros em todas
# as medidas simultaneamente
Erros <- c(sum(resultado1$tempErro), 
           sum(resultado3$tempErro), 
           sum(resultado5$tempErro), 
           sum(resultado7$tempErro), 
           sum(resultado15$tempErro), 
           sum(resultado30$tempErro),
           sum(resultado1$umidErro), 
           sum(resultado3$umidErro), 
           sum(resultado5$umidErro), 
           sum(resultado7$umidErro), 
           sum(resultado15$umidErro), 
           sum(resultado30$umidErro),
           sum(resultado1$tempErro | resultado1$umidErro | resultado1$ventErro | resultado1$sensErro),
           sum(resultado3$tempErro | resultado3$umidErro | resultado3$ventErro | resultado3$sensErro),
           sum(resultado5$tempErro | resultado5$umidErro | resultado5$ventErro | resultado5$sensErro),
           sum(resultado7$tempErro | resultado7$umidErro | resultado7$ventErro | resultado7$sensErro),
           sum(resultado15$tempErro | resultado15$umidErro | resultado15$ventErro | resultado15$sensErro),
           sum(resultado30$tempErro | resultado30$umidErro | resultado30$ventErro | resultado30$sensErro),
           sum(resultado1$tempErro & resultado1$umidErro & resultado1$ventErro & resultado1$sensErro),
           sum(resultado3$tempErro & resultado3$umidErro & resultado3$ventErro & resultado3$sensErro),
           sum(resultado5$tempErro & resultado5$umidErro & resultado5$ventErro & resultado5$sensErro),
           sum(resultado7$tempErro & resultado7$umidErro & resultado7$ventErro & resultado7$sensErro),
           sum(resultado15$tempErro & resultado15$umidErro & resultado15$ventErro & resultado15$sensErro),
           sum(resultado30$tempErro & resultado30$umidErro & resultado30$ventErro & resultado30$sensErro))

# Vetor com os valores das quatindades de NA em cada tipo de previsao, 1, 3, 5, 7, 15
# e 30 dias. Os valores de NA se repetem em todas as medidas para os mesmos periodos,
# então podemos simplesmente repetir os valores tantas vezes quantas são as análises
# nos vetores de Erro e Acerto
Na <- rep(c(sum(resultado1$Na), 
           sum(resultado3$Na), 
           sum(resultado5$Na), 
           sum(resultado7$Na), 
           sum(resultado15$Na), 
           sum(resultado30$Na)),
           4)

# Nome que indica qual o significado do valor de cada linha dos vetores 
# Erro, Acerto e Na
rotulos <- factor(c("Temperatura 1 dia", 
                  "Temperatura 3 dias", 
                  "Temperatura 5 dias", 
                  "Temperatura 7 dias", 
                  "Temperatura 15 dias", 
                  "Temperatura 30 dias",
                  "Umidade 1 dia", 
                  "Umidade 3 dias", 
                  "Umidade 5 dias", 
                  "Umidade 7 dias", 
                  "Umidade 15 dias", 
                  "Umidade 30 dias",
                  "Todas Medidas 1 dia",
                  "Todas Medidas 3 dias",
                  "Todas Medidas 5 dias",
                  "Todas Medidas 7 dias",
                  "Todas Medidas 15 dias",
                  "Todas Medidas 30 dias",
                  "Ao Menos uma med. 1 dia",
                  "Ao Menos uma med. 3 dias",
                  "Ao Menos uma med. 5 dias",
                  "Ao Menos uma med. 7 dias",
                  "Ao Menos uma med. 15 dias",
                  "Ao Menos uma med. 30 dias"), 
                  levels = c("Temperatura 1 dia", 
                            "Temperatura 3 dias", 
                            "Temperatura 5 dias", 
                            "Temperatura 7 dias", 
                            "Temperatura 15 dias", 
                            "Temperatura 30 dias",
                            "Umidade 1 dia", 
                            "Umidade 3 dias", 
                            "Umidade 5 dias", 
                            "Umidade 7 dias", 
                            "Umidade 15 dias", 
                            "Umidade 30 dias",
                            "Todas Medidas 1 dia",
                            "Todas Medidas 3 dias",
                            "Todas Medidas 5 dias",
                            "Todas Medidas 7 dias",
                            "Todas Medidas 15 dias",
                            "Todas Medidas 30 dias",
                            "Ao Menos uma med. 1 dia",
                            "Ao Menos uma med. 3 dias",
                            "Ao Menos uma med. 5 dias",
                            "Ao Menos uma med. 7 dias",
                            "Ao Menos uma med. 15 dias",
                            "Ao Menos uma med. 30 dias"),
                  ordered = TRUE)

# Repete os rotulos 3 vezes, uma vez para referenciar os valores de Acertos, uma para
# referenciar os valores de Erro, e outra para os valores de NA
Previsoes = rep(rotulos,3)
# Vetor que indica se cada medida se refere a um Acerto, Erro ou NA
Resposta <- factor(c(rep("Acerto",length(rotulos)),rep("Erro",length(rotulos)),rep("NA",length(rotulos))), 
                   levels = c("NA", "Erro", "Acerto"), ordered = TRUE)
# Vetor com os valores propriamente ditos de Acerto, Erro e NA
Frequencia <- c(Acertos,Erros,Na)

# Data frame com os dados da quantidade de Acertos, Erros, NAs para cada tipo de previsao
# e em cada tipo de medida: Temperatura, Umidade, 
# também todas as medidas simultaneamente, e ao menos uma das medidas.
dfPrevisao <- data.frame(Previsoes,Resposta,Frequencia)
g <- ggplot(dfPrevisao,aes(x=Previsoes,y=Frequencia,fill=Resposta))
g <- g+ geom_bar(stat="identity")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g

# Encontra a quantidade total de previsoes para cada medida (são todas iguais)
Total <- Acertos[1] + Erros[1] + Na[1]
# Converte cada resultado de Acerto, Erro e Na para porcentagem, mantendo 2 casas decimais
Porcentagem <- c(Acertos/Total,Erros/Total,Na/Total)
Porcentagem <- trunc(Porcentagem * 10000)
Porcentagem <- Porcentagem / 100

# Tabela com as pocentagens de cada tipo de previsao e seu respectivo resultado.
tabelaPorcentagem <- data.frame("Tipos de previsao" = rotulos,
                      Acertos = Porcentagem[1:(length(Porcentagem)/3)],
                      Erros = Porcentagem[((length(Porcentagem)/3)+1):(2*(length(Porcentagem)/3))],
                      NAs = Porcentagem[((2*(length(Porcentagem)/3))+1):length(Porcentagem)])
tabelaPorcentagem

# ano irá representar o ano extraído da Data de cada amostra realizada
ano <- strptime(maxPorDia$Data, "%Y-%m-%d")$year + 1900
# data frame que contem os dados da temperatura prevista para os casos utilizando 3 dias
# anteriores e 30 dias anteriores, além da tempertura real medida para cada dia
dfPrevReal <- data.frame(TempMin=minPorDia$Temperatura, TempMax=maxPorDia$Temperatura, 
                 PrevMin3=df3$minNTemp, PrevMax3 = df3$maxNTemp, 
                 PrevMin30=df30$minNTemp, PrevMax30 = df30$maxNTemp,
                 Data=maxPorDia$Data, Ano=ano)

# gráfico que irá mostrar as curvas de temperatura mínima e máxima prevista 
# utilizando a previsão com 3 dias de antecendia, e a temperatura real observada
g3 <- ggplot(dfPrevReal[dfPrevReal$Ano == "2016",], aes(x = Data))
g3 <- g3 + geom_line(aes(y = TempMin, color = "Real", group=1))
g3 <- g3 + geom_line(aes(y = TempMax, color = "Real", group=2))
g3 <- g3 + geom_line(aes(y = PrevMin3, color = "Previsao com 3 dias", group=1))
g3 <- g3 + geom_line(aes(y = PrevMax3, color = "Previsao com 3 dias", group=2))
g3 <- g3 + ylab("Temperatura") + xlab("2016")
g3 <- g3 + theme(axis.text.x=element_blank(),
                 axis.ticks.x=element_blank())
g3


# gráfico que irá mostrar as curvas de temperatura mínima e máxima prevista 
# utilizando a previsão com 30 dias de antecendia, e a temperatura real observada
g30 <- ggplot(dfPrevReal[dfPrevReal$Ano == "2016",], aes(x = Data))
g30 <- g30 + geom_line(aes(y = TempMin, color = "Real", group=1))
g30 <- g30 + geom_line(aes(y = TempMax, color = "Real", group=2))
g30 <- g30 + geom_line(aes(y = PrevMin30, color = "Previsao com 30 dias", group=3))
g30 <- g30 + geom_line(aes(y = PrevMax30, color = "Previsao com 30 dias", group=4))
g30 <- g30 + ylab("Temperatura") + xlab("2016")
g30 <- g30 + theme(axis.text.x=element_blank(),
                 axis.ticks.x=element_blank())
g30



#########   Analise 3 - estações do ano ############

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
gTemperatura <- gTemperatura + facet_wrap(~ Estacao, nrow = 1)
gTemperatura <- gTemperatura + xlab("Dia da estacao")
gTemperatura <- gTemperatura + ylab("Media da Temperatura por 3 Anos")
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
gUmidade <- gUmidade + facet_wrap(~ Estacao, nrow = 1)
gUmidade <- gUmidade + xlab("Dia da estacao")
gUmidade <- gUmidade + ylab("Media da Umidade por 3 Anos")
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
gVento <- gVento + facet_wrap(~ Estacao, nrow = 1)
gVento <- gVento + xlab("Dia da estacao")
gVento <- gVento + ylab("Media da Velocidade do Vento por 3 Anos")
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
gSensacao <- gSensacao + facet_wrap(~ Estacao, nrow = 1)
gSensacao <- gSensacao + xlab("Dia da estacao")
gSensacao <- gSensacao + ylab("Media da Sensacao Termica por 3 Anos")
# Exibe o gráfico
gSensacao



#########   Analise 4 - Qualidade do ar em Campinas em termos da umidade relativa ############
# Cria-se um data frame auxiliar para que possa facilitar a seleção dos dados
cepagriQualidade <- data.frame(Umidade=cepagri$Umidade, Horario=cepagri$Horario, Ano=cepagri$Horario$year+1900)
# Cria-se um data frame auxiliar para que possa facilitar a seleção dos dados
gQualidade <- ggplot (cepagriQualidade, aes(x = Horario, y = Umidade, group = Ano))
# As linhas amarela, marrom e vermelha denotam "estado de atenção", "estado de alerta" e 
# "estado de emergência", respectivamente. 
gQualidade <- gQualidade + geom_hline(yintercept = 30, color="yellow") + 
  geom_hline (yintercept = 20, color="brown") + geom_hline(yintercept = 12, color="red") + geom_line()
# Para facilitar a análise, nos graficos, tem-se a mesma escala da umidade
gQualidade <- gQualidade + facet_wrap(~ Ano, scales = "free_x", nrow = 3)
gQualidade



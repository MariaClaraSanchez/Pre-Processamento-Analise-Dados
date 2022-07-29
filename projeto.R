#1. Identificação do atributo alvo (saída);

library(readr)

base <- read.csv("breast-cancer.csv", stringsAsFactors = FALSE)
summary(base)

#Atributo alvo da minha base de dados é o Class
atributo_alvo <- base$class
table(atributo_alvo) 

#2. Identificação dos tipos de dados dos atributos de entrada (quantitativo, qualitativo);

#Ver um por um
unique(base$class)
unique(base$age)
unique(base$menopause)
unique(base$tumor.size)
unique(base$inv.nodes)
unique(base$node.caps)
unique(base$deg.malig)
unique(base$breast)
unique(base$breast.quad)
unique(base$irradiat)

#3. Identificação da escala de dados dos atributos de entrada (nominal, ordinal, intervalar, racional);


library('dplyr')

#Ver como um todo o tipo da variável 
glimpse(base)


table(base$class) 
table(base$age)
table(base$menopause)
table(base$tumor.size)
table(base$inv.nodes)
table(base$node.caps)
table(base$deg.malig)
table(base$breast)
table(base$breast.quad)
table(base$irradiat)

#4. Exploração dos dados através de medidas de localidade;

# Função que encontra a moda
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

find_mode(base$class)
find_mode(base$age)
find_mode(base$menopause)
find_mode(base$tumor.size)
find_mode(base$inv.nodes)
find_mode(base$node.caps)
find_mode(base$deg.malig)
find_mode(base$breast)
find_mode(base$breast.quad)
find_mode(base$irradiat)

# Nessa parte eu tive que pular para o passo 12 e 13 para realizar a conversão dos dados 
# Que ão todos simbólicos para numéricos. A normalização eu faço depois, apenas a convesão
# foi feita nessa sequência. E também o tratamento faltantes

#12. Limpeza de dados:

#D) Preencher dados ausentes, pela moda
base$breast.quad[base$breast.quad == "?"] <- find_mode(base$breast.quad)
base$node.caps[base$node.caps =="?"] <- find_mode(base$node.caps)

#13. Identificação e conversão dos tipos de dados (caso não seja necessário, analisar o porquê). Os tipos de conversão que podem ser utilizados são:

#A) Conversão dos atributos qualitativos para quantitativos:

#Com apenas 2 tipos
base$class = factor(base$class, levels = c('no-recurrence-events','recurrence-events'), labels = c(0,1))
base$irradiat = factor(base$irradiat, levels = c('no','yes'), labels = c(0,1))
base$breast = factor(base$breast, levels = c('left','right'), labels = c(0,1))
base$node.caps = factor(base$node.caps, levels = c('no','yes'), labels = c(0,1))
#Com 3 tipos
base$deg.malig = factor(base$deg.malig, levels = c('1','2','3'),labels = c(0,1,2))
#Com mais de 3
base$age = factor(base$age, levels = c('20-29','30-39','40-49','50-59','60-69','70-79'),labels = c(0,1,2,3,4,5))
base$menopause = factor(base$menopause, levels = c('premeno','lt40','ge40'),labels = c(0,1,2))
base$tumor.size = factor(base$tumor.size, levels = c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54'),labels = c(0,1,2,3,4,5,6,7,8,9,10))
base$inv.nodes = factor(base$inv.nodes, levels = c('0-2','3-5','6-8','9-11','12-14','15-17','24-26'), labels = c(0,1,2,3,4,5,6))

#Converter para numérico
base$class <- as.numeric(as.character(base$class))
base$age <- as.numeric(as.character(base$age))
base$menopause <- as.numeric(as.character(base$menopause))
base$tumor.size <- as.numeric(as.character(base$tumor.size))
base$inv.nodes <- as.numeric(as.character(base$inv.nodes))
base$node.caps <- as.numeric(as.character(base$node.caps))
base$deg.malig <- as.numeric(as.character(base$deg.malig))
base$breast <- as.numeric(as.character(base$breast))
base$irradiat <- as.numeric(as.character(base$irradiat))

#Criando novas colunas
base$left_low <- 0
base$left_low[base$breast.quad == "left_low"] <- 1

base$right_up <- 0
base$right_up[base$breast.quad == "right_up"] <- 1

base$left_up <- 0
base$left_up[base$breast.quad == "left_up"] <- 1

base$right_low <- 0
base$right_low[base$breast.quad == "right_low"] <- 1

base$central <- 0
base$central[base$breast.quad == "central"] <- 1

#verificar se deu certo
base[is.na(base$inv.nodes)]


#8. Identificação e eliminação de atributos não necessários;

#Depois de dividir em 4 atributos o atributo breast.quad, consegui eliminar esse atributo
base$breast.quad = NULL


# Depois de pular para o Passo 13 e o 8, posso voltar a seguir a ordem normal.


#Voltando no passo 4, depois de fazer as conversões para numéricos, é possível agora
# plotar gráficos de boxplot.

#Com os dados convertidos para numéricos:
#pch - outliers

boxplot(base$class,
        main = "Boxplot Atributo class",
        ylab = "Recorrência",
        pch = 16)

boxplot(base$age, 
        main = "Boxplot Atributo age",
        ylab = "Idade do indivíduo",
        pch = 16)

boxplot(base$menopause, 
        main = "Boxplot Atributo menopause ",
        ylab = "Indicação da Menopausa",
        pch = 16)

boxplot(base$tumor.size, 
        main = "Boxplot Atributo tumor.size",
        ylab = "Diâmetro do tumor",
        pch = 16)

boxplot(base$inv.nodes, 
        main = "Boxplot Atributo Inv.nodes",
        ylab = "Número de linfonodos axilares",
        pch = 16)

boxplot(base$node.caps, 
        main = "Boxplot Atributo node.caps",
        ylab = "Penetração do tumor",
        pch = 16)

boxplot(base$deg.malig , 
        main = "Boxplot Atributo deg.malig",
        ylab = "Grau de malignidade do tumor",
        pch = 16)

boxplot(base$breast, 
        main = "Boxplot Atributo breast",
        ylab = "Mama em que o câncer pode ocorrer",
        pch = 16)

boxplot(base$irradiat, 
        main = "Boxplot Atributo irradiat",
        ylab = "Irradiat",
        pch = 16)

boxplot(base$left_low, 
        main = "Boxplot Atributo left_low",
        ylab = "Quadrante da mama afetado",
        pch = 16)

boxplot(base$right_up, 
        main = "Boxplot Atributo right_up ",
        ylab = "Quadrante da mama afetado",
        pch = 16)

boxplot(base$left_up, 
        main = "Boxplot Atributo left_up",
        ylab = "Quadrante da mama afetado",
        pch = 16)

boxplot(base$right_low, 
        main = "Boxplot Atributo right_low",
        ylab = "Quadrante da mama afetado",
        pch = 16)

boxplot(base$central, 
        main = "Boxplot Atributo central",
        ylab = "Quadrante da mama afetado",
        pch = 16)

#5. Exploração dos dados através de medidas de espalhamento;

#class = rnorm(base$class,mean = base$class,sd(base$class))


hist(base$class,
     main = "Histograma Atributo class",
     ylab = "Frequência",
     xlab = "Tipo do dado")


hist(base$age, 
     main = "Histograma Atributo age",
     ylab = "Frequência",
     xlab = "Idade do indivíduo")


hist(base$menopause, 
     main = "Histograma Atributo menopause ",
     ylab = "Frequência",
     xlab = "Indicação da Menopausa")

hist(base$tumor.size, 
     main = "Histograma Atributo tumor.size",
     ylab = "Frequência",
     xlab = "Diâmetro do tumor")

hist(base$inv.nodes, 
     main = "Histograma Atributo Inv.nodes",
     ylab = "Frequência",
     xlab = "Número de linfonodos axilares")

hist(base$node.caps, 
     main = "Histograma Atributo node.caps",
     ylab = "Frequência",
     xlab = "Penetração do tumor")

hist(base$deg.malig , 
     main = "Histograma Atributo deg.malig",
     ylab = "Frequência",
     xlab = "Grau de malignidade do tumor")

hist(base$breast, 
     main = "Histograma Atributo breast",
     ylab = "Frequência",
     xlab = "Mama em que o câncer pode ocorrer")

hist(base$irradiat, 
     main = "Histograma Atributo irradiat",
     ylab = "Frequência",
     xlab = "Tipo do dado")

hist(base$left_low, 
     main = "Histograma Atributo left_low",
     ylab = "Frequência",
     xlab = "Quadrante da mama afetado")

hist(base$right_up, 
     main = "Histograma Atributo right_up ",
     ylab = "Frequência",
     xlab = "Quadrante da mama afetado")

hist(base$left_up, 
     main = "Histograma Atributo left_up",
     ylab = "Frequência",
     xlab = "Quadrante da mama afetado")

hist(base$right_low, 
     main = "Histograma Atributo right_low",
     ylab = "Frequência",
     xlab = "Quadrante da mama afetado")

hist(base$central, 
     main = "Histograma Atributo central",
     ylab = "Frequência",
     xlab = "Quadrante da mama afetado")

media <- data.frame(
  "Atributos" = c("class","age","menopause","tumor.size",
                  "inv.nodes","node.caps","deg.malig",
                  "breast","left_low","right_up",
                  "left_up","right_low","central","irradiat"),
  "Média" = c(mean(base$class),
              mean(base$age),
              mean(base$menopause),
              mean(base$tumor.size),
              mean(base$inv.nodes),
              mean(base$node.caps),
              mean(base$deg.malig),
              mean(base$breast),
              mean(base$left_low),
              mean(base$right_up),
              mean(base$left_up),
              mean(base$right_low),
              mean(base$central),
              mean(base$irradiat)))

library(gridExtra)

png("media.png", height = 50*nrow(media), width = 200*ncol(media))
grid.table(media)
dev.off()

#6. Exploração dos dados através de medidas de distribuição;

library('e1071')

#Momento
mC <-  moment(base$class)
mA <-  moment(base$age)
mM <-  moment(base$menopause)
mT <-  moment(base$tumor.size)
mI <-  moment(base$inv.nodes)
mN <-  moment(base$node.caps)
mD <-  moment(base$deg.malig)
mB <-  moment(base$breast)
mLl <- moment(base$left_low)
mRr <- moment(base$right_up)
mLu <- moment(base$left_up)
mRl <- moment(base$right_low)
mCc <- moment(base$central)
mIr <- moment(base$irradiat)

#Curtose
cC <-  kurtosis(base$class)
cA <-  kurtosis(base$age)
cM <-  kurtosis(base$menopause)
cT <-  kurtosis(base$tumor.size)
cI <-  kurtosis(base$inv.nodes)
cN <-  kurtosis(base$node.caps)
cD <-  kurtosis(base$deg.malig)
cB <-  kurtosis(base$breast)
cLl <- kurtosis(base$left_low)
cRr <- kurtosis(base$right_up)
cLu <- kurtosis(base$left_up)
cRl <- kurtosis(base$right_low)
cCc <- kurtosis(base$central)
cIr <- kurtosis(base$irradiat)

# obliquidade
oC <-  skewness(base$class)
oA <-  skewness(base$age)
oM <-  skewness(base$menopause)
oT <-  skewness(base$tumor.size)
oI <-  skewness(base$inv.nodes)
oN <-  skewness(base$node.caps)
oD <-  skewness(base$deg.malig)
oB <-  skewness(base$breast)
oLl <- skewness(base$left_low)
oRr <- skewness(base$right_up)
oLu <- skewness(base$left_up)
oRl <- skewness(base$right_low)
oCc <- skewness(base$central)
oIr <- skewness(base$irradiat)

data <- data.frame(
  "Atributos" = c("class","age","menopause","tumor.size",
                  "inv.nodes","node.caps","deg.malig",
                  "breast","left_low","right_up",
                  "left_up","right_low","central","irradiat"),
  "Momento" = c(mC,mA,mM,mT,mI,mN,mD,mB,
                mLl,mRr,mLu,mRl,mCc,mIr),
  "Curtose" = c(cC,cA,cM,cT,cI,cN,cD,cB,
                cLl,cRr,cLu,cRl,cCc,mIr),
  "Obliquidade" = c(oC,oA,oM,oT,oI,oN,oD,oB,
                    oLl,oRr,oLu,oRl,oCc,oIr))

png("distribuição.png", height = 50*nrow(data), width = 200*ncol(data))
grid.table(data)
dev.off()


#7. Identificação e separação do conjunto de teste, que será utilizado 
#para testar o desempenho dos modelos – o conjunto de testes deve ser 
#representativo e ter as características da população completa. 
#Caso sua base de dados já tenha o conjunto de teste definido, analisar
#se este segue as características do conjunto de treinamento;

#Biblioteca para divisão da base
library(caTools)

#Porção da base de dados
set.seed(1)

#Quanto maior for o SplitRatio maior é a base de treinamento
divisao <- sample.split(base$class,SplitRatio = 0.703)

#Criando a base de teste e de treinamento
# 201 dados - 70.3% da base
base_treinamento <- subset(base,divisao == TRUE)

#85 dados - 29.7 % da base
#Criando a variável de teste
base_teste <- subset(base,divisao == FALSE)


#11. Identificação e aplicação de técnicas para minimizar problemas 
#de desbalanceamento (caso não seja necessário, analisar o porquê);


#variável auxiliar, para armazenar os TRUE e FALSE
aux <- base_treinamento[,1]==1

#Base auxiliar para receber apenas os valores TRUE
base_aux <- subset(base_treinamento,aux == TRUE)

#Base de treinamentos recebe a combinção dela mesmo e da base auxiliar
base_treinamento <- rbind(base_aux,base_treinamento)


#12. Limpeza de dados:
#a. Identificação e eliminação de ruídos ou outliers;

# age, tumor.size, inv.nodes
boxplot(base_treinamento, 
        main = "Boxplot com os outliers base Treinamento",
        ylab = "Valores",
        xlab = "Atributos",
        pch = 16)

#Atributo age
summary(base_treinamento$class)

#Q3 - Q1
IQR_age = 1
x_age = mean(base_treinamento$age)
Ls_age = x_age + 1.5*IQR_age
Li_age = x_age - 1.5*IQR_age

table(base_treinamento$age)

base_treinamento$age[base_treinamento$age == 0] <- Li_age
base_treinamento$age[base_treinamento$age == 5] <- Ls_age


#Atributo tumor.size
summary(base_treinamento$tumor.size)

IQR_tumor.size = 6 - 4
x_tumor.size = mean(base_treinamento$tumor.size)
Ls_tumor.size = x_tumor.size + 1.5*IQR_tumor.size
Li_tumor.size = x_tumor.size - 1.5*IQR_tumor.size

table(base_treinamento$tumor.size)

base_treinamento$tumor.size[base_treinamento$tumor.size == 0] <- Li_tumor.size
base_treinamento$tumor.size[base_treinamento$tumor.size == 10] <- Ls_tumor.size

# Atributo inv.nodes
summary(base_treinamento$inv.nodes)

IQR_inv.nodes = 1
x_inv.nodes = mean(base_treinamento$inv.nodes)
Ls_inv.nodes = x_inv.nodes + 1.5*IQR_inv.nodes

table(base_treinamento$inv.nodes)

base_treinamento$inv.nodes[base_treinamento$inv.nodes >= 3] <- Ls_inv.nodes


boxplot(base_treinamento, 
        main = "Boxplot sem os outliers",
        ylab = "Valores",
        xlab = "Atributos",
        pch = 16)

#c. Identificação e eliminação de dados redundantes;

#Verifica colunas duplicadas e cria uma lista
colunas_duplicadas <- duplicated(as.list(base))

print(colunas_duplicadas)

#Mostra o nome das colunas duplicadas
colnames(base[colunas_duplicadas])


#13) B) Normalização dos dados (re-escala ou padronização);

base_treinamento[,2:14] = scale(base_treinamento[,2:14])

base_teste[,2:14] = scale(base_teste[,2:14])


#14. Análise e aplicação de alguma técnica para redução de dimensionalidade – pesquisar
#alguma técnica utilizada na literatura e aplicar;

# 0 - no-recurrence-events
# 1 - recurrence-events

#Aplicar redução
library('caret')

pca = preProcess(x = base_treinamento[-1], method = 'pca', pcaComp = 4)
base_treinamento_pca = predict(pca,base_treinamento)
base_teste_pca = predict(pca,base_teste)


#Testes - Extra


#Teste Com toda a base
classificador = naiveBayes(x = base_treinamento[-1], y = base_treinamento$class)

previsoes = predict(classificador,newdata = base_teste[-1]) 
# 64,7% de acertos
matriz_confusao = table(base_teste[,1],previsoes)
print(matriz_confusao)
#analisa a matriz de confusão e trás várias métricas
confusionMatrix(matriz_confusao)
#Accuracy : 0.6471


#Teste com apenas 4 atributos
classificador_pca = naiveBayes(x = base_treinamento_pca[-1], y = base_treinamento_pca$class)

previsoes_pca = predict(classificador_pca,newdata = base_teste_pca[-1]) 
# 70,6% de acertos
matriz_confusao_pca = table(base_teste_pca[,1],previsoes_pca)
print(matriz_confusao_pca)
#analisa a matriz de confusão e trás várias métricas

confusionMatrix(matriz_confusao_pca)
#Accuracy : 0.7059

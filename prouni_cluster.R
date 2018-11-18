##################
# Código com scripts para replicação dos resultados do seminário 
# Econometria Tradicional vs Machine Learning
# Autor: Pedro Cavalcante Oliveira, Departamento de Economia, UFF
# email: pedrocolrj@gmail.com
# Data da última alteração: 11/18/2018
# obs: ...
##################

##### Carregar bibliotecas e definir seed

set.seed(1010)
library(scales)
library(e1071)
library(tidyverse)

######################
dados = as.tibble(readRDS("prouni_limpo.Rds"))

dados$dummy = ifelse(dados$medicina == "Medicina", 1, 0)

dados$completo = complete.cases(dados)
dados = dados[dados$completo == TRUE,]
dados$completo = NULL

n = 10 # numero de validações

resultadosSVM = list()
resultadosOLS = list()
resultadosPROBIT = list()

for(i in 1:n) {

dados$index = sample(2, nrow(dados), 
                     replace = TRUE,
                     prob = c(0.75, 0.25))

sample = dados[dados$index == 1,]
out.sample = dados[dados$index == 2,]

#SVM

svmfit = svm(medicina ~ mensalidade + nota + vagas,
             data = sample,
             type = "C-classification",
             scale = TRUE,
             kernel = "polynomial")

out.sample$predicaosvm = predict(svmfit, out.sample)

resultadosSVM[[i]] = table(out.sample$medicina, out.sample$predicaosvm)

## modelo linear simples
modelolinear = lm(dummy ~ nota + mensalidade + vagas,
                  data = sample)

out.sample$predicaoOLS = predict(modelolinear, out.sample)
out.sample$predicaoOLS = ifelse(out.sample$predicaoOLS > .5, "Medicina", "Não-Medicina")

resultadosOLS[[i]] = table(out.sample$medicina, out.sample$predicaoOLS)

#modelo probit
modeloprobit = glm(dummy ~ mensalidade + nota + vagas,
                   data = dados, family = binomial(link = "probit"))

out.sample$predicaoPROBIT = predict(modeloprobit, out.sample)
out.sample$predicaoPROBIT = ifelse(out.sample$predicaoPROBIT > .5, "Medicina", "Não-Medicina")

resultadosPROBIT[[i]] = table(out.sample$medicina, out.sample$predicaoPROBIT)

}

plot(svmfit, dados, nota ~ mensalidade)
png(filename = "svmclassplot.png", width = 1280, 
    height = 720, res = 500)

##################

wssplot <- function(data, nc = 15, seed = 1234){
  wss <- (nrow(data)-1)*sum(apply(data, 2, var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type = "b", xlab = "Número de Agrupamentos",
       ylab = "Soma dos Quadrados Intragrupo")}

wssplot(sample) 

##### Pelos criterios anteriores, 3 clusters parece o adequado

analise_kmeans <- kmeans(sample, 
                          centers = 3)

##### Visuailzação e avaliação

table(sample$medicina, analise_kmeans$cluster)

plot(sample, 
     col = analise_kmeans$cluster)

sample %>%
  ggplot(aes(x=mensalidade, y=nota,
             colour = analise_kmeans$cluster, show.legend = FALSE)) +
  geom_point()+
  stat_density_2d()+
  xlab("Mensalidade do curso no ProUni")+
  ylab("Nota de Corte do curso no ProUni") +
  labs(col = "Agrupamento")





normal <- data.frame(apply(sample, 2, scale))
normal$medicina <- sample$medicina

#### Repetimos os procedimentos anteriores

wssplot(finalnormal, 
            nc=6) 

#### Observe que agora 3 parece ser um k melhor

analise_kmeans_normal <- kmeans(finalnormal, 
                                    centers = 3)

table(finalnormal$medicina, 
          analise_kmeans_normal$cluster)

plot(finalnormal, 
        col = analise_kmeans_normal$cluster)

clusplot(sample, analise_kmeans_normal$cluster,
            main='Procurando por 3 agrupamentos no ProUni',
              color=TRUE,
                shade=TRUE,
                  lines=0)



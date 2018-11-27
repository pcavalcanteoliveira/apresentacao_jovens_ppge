## DESCRITIVAS

library(tidyverse)
library(scales)

########
dados = as.tibble(readRDS("prouni_limpo.Rds"))
dados$dummy = ifelse(dados$medicina == "Medicina", 1, 0)
dados$completo = complete.cases(dados)
dados = dados[dados$completo == TRUE,]
dados$completo = NULL

dados$index = sample(2, nrow(dados), 
                     replace = TRUE,
                     prob=c(0.8,0.2))



sample = dados[dados$index == 1,]
out.sample = dados[dados$index == 2,]
table(sample$medicina) # averigua se medicina não ficou fora demais

#### GRAFICOS

dados %>%
  ggplot(aes(x=mensalidade)) + 
  xlim(0,2500) +
  geom_histogram(aes(y=..density..), binwidth = 50, fill = "#f26d6d") +
  xlab("Mensalidade do curso no ProUni") + 
  ylab("") +
  scale_y_continuous(labels = percent) +
  geom_vline(aes(xintercept=mean(mensalidade, na.rm=T)),   
             color="black", linetype="dashed", size=1) 

ggsave("imagem1.png", 
       dpi = 320)

dados %>%
  ggplot(aes(x=mensalidade, fill = medicina)) + 
  xlab("Mensalidade do curso no ProUni") + 
  ylab("") +
  geom_histogram(aes(y=..density..), binwidth = 300) +
  scale_y_continuous(labels = percent) 

ggsave("imagem2.png", 
       dpi = 320)


dados %>%
  ggplot(aes(x=nota)) + 
  xlim(400,800) +
  geom_histogram(aes(y=..density.., fill = "#f26d6d"), 
                 binwidth = 10, show.legend = FALSE) +
  xlab("Nota de Corte de Ampla Concorrência do curso no ProUni") + 
  ylab("") +
  scale_y_continuous(labels = percent) +
  geom_vline(aes(xintercept=mean(nota, na.rm=T)),   
             color="black", linetype="dashed", size=1)

ggsave("imagem3.png", 
       dpi = 320)

dados %>%
  ggplot(aes(x = mensalidade, y = nota))+
             #,colour = medicina, show.legend = FALSE)) +
  geom_jitter()+
  xlab("Mensalidade do curso no ProUni")+
  ylab("Nota de Corte do curso no ProUni")

ggsave("imagem4.png", 
       dpi = 320)

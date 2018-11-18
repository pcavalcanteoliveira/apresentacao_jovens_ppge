## DESCRITIVAS

## CARREGAR OS DADOS ANTES

library(tidyverse)

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

sample %>%
  ggplot(aes(x=mensalidade)) + 
  xlim(0,2500) +
  geom_histogram(aes(y=..density..), binwidth = 50, fill = "red") +
  xlab("Mensalidade do curso no ProUni") + 
  ylab("") +
  geom_density(colour = "blue", size = 1.5) +
  scale_y_continuous(labels = percent) +
  geom_vline(aes(xintercept=mean(mensalidade, na.rm=T)),   
             color="black", linetype="dashed", size=1) 

ggsave(paste(getwd(),"imagem1"), 
       dpi = 750, 
       device = "png")

sample %>%
  ggplot(aes(x=mensalidade, fill = medicina)) + 
  xlab("Mensalidade do curso no ProUni") + 
  ylab("") +
  geom_histogram(aes(y=..density..), binwidth = 300) +
  scale_y_continuous(labels = percent) 

ggsave(paste(getwd(),"imagem2"), 
       dpi = 750, 
       device = "png")


sample %>%
  ggplot(aes(x=nota)) + 
  xlim(400,800) +
  geom_histogram(aes(y=..density..), binwidth = 10) +
  xlab("Nota de Corte de Ampla Concorrência do curso no ProUni") + 
  ylab("") +
  geom_density(colour =" medium blue", size = 1.5) +
  scale_y_continuous(labels = percent) +
  geom_vline(aes(xintercept=mean(nota, na.rm=T)),   
             color="black", linetype="dashed", size=1)

ggsave(paste(getwd(),"imagem3"), 
       dpi = 750, 
       device = "png")


sample %>%
  ggplot(aes(x=nota)) + 
  xlab("Nota de Corte de Ampla Concorrência do curso no ProUni") + 
  ylab("") +
  geom_histogram(aes(y=..density..), binwidth = 10) +
  scale_y_continuous(labels = percent) +
  facet_wrap(~label) +
  geom_density(colour =" medium blue", size = 1)

ggsave(paste(getwd(),"imagem4"), 
       dpi = 750, 
       device = "png")


sample %>%
  ggplot(aes(x = mensalidade, y = nota,
             colour = medicina, show.legend = FALSE)) +
  geom_point()+
  stat_density_2d()+
  xlab("Mensalidade do curso no ProUni")+
  ylab("Nota de Corte do curso no ProUni")

ggsave(paste(getwd(),"imagem5"), 
       dpi = 750, 
       device = "png")

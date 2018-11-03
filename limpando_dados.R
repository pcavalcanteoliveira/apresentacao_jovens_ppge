temp = tempfile()
download.file(url = "https://github.com/pcavalcanteoliveira/apresentacao_jovens_ppge/blob/master/prouni.Rds?raw=true",
              mode = "wb", destfile = temp)
prouni = readRDS(temp)


##### Aqui selecionamos as variáveis de interesse: 
##### Dummy de Medicina
##### Mensalidade
##### Nota de Corte

coercivo <- data.frame(mensalidade = prouni$mensalidade, 
                       medicina = prouni$Medicina, 
                       nota = prouni$nota_integral_ampla,
                       vagas = prouni$total_bolsas,
                       uf = prouni$uf_busca,
                       uni = prouni$universidade_nome,
                       campus = prouni$campus_nome,
                       curso = prouni$nome)
##### Inspecione a base
summary(coercivo)

##### Algoritimos de clustering não lidam bem com NAs
##### Iremos retirar obs que não sejam completas

coercivo$dropador <- complete.cases(coercivo)
final <- coercivo[coercivo$dropador == TRUE,]
final$medicina <- ifelse(final$medicina == 1, "Medicina", "Não-Medicina")
final$medicina = factor(final$medicina)

##### Agora retiramos o vetor residual que indica se a obs é completa
final$dropador <- NULL

summary(final)

saveRDS(final, file = "prouni_limpo.Rds")
##### Análise Exploratória

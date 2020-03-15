# Bibliotecas
library("dplyr", lib.loc="~/R/win-library/3.6")
library("ggplot2", lib.loc="~/R/win-library/3.6")
library("readr", lib.loc="~/R/win-library/3.6")


# Geral
# Construindo o dataframe para trabalhar nas questões
# Comando
dados <- read.csv("~/fpcc2-lab1-nisston.git/trunk/data/dados-fpcc2.csv", encoding="UTF-8")

# Alterando o título das colunas do dataframe para melhor operar
# Comando
colnames(dados) <- c("data", "idade", "sexo", "curso", "area", "interesse", 
                     "progr", "origem", "uf", "irmaos", "altura")



# Questão 01
# Separando os valores (idade e Nível de interesse...) do dataframa
# Criando duas variáveis do tipo vetor com os dados
# Comandos
vl_idade <- dados$idade
vl_nivel <- dados$interesse

# Calculando a Média da Idade e do Nível de interesse...
# colocando em duas variáveis do tipo vetor
# Comandos
md_idade <- mean(vl_idade)
md_nivel <- mean(vl_nivel)

# Calculando o Desvio Padrão da Idade e Nível de interesse...
# colocando em duas variáveis do tipo vetor
# Comandos
dp_idade <- sd(vl_idade)
dp_nivel <- sd(vl_nivel)

# Criando um dataframe com os resultados ordenados
# Comando
dfresultado <- data.frame(Med_idade = md_idade, DesvP_idade = dp_idade, Med_nivel = md_nivel, Desvp_nivel = dp_nivel)

# Exibindo o resultado no relatório
# Comando
dfresultado

# Criando o arquivo com os resultados
# Comando
write.csv(dfresultado, "arquivo1.csv", row.names = FALSE)




# Questão 02
# Encontrando o número de registros por curso
# Comando
dados %>% group_by(curso) %>% summarise(Soma=n())

# Encontrando a média de idade por curso
# Comando
dados %>% group_by(curso) %>% summarise(Media=mean(idade))

# Encontrando a representatividade em % de cada curso
# Comando
dados %>% group_by(curso) %>% summarise(Valor=(n()/29)*100)


# Questão 03
# Encontrando a idade média por curso
# Comando
dados %>% group_by(curso) %>% summarise(Media=mean(idade)) %>% ggplot(aes(x=curso, y=Media, fill = curso)) + geom_bar(stat = "identity") + ggtitle("Gráfico Idade Média por Curso") + geom_text(aes(label=round(Media, digits = 2)), position = position_stack(vjust = 0.5), size=5)

# Gerando o arquivo png
ggsave("figura3.png")

# Exibindo os dados agrupados
dados %>% group_by(curso) %>% summarise(Media=mean(idade))




# Questão 04
# Encontrando o % de alunos por UF e construindo o gráfico
# Comando
dados %>% group_by(uf) %>% summarise(Percentual=(n()/29)) %>% ggplot(aes(uf, Percentual, fill = uf)) + geom_bar(stat = "identity") + ggtitle("Gráfico % de alunos por UF") + geom_text(aes(label=round(Percentual, digits = 3)), position = position_stack(vjust = 0.5), size = 3)

# Gerando o arquivo png
# Comando
ggsave("figura4.png")

# Exibindo os dados agrupados por UF
# Comando
dados %>% group_by(uf) %>% summarise(Percentual=(n()/29))




# Questão 05
# Já possuimos a Média geral da idade, pela variável md_idade
# Comando
md_idade

# Vamos agora criar um dataframe com os valores ordenados
# ele irá ordenar de forma crescente, do menor para o maior
# com isso o nosso Top são os de menor valor e os Bottom os
# de maior valor.
# Comando
idadeorder <- sort(vl_idade)

# Vamos agora calcular a média dos top 5
# os de menor idade, os mais novos
# comando
mdtop5_idade <- mean(idadeorder[1:5])

# Vamos agora calcular a média dos bottom 5
# os de maior idade, os mais velhos
# Comando
mdbot5_idade <- mean(idadeorder[25:29])

# Criando o dataframe com as médias
# Comando
dfq5 <- data.frame(Media_Geral=md_idade,Media_Top5=mdtop5_idade, Media_Bot5=mdbot5_idade)

# Exibindo o dataframe
# Comando
dfq5



# Questão 06
# Vamos criar um dataframe com os valores de idade e nível de interesse..
# lembrando que já temos as variáveis vl_idade e vl_nivel.
# Comando
dfq6 <- data.frame(vl_idade,vl_nivel)

# Vamos agora aplicar a função lm() que serve
# para calcular a regressão linear simples
# Comando
regressao<-lm(vl_idade ~ vl_nivel,data = dfq6)
regressao

# Exibindo os valores da regressao
# Comando
summary(regressao)

# Exibindo o valor do coeficiente de regressão
# Comando
regressao$coefficients

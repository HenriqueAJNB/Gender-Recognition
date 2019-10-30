# Importando os dados
dados <- read.csv("C:/Users/HENRIQUE/Desktop/voice.csv")

# --------------- Análise exploratória ---------------------- #
# Visualizando a tabela de dados
View(dados)

# Verificando o tipo dos dados
str(dados)

# Proporção da variável alvo (label)
table(dados$label)

# Resumo estatístico dos dados
summary(dados)

# Verificando valores NA
any(is.na(dados))

# Plotando histogramas das variáveis numéricas
library(purrr)
library(tidyr)
library(ggplot2)

dados %>%
  keep(is.numeric) %>% 
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# Plotando boxplot das variáveis numéricas
dados %>%
  keep(is.numeric) %>% 
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_boxplot(aes(y = value))

# ---------------- Pré-processamento -------------------- #
# Criando uma função de normalizaçã ormalizando os dados
normalizar <- function(x){
  return ( (x - min(x)) / (max(x) - min(x)) )
}

# Normalizando os dados
dados_norm <- as.data.frame(lapply(dados[,1:20], normalizar))
View(dados_norm)
dados_norm$label <-dados$label

# Separando os dados em treino e teste
set.seed(101)
sample <- sample.int(n = nrow(dados_norm), size = floor(.75*nrow(dados_norm)), replace = F)
View(sample)

dados_treino <- dados_norm[sample, 1:20]
dados_teste <- dados_norm[-sample, 1:20]

dados_treino_label <- dados_norm[sample, 21]
dados_teste_label <- dados_norm[-sample, 21]

View(dados_treino)
View(dados_teste)
View(dados_treino_label)
View(dados_teste_label)

# ------------- Criando o modelo ----------------------
library(class)
modelo_knn_v1 <- knn(train = dados_treino, 
                     test = dados_teste,
                     cl = dados_treino_label,
                     k = 21)

# Performance do modelo criado
summary(modelo_knn_v1)
library(gmodels)
CrossTable(x = dados_teste_label, y = modelo_knn_v1, prop.chisq = FALSE)

# Função para calcular acurácia do modelo
acuracia <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

tabela <- table(modelo_knn_v1, dados_teste_label)
acuracia(tabela)

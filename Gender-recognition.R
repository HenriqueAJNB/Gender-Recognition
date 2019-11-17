# Import dataset
dados <- read.csv("voice.csv")

# --------------- Exploratory analysis ---------------------- #
# Visualizing the dataset
View(dados)

# Check data type
str(dados)

# Target variable ratio (label) (label)
table(dados$label)

# Statistical summary from data
summary(dados)

# Checking for NA Values
any(is.na(dados))

# Plotting histograms of all numeric variables
library(purrr)
library(tidyr)
library(ggplot2)

dados %>%
  keep(is.numeric) %>% 
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# Plotting boxplot of all numeric variables
dados %>%
  keep(is.numeric) %>% 
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_boxplot(aes(y = value))

# ---------------- Pre-processing -------------------- #
# Creating a normalization function
normalizar <- function(x){
  return ( (x - min(x)) / (max(x) - min(x)) )
}

# Normalizing the data
dados_norm <- as.data.frame(lapply(dados[,1:20], normalizar))
View(dados_norm)
dados_norm$label <-dados$label

# Separating data in training and testing
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

# ---------------- Model fitting --------------------- #
# k-NN model
library(class)
modelo_knn_v1 <- knn(train = dados_treino, 
                     test = dados_teste,
                     cl = dados_treino_label,
                     k = 21)

# Created model performance
summary(modelo_knn_v1)

library(gmodels)
CrossTable(x = dados_teste_label, y = modelo_knn_v1, prop.chisq = FALSE)

# Evaluate model accuracy
acuracia <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

tabela <- table(modelo_knn_v1, dados_teste_label)
acuracia(tabela)

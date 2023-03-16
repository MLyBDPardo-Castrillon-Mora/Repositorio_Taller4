# Limpiamos nuestro ambiente
rm(list = ls())

# Cargar pacman (contiene la función p_load)
library(pacman) 

# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(tidyverse, # Manipular dataframes
       tm,   # para Text Mining
       tidytext, #Para tokenización
       wordcloud, # Nube de palabras 
       SentimentAnalysis, #Análisis de sentimientos 
       stringi,
       tokenizers,
       stopwords,
       SnowballC,
       tidyr,
       RColorBrewer, # Paleta de colores
       syuzhet
) 

train<- read.csv ("C:/BigDataLaura/Repositorio_Taller4/train.csv")
test<- read.csv ("C:/BigDataLaura/Repositorio_Taller4/test.csv")
head (train,1)



# DATOS ========================================================================

# Limpieza inicial del texto
train$text <- stri_trans_general(str = train$text, id = "Latin-ASCII")
train$text <- gsub('[^A-Za-z0-9 ]+', ' ', train$text)
train$text <- tolower(train$text)
train$text <- gsub('\\s+', ' ', train$text)
train$text <- gsub('\\d+', ' ', train$text)
train$text <- trimws(train$text)

head (train$text, 1)

# Tokenizacion

tweet <- tokenize_words(train$text)
head(tweet)

stop_1 <- stopwords(language = "es", source = "snowball")
stop_2 <- stopwords(language = "es", source = "nltk")
stop <- union(stop_1, stop_2)

tweet <- data.frame(matrix(tokenize_words(train$text)))
colnames(tweet) <- c("token")
tweet$token <- lapply(tweet$token, function(x) x[!x %in% stop])

# Stem

stem_tokens <- function(tokens) {
  lapply(tokens, wordStem, language = "spanish")
}

tweet <- tweet %>%
  mutate(stemmed_tokens = lapply(tweet$token, stem_tokens))
colnames(tweet) <- c("col1","col2")
tweet$col2 <- lapply(tweet$col2, unlist)

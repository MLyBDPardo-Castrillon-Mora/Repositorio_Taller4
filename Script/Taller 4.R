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
       RColorBrewer, # Paleta de colores
       syuzhet
) 

train<- read.csv ("C:/BigDataLaura/Repositorio_Taller4/train.csv")
test<- read.csv ("C:/BigDataLaura/Repositorio_Taller4/test.csv")
head (train,1)

#*************  Bloque de limpieza de datos *************

train$text <- removeNumbers(train$text)
train$text <- removePunctuation(train$text)
train$text <- tolower(train$text)
train$text <- stripWhitespace(train$text)
train$text <- iconv(train$text, from = "UTF-8", to="ASCII//TRANSLIT")

head (train,1)

#***********  Vamos Tokenizar para poder hacer el análisis exploratorio *************


tweet_text <- as.data.frame(train) %>% unnest_tokens( "word","text")
head(tweet_text)
head (train,1)

tweet_text  %>% 
  count(word, sort = TRUE)   %>% 
  head()

tweet_text

head(stopwords('spanish'))

# Vamos a eliminar estas las stop words con la función anti_join()

tweet_text_clean <- tweet_text  %>% anti_join(tibble(word=stopwords("spanish")))

tweet_text
nrow(tweet_text)
nrow(tweet_text_clean)
tasa<-nrow(tweet_text_clean)/nrow(tweet_text)-1
paste("La tasa de optimización fue de ->", tasa)

wordcloud(tweet_text_clean$word, max.words=200, min.freq = 5, colors=brewer.pal(8,"Paired"))

tweet_text_clean$stem <- stemDocument( tweet_text_clean$word, language="spanish")
tweet_text_clean %>% 
head()

#En R, podemos usar la función unnest_tokens() del paquete tidytext para generar n-grams a partir de un texto.


bigrams <- as.data.frame(tweet_text_clean$word) %>%
  unnest_tokens(bigram, tweet_text_clean$word, token = "ngrams", n = 2)

bigrams

stop_words <- data.frame(word1 = stopwords("es"), 
                         word2 = stopwords("es"))


# Calcular la frecuencia de los bigramas
word_freq <- tweet_text_clean %>%
  count(tweet_text_clean$word, sort = TRUE)







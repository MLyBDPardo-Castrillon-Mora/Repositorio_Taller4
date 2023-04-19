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

train$text <- stri_trans_general(str = train$text, id = "Latin-ASCII")
train$text <- gsub('[^A-Za-z0-9 ]+', ' ', train$text)
train$text <- tolower(train$text)
train$text <- gsub('\\s+', ' ', train$text)
train$text <- gsub('\\d+', ' ', train$text)
train$text <- trimws(train$text)

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

tweet_text_clean %>% group_by(name)%>% summarise(n=n())%>% 
  ungroup() 
  
tweet_text_clean %>% group_by(name)%>% summarise(n=n())%>% 
  ungroup() %>%
  ggplot(aes(x = n)) +
  geom_histogram(fill = "darkblue", alpha = 0.7) +
  theme_bw() +
  labs(x = "Número de tweets por personaje", y = "Cantidad")
  


# Tokenizacion
tweet_original<-train$text
head(tweet_original)
tweet <- tokenize_words(train$text)
head(tweet)

stop_1 <- stopwords(language = "es", source = "snowball")
stop_2 <- stopwords(language = "es", source = "nltk")
stop <- union(stop_1, stop_2)
stop

tweet <- data.frame(matrix(tokenize_words(train$text)))
colnames(tweet) <- c("token")
tweet$token <- lapply(tweet$token, function(x) x[!x %in% stop])
tweet$token[1]
# Stem

stem_tokens <- function(tokens) {
  lapply(tokens, wordStem, language = "spanish")
}

tweet <- tweet %>%
  mutate(stemmed_tokens = lapply(tweet$token, stem_tokens))
colnames(tweet) <- c("col1","col2")
  tweet$col2 <- lapply(tweet$col2, unlist)
  tweet$col2


#wordcloud(tweet$col1, max.words=200, min.freq = 20, colors=brewer.pal(8,"Paired"))

#En R, podemos usar la función unnest_tokens() del paquete tidytext para generar n-grams a partir de un texto.


bigrams_tweet <- as.data.frame(tweet_original) %>%
  unnest_tokens(bigram, tweet_original , token = "ngrams", n = 2)

bigrams_tweet

stop_words <- data.frame(word1 = stop, 
                         word2 = stop)

# Eliminar los bigramas que contengan palabras de parada
bigrams_tweet <- bigrams_tweet %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  anti_join(stop_words, by = "word1") %>%
  anti_join(stop_words, by = "word2") %>%
  unite(bigram, word1, word2, sep = " ")


# Calcular la frecuencia de los bigramas
bigram_freq <- bigrams_tweet %>%
  count(bigram, sort = TRUE)

# Visualizar los bigramas más frecuentes
ggplot(bigram_freq[1:10, ], aes(y = reorder(bigram, -n), x = n)) +
  geom_bar(stat = "identity", fill = "#4e79a7") +
  ggtitle("Bigramas más frecuentes") +
  ylab("Bigramas") +
  xlab("Frecuencia")

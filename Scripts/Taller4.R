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
       udpipe,
       keras,
       tensorflow,
       RColorBrewer, # Paleta de colores
       syuzhet
) 

train_true <- read.csv ("C:/BigDataLaura/Repositorio_Taller4/train.csv")
test <- read.csv ("C:/BigDataLaura/Repositorio_Taller4/test.csv")
head (train,1)

# 9349 observaciones en train, 1500 en tes

# DATOS ========================================================================

# Set seed
set.seed(31416)

aux <- test
aux$name <- rep(NA, nrow(aux))
aux <- aux %>% select(id, name, text)
train <- rbind(train_true, aux)

# Limpieza inicial del texto
train$text <- stri_trans_general(str = train$text, id = "Latin-ASCII")
train$text <- gsub('[^A-Za-z0-9 ]+', ' ', train$text)
train$text <- tolower(train$text)
train$text <- gsub('\\s+', ' ', train$text)
train$text <- gsub('\\d+', ' ', train$text)
train$text <- trimws(train$text)

head (train$text, 1)

# Arreglo de base de datos

train <- train %>%
  mutate(rnum = row_number())

train <- select(train, id, rnum, name, text)

words <- train %>%
  unnest_tokens(output = "word", input = "text")

# Stopwords

sw <- c()
for (s in c("snowball", "stopwords-iso", "nltk")) {
  temp <- get_stopwords("spanish", source = s)$word
  sw <- c(sw, temp)
}
sw <- unique(sw)
sw <- unique(stri_trans_general(str = sw, id = "Latin-ASCII"))
sw <- data.frame(word = sw)

words <- words %>%
  anti_join(sw, by = "word")

# Lematizar

udmodel <- udpipe_load_model(file = "spanish-gsd-ud-2.5-191206.udpipe")
palabras_unicas <- words %>% distinct(word)
udpipe_results <- udpipe_annotate(udmodel, x = palabras_unicas$word)
udpipe_results <- as_tibble(udpipe_results)
udpipe_results <- udpipe_results %>% 
  select(token, lemma) %>%
  rename("word" = "token")
words <- words %>%
  left_join(udpipe_results, by = "word", multiple = "all")
words[is.na(words$lemma), "lemma"] <- words[is.na(words$lemma), "word"]

words %>%
  count(lemma) %>%
  arrange(desc(n)) %>%
  tail(100)

palabras_eliminar <- words %>%
  count(lemma) %>%
  filter(n < 10)

words <- words %>%
  anti_join(palabras_eliminar, by = "lemma")

# Reagrupar a base original

data_clean <- words %>%
  group_by(id, rnum, name) %>% 
  summarise(text = str_c(lemma, collapse = " ")) %>%
  ungroup()

data_clean <- arrange(data_clean, rnum)

drows <- setdiff(train$rnum, data_clean$rnum)
d_id <- setdiff(train$id, data_clean$id)
train <- train[-which(train$rnum %in% drows), ]

# Corpus de texto

tm_corpus <- Corpus(VectorSource(x = data_clean$text))
tf_idf <- TermDocumentMatrix(tm_corpus,
                             control = list(weighting = weightTfIdf))

# TF-IDF

tf_idf <- as.matrix(tf_idf) %>%
  t() %>%
  as.data.frame()

dim(tf_idf)

# Reducir dimension

columnas_seleccionadas <- colSums(tf_idf) %>%
  data.frame() %>%
  arrange(desc(.)) %>%
  head(500) %>%
  rownames()

tf_redu <- tf_idf %>%
  select(all_of(columnas_seleccionadas))

# Split

split_row <- 9200
X_train <- tf_redu[1:split_row, ]
X_test <- tf_redu[(split_row + 1):nrow(tf_redu), ]

Y <- train[1:split_row, ]
Y <- Y$name

Y <- ifelse(Y=="Petro", 0, Y)
Y <- ifelse(Y=="Lopez", 1, Y)
Y <- ifelse(Y=="Uribe", 2, Y)
Y <- to_categorical(Y)
Y <- as.matrix(Y)

save(X_train, X_test, Y, file = "modelo.RData")

# EXPORT =======================================================================

drows <- c(drows)
drows_test <- drows[drows > 9349]

test_import <- data_clean[data_clean$rnum > 9349, ]
random <- train[train$rnum > 9349, ]
test_random <- random[random$rnum %in% drows_test, ]
test_random$name <- ifelse(is.na(test_random$name), sample(0:2, sum(is.na(test_random$name)), replace = TRUE), 0)

# Traer datos de Collab

load("import2.RData")
test_import$name <- import
export <- rbind(test_import, test_random)
ordenar <- order(export$rnum)
export <- export[ordenar, ]
export <- select(export, id, name)
export$name <- ifelse(export$name==0, "Petro", export$name)
export$name <- ifelse(export$name==1, "Lopez", export$name)
export$name <- ifelse(export$name==2, "Uribe", export$name)
write.csv(export, "prueba.csv", row.names=FALSE)
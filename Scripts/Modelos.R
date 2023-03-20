library(keras)

#IMPORTE DE INFORMACION==============================================
set.seed(31416)
load("modelo.RData")

# X de entrenamiento
X_test <- as.matrix(X_test)
X_train <- as.matrix(X_train)
head(X_train)

#
dim(X_train)

# Variable dependiente -> target
head(Y)
dim(Y)
n_h = nrow(X_train)/(2*(ncol(X_train)+3))
n_h

# MODELO 1 ENTRENAMIENTO ==========================
model.1 <- keras_model_sequential() %>%
  layer_dense(units = 6, activation = "relu", input_shape = ncol(X_train) ) %>%
  layer_dense(units = 3, activation = "softmax")
summary(model.1)

# Definir Compilación
model.1 %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  metrics = c("accuracy")
)
# Entrenamiento
history.1 <- model.1 %>% fit(
  x = X_train, 
  y = Y,
  epochs = 25, batch_size = 64,
  validation_split = 0.1
)
# Evaluación
model.1 %>% evaluate(X_train, Y)

# PREDICCION ===========================================
import_names <- model.1  %>% predict(X_test) %>% k_argmax()


#MODELO 2 ================================
model.2 <- keras_model_sequential() 

model.2 %>%
  layer_dense(units = 3, activation = 'relu', input_shape = ncol(X_train) ) %>% 
  layer_dense(units = 2, activation = 'relu') %>%
  layer_dense(units = 3, activation = 'softmax')
summary(model.2)

# Definir Compilación
model.2 %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  metrics = c("accuracy")
)
# Entrenamiento
history.2 <- model.2 %>% fit(
  X_train, Y, 
  epochs = 30, 
  batch_size = 80,
  validation_split = 0.1
)
# Evaluación
model.2 %>% evaluate(X_train, Y)

# MODELO 3==========================================
model.3 <- keras_model_sequential() 
model.3 %>%
  layer_dense(units = 9, activation = 'relu', input_shape = ncol(X_train)) %>% 
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 6, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 3, activation = 'softmax')
summary(model.3)

model.3 %>% compile(
  optimizer = 'adam',
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

history.3 <- model.3 %>% fit(
  X_train, Y, 
  epochs = 30, 
  batch_size = 128,
  validation_split = 0.2
)

model.3 %>% evaluate(X_train, Y)

# MODELO 4 ===========================================
model.4 <- keras_model_sequential() 
model.4 %>%
  layer_dense(units = 27, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 9, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 3, activation = 'softmax')

model.4 %>% compile(
  optimizer = 'adam',
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

history.4 <- model.4 %>% fit(
  X_train, Y, 
  epochs = 30, 
  batch_size = 128,
  validation_split = 0.2
)

model.4 %>% evaluate(X_train, Y)

# Predicción

library(reticulate)
import_names <- model.4  %>% predict(X_test) %>% k_argmax()
my_array <- py_call(import_names$numpy)
import <- array(as.numeric(my_array))
save(import, file = "import_m4.RData")

model.1 %>% evaluate(X_train, Y)
model.2 %>% evaluate(X_train, Y) 
model.3 %>% evaluate(X_train, Y) 
model.4 %>% evaluate(X_train, Y) 




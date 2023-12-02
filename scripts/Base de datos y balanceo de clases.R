### modelo de predicción: ridge  ##### 

#Limpieza area de trabajo 
rm(list=ls())
cat('\014')

# cargar paquetes 
install.packages("pacman")
library(pacman)
# cargar librerias 
p_load(tidyverse, # Manipular dataframes
       rio, # Importar datos fácilmente
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       rgeos, # Calcular centroides de un polígono
       units, # unidades
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Obtener datos de OpenStreetMap (OSM)
       tidymodels, # Modelado de datos limpios y ordenados
       randomForest, # Modelos de bosque aleatorio
       rattle, # Interfaz gráfica para el modelado de datos
       spatialsample, # Muestreo espacial para modelos de aprendizaje automático
       xgboost,
       scals,
       purr,
       glmnet,
       keras, 
       themis,
       yardstick,
       tensorflow) 

# cargar base de datos 
bd <- read.csv("https://media.githubusercontent.com/media/sebastian23e/predicting_poverty_bdmc/main/stores/data_h.csv")


#Convertimos como factor a las variables correspondientes
bd<-bd %>% 
  mutate(Clase     = as.factor(Clase),
         tipo_vivienda= as.factor(tipo_vivienda),
         pobre        = as.factor(pobre),
         Dominio   = as.factor(Dominio),
         tipo_vivienda=as.factor(tipo_vivienda))

table(bd$pobre)/nrow(bd)

#Ya que la variable pobre en la categoria 1 solo tiene el 14.2% de los datos, hay un problema de desbalanceo MODERADO

#Creación de subsets de entrenamiento y prueba
train <-  bd %>%
  subset(sample == "train")

# crear subset de testeo
test <- bd %>%
  subset(sample == "test")

# Crear subset de validación con train

set.seed(123)
train_splitting<- initial_split(train, prop=0.8)
train<- training(train_splitting)
valid<- testing(train_splitting)

# Separar las variables predictoras (regresores) y la variable objetivo
x_train <- train %>% select(- c(id, pobre, l_indigencia, l_pobreza, sample))
y_train <-train %>% pull(pobre)
x_val <- valid %>% select(- c(id, pobre, l_indigencia, l_pobreza, sample))
y_val <- valid %>% pull(pobre)
x_test <- test %>% select(- c(id, pobre, l_indigencia, l_pobreza, sample))
y_test <-test %>% pull(pobre)


#Normalización de las variables numéricas
receta <- recipe(~., data = x_train) %>%
  step_normalize(all_numeric())
# Aplicar el preprocesamiento para normalizar los datos
x_test<- as.matrix(prep(receta) %>% bake(new_data = x_test))
x_train <- as.matrix(prep(receta) %>% bake(new_data = x_train))
x_val <- as.matrix(prep(receta) %>% bake(new_data = x_val))


#MODELO CON REDES NEURONALES

#Definición de las métricas
METRICS <- list(
  metric_set(
  metric_binary_accuracy(name = 'accuracy'),
  metric_precision(name = 'precision'),
  metric_recall(name = 'recall'),
  metric_auc(name = 'auc')
))


#Realizaremos oversampling (over ratio<1)
over_receta<-recipe(pobre~., data=train)%>%
  step_rm(id,l_indigencia, l_pobreza, sample)%>%
  step_normalize(all_numeric_predictors())%>%
  step_upsample(pobre, over_ratio=.8)
#Guardamosel proceso de la receta sobre los datos
  over_train<- prep(over_receta)%>% bake(new_data=NULL)

  #Nueva distribución de clases
  table(over_train$pobre)/nrow(over_train)
  
##A continuación se hará el modelo de redes neuronales
    # Reordenar
  over_train <- over_train %>%
    sample_n(nrow(over_train))
  # convertir a formato de matriz y vetcor
  x_train_over <- as.matrix(over_train %>% select(- c(pobre)))
  y_train_over <- over_train %>% pull(pobre)

##Corremos el modelo con los nuevos datos de enrenamiento
  model_oversampling <- keras_model_sequential() %>%
    layer_dense(units = 16, activation = 'relu',
                input_shape = dim(x_train)[2],
                kernel_initializer = initializer_random_uniform()) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1, activation = 'sigmoid')
  
  model_oversampling %>% compile(
    optimizer = optimizer_adam(learning_rate = 1e-3),
    loss = 'binary_crossentropy',
    metrics = METRICS
  )
  model_oversampling %>% fit(
    x = x_train_over,
    y = y_train_over,
    batch_size = 32,
    epochs = 20,
    validation_data = list(x_val, y_val),
    verbose = 0,
    seed = 12
  )
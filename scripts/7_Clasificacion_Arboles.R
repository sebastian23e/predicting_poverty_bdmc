
### modelo de clasificación: Random Forest

#Limpieza area de trabajo 
rm(list=ls())
cat('\014')

# cargar paquetes 
require("pacman")
# cargar librerias 
p_load(tidyverse, # Manipular dataframes
       rio, # Importar datos fácilmente
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       units, # unidades
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Obtener datos de OpenStreetMap (OSM)
       tidymodels, # Modelado de datos limpios y ordenados
       randomForest, # Modelos de bosque aleatorio
       rattle, # Interfaz gráfica para el modelado de datos
       spatialsample, # Muestreo espacial para modelos de aprendizaje automático
       xgboost,
       purrr,
       glmnet, 
       keras,
       themis,
       yardstick,
       tensorflow) 

#Creación de directorios
templates <- paste0(getwd(),'/templates/') # Directorio para crear templates

# cargar base de datos 
load(paste0(getwd(),'/stores/','base_completa.RData'))
bd<-base.completa

#Creación de subsets de entrenamiento y prueba
train <-  bd %>%
  subset(sample == "train")
train <- train%>%
  select(-c(id,l_indigencia, l_pobreza, sample, ingtotug))

# crear subset de testeo
test <- bd %>%
  subset(sample == "test")


##Crear un recipe para ambos modelos HACIENDO OVERSAMPLING

receta<-recipe(pobre~., data=train)%>%
  step_normalize(all_numeric_predictors()) %>% # normalizamos las variables categóricas
  step_upsample(pobre, over_ratio=0.8)

# Reescalamos y términamos de hacerel procedimiento de oversampling 
over_train<- prep(receta)%>% bake(new_data=NULL)


#Convertir a formato de matriz y vector que incluya 
x_train_over<- as.matrix(over_train%>%select(-c(pobre)))
y_train_over<- over_train%>%pull(pobre)

# Tuning ------------------------------------------------------------------
tune_grid_tree <- grid_regular(
  tree_depth(range = c(1,20)),
  min_n(range = c(1,20)),
  levels = 100
)

# Modelo arboles -----------------------------------------------------------
tree_spec <- decision_tree(
  tree_depth = tune(),
  min_n = tune()) %>%
  set_mode("classification")

# Workflow ----------------------------------------------------------------
workflow_1.1 <- workflow() %>%
  add_recipe(receta) %>%
  add_model(tree_spec)

df_fold <- vfold_cv(over_train, v = 5)

tune_tree <- tune_grid(
  workflow_1.1,
  resamples = df_fold, 
  grid = tune_grid_tree
)

# Mejor hiperparametro ----------------------------------------------------
# Obtener el mejor hiper-parámetro
best_params <- tune_tree %>% select_best(metric = "accuracy")
best_params




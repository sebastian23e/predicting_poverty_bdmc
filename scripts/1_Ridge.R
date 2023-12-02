
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
bd <- base.completa %>% 
  select(-c(id,l_indigencia, l_pobreza, pobre))

#Creación de subsets de entrenamiento y prueba
train <-  bd %>%
  subset(sample == "train") %>% 
  select(-sample)

# crear subset de testeo
test <- bd %>%
  subset(sample == "test") %>% 
  select(-sample)

#Especificación del modelo
ridge<- linear_reg(penalty = tune(), mixture = 0) %>%
  set_mode("regression") %>%
  set_engine("glmnet") 

# definir intervalo de parametros

penalty_grid <- grid_regular(penalty(), levels = 100)

#receta de preprocesamiento
receta <- recipe(formula = ingtotug ~ ., data = train) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# crear flujo de trabajo 

ridge_workflow <- workflow() %>%
  add_recipe(receta) %>%
  add_model(ridge)

# Busqueda hiperparametros ------------------------------------------------
# Cross Validation
df_fold <- vfold_cv(train, v = 5)

tune_res <- tune_grid(
  ridge_workflow,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = df_fold,  # Folds de validación cruzada
  grid = penalty_grid,        # Grilla de valores de penalización
  metrics = metric_set(rmse)
)
tune_res

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
       glmnet) 

# cargar base de datos 
bd <- read.csv("https://media.githubusercontent.com/media/sebastian23e/predicting_poverty_bdmc/main/stores/data_h.csv")

#Creación de subsets de entrenamiento y prueba
train <-  bd %>%
  subset(sample == "train")

# crear subset de testeo
test <- bd %>%
  subset(sample == "test")

#Especificación del modelo
ridge<- linear_reg(penalty = tune(), mixture = 0) %>%
  set_mode("regression") %>%
  set_engine("glmnet") 

# definir intervalo de parametros

penalty_grid <- grid_regular(penalty(range = c(-4, 2)), levels = 30)

#receta de preprocesamiento
receta <- recipe(formula = ingtotug ~ cuartos+dormitorio+tipo_vivienda+nper_ugasto+nper, data = bd) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# crear flujo de trabajo 

ridge_workflow <- workflow() %>%
  add_recipe(receta) %>%
  add_model(ridge)

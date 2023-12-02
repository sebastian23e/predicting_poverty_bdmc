
### modelo de prediccion: Ridge

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
bd <- base.completa 

#Creación de subsets de entrenamiento y prueba
train <-  bd %>%
  subset(sample == "train") %>% 
  select(-c(id,l_indigencia, l_pobreza, pobre))

# crear subset de testeo
test <- bd %>%
  subset(sample == "test") 

#Especificación del modelo
lasso<- linear_reg(penalty = tune(), mixture = 1) %>%
  set_mode("regression") %>%
  set_engine("glmnet") 

# definir intervalo de parametros

penalty_grid <- grid_regular(penalty(range = c(-2, 2)), levels = 30)
#receta de preprocesamiento
receta <- recipe(formula = ingtotug ~ ., data = train) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# crear flujo de trabajo 

lasso_workflow <- workflow() %>%
  add_recipe(receta) %>%
  add_model(lasso)

# Busqueda hiperparametros ------------------------------------------------
# Cross Validation
set.seed(123)
df_fold <- vfold_cv(train, v = 5)

tune_res <- tune_grid(
  ridge_workflow,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = df_fold,  # Folds de validación cruzada
  grid = penalty_grid,        # Grilla de valores de penalización
  metrics = metric_set(rmse)
)
tune_res

# Mejor penalidad
best_penalty <- select_best(tune_res, metric = "rmse")
best_penalty

# Finalizar el flujo de trabajo 'ridge_workflow' con el mejor valor de penalización
lasso_final <- finalize_workflow(lasso_workflow, best_penalty)

# Ajustar el modelo de regresión ridge utilizando los datos de entrenamiento
lasso_final_fit <- fit(lasso_final, data = train)

# Aumentar los datos 
augment(lasso_final_fit, new_data = base.completa) %>% 
  mae(truth = ingtotug, estimate = .pred)

# Predecir ingreso para la base test
test <- test %>% 
  mutate(ingtotug = predict(lasso_final_fit, new_data=test)$.pred)

# Colocar pobreza 1 si ingtotug esta debajo de linea pobreza
test.comprobar <- base.completa %>% 
  dplyr::filter(sample == 'test') %>% 
  select(id, l_pobreza) 

base.para.pobreza <- full_join(test, test.comprobar) %>% 
  mutate(pobre = ifelse(ingtotug < l_pobreza, 1, 0)) %>% 
  select(id, pobre)

# Template de Kaggle
write.csv(base.para.pobreza, file= paste0(templates,'02_lasso_penalty0.01.csv'),
          row.names = F)

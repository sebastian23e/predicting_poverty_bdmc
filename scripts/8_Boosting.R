
### modelo de prediccion: Boosting

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

# crear subset de entrenamiento
train <-  bd %>%
  subset(sample == 'train')

# crear subset de testeo
test <- bd %>%
  subset(sample == 'test')

# Especificacion del modelo -----------------------------------------------
boost_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune()
) %>%
  set_mode("regression")  

# Tune grid regular para el modelo de boost
boost_grid <- grid_regular(
  trees(range = c(95, 120)),
  tree_depth(range = c(1,1)), # Siempre nos daba el mejor tree_depth 1
  learn_rate(range = c(0.001, 0.008)),
  levels = 40
) # Grilla de 1000 observaciones

# Receta  -----------------------------------------------------------------
# Para tener la receta primero necesitamos una formula adecuada
variables.exogenas  <-  c('Clase','Dominio','cuartos','dormitorio','tipo_vivienda','nper','nivel.educacion',
                          'pension','subsidio.alimenticio','subsidio.transporte','subsidio.familiar',
                          'subsidio.educativo','proporcion.menores.edad','numero.tercera.edad') 

formula.boosting       <- as.formula(paste('ingtotug', paste(c(variables.exogenas),collapse ='+'),
                                           sep='~'))

bd.seleccion <- bd %>% 
  select(-c(setdiff(colnames(bd),c(variables.exogenas)))) %>% 
  mutate(ingtotug = bd$ingtotug)

# Ya podemos añadir las variables necesarias para la estimacion
receta <- recipe(formula = ingtotug ~ ., data = bd.seleccion) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# Workflow ----------------------------------------------------------------
workflow <- workflow() %>%
  add_recipe(receta) %>%
  add_model(boost_spec)

# Busqueda hiperparametros CV  ------------------------------------
set.seed(123)
block_folds <- vfold_cv(train, v = 10)

# estimar el modelo -
tune_boost <- tune_grid(
  workflow, # specifica un modelo de randomn forest
  resamples = block_folds, # Usa los pliegues de validación cruzada definidos previamente en block_folds
  grid = boost_grid, # Especifica una grilla de parámetros a probar en la afinación
  metrics = metric_set(mae) # Usa el Error Absoluto Medio (MAE) como métrica para evaluar los modelos
)

# visualizacion los resultados de la busqueda de hiperparametros 
autoplot(tune_boost)

# Mejores estimaciones de parametros --------------------------------------
best_parms_boost <- select_best(tune_boost, metric = "mae")
best_parms_boost

# crear subset de entrenamiento
train <-  bd %>%
  subset(sample == 'train') %>% 
  select(c('sample',colnames(bd.seleccion)))

# crear subset de testeo
test <- bd %>%
  subset(sample == 'test') %>% 
  select(c('sample','id',colnames(bd.seleccion)))

# Actualizar parametros con finalize_workflow() 
boost_final <- finalize_workflow(workflow, best_parms_boost)

# Ajustar el modelo con los datos de entrenamiento
boost_final_fit <- fit(boost_final, data = train)

# predecimos el precio para los datos de test 
test <- test %>%
  mutate(ingtotug = predict(boost_final_fit, new_data = test)$.pred)

collect_metrics(tune_boost)
augment(boost_final_fit, new_data = bd) %>%
  mae(truth = ingtotug, estimate = .pred)

# Crear variable pobre ----------------------------------------------------
linea.pobreza.test <- base.completa %>% 
  filter(sample == 'test') %>% 
  select(id, l_pobreza)

base.para.pobre <- full_join(test, linea.pobreza.test)

template.kaggle <- base.para.pobre %>% 
  mutate(pobre = ifelse(ingtotug < l_pobreza, 1, 0)) %>% 
  select(id, pobre)

# Exportar a CSV en la carpeta de templates
write.csv(template.kaggle, 
          file= paste0(templates,'boosting_trees',round(best_parms_boost$trees,4),'_depth',round(best_parms_boost$tree_depth,4),
                       '_learnrate',round(best_parms_boost$learn_rate,4),'.csv'),
          row.names = F)


### modelo de predicción: Arboles de decisión ##### 

## limpieza y transformación de datos ##

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
       spatialsample,
       xgboost,
       scals,
       purr) # Muestreo espacial para modelos de aprendizaje automático

#Creación de directorios
templates <- paste0(getwd(),'/templates/') # Directorio para crear templates

# cargar base de datos 
load(paste0(getwd(),'/stores/','base_completa.RData'))
bd <- base.completa 

#Creación de subsets de entrenamiento y prueba
train <-  bd %>%
  subset(sample == "train") %>% 
  select(-c(id,l_indigencia, l_pobreza,pobre))

# crear subset de testeo
test <- bd %>%
  subset(sample == "test") 


### modelo de arboles de regresión ###

# especificación del modelo 
tree_model <- decision_tree(tree_depth = tune(),
                            min_n      = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("regression") 

# crear grilla de parametros 

tree_grid <- grid_random(
  tree_depth(range = c(1, 20), trans= NULL),# modificar para cada modelo 
  min_n(range = c(20,100 ), trans = NULL),# modificar para cada modelo 
  size = 15
)
head(tree_grid, n=20)
dim(tree_grid)

# especificar receta 

receta <- recipe(ingtotug ~ ., data = train) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) 

# crear flujo de trabajo

workflow <- workflow() %>%
  add_recipe(receta) %>%
  add_model(tree_model)

# validación cruzada 
# Crear pliegues para la validación cruzada

df_fold <- vfold_cv(train, v = 5)

# estimar el modelo - long time ....

tune_tree <- tune_grid(
  workflow, # specifica un modelo de árbol de decisión
  resamples = df_fold, # Usa los pliegues de validación cruzada definidos previamente en block_folds
  grid = tree_grid, # Especifica una grilla de parámetros a probar en la afinación
  metrics = metric_set(mae) # Usa el Error Absoluto Medio (MAE) como métrica para evaluar los modelos
)

# visualizacion los resultados de la busqueda de hiperparametros 
autoplot(tune_tree)

# seleccionar las mejores estimaciones de parametros

best_parms_tree <- select_best(tune_tree, metric = "mae")
best_parms_tree

# actulizar parametros con finalize_workflow() 

tree_final <- finalize_workflow(workflow, best_parms_tree)

#ajustar el modelo con los datos de test
tree_final_fit <- fit(tree_final, data = train)

# predecimos el precio para los datos de test 

test <- test %>%
  mutate( ingtotug= predict(tree_final_fit, new_data = test)$.pred)


# Colocar pobreza 1 si ingtotug esta debajo de linea pobreza
test.comprobar <- base.completa %>% 
  dplyr::filter(sample == 'test') %>% 
  select(id, l_pobreza) 

base.para.pobreza <- full_join(test, test.comprobar) %>% 
  mutate(pobre = ifelse(ingtotug < l_pobreza, 1, 0)) %>% 
  select(id, pobre)

# Template de Kaggle
write.csv(base.para.pobreza, file= paste0(templates,'09.arbol de decision prediccion_td15_minn85.csv'),
          row.names = F)


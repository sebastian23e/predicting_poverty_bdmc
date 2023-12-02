
### modelo de predicción: Logit y Probit

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
bd <- load(paste0(getwd(),'/stores/','base_completa.RData'))
#Creación de subsets de entrenamiento y prueba
train <-  bd %>%
  subset(sample == "train")

# crear subset de testeo
test <- bd %>%
  subset(sample == "test")


##Crear un recipe para ambos modelos HACIENDO OVERSAMPLING
receta<-recipe(pobre~., data=train)%>%
  step_center(all_predictors()) %>% # Centramos todas las variables
  step_scale(all_predictors())%>%
  step_upsample(pobre, over_ratio=0.8)

# Reescalamos y términamos de hacerel procedimiento de oversampling 
over_train<- prep(receta)%>% bake(new_data=NULL)
over_train<-over_train%>%
  sample_n(nrom(over_train))

#Convertir a formato de matriz y vector que incluya 
x_train_over<- as.matrix(over_tarin%>%select(variables_exogenas))
y_train_over<- over_train%>%pull(pobre)

#-------------------Modelo Logit----------------------
#Creamos la específicación del modelo, en este caso probit
logit <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

#Se crea el flujo de trabajo
workflow_logit <- workflow() %>%
  add_recipe(receta) %>%
  add_model(logit)

#Entrenamos el modelo con observaciones train
modelo_logit <- workflow_logit %>%
  fit(data = train)

#Tomamos las predicciones del conjunto de prueba
test_data <- test_data %>%
  mutate(predicciones_logit = predict(modelo_logit, test_data)$.pred_class)

#Observemos la matriz de confusión
matriz_logit <- conf_mat(test_data, truth = ciudad, estimate = predicciones_logit)
print(matriz_logit)




##---------------PROBIT-----------------------

#Creamos la específicación del modelo, en este caso probit
probit <- logistic_reg() %>% 
  set_engine("glm", family = stats::binomial(link = "probit")) %>%
  set_mode("classification") %>% 
  translate()

#Se crea el flujo de trabajo
workflow_probit <- workflow() %>%
  add_recipe(receta) %>%
  add_model(probit)

#Entrenamos el modelo de probit con la data de entrenamiento
modelo_probit <- fit(workflow_probit, data = train)

#Vemos las predicciones con el conjunto de prueba
test <- test %>%
  mutate(predicciones_probit = predict(modelo_probit, test)$.pred_class)


#Observamos la matriz de cinfución para el modelo
matriz_probit <- conf_mat(test, truth = pobre, estimate = predicciones_probit)
print(matriz_probit)



#-----Conclusiones------------
tidy(modelo_lineal)
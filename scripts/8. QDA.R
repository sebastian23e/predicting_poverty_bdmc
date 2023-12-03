#Modelo de árbol para clasificación

# Cargar pacman (contiene la función p_load)
library(pacman) 
# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(tidyverse, # Manipular dataframe
       tidymodels, # ML modelos
       yardstick, # Matriz de Confusión
       GGally,  #extensiones de ggplot2
       rattle, # Grafico de arboles de desicion
       randomForest,  # Para random fores
       C50, naivebayes, # modelo bayes
       discrim, # modelos lda y qda
       kknn) # Graficas
#Creación de directorios
templates <- paste0(getwd(),'/templates/') # Directorio para crear templates

# cargar base de datos 
load(paste0(getwd(),'/stores/','base_completa.RData'))
bd <- base.completa

bd <- base.completa %>%
  select(-c(individuos.hogar))%>%
  mutate(dormitorio =as.numeric(dormitorio),
nper=as.numeric(nper),
proporcion.menores.edad=as.numeric(proporcion.menores.edad),
cuartos= as.numeric(cuartos),
nper_ugasto=as.numeric(nper_ugasto),
ingtotug=as.numeric(ingtotug),
nivel.educacion=as.numeric(nivel.educacion),
numero.tercera.edad=as.numeric(numero.tercera.edad), 
Clase     = as.factor(Clase),
tipo_vivienda= as.factor(tipo_vivienda),
pobre        = as.factor(pobre),
Dominio   = as.factor(Dominio),
tipo_vivienda=as.factor(tipo_vivienda))


#Creación de subsets de entrenamiento y prueba
train <-  bd %>%
  subset(sample == "train") #%>% 
#select(-c(id,l_indigencia, l_pobreza, pobre))

# crear subset de testeo
test <- bd %>%
  subset(sample == "test") 

train_data<-train%>%
  select(-c(id,l_indigencia, l_pobreza))

rec <- recipe(pobre ~ ., data = train_data) %>% 
  step_normalize(all_numeric_predictors())

# Crear modelo
modelo_qda <- discrim_quad() %>% 
  set_engine("MASS")%>% 
  set_mode("classification")

# Crear workflow 
workflow_qda <- workflow() %>%
  add_recipe(rec) %>%
  add_model(modelo_qda)
# Entrenar
qda_fit <- workflow_qda %>% 
  fit(data = train_data)

test_data <- test %>%
  mutate(predicciones_qda  = predict(qda_fit, test)$.pred_class)

# Predicciones con probabilidades para QDA
test_data <- test_data %>%
  mutate(probas_qda  = predict(qda_fit, test_data,  type = "prob")$`.pred_NY`)

autoplot(roc_curve(data = test_data, truth = ciudad, probas_qda))



# Verificar qué variables son factores
factores <- sapply(bd, is.numeric)
# Imprimir las variables que son factores
print(names(factores[factores]))

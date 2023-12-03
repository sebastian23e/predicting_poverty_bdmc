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
  mutate(Clase = as.factor(Clase),
         tipo_vivienda = as.factor(tipo_vivienda),
         pobre         = as.factor(pobre),
         Dominio       = as.factor(Dominio),
         trabajo.formal          = as.factor(trabajo.formal), 
         pension                 = as.factor(pension), 
         subsidio.alimenticio    = as.factor(subsidio.alimenticio),
         subsidio.transporte     = as.factor(subsidio.transporte),
         subsidio.familiar       = as.factor(subsidio.familiar),
         subsidio.educativo      = as.factor(subsidio.educativo), 
         numero.menores.edad     = as.factor(numero.menores.edad),
         dormitorio =as.factor(dormitorio),
         nper=as.numeric(nper),
         proporcion.menores.edad=as.numeric(proporcion.menores.edad),
         cuartos= as.factor(cuartos),
         nper_ugasto=as.numeric(nper_ugasto),
         ingtotug=as.numeric(ingtotug),
         nivel.educacion=as.factor(nivel.educacion),
         numero.tercera.edad=as.factor(numero.tercera.edad))
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
naive_model <- naive_Bayes() %>% 
  set_engine("naivebayes") %>% 
  set_mode("classification")

# Crear workflow
workflow_nb <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(naive_model)

# Entrenar modelo
nb_fit <- workflow_nb %>% 
  fit(data = train_data)
  
  # Predicciones con probabilidades
test_data <- test%>%
  mutate(predicciones_bayes = predict(nb_fit, test)$.pred_class)

test_data<-test_data%>%
  mutate(pobre=predicciones_bayes)
template.kagle<-test_data %>% 
  select(id, pobre) 

write.csv(template.kagle, file= paste0(templates,'05_Naive_bayes.csv'),
          row.names = F)

  
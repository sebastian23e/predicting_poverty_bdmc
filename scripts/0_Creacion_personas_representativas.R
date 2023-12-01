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
personas <- read.csv("https://media.githubusercontent.com/media/sebastian23e/predicting_poverty_bdmc/main/stores/data_p.csv")

  personas_agrupadas<-personas %>%
  group_by(id) %>%
  mutate(id=id)
  mutate(nivel_edu = max(nivel_edu))%>%
  mutate(trabajo_formal= ifelse(trabajo_formal==1,1,0)) %>%          
  mutate(pension=ifelse(pension==1,1,0))
  
  for (i in 1:nrow(personas)) {
    # Verifica si la Variable es igual al valor deseado
    if (personas$Variable[i] == ) {
      # Realiza alguna acción si se cumple la condición
      print(paste("Se encontró", valor_deseado, "en la observación", i))
    }
  }
            
            
            


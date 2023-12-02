
# Limpieza area de trabajo ------------------------------------------------
rm(list=ls())
cat('\014')
dir.base <- paste0(getwd(),'/stores/')

# Paquetes ----------------------------------------------------------------
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
       glmnet) 

# Cargar base de datos ----------------------------------------------------
personas <- read.csv("https://media.githubusercontent.com/media/sebastian23e/predicting_poverty_bdmc/main/stores/data_p.csv")

personas <- personas %>% 
  mutate(menor.edad = ifelse(edad <18, 1, 0),
         tercera.edad = ifelse(edad > 59, 1, 0))

personas_agrupadas <- personas %>%
  group_by(id) %>%
  summarise(nivel.educacion         = max(nivel_edu), 
            trabajo.formal          = ifelse(sum(trabajo_formal) > 0, 1, 0), 
            pension                 = ifelse(sum(pension) > 0, 1, 0), 
            subsidio.alimenticio    = ifelse(sum(a_alim) > 0, 1, 0),
            subsidio.transporte     = ifelse(sum(a_trans) > 0, 1, 0),
            subsidio.familiar       = ifelse(sum(a_fam) > 0, 1, 0),
            subsidio.educativo      = ifelse(sum(a_edu) > 0, 1, 0), 
            numero.menores.edad     = sum(menor.edad),
            individuos.hogar        = n(),
            proporcion.menores.edad = sum(menor.edad)/n(),
            numero.tercera.edad     = sum(tercera.edad))

# Merge con la base de hogares --------------------------------------------
base.hogares  <- read_csv("https://media.githubusercontent.com/media/sebastian23e/predicting_poverty_bdmc/main/stores/data_h.csv")
base.completa <- full_join(base.hogares, personas_agrupadas, by = 'id')

# Mutar factores ----------------------------------------------------------
base.completa <- base.completa %>% 
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
         numero.menores.edad     = as.factor(numero.menores.edad)
         )

# Guardar base ------------------------------------------------------------
save(base.completa, file= paste0(getwd(),'/stores/','base_completa.RData'))

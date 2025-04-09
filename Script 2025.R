##Limpieza del ambiente
rm(list = ls())

## Carga de librerias
library(readxl) ## Abrir exceles
library(PASWR) ## Tiene funcion EDA que me facilita analisis exploratorio
library(DataExplorer) ## Multiples funciones analisis exploratorio y construccion de reporte
library(skimr) ## Me sirve para calcular medias de un grupo seleccionado
library(janitor) ## Tiene funcion tabyl para tablas exploratoria
library(ggthemes) ## Para personalizar reportes
library(ggplot2) ## Para personalizar reportes
library(GGally) ## Para personalizar reportes
library (knitr) ## Para personalizar graficos
library(gtsummary) ## Para generar tablas reporte
library(dplyr) ## Para manejo de datos en los analisis 
library(tidyverse) ## Para generacion de nuevos dataframe
library(tidyr) ## Para generacion de nuevos dataframe
library (reshape2) ## Para reorganizar dataframes (wide or long)
library (stats) ## Estadisticas basicas
library(textshape) ## Para reshaping text 
library (FactoMineR) ## Para realizacion de componentes principales
library(missMDA) ## Para manejo de missing data
library (ggfortify) ## Visualizacion grafia de analisis estadisticos (en mi caso para el componentes principales)
library (factoextra) ## Para completar y revalorar el analisis de componentes principales
library(mdatools)
library(mixOmics)
library(cutpointr) ### para puntos de corte
library(generalhoslem) ## hosmer lemeshow
library(lmtest) # LR test
library(pROC)



## Carga de los datos
load("data/Clean_data_RData/participantes.RData")
load("data/Clean_data_RData/criterios_inclusion.RData")
load("data/Clean_data_RData/evaluacion_base.RData")
load("data/Clean_data_RData/hemopre.RData")
load("data/Clean_data_RData/fin_tto.RData")
load("data/Clean_data_RData/hemopos.RData")
load("data/Clean_data_RData/sem_8.RData")
load("data/Clean_data_RData/sem_13.RData")
load("data/Clean_data_RData/sem_26.RData")
load("data/Clean_data_RData/visita_adic.RData")
load("data/Clean_data_RData/especie_parasit.RData")
load("data/Clean_data_RData/estado_fin.RData")
load("data/Clean_data_RData/gran_base_resumen.RData")
load("data/Clean_data_RData/Big_hemo.RData")
load("data/Clean_data_RData/hemogramas_centrados.RData")
load("data/Clean_data_RData/Base_regresion_final.RData")

# Bases
base_paper <- data.frame(
  codigo_paciente = gran_base_resumen$codigo_paciente,
  sexo = gran_base_resumen$sexo,
  etnia = gran_base_resumen$etnia,
  edad = gran_base_resumen$edad,
  edad_categorica_corta = gran_base_resumen$edad_categorica_corta,
  tiempo_evolucion = gran_base_resumen$tiempo_sintom_semanas,
  imc = gran_base_resumen$imc,
  numero_lesiones = gran_base_resumen$numero_lesiones,
  tipo_lesion = gran_base_resumen$tipo_lesion_eval_base_corta, # Verifica si debe ser tipo_lesion_evalbas
  adenopatia_pre = gran_base_resumen$adenopatia_evalbas,
  infeccion_pre = gran_base_resumen$infeccion_concom_evalbas,
  especie = gran_base_resumen$especie_corta,
  tratamiento = gran_base_resumen$tratamiento,
  variacion_lesion = gran_base_resumen$variacion_lesion_post2,
  adenopatia_post = gran_base_resumen$adenopatia_fin_tto,
  infeccion_post = gran_base_resumen$infeccion_concom_fintto,
  adherencia_tto = gran_base_resumen$adherencia_tto.1,
  Estado_final = gran_base_resumen$estado_final.1 # O estado_final, revisa cuál es correcto
)


names(base_paper)


base_paper  <- base_paper  %>% 
  mutate(imc_cat_corta = case_when(
    base_paper$imc >= 18 & base_paper$imc <= 25 ~ "0",
    base_paper$imc < 18 ~ "1",
    base_paper$imc > 25 ~ "2"))

base_paper$imc_cat_corta  <- factor(base_paper$imc_cat_corta, 
                                    levels = c("0", "1", "2"), 
                                    labels = c("Between 18 and 25 (Normal)", "Less than 18 (Underweight)", "Greater than 25 (Overweight and Obesity)"))
                      

base_paper <- base_paper %>%
  dplyr::select(codigo_paciente, sexo, etnia, edad, edad_categorica_corta, tiempo_evolucion, 
                imc, imc_cat_corta, numero_lesiones, tipo_lesion, adenopatia_pre, infeccion_pre, 
                especie, tratamiento, variacion_lesion, adenopatia_post, infeccion_post, 
                adherencia_tto, Estado_final)

names(base_paper)

library(dplyr)
library(gtsummary)

library(dplyr)
library(gtsummary)


# Crear tabla descriptiva
tble1 <- 
  base_paper %>%
  dplyr::select(
    sexo, etnia, edad, edad_categorica_corta, tiempo_evolucion, 
    imc, imc_cat_corta, numero_lesiones, tipo_lesion, adenopatia_pre, infeccion_pre, 
    especie, tratamiento, variacion_lesion, adenopatia_post, infeccion_post, 
    adherencia_tto, Estado_final
  ) %>% 
  tbl_summary(
    by = Estado_final,  # Agrupar por estado final
    missing_text = "No data / Not applicable",
    digits = everything() ~ 2, 
    percent = "row",  # Calcular porcentajes por fila en grupos
    label = list(
      edad = "Age (Me [RIQ])",
      edad_categorica_corta = "Age by category. n (%)",
      sexo = "Sex. n (%)",
      etnia = "Ethnicity. n (%)",
      tiempo_evolucion = "Symptom evolution time in weeks. Me (RIQ)",
      imc = "Body mass index. Me (RIQ)",
      imc_cat_corta = "Body mass index categories. n (%)",
      numero_lesiones = "Number of Pre-Tx lesions. Me (RIQ)",
      tipo_lesion = "Type of main Pre-Tx lesion. n (%)",
      adenopatia_pre = "Presence of Pre-Tx lymphadenopathy. n (%)",
      infeccion_pre = "Pre-Tx concomitant infection. n (%)",
      especie = "Isolated parasitic species. n (%)",
      tratamiento = "Prescribed treatment. n (%)",
      variacion_lesion = "Evolution of the main lesion (Pre-Tx - EoTx). n (%)",
      adenopatia_post = "Presence of EoTx lymphadenopathy. n (%)",
      infeccion_post = "Concomitant EoTx infection. n (%)",
      adherencia_tto = "Percentage of adherence to treatment. Mean (SD)"
    ),
    type = c(
      edad, tiempo_evolucion, imc, numero_lesiones, adherencia_tto
    ) ~ "continuous",
    statistic = list(
      adherencia_tto ~ "{mean} ({sd})"
    )
  ) %>%
  add_overall(col_label = "Overall", include = everything()) %>%  # Agregar columna general sin porcentaje
  modify_header(update = list(stat_0 = "**Overall**")) %>%  # Asegurar que "Overall" no tenga porcentaje
  add_p()  # Agregar p-value para comparar por Estado_final

# Mostrar la tabla
tble1

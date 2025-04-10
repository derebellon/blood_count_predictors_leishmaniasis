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
load("data/Clean_data_RData/final_df_cleaned.RData")

## Renombrando final_df_cleaned como df para facilitar el trabajo
df <- final_df_cleaned

## Eliminando final_df_cleaned
rm(final_df_cleaned)

# Tabla descriptiva
tble1 <- 
  df %>%
  dplyr::select(sexo, edad, edad_categorica_corta, etnia, 
                tiempo_sintom_semanas, imc, imc_cat_corta,
                numero_lesiones, tipo_lesion_evalbas,
                adenopatia_evalbas, infeccion_concom_evalbas,
                especie, tratamiento, variacion_lesion_post2, estado_final) %>%
  tbl_summary(
    by = estado_final,
    missing_text = "No dato / No aplica",
    label = list(
      sexo = "Sex",
      edad = "Age (years)",
      edad_categorica_corta = "Age category",
      etnia = "Ethnicity",
      tiempo_sintom_semanas = "Symptom evolution time (weeks)",
      imc = "BMI",
      imc_cat_corta = "BMI category",
      numero_lesiones = "Number of lesions",
      tipo_lesion_evalbas = "Type of main lesion (baseline)",
      adenopatia_evalbas = "Lymphadenopathy (baseline)",
      infeccion_concom_evalbas = "Concomitant infection (baseline)",
      especie = "Parasitic species",
      tratamiento = "Prescribed treatment",
      variacion_lesion_post2 = "Lesion variation (Pre - EoT)",
      estado_final = "Final outcome"
    ),
    type = c(edad, tiempo_sintom_semanas, imc, numero_lesiones) ~ "continuous",
    statistic = list(all_continuous() ~ "{median} ({p25}, {p75})")
  ) %>%
  add_overall() %>%
  add_p()

# Tabla de regresiones univariadas con Poisson
tabla_rr_uni <- 
  df %>%
  tbl_uvregression(
    method = glm,
    y = estado_final == "Therapeutic failure",  # Binary outcome (TRUE = failure)
    method.args = list(family = poisson(link = "log")),
    include = c(sexo, edad, edad_categorica_corta, etnia, 
                tiempo_sintom_semanas, imc, imc_cat_corta,
                numero_lesiones, tipo_lesion_evalbas,
                adenopatia_evalbas, infeccion_concom_evalbas,
                especie, tratamiento, variacion_lesion_post2),
    exponentiate = TRUE,
    label = list(
      sexo = "Sex",
      edad = "Age (years)",
      edad_categorica_corta = "Age category",
      etnia = "Ethnicity",
      tiempo_sintom_semanas = "Symptom evolution time (weeks)",
      imc = "BMI",
      imc_cat_corta = "BMI category",
      numero_lesiones = "Number of lesions",
      tipo_lesion_evalbas = "Type of main lesion (baseline)",
      adenopatia_evalbas = "Lymphadenopathy (baseline)",
      infeccion_concom_evalbas = "Concomitant infection (baseline)",
      especie = "Parasitic species",
      tratamiento = "Prescribed treatment",
      variacion_lesion_post2 = "Lesion variation (Pre - EoT)"
    )
  )

# Unir ambas tablas
tabla_final <- tbl_merge(
  tbls = list(tble1, tabla_rr_uni),
  tab_spanner = c("**Summary**", "**Univariate RR (95% CI)**")
)

# Mostrar tabla final
tabla_final %>% as_gt()




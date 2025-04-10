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
library(webshot2)
library(gt)



## Carga de los datos
load("data/Clean_data_RData/final_df_cleaned.RData")

## Renombrando final_df_cleaned como df para facilitar el trabajo
df <- final_df_cleaned

## Eliminando final_df_cleaned
rm(final_df_cleaned)

# Table overall
tble_overall <- 
  df %>% 
  dplyr::select(sexo, edad, edad_categorica_corta, etnia, 
                tiempo_sintom_semanas, imc, imc_cat_corta,
                numero_lesiones, tipo_lesion_evalbas,
                linfadenop_before_after_tto, infeccion_concom_any_moment,
                especie, tratamiento, variacion_lesion_post2) %>%
  tbl_summary(
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
      linfadenop_before_after_tto = "Lymphadenopathy before and/or after treatment",
      infeccion_concom_any_moment = "Co-infection before or during the treatment",
      especie = "Parasitic species",
      tratamiento = "Prescribed treatment",
      variacion_lesion_post2 = "Lesion variation (Pre - EoT)"
    ),
    type = list(
      # For continuous variables, lo forzamos a "continuous"
      edad = "continuous",
      tiempo_sintom_semanas = "continuous",
      imc = "continuous",
      numero_lesiones = "continuous",
      # Para las binarias, forzamos el desglose
      linfadenop_before_after_tto = "categorical",
      infeccion_concom_any_moment = "categorical"
    ),
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = everything() ~ 2,
    percent = "column",
    missing = "no",
    # Solo para las dos variables que queremos mostrar todas las categorías
    value = list(
      linfadenop_before_after_tto = "all",
      infeccion_concom_any_moment = "all"
    )
  ) %>% 
  add_n(location = "first")

# Tabla descriptiva
tble_by_outcome <- 
  df %>%
  dplyr::select(sexo, edad, edad_categorica_corta, etnia, 
                tiempo_sintom_semanas, imc, imc_cat_corta,
                numero_lesiones, tipo_lesion_evalbas,
                linfadenop_before_after_tto, infeccion_concom_any_moment,
                especie, tratamiento, variacion_lesion_post2, estado_final) %>%
  tbl_summary(
    by = estado_final,
    missing = "no",
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
      linfadenop_before_after_tto = "Lymphadenopathy before and/or after treatment",
      infeccion_concom_any_moment = "Co-infection before or during the treatment",
      especie = "Parasitic species",
      tratamiento = "Prescribed treatment",
      variacion_lesion_post2 = "Lesion variation (Pre - EoT)",
      estado_final = "Final outcome"
    ),
    type = list(
      edad = "continuous",
      tiempo_sintom_semanas = "continuous",
      imc = "continuous",
      numero_lesiones = "continuous",
      linfadenop_before_after_tto = "categorical",
      infeccion_concom_any_moment = "categorical"
    ),
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = everything() ~ 2,
    value = list(
      linfadenop_before_after_tto = "all",
      infeccion_concom_any_moment = "all"
    ),
    percent = "row"
  ) %>%
  add_p()

# Tabla de regresiones univariadas con Poisson
tabla_rr_uni <- 
  df %>%
  tbl_uvregression(
    method = glm,
    y = estado_final == "Therapeutic failure",  # Binary outcome (TRUE = failure)
    method.args = list(family = binomial(link = "log")),
    include = c(sexo, edad, edad_categorica_corta, etnia, 
                tiempo_sintom_semanas, imc, imc_cat_corta,
                numero_lesiones, tipo_lesion_evalbas,
                linfadenop_before_after_tto, infeccion_concom_any_moment,
                especie, tratamiento, variacion_lesion_post2),
    exponentiate = TRUE,
    hide_n = TRUE,
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
      linfadenop_before_after_tto = "Lymphadenopathy before and/or after treatment",
      infeccion_concom_any_moment = "Co-infection before or during the treatment",
      especie = "Parasitic species",
      tratamiento = "Prescribed treatment",
      variacion_lesion_post2 = "Lesion variation (Pre - EoT)"
    )
  )

# Uniendo las tablas
table_1 <- tbl_merge(
  tbls = list(tble_overall, tble_by_outcome, tabla_rr_uni),
  tab_spanner = c("**Overall**", "**Summary by Outcome**", "**Univariate RR (95% CI)**")
)


# Mostrar tabla final
table_1 %>% as_gt()

# Guardar como HTML
table_1 %>%
  as_gt() %>%
  gt::gtsave(filename = "tables/Tables Phase 2/Table_1_paper.html")

table_1 %>%
  as_gt() %>%
  gtsave("tables/Tables Phase 2/Table_1_paper.png")


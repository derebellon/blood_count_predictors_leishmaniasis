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


## Proceso de imputacion multiple
library(dplyr)
library(mice)

### --- STEP 1: Select variables for imputation --- ###
# Note: The derived variables “Granulocitos_pre_exp” and “Granulocitos_post_exp” will be computed from the raw values,
# so they are not included in the imputation.
vars_impute <- c(
  "codigo_paciente", "estado_final", "sexo", "edad", "edad_categorica_corta", "etnia",
  "tiempo_sintom_semanas", "imc", "imc_cat_corta", "numero_lesiones", "tipo_lesion_evalbas",
  "linfadenop_before_after_tto", "infeccion_concom_any_moment", "especie_corta", "tratamiento", "variacion_lesion_post2",
  "adenopatia_evalbas", "infeccion_concom_evalbas", "comorbilidades", "adherencia_tto",
  # Hemograma pre-treatment (raw values)
  "Leucocitos_pre", "Neutrofilos_pre", "Linfocitos_pre", "Monocitos_pre", "Eosinofilos_pre", "Basofilos_pre",
  "Granulocitos_pre", "Globulos_rojos_pre", "Hemoglobina_pre", "Hematocrito_pre",
  # Hemograma post-treatment (raw values)
  "Leucocitos_post", "Neutrofilos_post", "Linfocitos_post", "Monocitos_post", "Eosinofilos_post", "Basofilos_post",
  "Granulocitos_post", "Globulos_rojos_post", "Hemoglobina_post", "Hematocrito_post",
  # Variables used to later compute additional variables
  "adenopatia_evalbas", "adenopatia_fin_tto", "infeccion_concom_evalbas", "infeccion_concom_ftto"
)

df_impute <- df %>% dplyr::select(all_of(vars_impute))

### --- STEP 2: Specify imputation methods --- ###
# Create a vector of methods based on variable classes.
meth <- make.method(df_impute)
# Do not impute the identifier.
meth["codigo_paciente"] <- ""

# For numeric variables, use predictive mean matching ("pmm")
num_vars <- c("edad", "tiempo_sintom_semanas", "imc", "numero_lesiones",
              "Leucocitos_pre", "Neutrofilos_pre", "Linfocitos_pre", "Monocitos_pre",
              "Eosinofilos_pre", "Basofilos_pre", "Granulocitos_pre",
              "Leucocitos_post", "Neutrofilos_post", "Linfocitos_post", "Monocitos_post",
              "Eosinofilos_post", "Basofilos_post", "Granulocitos_post", "Globulos_rojos_pre", "Hemoglobina_pre", "Hematocrito_pre",
              "Globulos_rojos_post", "Hemoglobina_post", "Hematocrito_post")
meth[num_vars] <- "pmm"

# For categorical variables: use "polyreg" for those with >2 levels.
fact_vars <- setdiff(names(df_impute), c("codigo_paciente", num_vars))
vars_poly <- c("edad_categorica_corta", "etnia", "imc_cat_corta", "tipo_lesion_evalbas", "variacion_lesion_post2")
meth[vars_poly] <- "polyreg"
# For the remaining categorical (assumed binary), use "logreg"
fact_binary <- setdiff(fact_vars, vars_poly)
meth[fact_binary] <- "logreg"

#print(meth)

### --- STEP 3: Configure predictor matrix --- ###
pred_matrix <- make.predictorMatrix(df_impute)
pred_matrix[, "codigo_paciente"] <- 0
pred_matrix["codigo_paciente", ] <- 0

### --- STEP 4: Run multiple imputation --- ###
set.seed(123)
imp_results <- mice(df_impute, m = 5, method = meth, predictorMatrix = pred_matrix, seed = 123)

# Extract the first imputed dataset
df_imputed <- complete(imp_results, 1)

### --- STEP 5: Recalculate derived variables --- ###
# First, create the exponentiated granulocyte variables.
df_imputed <- df_imputed %>%
  mutate(
    Granulocitos_pre_exp = exp(Granulocitos_pre),
    Granulocitos_post_exp = exp(Granulocitos_post)
  )

# Now, recalc all derived indices and ratios using the exponentiated values for granulocytes.
df_imputed <- df_imputed %>%
  mutate(
    # Percentages (pre-treatment)
    porcentaje_neutro_pre = ifelse(Leucocitos_pre == 0, NA, 100 * (Neutrofilos_pre / Leucocitos_pre)),
    porcentaje_linfo_pre  = ifelse(Leucocitos_pre == 0, NA, 100 * (Linfocitos_pre / Leucocitos_pre)),
    porcentaje_mono_pre   = ifelse(Leucocitos_pre == 0, NA, 100 * (Monocitos_pre / Leucocitos_pre)),
    porcentaje_eosino_pre = ifelse(Leucocitos_pre == 0, NA, 100 * (Eosinofilos_pre / Leucocitos_pre)),
    porcentaje_baso_pre   = ifelse(Leucocitos_pre == 0, NA, 100 * (Basofilos_pre / Leucocitos_pre)),
    porcentaje_granulo_pre = ifelse(Leucocitos_pre == 0, NA, 100 * (Granulocitos_pre / Leucocitos_pre)),
    
    # Percentages (post-treatment)
    porcentaje_neutro_post = ifelse(Leucocitos_post == 0, NA, 100 * (Neutrofilos_post / Leucocitos_post)),
    porcentaje_linfo_post  = ifelse(Leucocitos_post == 0, NA, 100 * (Linfocitos_post / Leucocitos_post)),
    porcentaje_mono_post   = ifelse(Leucocitos_post == 0, NA, 100 * (Monocitos_post / Leucocitos_post)),
    porcentaje_eosino_post = ifelse(Leucocitos_post == 0, NA, 100 * (Eosinofilos_post / Leucocitos_post)),
    porcentaje_baso_post   = ifelse(Leucocitos_post == 0, NA, 100 * (Basofilos_post / Leucocitos_post)),
    porcentaje_granulo_post = ifelse(Leucocitos_post == 0, NA, 100 * (Granulocitos_post / Leucocitos_post)),
    
    # Indices (pre-treatment) for variables not involving granulocytes
    i_neu_linfo_pre = ifelse(Linfocitos_pre == 0, NA, Neutrofilos_pre / Linfocitos_pre),
    i_eos_linfo_pre = ifelse(Linfocitos_pre == 0, NA, Eosinofilos_pre / Linfocitos_pre),
    i_mono_linfo_pre = ifelse(Linfocitos_pre == 0, NA, Monocitos_pre / Linfocitos_pre),
    i_baso_linfo_pre = ifelse(Linfocitos_pre == 0, NA, Basofilos_pre / Linfocitos_pre),
    # Indices involving granulocytes (pre-treatment): use exponentiated denominators
    i_neu_granu_pre = Neutrofilos_pre / Granulocitos_pre_exp,
    i_eos_granu_pre = Eosinofilos_pre / Granulocitos_pre_exp,
    i_baso_granu_pre = Basofilos_pre / Granulocitos_pre_exp,
    
    # Indices (post-treatment) for variables not involving granulocytes
    i_neu_linfo_post = ifelse(Linfocitos_post == 0, NA, Neutrofilos_post / Linfocitos_post),
    i_eos_linfo_post = ifelse(Linfocitos_post == 0, NA, Eosinofilos_post / Linfocitos_post),
    i_mono_linfo_post = ifelse(Linfocitos_post == 0, NA, Monocitos_post / Linfocitos_post),
    i_baso_linfo_post = ifelse(Linfocitos_post == 0, NA, Basofilos_post / Linfocitos_post),
    # Indices involving granulocytes (post-treatment): use exponentiated denominators
    i_neu_granu_post = Neutrofilos_post / Granulocitos_post_exp,
    i_eos_granu_post = Eosinofilos_post / Granulocitos_post_exp,
    i_baso_granu_post = Basofilos_post / Granulocitos_post_exp,
    
    # Combined index (monoeosneu) 
    i_monoeosneu_lin_pre = ifelse(Linfocitos_pre == 0, NA, (Monocitos_pre + Eosinofilos_pre + Neutrofilos_pre) / Linfocitos_pre),
    i_monoeosneu_lin_post = ifelse(Linfocitos_post == 0, NA, (Monocitos_post + Eosinofilos_post + Neutrofilos_post) / Linfocitos_post),
    
    # Variations for original variables (post / pre)
    variacion_leucocitos = ifelse(Leucocitos_pre == 0, NA, Leucocitos_post / Leucocitos_pre),
    variacion_neutrofilos = ifelse(Neutrofilos_pre == 0, NA, Neutrofilos_post / Neutrofilos_pre),
    variacion_linfocitos = ifelse(Linfocitos_pre == 0, NA, Linfocitos_post / Linfocitos_pre),
    variacion_monocitos = ifelse(Monocitos_pre == 0, NA, Monocitos_post / Monocitos_pre),
    variacion_eosinofilos = ifelse(Eosinofilos_pre == 0, NA, Eosinofilos_post / Eosinofilos_pre),
    variacion_basofilos = ifelse(Basofilos_pre == 0, NA, Basofilos_post / Basofilos_pre),
    variacion_granulocitos = ifelse(Granulocitos_pre == 0, NA, Granulocitos_post / Granulocitos_pre),
    variacion_globulos_rojos = ifelse(Globulos_rojos_pre == 0, NA, Globulos_rojos_post / Globulos_rojos_pre),
    variacion_hemoglobina = ifelse(Hemoglobina_pre == 0, NA, Hemoglobina_post / Hemoglobina_pre),
    variacion_hematocrito = ifelse(Hematocrito_pre == 0, NA, Hematocrito_post / Hematocrito_pre),
    
    # Variations of percentages
    variacion_porcentaje_neutro = ifelse(porcentaje_neutro_pre == 0, NA, porcentaje_neutro_post / porcentaje_neutro_pre),
    variacion_porcentaje_linfo = ifelse(porcentaje_linfo_pre == 0, NA, porcentaje_linfo_post / porcentaje_linfo_pre),
    variacion_porcentaje_mono = ifelse(porcentaje_mono_pre == 0, NA, porcentaje_mono_post / porcentaje_mono_pre),
    variacion_porcentaje_eosino = ifelse(porcentaje_eosino_pre == 0, NA, porcentaje_eosino_post / porcentaje_eosino_pre),
    variacion_porcentaje_baso = ifelse(porcentaje_baso_pre == 0, NA, porcentaje_baso_post / porcentaje_baso_pre),
    variacion_porcentaje_granulo = ifelse(porcentaje_granulo_pre == 0, NA, porcentaje_granulo_post / porcentaje_granulo_pre),
    
    # Variations of indices not involving granulocytes
    variacion_i_neu_linfo = ifelse(i_neu_linfo_pre == 0, NA, i_neu_linfo_post / i_neu_linfo_pre),
    variacion_i_eos_linfo = ifelse(i_eos_linfo_pre == 0, NA, i_eos_linfo_post / i_eos_linfo_pre),
    variacion_i_mono_linfo = ifelse(i_mono_linfo_pre == 0, NA, i_mono_linfo_post / i_mono_linfo_pre),
    variacion_i_baso_linfo = ifelse(i_baso_linfo_pre == 0, NA, i_baso_linfo_post / i_baso_linfo_pre),
    # Variations of indices involving granulocytes (using the exponentiated denominators calculated above)
    variacion_i_neu_granu = i_neu_granu_post / i_neu_granu_pre,
    variacion_i_eos_granu = i_eos_granu_post / i_eos_granu_pre,
    variacion_i_baso_granu = i_baso_granu_post / i_baso_granu_pre,
    # Combined index variation
    variacion_i_monoeosneu_lin = ifelse(i_monoeosneu_lin_pre == 0, NA, i_monoeosneu_lin_post / i_monoeosneu_lin_pre)
  )

# --- STEP 5b: Fill NA in granulocyte variation using exponentiated values --- #
df_imputed <- df_imputed %>%
  mutate(
    variacion_granulocitos = ifelse(is.na(variacion_granulocitos),
                                    Granulocitos_post_exp / Granulocitos_pre_exp,
                                    variacion_granulocitos),
    variacion_porcentaje_granulo = ifelse(is.na(variacion_porcentaje_granulo),
                                          Granulocitos_post_exp / Granulocitos_pre_exp,
                                          variacion_porcentaje_granulo)
  )


### --- STEP 6: Create additional variables (post-imputation) --- ###
# Use the original baseline and treatment-end variables for regional lymphadenopathy and concomitant infection.
# (Asegúrate de que en df estén disponibles: "adenopatia_evalbas", "adenopatia_fin_tto", 
#  "infeccion_concom_evalbas", "infeccion_concom_ftto")
df_imputed <- df_imputed %>%
  mutate(
    linfadenop_before_after_tto = ifelse(adenopatia_evalbas == "Yes" | adenopatia_fin_tto == "Yes", "1", "0"),
    linfadenop_before_after_tto = factor(linfadenop_before_after_tto, levels = c("0", "1"), labels = c("No", "Yes")),
    infeccion_concomitante = ifelse(infeccion_concom_evalbas == "Yes" | infeccion_concom_ftto == "Yes", "1", "0"),
    infeccion_concomitante = factor(infeccion_concomitante, levels = c("0", "1"), labels = c("No", "Yes"))
  )

### --- STEP 7: Confirm and clean up --- ###
na_por_variable <- colSums(is.na(df_imputed))
print(na_por_variable[na_por_variable > 0])

# Remove the temporary dataset used for imputation.
rm(df_impute)

names(df_imputed)



### lasso regression
library(glmnet)
library(caret)
library(dplyr)

#### 1. Preparar datos ####
# La variable de desenlace "estado_final" ya es binaria:
str(df_imputed$estado_final)
levels(df_imputed$estado_final)
# Crear matriz X y vector y (model.matrix convierte las variables categóricas en dummies)
x <- model.matrix(estado_final ~ . - codigo_paciente, data = df_imputed)[, -1]  # se elimina intercepto
y <- df_imputed$estado_final

#### 2. Dividir en entrenamiento y prueba ####
set.seed(123)
indexTrain <- createDataPartition(y, p = 0.8, list = FALSE)
xTrain <- x[indexTrain, ]
yTrain <- y[indexTrain]
xTest <- x[-indexTrain, ]
yTest <- y[-indexTrain]

#### 3. Validación cruzada para elegir lambda óptimo ####
cv_lasso <- cv.glmnet(xTrain, yTrain, alpha = 1, family = "binomial", standardize = TRUE)
best_lambda <- cv_lasso$lambda.min
plot(cv_lasso, main = "CV LASSO")
print(paste("Best lambda:", best_lambda))

#### 4. Ajustar el modelo final LASSO ####
lasso_final <- glmnet(xTrain, yTrain, alpha = 1, family = "binomial", lambda = best_lambda)

#### 5. Obtener coeficientes no nulos ####
coef_lasso <- coef(lasso_final)
# Extraer las filas (variables dummy) con coeficiente distinto de cero, excluyendo el intercepto
selected_vars_raw <- rownames(coef_lasso)[which(coef_lasso != 0)]
selected_vars_raw <- selected_vars_raw[selected_vars_raw != "(Intercept)"]
print("Variables seleccionadas (raw) de glmnet:")
print(selected_vars_raw)

# Debido a que model.matrix cambia los nombres, extraemos el nombre original comparando con los nombres de df_imputed.
original_vars <- sapply(selected_vars_raw, function(v) {
  matches <- names(df_imputed)[startsWith(v, names(df_imputed))]
  if(length(matches) > 0) return(matches[1]) else return(NA)
})
original_vars <- unique(na.omit(original_vars))
print("Variables seleccionadas (original):")
print(original_vars)

#### 6. Crear dataset reducido con solo las variables seleccionadas ####
df_lasso <- df_imputed %>% 
  dplyr::select(c("codigo_paciente", "estado_final", all_of(original_vars)))
print("Dimensión del dataset reducido:")
print(dim(df_lasso))

#### 7. Evaluación del modelo ####
# Predecir en entrenamiento y prueba y forzar a factor con los mismos niveles que yTrain / yTest
ypred_train <- predict(lasso_final, newx = xTrain, type = "class")
ypred_train <- factor(ypred_train, levels = levels(yTrain))
ypred_test <- predict(lasso_final, newx = xTest, type = "class")
ypred_test <- factor(ypred_test, levels = levels(yTest))

conf_train <- confusionMatrix(data = ypred_train, reference = yTrain, positive = "Therapeutic failure")
conf_test <- confusionMatrix(data = ypred_test, reference = yTest, positive = "Therapeutic failure")

print("Matriz de confusión - Entrenamiento:")
print(conf_train)
print("Matriz de confusión - Prueba:")
print(conf_test)



################################
# Construccion de superlearners


library(dplyr)
library(caret)
library(glmnet)  # Por si se requiere en SL.glm o para otras comparaciones
library(ranger)
library(xgboost)
library(SuperLearner)
library(ggplot2)

### Preparar el dataset para el modelado ###
# Excluir el identificador; para xgboost necesitamos la variable de respuesta en formato numérico (0/1)
df_model <- df_imputed %>%
  dplyr::select(-codigo_paciente)
# Convertir la variable desenlace a numérica: "Therapeutic failure" = 1 y "Definitive cure" = 0
df_model <- df_model %>%
  mutate(estado_final_numeric = ifelse(estado_final == "Therapeutic failure", 1, 0))

### MODELO 1: RANDOM FOREST (RANGER) ###
set.seed(123)
rf_model <- train(estado_final ~ ., data = df_model %>% dplyr::select(-estado_final_numeric),
                  method = "ranger",
                  trControl = trainControl(method = "cv", number = 10),
                  tuneLength = 10,
                  importance = "impurity")  # <- esta línea es clave

print(rf_model)

# Importancia de variables
rf_imp <- varImp(rf_model, scale = FALSE)
print("Importancia variables - Random Forest:")
print(rf_imp)


### MODELO 2: XGBOOST ###
# Preparar la matriz de predictores usando model.matrix (excluyendo el outcome y otros identificadores)
x <- model.matrix(estado_final ~ . - estado_final - estado_final_numeric, data = df_model)
y <- df_model$estado_final_numeric
set.seed(123)
dtrain <- xgb.DMatrix(data = x, label = y)
params <- list(objective = "binary:logistic", eval_metric = "error") # error es 1 - accuracy
# Validación cruzada para hallar el número óptimo de iteraciones (nrounds)
cv_xgb <- xgb.cv(params = params, data = dtrain, nrounds = 100, nfold = 10, verbose = 0)
best_iter <- which.min(cv_xgb$evaluation_log$test_error_mean)
cat("Número óptimo de iteraciones (best nrounds):", best_iter, "\n")
# Ajustar el modelo final
xgb_model <- xgb.train(params = params, data = dtrain, nrounds = best_iter, verbose = 0)
xgb_imp <- xgb.importance(model = xgb_model)
print("Importancia variables - XGBoost:")
print(xgb_imp)
# Opcional: graficar la importancia
xgb.plot.importance(xgb_imp, main = "Importancia - XGBoost")

### MODELO 3: SUPER LEARNER ###
# Preparar la data para Super Learner: 
# Se utiliza como predictors todas las variables excepto el outcome; debemos asegurarnos que X sea un data.frame.
X_super <- df_model %>% dplyr::select(-estado_final, -estado_final_numeric)
Y_super <- df_model$estado_final_numeric

# Definir la lista de candidatos para el Super Learner
SL.library <- c("SL.ranger", "SL.xgboost", "SL.glm")

set.seed(123)
sl_model <- SuperLearner(Y = Y_super, X = X_super, family = binomial(),
                         SL.library = SL.library, cvControl = list(V = 10))
print("Resultados del Super Learner:")
print(sl_model)
print("Pesos asignados por el Super Learner:")
print(sl_model$coef)

# Evaluación en CV: usaremos CV.SuperLearner y extraer el MSE (para riesgo, se usa la log-loss o MSE en función del problema)
set.seed(123)
sl_cv <- CV.SuperLearner(Y = Y_super, X = X_super, family = binomial(),
                         SL.library = SL.library, cvControl = list(V = 10))
SLrisk <- mean((Y_super - sl_cv$SL.predict)^2)
cat("CV Risk (MSE) del Super Learner:", SLrisk, "\n")

### COMPARACIÓN Y CONCLUSIONES ###
# En este ejemplo, ajustamos tres modelos (RF, XGBoost y un Super Learner que integra varias alternativas).
# Puedes comparar las importancias y las métricas de desempeño para determinar cuál modelo (o combinación) se ajusta mejor a tu problema.




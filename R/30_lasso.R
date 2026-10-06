######################################################################
## RESEARCH PROJECT: "Blood count parameters as early predictors of
## therapeutic outcome in cutaneous leishmaniasis"
## 30_lasso.R
## LASSO (regresion logistica penalizada L1) para seleccion de variables y
## prediccion de FALLA. Validacion por CV estratificada repetida con imputacion
## train-fit (sin fuga) y balanceo por pesos de clase (en train). El lambda se
## elige por CV interna (anidada). Escenarios: clinico / clinico+hemograma /
## hemograma solo, en PRE y EoTx -> responde la hipotesis con validacion OOS.
##
## INPUT : data/Clean_data_csv/final_df_cleaned.csv (via utils/ml_data.R)
## OUTPUT: outputs/30_lasso/lasso_cv_metrics.csv , .rds
######################################################################

source("R/00_config.R")
source("R/utils/validation_utils.R")
source("R/utils/ml_data.R")
suppressPackageStartupMessages({ library(glmnet); library(dplyr) })
paso <- "30_lasso"; dest <- out_dir(paso)

datos <- cargar_datos_ml()
pr <- datos$predictores

## ---- Aprendiz LASSO ---------------------------------------------------
## type.measure="auc" (optimiza discriminacion, no deviance) + lambda.min:
## con tan pocos eventos, deviance/lambda.1se sobre-regulariza a modelo nulo.
fit_lasso <- function(X, y, w) {
  glmnet::cv.glmnet(X, y, family = "binomial", weights = w, alpha = 1,
                    nfolds = 5, type.measure = "auc")
}
pred_lasso <- function(m, X) as.numeric(predict(m, newx = X, s = "lambda.min", type = "response"))

## ---- Escenarios -------------------------------------------------------
escenarios <- list(
  list(nombre = "PRE  clinical only",          datos = datos$pre,  vars = pr$clinicas),
  list(nombre = "PRE  clinical + blood count", datos = datos$pre,  vars = c(pr$clinicas, pr$hemo_pre)),
  list(nombre = "PRE  blood count only",       datos = datos$pre,  vars = pr$hemo_pre),
  list(nombre = "EoTx clinical only",          datos = datos$post, vars = c(pr$clinicas, pr$clinicas_post)),
  list(nombre = "EoTx clinical + blood count", datos = datos$post, vars = c(pr$clinicas, pr$clinicas_post, pr$hemo_pre, pr$hemo_post)),
  list(nombre = "EoTx blood count only",       datos = datos$post, vars = c(pr$hemo_pre, pr$hemo_post))
)

## ---- Correr CV para cada escenario (balanceo por pesos de clase) -----
REPS <- as.integer(Sys.getenv("CV_REPEATS_RUN", "20"))
filas <- list()
for (e in escenarios) {
  message("[30] CV LASSO: ", e$nombre, " ...")
  res <- evaluar_cv(e$datos, e$vars, fit_lasso, pred_lasso,
                    k = CV_FOLDS, repeticiones = REPS, balance = "weights")
  r <- res$resumen
  filas[[e$nombre]] <- data.frame(
    Scenario = e$nombre,
    PR_AUC  = round(r["PR_AUC","media"], 3),
    ROC_AUC = round(r["ROC_AUC","media"], 3),
    Recall_fail = round(r["Recall_fail","media"], 3),
    F2 = round(r["F2","media"], 3),
    Brier = round(r["Brier","media"], 3)
  )
}
tabla <- do.call(rbind, filas); rownames(tabla) <- NULL
write.csv(tabla, file.path(dest, "lasso_cv_metrics.csv"), row.names = FALSE)
saveRDS(tabla, file.path(dest, "lasso_cv_metrics.rds"))

message("\n[30] LASSO — metricas CV (centradas en falla):")
print(tabla, row.names = FALSE)
message("[30] DONE. ", file.path(dest, "lasso_cv_metrics.csv"))

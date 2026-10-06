######################################################################
## RESEARCH PROJECT: "Blood count parameters as early predictors of
## therapeutic outcome in cutaneous leishmaniasis"
## 31_random_forest.R
## Random Forest (ranger, probabilidad) para prediccion de FALLA. Misma
## validacion que LASSO (CV estratificada repetida, imputacion train-fit sin
## fuga, balanceo por pesos de caso en train). Arboles poco profundos / nodos
## grandes por el bajo numero de eventos (evitar sobreajuste). Mismos escenarios.
##
## INPUT : via utils/ml_data.R
## OUTPUT: outputs/31_random_forest/rf_cv_metrics.csv , .rds
######################################################################

source("R/00_config.R")
source("R/utils/validation_utils.R")
source("R/utils/ml_data.R")
suppressPackageStartupMessages({ library(ranger); library(dplyr) })
paso <- "31_random_forest"; dest <- out_dir(paso)

datos <- cargar_datos_ml(); pr <- datos$predictores

## ---- Aprendiz Random Forest ------------------------------------------
## min.node.size grande + mtry moderado -> regulariza con pocos eventos.
fit_rf <- function(X, y, w) {
  ranger::ranger(x = as.data.frame(X), y = factor(y, levels = c(0, 1)),
                 probability = TRUE, num.trees = 1000,
                 min.node.size = 10, mtry = max(2, floor(sqrt(ncol(X)))),
                 case.weights = w, respect.unordered.factors = "order")
}
pred_rf <- function(m, X) predict(m, as.data.frame(X))$predictions[, "1"]

escenarios <- list(
  list(nombre = "PRE  clinical only",          datos = datos$pre,  vars = pr$clinicas),
  list(nombre = "PRE  clinical + blood count", datos = datos$pre,  vars = c(pr$clinicas, pr$hemo_pre)),
  list(nombre = "PRE  blood count only",       datos = datos$pre,  vars = pr$hemo_pre),
  list(nombre = "EoTx clinical only",          datos = datos$post, vars = c(pr$clinicas, pr$clinicas_post)),
  list(nombre = "EoTx clinical + blood count", datos = datos$post, vars = c(pr$clinicas, pr$clinicas_post, pr$hemo_pre, pr$hemo_post)),
  list(nombre = "EoTx blood count only",       datos = datos$post, vars = c(pr$hemo_pre, pr$hemo_post))
)

REPS <- as.integer(Sys.getenv("CV_REPEATS_RUN", "15"))
filas <- list()
for (e in escenarios) {
  message("[31] CV RF: ", e$nombre, " ...")
  res <- evaluar_cv(e$datos, e$vars, fit_rf, pred_rf,
                    k = CV_FOLDS, repeticiones = REPS, balance = "weights")
  r <- res$resumen
  filas[[e$nombre]] <- data.frame(Scenario = e$nombre,
    PR_AUC = round(r["PR_AUC","media"],3), ROC_AUC = round(r["ROC_AUC","media"],3),
    Recall_fail = round(r["Recall_fail","media"],3), F2 = round(r["F2","media"],3),
    Brier = round(r["Brier","media"],3))
}
tabla <- do.call(rbind, filas); rownames(tabla) <- NULL
write.csv(tabla, file.path(dest, "rf_cv_metrics.csv"), row.names = FALSE)
saveRDS(tabla, file.path(dest, "rf_cv_metrics.rds"))
message("\n[31] Random Forest — metricas CV (centradas en falla):")
print(tabla, row.names = FALSE)
message("[31] DONE.")

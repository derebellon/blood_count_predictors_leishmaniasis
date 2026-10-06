######################################################################
## RESEARCH PROJECT: "Blood count parameters as early predictors of
## therapeutic outcome in cutaneous leishmaniasis"
## 32_xgboost.R
## Gradient boosting (XGBoost) para prediccion de FALLA. Misma validacion que
## LASSO/RF. Arboles poco profundos, pocas rondas y submuestreo por el bajo
## numero de eventos (evitar sobreajuste). Balanceo por pesos de caso.
##
## INPUT : via utils/ml_data.R
## OUTPUT: outputs/32_xgboost/xgb_cv_metrics.csv , .rds
######################################################################

source("R/00_config.R")
source("R/utils/validation_utils.R")
source("R/utils/ml_data.R")
suppressPackageStartupMessages({ library(xgboost); library(dplyr) })
paso <- "32_xgboost"; dest <- out_dir(paso)

datos <- cargar_datos_ml(); pr <- datos$predictores

## ---- Aprendiz XGBoost (regularizado para pocos eventos) --------------
fit_xgb <- function(X, y, w) {
  dtrain <- xgboost::xgb.DMatrix(data = X, label = y, weight = w)
  xgboost::xgb.train(
    params = list(objective = "binary:logistic", eval_metric = "auc",
                  max_depth = 2, eta = 0.05, subsample = 0.8,
                  colsample_bytree = 0.6, min_child_weight = 5,
                  lambda = 1, alpha = 0.5),
    data = dtrain, nrounds = 60, verbose = 0)
}
pred_xgb <- function(m, X) predict(m, xgboost::xgb.DMatrix(data = X))

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
  message("[32] CV XGB: ", e$nombre, " ...")
  res <- evaluar_cv(e$datos, e$vars, fit_xgb, pred_xgb,
                    k = CV_FOLDS, repeticiones = REPS, balance = "weights")
  r <- res$resumen
  filas[[e$nombre]] <- data.frame(Scenario = e$nombre,
    PR_AUC = round(r["PR_AUC","media"],3), ROC_AUC = round(r["ROC_AUC","media"],3),
    Recall_fail = round(r["Recall_fail","media"],3), F2 = round(r["F2","media"],3),
    Brier = round(r["Brier","media"],3))
}
tabla <- do.call(rbind, filas); rownames(tabla) <- NULL
write.csv(tabla, file.path(dest, "xgb_cv_metrics.csv"), row.names = FALSE)
saveRDS(tabla, file.path(dest, "xgb_cv_metrics.rds"))
message("\n[32] XGBoost — metricas CV (centradas en falla):")
print(tabla, row.names = FALSE)
message("[32] DONE.")

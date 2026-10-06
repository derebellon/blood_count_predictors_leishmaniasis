######################################################################
## RESEARCH PROJECT: "Blood count parameters as early predictors of
## therapeutic outcome in cutaneous leishmaniasis"
## 60_superlearner.R
## SuperLearner: apila (stacking) los 4 modelos -logistica, LASSO, random forest
## y XGBoost- en un ensamble, con CV para asignar los pesos optimos. Se valida
## FUERA DE MUESTRA con CV.SuperLearner (CV anidada). Pregunta: el ensamble le
## gana al mejor modelo individual, o la parsimonia ya era suficiente?
## Se corre sobre el set CURADO (donde los modelos rinden ~0.75).
##
## INPUT : via utils/ml_data.R
## OUTPUT: outputs/60_superlearner/superlearner_{pre,eotx}.csv
######################################################################

source("R/00_config.R")
source("R/utils/ml_data.R")
suppressPackageStartupMessages({ library(SuperLearner); library(pROC); library(dplyr) })
paso <- "60_superlearner"; dest <- out_dir(paso)

datos <- cargar_datos_ml()

## Imputacion simple (mediana/moda) para el ensamble (set curado, poca falta).
imputar_todo <- function(d, vars) {
  for (v in vars) {
    if (is.numeric(d[[v]])) d[[v]][is.na(d[[v]])] <- stats::median(d[[v]], na.rm = TRUE)
    else { m <- names(sort(table(d[[v]]), decreasing = TRUE))[1]; d[[v]][is.na(d[[v]])] <- m }
  }
  d
}

## ---- Aprendiz XGBoost poco profundo (la "mejor version" nuestra) ------
SL.xgb.shallow <- function(...) SL.xgboost(..., ntrees = 60, max_depth = 2,
                                           shrinkage = 0.05, minobspernode = 5)
biblioteca <- c("SL.glm", "SL.glmnet", "SL.ranger", "SL.xgb.shallow")

correr_sl <- function(df, vars, etiqueta) {
  df <- imputar_todo(df, vars)
  for (v in vars) if (is.character(df[[v]])) df[[v]] <- factor(df[[v]])
  X <- as.data.frame(stats::model.matrix(stats::as.formula(paste("~", paste(vars, collapse = "+"))), df)[, -1, drop = FALSE])
  Y <- df$fail01
  set.seed(SEED)
  ## method.AUC: los pesos del ensamble se eligen MAXIMIZANDO AUC (no error
  ## cuadratico/NNLS por defecto) -> comparable con como lo evaluamos (AUC).
  cvsl <- CV.SuperLearner(Y = Y, X = X, family = binomial(),
                          SL.library = biblioteca, method = "method.AUC",
                          cvControl = list(V = 10), verbose = FALSE)
  ## AUC OOS del SuperLearner y de cada aprendiz (de las predicciones CV)
  auc <- function(p) as.numeric(pROC::auc(pROC::roc(Y, p, quiet = TRUE)))
  sl_auc <- auc(cvsl$SL.predict)
  lib_auc <- sapply(seq_len(ncol(cvsl$library.predict)),
                    function(j) auc(cvsl$library.predict[, j]))
  names(lib_auc) <- colnames(cvsl$library.predict)
  ## pesos promedio asignados a cada aprendiz
  pesos <- colMeans(coef(cvsl))
  res <- data.frame(Learner = c("SuperLearner", names(lib_auc)),
                    ROC_AUC = round(c(sl_auc, lib_auc), 3),
                    Weight = c(NA, round(pesos[names(lib_auc)], 3)))
  write.csv(res, file.path(dest, paste0("superlearner_", etiqueta, ".csv")), row.names = FALSE)
  message("[60] ", etiqueta, " (ROC-AUC OOS | peso en el ensamble):")
  for (i in seq_len(nrow(res)))
    message(sprintf("     %-16s AUC=%.3f %s", res$Learner[i], res$ROC_AUC[i],
                    ifelse(is.na(res$Weight[i]), "", sprintf("(w=%.2f)", res$Weight[i]))))
  res
}

clin_pre  <- c("edad", "sexo", "adenopatia_evalbas", "infeccion_concom_evalbas")
clin_post <- c("edad", "sexo", "infeccion_concom_ftto", "adenopatia_evalbas")
message("[60] SuperLearner PRE (curado: clinico + plaquetas + IG)...")
correr_sl(datos$pre,  c(clin_pre,  "Granulocitos_pre", "Plaquetas_pre"), "pre")
message("[60] SuperLearner EoTx (curado: clinico + eos% + razon monocitos)...")
correr_sl(datos$post, c(clin_post, "pct_eosino_post", "mono_ratio"), "eotx")
message("[60] DONE. Tablas en ", dest)

######################################################################
## 71_rf_shap.R  -  SHAP de Random Forest (treeshap, tree-SHAP exacto)
## Complemento a 70 (XGBoost). Mismo set COMPLETO de features. Permite
## comparar que variables resalta el RF vs el XGBoost (y si emerge algo).
## treeshap sobre un ranger de REGRESION ajustado a y (0/1): el SHAP explica
## la contribucion a la probabilidad predicha de falla.
## OUTPUT: outputs/70_interpretability_shap/rf_shap_importance_{pre,eotx}.csv
######################################################################
source("R/00_config.R")
source("R/utils/ml_data.R")
suppressPackageStartupMessages({ library(ranger); library(treeshap); library(dplyr) })
dest <- out_dir("70_interpretability_shap")

datos <- cargar_datos_ml(); pr <- datos$predictores
imputar_todo <- function(d, vars) {
  for (v in vars) {
    if (is.numeric(d[[v]])) d[[v]][is.na(d[[v]])] <- stats::median(d[[v]], na.rm = TRUE)
    else { m <- names(sort(table(d[[v]]), decreasing = TRUE))[1]; d[[v]][is.na(d[[v]])] <- m }
  }
  d
}

shap_rf <- function(df, vars, etiqueta) {
  df <- imputar_todo(df, vars)
  for (v in vars) if (is.character(df[[v]])) df[[v]] <- factor(df[[v]])
  form <- stats::as.formula(paste("~", paste(vars, collapse = " + ")))
  X <- as.data.frame(stats::model.matrix(form, df)[, -1, drop = FALSE])
  y <- df$fail01
  set.seed(SEED)
  rf <- ranger::ranger(x = X, y = y, num.trees = 1000, min.node.size = 10, respect.unordered.factors = "order")
  uni <- treeshap::ranger.unify(rf, X)
  ts  <- treeshap::treeshap(uni, X, verbose = FALSE)
  sh  <- as.matrix(ts$shaps)
  imp <- colMeans(abs(sh))
  dir <- sapply(colnames(X), function(c) {
    v <- X[[c]]; s <- sh[, c]
    if (stats::sd(v) == 0 || stats::sd(s) == 0) NA else stats::cor(v, s)
  })
  tab <- data.frame(feature = names(imp), mean_abs_shap = round(as.numeric(imp), 4),
                    direction = ifelse(is.na(dir), "", ifelse(dir > 0, "+ (higher -> more failure)", "- (higher -> less failure)")))
  tab <- tab[order(-tab$mean_abs_shap), ]
  write.csv(tab, file.path(dest, paste0("rf_shap_importance_", etiqueta, ".csv")), row.names = FALSE)
  message("[71] RF SHAP ", etiqueta, " - top 10:")
  for (i in seq_len(min(10, nrow(tab))))
    message(sprintf("     %-24s %.4f  %s", tab$feature[i], tab$mean_abs_shap[i], tab$direction[i]))
  tab
}

message("[71] RF SHAP PRE...")
shap_rf(datos$pre,  c(pr$clinicas, pr$hemo_pre), "pre")
message("[71] RF SHAP EoTx...")
shap_rf(datos$post, c(pr$clinicas, pr$clinicas_post, pr$hemo_pre, pr$hemo_post), "eotx")
message("[71] DONE.")

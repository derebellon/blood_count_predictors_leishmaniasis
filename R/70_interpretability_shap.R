######################################################################
## RESEARCH PROJECT: "Blood count parameters as early predictors of
## therapeutic outcome in cutaneous leishmaniasis"
## 70_interpretability_shap.R
## Interpretabilidad con SHAP (valores de Shapley) usando el SHAP NATIVO de
## XGBoost (exacto para arboles, predcontrib=TRUE). Se ajusta XGBoost sobre el
## set COMPLETO de features y se mira que variables DOMINAN la prediccion de
## falla -> se confirma si el ML "coincide" con los biomarcadores que la
## regresion selecciono (plaquetas, IG, eosinofilos, razon de monocitos) y si
## emerge algo que valga mirar.
##
## INPUT : via utils/ml_data.R
## OUTPUT: outputs/70_interpretability_shap/shap_importance_{pre,eotx}.csv
##         figures/paper/shap_importance_{pre,eotx}.png
######################################################################

source("R/00_config.R")
source("R/utils/ml_data.R")
suppressPackageStartupMessages({ library(xgboost); library(ggplot2); library(dplyr) })
paso <- "70_interpretability_shap"; dest <- out_dir(paso)

datos <- cargar_datos_ml(); pr <- datos$predictores

## Imputacion simple (mediana/moda) sobre TODO el frame -> SHAP necesita completo.
## (Es solo para interpretabilidad; la estimacion de desempeno usa CV in-fold.)
imputar_todo <- function(d, vars) {
  for (v in vars) {
    if (is.numeric(d[[v]])) d[[v]][is.na(d[[v]])] <- stats::median(d[[v]], na.rm = TRUE)
    else { m <- names(sort(table(d[[v]]), decreasing = TRUE))[1]; d[[v]][is.na(d[[v]])] <- m }
  }
  d
}

## Ajusta XGBoost + SHAP nativo; devuelve ranking por mean(|SHAP|) + direccion.
shap_xgb <- function(df, vars, etiqueta, archivo_fig) {
  df <- imputar_todo(df, vars)
  for (v in vars) if (is.character(df[[v]])) df[[v]] <- factor(df[[v]])
  form <- stats::as.formula(paste("~", paste(vars, collapse = " + ")))
  X <- stats::model.matrix(form, df)[, -1, drop = FALSE]
  y <- df$fail01
  w <- ifelse(y == 1, sum(y == 0) / sum(y == 1), 1)            # peso a la clase falla
  modelo <- xgboost::xgb.train(
    params = list(objective = "binary:logistic", eval_metric = "auc", max_depth = 2,
                  eta = 0.05, subsample = 0.8, colsample_bytree = 0.6, min_child_weight = 5,
                  lambda = 1, alpha = 0.5),
    data = xgboost::xgb.DMatrix(X, label = y, weight = w), nrounds = 80, verbose = 0)

  ## SHAP nativo (ultima columna = BIAS); se quita por posicion y se renombran
  sh <- predict(modelo, xgboost::xgb.DMatrix(X), predcontrib = TRUE)
  sh <- sh[, -ncol(sh), drop = FALSE]
  colnames(sh) <- colnames(X)
  imp <- colMeans(abs(sh))
  # direccion: correlacion entre valor de la feature y su SHAP (>0 = sube riesgo de falla)
  dir <- sapply(colnames(X), function(c) {
    v <- X[, c]; s <- sh[, c]
    if (stats::sd(v) == 0 || stats::sd(s) == 0) NA else stats::cor(v, s)
  })
  tab <- data.frame(feature = names(imp), mean_abs_shap = round(as.numeric(imp), 4),
                    direction = ifelse(is.na(dir), "", ifelse(dir > 0, "+ (higher -> more failure)", "- (higher -> less failure)")))
  tab <- tab[order(-tab$mean_abs_shap), ]
  write.csv(tab, file.path(dest, paste0("shap_importance_", etiqueta, ".csv")), row.names = FALSE)

  ## Figura: top 15 por mean(|SHAP|)
  top <- head(tab, 15)
  top$feature <- factor(top$feature, levels = rev(top$feature))
  g <- ggplot(top, aes(x = feature, y = mean_abs_shap)) +
    geom_col(fill = "#2A6F97") + coord_flip() +
    labs(title = paste0("SHAP importance (XGBoost) - ", etiqueta),
         x = NULL, y = "mean(|SHAP|)") + theme_minimal(base_size = 11)
  ggsave(file.path(FIG_DIR, "paper", archivo_fig), g, width = 7, height = 5, dpi = 150)

  message("[70] ", etiqueta, " - top 10 por mean(|SHAP|):")
  for (i in seq_len(min(10, nrow(tab))))
    message(sprintf("     %-24s %.4f  %s", tab$feature[i], tab$mean_abs_shap[i], tab$direction[i]))
  tab
}

message("[70] SHAP PRE (clinical + blood count)...")
shap_xgb(datos$pre,  c(pr$clinicas, pr$hemo_pre), "pre",  "shap_importance_pre.png")
message("[70] SHAP EoTx (clinical + blood count)...")
shap_xgb(datos$post, c(pr$clinicas, pr$clinicas_post, pr$hemo_pre, pr$hemo_post), "eotx", "shap_importance_eotx.png")
message("[70] DONE. Tablas en ", dest, " ; figuras en ", file.path(FIG_DIR, "paper"))

######################################################################
## RESEARCH PROJECT: "Blood count parameters as early predictors of
## therapeutic outcome in cutaneous leishmaniasis"
## 33_hierarchical_lasso.R
## LASSO JERARQUICO (idea de D. Rebellon): en vez de un LASSO sobre las 47
## variables (que con 31 eventos se sobre-regulariza), se corre un LASSO DENTRO
## de cada CATEGORIA (clinicas / conteos / porcentajes / ratios / variaciones),
## se juntan los sobrevivientes, y un modelo FINAL (logistica) sobre ellos.
## TODA la seleccion (por categoria + final) va ANIDADA dentro de la CV -> sin fuga.
##
## INPUT : via utils/ml_data.R
## OUTPUT: outputs/30_lasso/hierarchical_cv_metrics.csv
######################################################################

source("R/00_config.R")
source("R/utils/validation_utils.R")
source("R/utils/ml_data.R")
suppressPackageStartupMessages({ library(glmnet); library(dplyr) })
dest <- out_dir("30_lasso")

datos <- cargar_datos_ml(); pr <- datos$predictores

## Asigna una CATEGORIA a cada columna de la matriz de diseno (por su nombre)
categoria <- function(cn) {
  ifelse(grepl("^pct_", cn), "percent",
  ifelse(grepl("^i_", cn), "ratio",
  ifelse(grepl("^var_|^mono_ratio", cn), "variation",
  ifelse(grepl("(Neutrofilos|Linfocitos|Monocitos|Eosinofilos|Basofilos|Granulocitos|Globulos_rojos|Hemoglobina|Hematocrito|Plaquetas)_(pre|post)$", cn), "count",
  "clinical"))))
}

## ---- Aprendiz: LASSO por categoria -> logistica final ----------------
fit_hier <- function(X, y, w) {
  cats <- categoria(colnames(X))
  sel <- integer(0)
  for (g in unique(cats)) {
    cols <- which(cats == g)
    if (length(cols) == 1) { sel <- c(sel, cols); next }      # 1 var: pasa directo
    cvg <- tryCatch(glmnet::cv.glmnet(X[, cols, drop = FALSE], y, family = "binomial",
                                      weights = w, alpha = 1, nfolds = 5, type.measure = "auc"),
                    error = function(e) NULL)
    if (is.null(cvg)) next
    co <- as.numeric(coef(cvg, s = "lambda.min"))[-1]          # sin intercepto
    sel <- c(sel, cols[co != 0])
  }
  sel <- unique(sel)
  if (length(sel) == 0) sel <- seq_len(ncol(X))               # fallback
  Xs <- X[, sel, drop = FALSE]
  # modelo FINAL: logistica sobre los sobrevivientes (si falla por separacion -> LASSO)
  final <- tryCatch({
    d <- data.frame(y = y, Xs); suppressWarnings(glm(y ~ ., data = d, family = binomial, weights = w))
  }, error = function(e) NULL)
  tipo <- "glm"
  if (is.null(final) || any(is.na(coef(final)))) {
    final <- glmnet::cv.glmnet(Xs, y, family = "binomial", weights = w, alpha = 1, nfolds = 5, type.measure = "auc")
    tipo <- "lasso"
  }
  list(model = final, cols = sel, tipo = tipo)
}
pred_hier <- function(m, X) {
  Xs <- X[, m$cols, drop = FALSE]
  if (m$tipo == "glm") as.numeric(predict(m$model, newdata = data.frame(Xs), type = "response"))
  else as.numeric(predict(m$model, newx = Xs, s = "lambda.min", type = "response"))
}

escenarios <- list(
  list(n = "PRE  clinical + blood count (all)", d = datos$pre,  v = c(pr$clinicas, pr$hemo_pre)),
  list(n = "EoTx clinical + blood count (all)", d = datos$post, v = c(pr$clinicas, pr$clinicas_post, pr$hemo_pre, pr$hemo_post))
)
REPS <- as.integer(Sys.getenv("CV_REPEATS_RUN", "15"))
filas <- list()
for (e in escenarios) {
  message("[33] CV LASSO jerarquico: ", e$n, " ...")
  res <- evaluar_cv(e$d, e$v, fit_hier, pred_hier, k = CV_FOLDS, repeticiones = REPS, balance = "weights")
  r <- res$resumen
  filas[[e$n]] <- data.frame(Scenario = e$n,
    ROC_AUC = round(r["ROC_AUC","media"],3), PR_AUC = round(r["PR_AUC","media"],3),
    Recall_fail = round(r["Recall_fail","media"],3), F2 = round(r["F2","media"],3))
}
tabla <- do.call(rbind, filas); rownames(tabla) <- NULL
write.csv(tabla, file.path(dest, "hierarchical_cv_metrics.csv"), row.names = FALSE)
message("\n[33] LASSO jerarquico — metricas CV:")
print(tabla, row.names = FALSE)
message("[33] DONE.")

######################################################################
## utils/validation_utils.R
## Motor de validacion compartido por los modelos de ML (LASSO/RF/XGB/SL).
## Principios acordados con D. Rebellon:
##  - PRIMERO se parte, LUEGO se balancea SOLO el entrenamiento (nunca el test).
##  - Imputacion dentro del fold, ajustada SOLO en entrenamiento y SIN el
##    desenlace (reproducible al llegar un paciente nuevo, sin fuga).
##  - Metricas centradas en la FALLA (clase minoritaria): PR-AUC, recall/
##    sensibilidad de falla, especificidad, PPV, F2, Brier, ROC-AUC.
##  - Validacion = CV estratificada repetida (y bootstrap de optimismo aparte).
## Este archivo NO se corre solo; lo cargan los scripts 30/31/32/50/60.
######################################################################

suppressPackageStartupMessages({
  library(pROC); library(PRROC); library(dplyr)
})

## ---- Metricas centradas en la FALLA ----------------------------------
## y_true: 0/1 (1 = falla). p_pred: probabilidad predicha de falla.
metricas_falla <- function(y_true, p_pred, umbral = 0.5) {
  roc_auc <- tryCatch(as.numeric(pROC::auc(pROC::roc(y_true, p_pred, quiet = TRUE,
                                                     levels = c(0, 1), direction = "<"))),
                      error = function(e) NA_real_)
  pr_auc <- tryCatch({
    pos <- p_pred[y_true == 1]; neg <- p_pred[y_true == 0]
    if (length(pos) == 0 || length(neg) == 0) NA_real_
    else PRROC::pr.curve(scores.class0 = pos, scores.class1 = neg)$auc.integral
  }, error = function(e) NA_real_)
  cls <- as.integer(p_pred >= umbral)
  tp <- sum(cls == 1 & y_true == 1); fp <- sum(cls == 1 & y_true == 0)
  fn <- sum(cls == 0 & y_true == 1); tn <- sum(cls == 0 & y_true == 0)
  recall <- if ((tp + fn) > 0) tp / (tp + fn) else NA        # sensibilidad de FALLA
  spec   <- if ((tn + fp) > 0) tn / (tn + fp) else NA
  ppv    <- if ((tp + fp) > 0) tp / (tp + fp) else NA
  f2     <- if (!is.na(ppv) && (ppv + recall) > 0) 5 * ppv * recall / (4 * ppv + recall) else NA
  brier  <- mean((p_pred - y_true)^2)
  data.frame(ROC_AUC = roc_auc, PR_AUC = pr_auc, Recall_fail = recall,
             Specificity = spec, PPV = ppv, F2 = f2, Brier = brier)
}

## ---- Imputacion ajustada SOLO en entrenamiento (sin desenlace) -------
## Numericas -> mediana del train; categoricas -> moda del train. Sin fuga.
imputar_train <- function(train, test, vars) {
  for (v in vars) {
    if (is.numeric(train[[v]])) {
      relleno <- stats::median(train[[v]], na.rm = TRUE)
    } else {
      tb <- sort(table(train[[v]]), decreasing = TRUE)
      relleno <- names(tb)[1]
    }
    train[[v]][is.na(train[[v]])] <- relleno
    test[[v]][is.na(test[[v]])]   <- relleno
  }
  list(train = train, test = test)
}

## ---- Pesos de clase (balanceo sin crear datos) -----------------------
## Peso inversamente proporcional a la frecuencia -> la falla "pesa" mas.
pesos_clase <- function(y) {
  n <- length(y); n1 <- sum(y == 1); n0 <- sum(y == 0)
  ifelse(y == 1, n / (2 * n1), n / (2 * n0))
}

## ---- Folds estratificados por el desenlace ---------------------------
folds_estratificados <- function(y, k) {
  idx <- integer(length(y))
  for (clase in unique(y)) {
    pos <- which(y == clase)
    pos <- sample(pos)
    idx[pos] <- rep(seq_len(k), length.out = length(pos))
  }
  idx
}

## ---- CV estratificada repetida para UN aprendiz bajo UN escenario ----
## datos: data.frame con 'fail01' (0/1) + predictores (pueden tener NA).
## fit_fun(x_mat, y, w) -> modelo ; pred_fun(modelo, x_mat) -> prob de falla.
## balance: "none" | "weights". (SMOTE se maneja en 40_balancing.)
evaluar_cv <- function(datos, predictores, fit_fun, pred_fun,
                       k = 10, repeticiones = 20, balance = "none",
                       umbral = 0.5, semilla = SEED) {
  set.seed(semilla)
  # niveles de factores fijos (columnas consistentes train/test)
  for (v in predictores) if (is.character(datos[[v]])) datos[[v]] <- factor(datos[[v]])
  form <- stats::as.formula(paste("~", paste(predictores, collapse = " + ")))
  y_all <- datos$fail01
  metr <- list()
  for (r in seq_len(repeticiones)) {
    fold <- folds_estratificados(y_all, k)
    for (f in seq_len(k)) {
      tr <- datos[fold != f, , drop = FALSE]
      te <- datos[fold == f, , drop = FALSE]
      imp <- imputar_train(tr, te, predictores)       # sin fuga, sin desenlace
      Xtr <- stats::model.matrix(form, imp$train)[, -1, drop = FALSE]
      Xte <- stats::model.matrix(form, imp$test)[,  -1, drop = FALSE]
      Xte <- Xte[, colnames(Xtr), drop = FALSE]        # mismas columnas
      ytr <- imp$train$fail01; yte <- imp$test$fail01
      w <- if (balance == "weights") pesos_clase(ytr) else rep(1, length(ytr))
      modelo <- fit_fun(Xtr, ytr, w)
      p <- pred_fun(modelo, Xte)
      metr[[length(metr) + 1]] <- metricas_falla(yte, p, umbral)
    }
  }
  M <- do.call(rbind, metr)
  resumen <- data.frame(t(sapply(M, function(col) c(media = mean(col, na.rm = TRUE),
                                                    de = stats::sd(col, na.rm = TRUE)))))
  list(resumen = resumen, por_fold = M)
}

## ---- Formateo compacto del resumen -----------------------------------
fmt_resumen <- function(res) {
  r <- res$resumen
  sapply(rownames(r), function(m) sprintf("%s=%.3f", m, r[m, "media"]))
}

######################################################################
## 40_balancing.R
## Class-imbalance sensitivity analysis (failures are the minority class,
## ~15.5%). We re-evaluate the headline pre-treatment logistic model
## (clinical + pre-Tx blood counts) under three training-set strategies:
##   (i)  none     -> no balancing (the primary analysis),
##   (ii) weights  -> inverse-frequency class weights,
##   (iii) SMOTE   -> synthetic minority oversampling (smotefamily),
## all inside repeated stratified 10-fold CV, balancing the TRAINING folds
## only (never the held-out fold). Purpose: show the conclusions do not hinge
## on how imbalance is handled.
## INPUT : via utils (raw ML frame, imputed inside each fold)
## OUTPUT: outputs/40_balancing/balancing_sensitivity.csv
######################################################################

source("R/00_config.R")                                   # paths, SEED, out_dir()
source("R/utils/ml_data.R")                               # cargar_datos_ml()
source("R/utils/validation_utils.R")                      # folds_estratificados(), imputar_train(), metricas_falla(), pesos_clase()
suppressPackageStartupMessages({ library(pROC); library(smotefamily) })  # ROC metric + SMOTE implementation
dest <- out_dir("40_balancing")                           # this step's output folder

datos    <- cargar_datos_ml()                             # prepared ML datasets (raw, with NA)
d_pre    <- datos$pre                                     # pre-treatment frame (n=200)
clin_pre <- c("edad","sexo","adenopatia_evalbas","infeccion_concom_evalbas")  # curated clinical set (same as 91)
predictores <- c(clin_pre, "Granulocitos_pre", "Plaquetas_pre")               # headline scenario: clinical + pre-Tx blood counts

fit_glm  <- function(X, y, w) suppressWarnings(glm(y ~ ., data = data.frame(y = y, X), family = binomial, weights = w))  # weighted logistic
pred_glm <- function(m, X) as.numeric(predict(m, newdata = data.frame(X), type = "response"))                            # prob of failure

## ---- repeated stratified CV for one balancing strategy ---------------------
evaluar_balance <- function(data, predictores, balance = "none", k = 10, reps = 15, seed = SEED) {
  set.seed(seed)                                          # reproducible folds across strategies
  for (v in predictores) if (is.character(data[[v]])) data[[v]] <- factor(data[[v]])  # fix factor types
  form <- as.formula(paste("~", paste(predictores, collapse = " + ")))                # design formula
  y_all <- data$fail01                                    # 0/1 outcome
  rows <- list()                                          # per-fold metric rows
  for (r in seq_len(reps)) {                              # repeat the whole CV
    fold <- folds_estratificados(y_all, k)                # stratified folds for this repetition
    for (f in seq_len(k)) {                               # loop folds
      tr <- data[fold != f, , drop = FALSE]; te <- data[fold == f, , drop = FALSE]    # train / held-out fold
      imp <- imputar_train(tr, te, predictores)           # impute within train only (no leakage)
      Xtr <- model.matrix(form, imp$train)[, -1, drop = FALSE]                         # train design matrix
      Xte <- model.matrix(form, imp$test )[, -1, drop = FALSE]                         # test design matrix
      Xte <- Xte[, colnames(Xtr), drop = FALSE]           # align columns
      ytr <- imp$train$fail01; yte <- imp$test$fail01     # train / test outcomes
      w <- rep(1, length(ytr))                            # default: equal weights
      if (balance == "weights") {                         # strategy (ii): inverse-frequency weights
        w <- pesos_clase(ytr)                             # failure cases weigh more
      } else if (balance == "smote") {                    # strategy (iii): SMOTE oversampling of the TRAIN fold
        sm <- tryCatch(smotefamily::SMOTE(as.data.frame(Xtr), as.character(ytr),      # synthesise minority (failure) rows
                                          K = 5)$data, error = function(e) NULL)
        if (!is.null(sm)) {                               # if SMOTE succeeded
          ycol <- ncol(sm)                                # last column holds the class label
          ytr <- as.integer(as.character(sm[[ycol]]))     # rebuilt (balanced) training outcome
          Xtr <- as.matrix(sm[, -ycol, drop = FALSE])     # rebuilt (balanced) training features
          w <- rep(1, length(ytr))                        # SMOTE balances by resampling, so weights stay equal
        }
      }
      m <- tryCatch(fit_glm(Xtr, ytr, w), error = function(e) NULL)                    # fit on the (possibly balanced) train
      if (is.null(m)) next                                # skip degenerate fold
      p <- tryCatch(pred_glm(m, Xte), error = function(e) NULL)                        # predict on the untouched test fold
      if (is.null(p)) next                                # skip if prediction failed
      rows[[length(rows) + 1]] <- metricas_falla(yte, p)  # failure-focused metrics on the held-out fold
    }
  }
  M <- do.call(rbind, rows)                               # stack fold metrics
  data.frame(strategy = balance,                          # mean over folds x repetitions
             ROC_AUC = round(mean(M$ROC_AUC, na.rm = TRUE), 3),
             PR_AUC  = round(mean(M$PR_AUC,  na.rm = TRUE), 3),
             Sens_fail = round(mean(M$Recall_fail, na.rm = TRUE), 3),
             Spec_fail = round(mean(M$Specificity, na.rm = TRUE), 3),
             Brier = round(mean(M$Brier, na.rm = TRUE), 3))
}

## ---- run the three strategies and save the comparison ----------------------
res <- do.call(rbind, lapply(c("none","weights","smote"),                 # evaluate each balancing strategy
                             function(b) evaluar_balance(d_pre, predictores, balance = b)))
write.csv(res, file.path(dest, "balancing_sensitivity.csv"), row.names = FALSE)         # save the sensitivity table
message("[40] Class-imbalance sensitivity (pre-treatment, clinical + blood count):")    # log header
print(res, row.names = FALSE)                                                           # echo the comparison
message("[40] DONE -> ", dest)                                                          # mark completion

######################################################################
## RESEARCH PROJECT: "Blood count parameters as early predictors of
## therapeutic outcome in cutaneous leishmaniasis"
## 91_master_metrics.R
## TABLA MAESTRA de desempeno (la "T5" del paper): para CADA modelo
## [logistica parsimoniosa, LASSO, random forest, XGBoost, SuperLearner]
## y CADA escenario [PRE/EoTx x clinico-solo / clinico+hemograma, y la
## combinacion pre+post], reporta fuera de muestra:
##   accuracy, ROC-AUC (IC95%), sensibilidad y especificidad para FALLA,
##   VPP, VPN, F1 y Brier, con umbral de Youden.
## Base learners: CV estratificada repetida (OOF agrupado por repeticion,
## umbral de Youden por repeticion, IC = percentiles entre repeticiones).
## SuperLearner: CV.SuperLearner (CV anidada) + bootstrap de las predicciones
## OOF para el IC. Metricas centradas en la clase FALLA.
##
## INPUT : via utils/ml_data.R
## OUTPUT: outputs/91_master_metrics/master_metrics.csv
######################################################################

source("R/00_config.R")
source("R/utils/validation_utils.R")
source("R/utils/ml_data.R")
suppressPackageStartupMessages({
  library(glmnet); library(ranger); library(xgboost)
  library(pROC); library(dplyr)
})
paso <- "91_master_metrics"; dest <- out_dir(paso)

REPS   <- as.integer(Sys.getenv("CV_REPEATS_RUN", "15"))
BBOOT  <- as.integer(Sys.getenv("BOOT_SL", "500"))

datos <- cargar_datos_ml()

## ---- Metricas completas centradas en FALLA, a un umbral dado ----------
metricas_full <- function(y, p, thr) {
  roc_auc <- tryCatch(as.numeric(pROC::auc(pROC::roc(y, p, quiet = TRUE,
                                            levels = c(0, 1), direction = "<"))),
                      error = function(e) NA_real_)
  cls <- as.integer(p >= thr)
  tp <- sum(cls == 1 & y == 1); fp <- sum(cls == 1 & y == 0)
  fn <- sum(cls == 0 & y == 1); tn <- sum(cls == 0 & y == 0)
  sens <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_   # sensibilidad FALLA
  spec <- if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
  ppv  <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
  npv  <- if ((tn + fn) > 0) tn / (tn + fn) else NA_real_
  f1   <- if (!is.na(ppv) && !is.na(sens) && (ppv + sens) > 0) 2 * ppv * sens / (ppv + sens) else NA_real_
  acc  <- (tp + tn) / length(y)
  brier <- mean((p - y)^2)
  c(Accuracy = acc, ROC_AUC = roc_auc, Sens_fail = sens, Spec_fail = spec,
    PPV = ppv, NPV = npv, F1 = f1, Brier = brier)
}

youden_thr <- function(y, p) {
  r <- tryCatch(pROC::roc(y, p, quiet = TRUE, levels = c(0, 1), direction = "<"),
                error = function(e) NULL)
  if (is.null(r)) return(0.5)
  co <- tryCatch(as.numeric(pROC::coords(r, "best", best.method = "youden",
                                         ret = "threshold", transpose = FALSE)[1, 1]),
                 error = function(e) 0.5)
  if (length(co) == 0 || is.na(co) || is.infinite(co)) 0.5 else co
}

## ---- Aprendices (misma config que 50/60) ------------------------------
fit_glm   <- function(X,y,w){ d<-data.frame(y=y,X); suppressWarnings(glm(y~.,data=d,family=binomial,weights=w)) }
pred_glm  <- function(m,X) as.numeric(predict(m,newdata=data.frame(X),type="response"))
fit_lasso <- function(X,y,w) glmnet::cv.glmnet(X,y,family="binomial",weights=w,alpha=1,nfolds=5,type.measure="auc")
pred_lasso<- function(m,X) as.numeric(predict(m,newx=X,s="lambda.min",type="response"))
fit_rf    <- function(X,y,w) ranger::ranger(x=as.data.frame(X),y=factor(y,levels=c(0,1)),probability=TRUE,
                                            num.trees=1000,min.node.size=10,case.weights=w,respect.unordered.factors="order")
pred_rf   <- function(m,X) predict(m,as.data.frame(X))$predictions[,"1"]
fit_xgb   <- function(X,y,w){ dt<-xgboost::xgb.DMatrix(X,label=y,weight=w)
  xgboost::xgb.train(list(objective="binary:logistic",eval_metric="auc",max_depth=2,eta=0.05,
                          subsample=0.8,colsample_bytree=0.6,min_child_weight=5,lambda=1,alpha=0.5),dt,nrounds=60,verbose=0) }
pred_xgb  <- function(m,X) predict(m,xgboost::xgb.DMatrix(X))

aprendices <- list(
  list(n="Parsimonious logistic", f=fit_glm,   p=pred_glm),
  list(n="LASSO",                 f=fit_lasso, p=pred_lasso),
  list(n="Random Forest",         f=fit_rf,    p=pred_rf),
  list(n="XGBoost",               f=fit_xgb,   p=pred_xgb)
)

## ---- CV repetida que AGRUPA las predicciones OOF por repeticion -------
## Devuelve, por repeticion, la fila de metricas (umbral de Youden propio).
eval_base <- function(df, vars, fit_fun, pred_fun, k = CV_FOLDS, reps = REPS, semilla = SEED) {
  set.seed(semilla)
  for (v in vars) if (is.character(df[[v]])) df[[v]] <- factor(df[[v]])
  form <- stats::as.formula(paste("~", paste(vars, collapse = " + ")))
  y_all <- df$fail01
  filas <- vector("list", reps)
  for (r in seq_len(reps)) {
    fold <- folds_estratificados(y_all, k)
    oof  <- rep(NA_real_, nrow(df))
    for (f in seq_len(k)) {
      tr <- df[fold != f, , drop = FALSE]; te <- df[fold == f, , drop = FALSE]
      imp <- imputar_train(tr, te, vars)
      Xtr <- stats::model.matrix(form, imp$train)[, -1, drop = FALSE]
      Xte <- stats::model.matrix(form, imp$test)[,  -1, drop = FALSE]
      Xte <- Xte[, colnames(Xtr), drop = FALSE]
      m <- tryCatch(fit_fun(Xtr, imp$train$fail01, rep(1, nrow(Xtr))), error = function(e) NULL)
      if (!is.null(m)) oof[fold == f] <- tryCatch(pred_fun(m, Xte), error = function(e) NA_real_)
    }
    ok <- !is.na(oof)
    thr <- youden_thr(y_all[ok], oof[ok])
    filas[[r]] <- metricas_full(y_all[ok], oof[ok], thr)
  }
  M <- do.call(rbind, filas)
  data.frame(
    Accuracy  = mean(M[,"Accuracy"],  na.rm=TRUE),
    ROC_AUC   = mean(M[,"ROC_AUC"],   na.rm=TRUE),
    AUC_lo    = as.numeric(quantile(M[,"ROC_AUC"], .025, na.rm=TRUE)),
    AUC_hi    = as.numeric(quantile(M[,"ROC_AUC"], .975, na.rm=TRUE)),
    Sens_fail = mean(M[,"Sens_fail"], na.rm=TRUE),
    Spec_fail = mean(M[,"Spec_fail"], na.rm=TRUE),
    PPV       = mean(M[,"PPV"],       na.rm=TRUE),
    NPV       = mean(M[,"NPV"],       na.rm=TRUE),
    F1        = mean(M[,"F1"],        na.rm=TRUE),
    Brier     = mean(M[,"Brier"],     na.rm=TRUE)
  )
}

## ---- SuperLearner: OOF via CV.SuperLearner + bootstrap para el IC -----
imputar_todo <- function(d, vars) {
  for (v in vars) {
    if (is.numeric(d[[v]])) d[[v]][is.na(d[[v]])] <- stats::median(d[[v]], na.rm = TRUE)
    else { m <- names(sort(table(d[[v]]), decreasing = TRUE))[1]; d[[v]][is.na(d[[v]])] <- m }
  }
  d
}
eval_sl <- function(df, vars) {
  suppressPackageStartupMessages(library(SuperLearner))
  SL.xgb.shallow <- function(...) SL.xgboost(..., ntrees = 60, max_depth = 2, shrinkage = 0.05, minobspernode = 5)
  assign("SL.xgb.shallow", SL.xgb.shallow, envir = .GlobalEnv)
  biblioteca <- c("SL.glm", "SL.glmnet", "SL.ranger", "SL.xgb.shallow")
  df <- imputar_todo(df, vars)
  for (v in vars) if (is.character(df[[v]])) df[[v]] <- factor(df[[v]])
  X <- as.data.frame(stats::model.matrix(stats::as.formula(paste("~", paste(vars, collapse="+"))), df)[, -1, drop=FALSE])
  Y <- df$fail01
  set.seed(SEED)
  cvsl <- CV.SuperLearner(Y = Y, X = X, family = binomial(), SL.library = biblioteca,
                          method = "method.AUC", cvControl = list(V = 10), verbose = FALSE)
  p <- as.numeric(cvsl$SL.predict)
  thr <- youden_thr(Y, p)
  punto <- metricas_full(Y, p, thr)
  ## bootstrap de las predicciones OOF para IC
  set.seed(SEED)
  bb <- t(replicate(BBOOT, {
    idx <- sample(seq_along(Y), replace = TRUE)
    metricas_full(Y[idx], p[idx], thr)
  }))
  data.frame(
    Accuracy  = punto["Accuracy"], ROC_AUC = punto["ROC_AUC"],
    AUC_lo    = as.numeric(quantile(bb[,"ROC_AUC"], .025, na.rm=TRUE)),
    AUC_hi    = as.numeric(quantile(bb[,"ROC_AUC"], .975, na.rm=TRUE)),
    Sens_fail = punto["Sens_fail"], Spec_fail = punto["Spec_fail"],
    PPV = punto["PPV"], NPV = punto["NPV"], F1 = punto["F1"], Brier = punto["Brier"]
  )
}

## ---- Escenarios (sets CURADOS, igual que 50/60) + combinacion pre+post
clin_pre  <- c("edad", "sexo", "adenopatia_evalbas", "infeccion_concom_evalbas")
clin_post <- c("edad", "sexo", "infeccion_concom_ftto", "adenopatia_evalbas")
escenarios <- list(
  list(key="PRE clinical only",            df="pre",  vars=clin_pre),
  list(key="PRE clinical + blood count",   df="pre",  vars=c(clin_pre, "Granulocitos_pre","Plaquetas_pre")),
  list(key="EoTx clinical only",           df="post", vars=clin_post),
  list(key="EoTx clinical + blood count",  df="post", vars=c(clin_post, "pct_eosino_post","mono_ratio")),
  list(key="Combined PRE+EoTx blood count",df="post", vars=c(clin_post, "Granulocitos_pre","Plaquetas_pre","pct_eosino_post","mono_ratio"))
)

res <- list()
for (sc in escenarios) {
  D <- datos[[sc$df]]
  for (a in aprendices) {
    message(sprintf("[91] %s | %s ...", sc$key, a$n))
    m <- tryCatch(eval_base(D, sc$vars, a$f, a$p), error = function(e){ message("   ! ", conditionMessage(e)); NULL })
    if (!is.null(m)) res[[length(res)+1]] <- cbind(Scenario=sc$key, Model=a$n, m)
  }
  message(sprintf("[91] %s | SuperLearner ...", sc$key))
  m <- tryCatch(eval_sl(D, sc$vars), error = function(e){ message("   ! SL ", conditionMessage(e)); NULL })
  if (!is.null(m)) res[[length(res)+1]] <- cbind(Scenario=sc$key, Model="SuperLearner", m)
}

tabla <- do.call(rbind, res)
num <- sapply(tabla, is.numeric)
tabla[num] <- lapply(tabla[num], function(x) round(x, 3))
write.csv(tabla, file.path(dest, "master_metrics.csv"), row.names = FALSE)
message("\n[91] TABLA MAESTRA guardada en ", file.path(dest, "master_metrics.csv"))
print(tabla, row.names = FALSE)
message("[91] DONE.")

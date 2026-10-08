######################################################################
## RESEARCH PROJECT: "Blood count parameters as early predictors of
## therapeutic outcome in cutaneous leishmaniasis"
## 50_models_split_validation.R
## Comparacion MAESTRA de prediccion (CV estratificada repetida fuera de muestra):
## modelo parsimonioso (logistica) vs LASSO/RF/XGBoost, sobre el set CURADO
## (clinico + biomarcadores SELECCIONADOS) y el set COMPLETO. Responde: con las
## variables correctas, el ML iguala a la regresion? el hemograma agrega?
## Metricas centradas en falla (ROC-AUC y PR-AUC, independientes del umbral).
##
## INPUT : via utils/ml_data.R ; outputs/{30,31,32}/*_cv_metrics.csv (set completo)
## OUTPUT: outputs/50_models_split_validation/master_comparison.csv
######################################################################

source("R/00_config.R")
source("R/utils/validation_utils.R")
source("R/utils/ml_data.R")
suppressPackageStartupMessages({ library(glmnet); library(ranger); library(xgboost); library(dplyr) })
paso <- "50_models_split_validation"; dest <- out_dir(paso)

datos <- cargar_datos_ml()

## ---- Set CURADO: clinico + biomarcadores seleccionados ---------------
clin_pre_cur  <- c("edad", "sexo", "adenopatia_evalbas", "infeccion_concom_evalbas")
cur_pre_clin  <- clin_pre_cur
cur_pre_both  <- c(clin_pre_cur, "Granulocitos_pre", "Plaquetas_pre")        # + IG, plaquetas
clin_post_cur <- c("edad", "sexo", "infeccion_concom_ftto", "adenopatia_evalbas")
cur_post_clin <- clin_post_cur
cur_post_both <- c(clin_post_cur, "pct_eosino_post", "mono_ratio")           # + eos%, razon monocitos

## ---- Aprendices ------------------------------------------------------
fit_glm <- function(X,y,w){ d<-data.frame(y=y,X); suppressWarnings(glm(y~.,data=d,family=binomial,weights=w)) }
pred_glm<- function(m,X) as.numeric(predict(m,newdata=data.frame(X),type="response"))
fit_lasso<-function(X,y,w) glmnet::cv.glmnet(X,y,family="binomial",weights=w,alpha=1,nfolds=5,type.measure="auc")
pred_lasso<-function(m,X) as.numeric(predict(m,newx=X,s="lambda.min",type="response"))
fit_rf  <- function(X,y,w) ranger::ranger(x=as.data.frame(X),y=factor(y,levels=c(0,1)),probability=TRUE,
                                          num.trees=1000,min.node.size=10,case.weights=w,respect.unordered.factors="order")
pred_rf <- function(m,X) predict(m,as.data.frame(X))$predictions[,"1"]
fit_xgb <- function(X,y,w){ dt<-xgboost::xgb.DMatrix(X,label=y,weight=w)
  xgboost::xgb.train(list(objective="binary:logistic",eval_metric="auc",max_depth=2,eta=0.05,
                          subsample=0.8,colsample_bytree=0.6,min_child_weight=5,lambda=1,alpha=0.5),dt,nrounds=60,verbose=0) }
pred_xgb<- function(m,X) predict(m,xgboost::xgb.DMatrix(X))

aprendices <- list(
  list(n="Parsimonious logistic", f=fit_glm,   p=pred_glm),
  list(n="LASSO",                 f=fit_lasso, p=pred_lasso),
  list(n="Random Forest",         f=fit_rf,    p=pred_rf),
  list(n="XGBoost",               f=fit_xgb,   p=pred_xgb)
)

REPS <- as.integer(Sys.getenv("CV_REPEATS_RUN", "15"))
corre <- function(datos_df, vars, etiqueta) {
  do.call(rbind, lapply(aprendices, function(a) {
    res <- evaluar_cv(datos_df, vars, a$f, a$p, k=CV_FOLDS, repeticiones=REPS, balance="none")
    data.frame(Scenario=etiqueta, Model=a$n,
               ROC_AUC=round(res$resumen["ROC_AUC","media"],3),
               PR_AUC =round(res$resumen["PR_AUC","media"],3))
  }))
}

message("[50] Corriendo set CURADO (clinico + biomarcadores seleccionados)...")
tabla <- rbind(
  corre(datos$pre,  cur_pre_clin,  "CURATED PRE  clinical only"),
  corre(datos$pre,  cur_pre_both,  "CURATED PRE  clinical + blood count"),
  corre(datos$post, cur_post_clin, "CURATED EoTx clinical only"),
  corre(datos$post, cur_post_both, "CURATED EoTx clinical + blood count")
)
write.csv(tabla, file.path(dest, "master_comparison.csv"), row.names = FALSE)

message("\n[50] COMPARACION MAESTRA — set CURADO (ROC-AUC fuera de muestra):")
for (sc in unique(tabla$Scenario)) {
  message("  ", sc)
  t <- tabla[tabla$Scenario==sc,]
  for (i in seq_len(nrow(t))) message(sprintf("     %-22s ROC-AUC=%.3f  PR-AUC=%.3f", t$Model[i], t$ROC_AUC[i], t$PR_AUC[i]))
}
message("[50] DONE. ", file.path(dest, "master_comparison.csv"))

######################################################################
## Robustness of the headline pre-treatment model (clinical + blood count):
##  (a) stratified 70:30 hold-out, (b) 500-bootstrap optimism correction,
##  (c) calibration of the cross-validated out-of-fold probabilities.
## These complement the repeated CV above and are the "validation" part of
## this step. (We rely on the likelihood-ratio test for the added value of
## blood counts, so no AUC confidence-interval comparison is reported here.)
######################################################################
suppressPackageStartupMessages(library(pROC))             # ROC/AUC utilities
vars_hl <- cur_pre_both                                   # headline predictor set: clinical + pre-Tx blood counts
form_hl <- as.formula(paste("~", paste(vars_hl, collapse = " + ")))  # its design formula
aucf <- function(y, p) as.numeric(pROC::auc(pROC::roc(y, p, quiet = TRUE, levels = c(0,1), direction = "<")))  # AUC helper

## (a) stratified 70:30 hold-out -------------------------------------------
set.seed(SEED)                                            # reproducible split
dpre <- datos$pre; y <- dpre$fail01                       # pre-treatment data + outcome
tr_idx <- unlist(lapply(unique(y), function(c){           # take 70% within each outcome class (stratified)
  idx <- which(y == c); sample(idx, round(0.70 * length(idx))) }))
train <- dpre[tr_idx, , drop = FALSE]; test <- dpre[-tr_idx, , drop = FALSE]  # 70/30 partition
imp <- imputar_train(train, test, vars_hl)                # impute using TRAIN only (no leakage)
Xtr <- model.matrix(form_hl, imp$train)[, -1, drop = FALSE]                   # train design matrix
Xte <- model.matrix(form_hl, imp$test )[, -1, drop = FALSE]; Xte <- Xte[, colnames(Xtr), drop = FALSE]  # aligned test matrix
mh <- fit_glm(Xtr, imp$train$fail01, rep(1, nrow(Xtr)))   # fit logistic on the 70% train
ph <- pred_glm(mh, Xte)                                   # predict on the 30% test
thr <- as.numeric(pROC::coords(pROC::roc(imp$train$fail01, pred_glm(mh, Xtr), quiet=TRUE,       # Youden threshold from TRAIN only
        levels=c(0,1), direction="<"), "best", best.method="youden", ret="threshold", transpose=FALSE)[1,1])
cls <- as.integer(ph >= thr); yt <- imp$test$fail01       # classify the test set at that threshold
holdout <- data.frame(n_train = nrow(train), n_test = nrow(test),
                      AUC = round(aucf(yt, ph), 3),
                      Sens_fail = round(sum(cls==1 & yt==1)/sum(yt==1), 3),
                      Spec_fail = round(sum(cls==0 & yt==0)/sum(yt==0), 3))
write.csv(holdout, file.path(dest, "holdout_70_30.csv"), row.names = FALSE)   # save hold-out result

## (b) 500-bootstrap optimism correction (Harrell/Efron) --------------------
fit_on  <- function(d){ im <- imputar_train(d, d, vars_hl); X <- model.matrix(form_hl, im$train)[,-1,drop=FALSE]  # fit on a dataset
                        list(m = fit_glm(X, im$train$fail01, rep(1,nrow(X))), cols = colnames(X)) }
pred_on <- function(fo, d){ im <- imputar_train(d, d, vars_hl); X <- model.matrix(form_hl, im$train)[,-1,drop=FALSE]  # predict on a dataset
                            X <- X[, fo$cols, drop=FALSE]; pred_glm(fo$m, X) }
full <- fit_on(dpre); app_auc <- aucf(dpre$fail01, pred_on(full, dpre))       # apparent (optimistic) AUC on full data
set.seed(SEED); B <- 500; opt <- numeric(B)               # bootstrap optimism estimates
for (b in seq_len(B)) {                                   # Efron-Harrell optimism loop
  idx <- sample(nrow(dpre), replace = TRUE)               # a bootstrap resample
  bf <- fit_on(dpre[idx,,drop=FALSE])                     # refit on the resample
  opt[b] <- aucf(dpre$fail01[idx], pred_on(bf, dpre[idx,,drop=FALSE])) -       # AUC on the resample ...
            aucf(dpre$fail01,      pred_on(bf, dpre))      # ... minus AUC of that model on the original data
}
boot <- data.frame(apparent_AUC = round(app_auc,3), optimism = round(mean(opt,na.rm=TRUE),3),
                   optimism_corrected_AUC = round(app_auc - mean(opt,na.rm=TRUE),3), B = B)
write.csv(boot, file.path(dest, "bootstrap_optimism.csv"), row.names = FALSE) # save optimism-corrected AUC

## (c) calibration of the repeated-CV out-of-fold probabilities -------------
set.seed(SEED); REPSC <- 15; acc <- matrix(NA_real_, nrow(dpre), REPSC)       # patients x repetitions OOF prob matrix
dd <- dpre; for (v in vars_hl) if (is.character(dd[[v]])) dd[[v]] <- factor(dd[[v]])  # fix factor types
for (r in seq_len(REPSC)) {                               # repeated CV to collect OOF probabilities
  fold <- folds_estratificados(dd$fail01, CV_FOLDS)       # stratified folds
  for (f in seq_len(CV_FOLDS)) {                          # loop folds
    tr <- dd[fold!=f,,drop=FALSE]; te <- dd[fold==f,,drop=FALSE]; im <- imputar_train(tr, te, vars_hl)  # impute within train
    Xtr <- model.matrix(form_hl, im$train)[,-1,drop=FALSE]; Xte <- model.matrix(form_hl, im$test)[,-1,drop=FALSE]
    Xte <- Xte[, colnames(Xtr), drop=FALSE]               # align columns
    mm <- tryCatch(fit_glm(Xtr, im$train$fail01, rep(1,nrow(Xtr))), error=function(e) NULL)  # fit
    if (!is.null(mm)) acc[fold==f, r] <- pred_glm(mm, Xte)# store OOF probs
  }
}
poof <- rowMeans(acc, na.rm = TRUE)                       # mean OOF prob per patient
pc <- pmin(pmax(poof, 1e-6), 1-1e-6)                      # clamp away from 0/1 for the logit
cal <- data.frame(                                        # calibration summary
  calibration_slope     = round(unname(coef(glm(dd$fail01 ~ qlogis(pc), family=binomial))[2]), 3),  # slope (ideal 1)
  calibration_intercept = round(unname(coef(glm(dd$fail01 ~ offset(qlogis(pc)), family=binomial))[1]), 3),  # calibration-in-the-large
  Brier        = round(mean((poof - dd$fail01)^2), 3),    # Brier of OOF probs
  Brier_noinfo = round(mean((mean(dd$fail01) - dd$fail01)^2), 3))  # no-information Brier (prevalence)
write.csv(cal, file.path(dest, "calibration.csv"), row.names = FALSE)         # save calibration metrics

message("[50] hold-out 70:30 -> AUC ", holdout$AUC, " (sens ", holdout$Sens_fail, ", spec ", holdout$Spec_fail, ")")
message("[50] optimism-corrected AUC ", boot$optimism_corrected_AUC, " (apparent ", boot$apparent_AUC, ")")
message("[50] calibration slope ", cal$calibration_slope, " ; Brier ", cal$Brier, " vs no-info ", cal$Brier_noinfo)

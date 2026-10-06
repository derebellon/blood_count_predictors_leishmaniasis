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
